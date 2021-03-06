-module(eiger_updatetx_coord_fsm).

-behavior(gen_fsm).

-include("antidote.hrl").

-define(VECTORCLOCK, vectorclock).

%% API
-export([start_link/5]).

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).

%% States
-export([scatter_updates/2,
         gather_prepare/2,
         send_commit/2,
         wait_for_commit/3,
         gather_commit/2]).

-record(state, {
          from,
          vnode,
          updates,
          deps,
          debug,
          transaction,
          scattered_updates,
          commit_time,
          ack,
          servers}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Vnode, From, Updates, Deps, Debug) ->
    %lager:info("Starting up fsm ~w", [Updates]),
    gen_fsm:start_link(?MODULE, [Vnode, From, Updates, Deps, Debug], []).

%%%===================================================================
%%% States
%%%===================================================================

%% @doc Initialize the state.
init([Vnode, From, Updates, Deps, Debug]) ->
    %lager:info("Vnode got updates ~w", [Updates]),
    {ok, SnapshotTime} = clocksi_interactive_tx_coord_fsm:get_snapshot_time(),
    DcId = dc_utilities:get_my_dc_id(),
    LocalClock = ?VECTORCLOCK:get_clock_of_dc(DcId, SnapshotTime),
    TransactionId = #tx_id{snapshot_time=LocalClock, server_pid=self()},
    Transaction = #transaction{snapshot_time=LocalClock,
                               vec_snapshot_time=none,
                               txn_id=TransactionId},
    SD = #state{
            vnode=Vnode,
            from=From,
            debug=Debug,
            deps=Deps,
            transaction=Transaction,
            updates=Updates
           },
    {ok, scatter_updates, SD, 0}.

scatter_updates(timeout, SD0=#state{vnode=Vnode, updates=Updates, transaction=Transaction}) ->
    %lager:info("Before scattering updates ~w", [Updates]),
    ScatteredUpdates = lists:foldl(fun(Update, Dict0) ->
                                    {Key, _Type, _Param} = Update,
                                    Preflist = log_utilities:get_preflist_from_key(Key),
                                    IndexNode = hd(Preflist),
                                    dict:append(IndexNode, Update, Dict0)
                                   end, dict:new(), Updates),
    {ok, Clock} = eiger_vnode:get_clock(Vnode), 
    lists:foreach(fun(Slice) ->
                    {IndexNode, ListUpdates} = Slice,
                    Keys = [Key || {Key, _Type, _Param} <- ListUpdates],
                    eiger_vnode:prepare(IndexNode, Transaction, Clock, Keys)
                  end, dict:to_list(ScatteredUpdates)),
    Servers = length(dict:to_list(ScatteredUpdates)),
    %lager:info("Scattered updates to ~w", [Servers]),
    {next_state, gather_prepare, SD0#state{scattered_updates=ScatteredUpdates, servers=Servers, ack=0, commit_time=0}}.

gather_prepare({prepared, Clock}, SD0=#state{vnode=Vnode, servers=Servers, ack=Ack0, from=From, debug=Debug, commit_time=CommitTime0}) ->
    ok = eiger_vnode:update_clock(Vnode, Clock),
    CommitTime = max(CommitTime0, Clock),
    Ack = Ack0 + 1,
    %lager:info("Preparing OK"),
    case Ack of
        Servers ->
            case Debug of
                debug ->
                    %lager:info("Waiting for commit"),
                    riak_core_vnode:reply(From, {ok, self()}),
                    {next_state, wait_for_commit, SD0#state{ack=0, commit_time=CommitTime}};
                undefined ->
                    {next_state, send_commit, SD0#state{ack=0, commit_time=CommitTime},0}
            end;
        _ ->
            {next_state, gather_prepare, SD0#state{ack=Ack, commit_time=CommitTime}}
    end.

wait_for_commit(commit, Sender, SD0) ->
    %lager:info("Got commit"),
    {next_state, send_commit, SD0#state{from=Sender}, 0}.

send_commit(timeout, SD0=#state{scattered_updates=ScatteredUpdates, deps=Deps, 
                transaction=Transaction, commit_time=CommitTime, updates=Updates, servers=Servers}) ->
    DcId = dc_utilities:get_my_dc_id(),
    lists:foreach(fun(Slice) ->
                    {IndexNode, ListUpdates} = Slice,
                    eiger_vnode:commit(IndexNode, Transaction, ListUpdates, {Deps, Servers}, {DcId, CommitTime}, CommitTime, length(Updates))
                  end, dict:to_list(ScatteredUpdates)),
    {next_state, gather_commit, SD0}.

gather_commit({committed, Clock}, SD0=#state{vnode=Vnode, scattered_updates=_ScatteredUpdates, servers=Servers, ack=Ack0, from=From, commit_time=CommitTime, debug=Debug}) ->
    ok = eiger_vnode:update_clock(Vnode, Clock),
    Ack = Ack0 + 1,
    DcId = dc_utilities:get_my_dc_id(),
    case Ack of
        Servers ->
            case Debug of
                debug ->
                    gen_fsm:reply(From, {ok, {DcId, CommitTime}});
                undefined ->
                    riak_core_vnode:reply(From, {ok, {DcId, CommitTime}})
            end,
            {stop, normal, SD0#state{ack=Ack}};
        _ ->
            {next_state, gather_commit, SD0#state{ack=Ack}}
    end. 

handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    ok.
