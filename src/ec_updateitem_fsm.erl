-module(ec_updateitem_fsm).

-behavior(gen_fsm).

-include("antidote.hrl").

%% API
-export([start_link/3]).

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).

%% States
-export([update_item/2]).

-record(state,
        {partition, vclock, coordinator}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Coordinator, Tx, Vclock) ->
    gen_fsm:start_link(?MODULE, [Coordinator, Tx, Vclock], []).

%%%===================================================================
%%% States
%%%===================================================================

init([Coordinator, VecSnapshotTime, Partition]) ->
    SD = #state{partition=Partition, coordinator=Coordinator,
                vclock=VecSnapshotTime},
    {ok, check_clock, SD, 0}.

%% @doc simply finishes the fsm.
update_item(timeout, SD0=#state{coordinator=Coordinator}) ->
    riak_core_vnode:reply(Coordinator, ok),
    {stop, normal, SD0}.

handle_info(_Info, _StateName, StateData) ->
    {stop, badmsg, StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop, badmsg, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, badmsg, StateData}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

