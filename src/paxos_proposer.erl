-module(paxos_proposer).

-include("include/paxos.hrl").

-behaviour(gen_server).

%% API
-export([propose/2, promise/3]).

%% gen_server API
-export([init/1, handle_info/2, handle_call/3, handle_cast/2,
        terminate/2, code_change/3]).

-record(state, {
        acceptors :: list(pid()),

        % number of promises from acceptors
        promises = 0 :: non_neg_integer(),

        init_n :: non_neg_integer(),

        % number of running round
        n :: non_neg_integer(),

        % Value that might be proposed in phase2
        val :: {non_neg_integer(), term()}
    }).

%% =============================================================================
%% API
%% =============================================================================

%% @doc Executed by application in order to propose a value
-spec propose(pid(), epaxos_types:payload()) -> ok.
propose(ServerRef, Value) ->
    gen_server:cast(ServerRef, {propose, Value}).

%% @doc Called by acceptor on promise that it will not accept any proposals
%% numbered less than N (our call), with the HighestAcceptedVal.
-spec promise(pid(), non_neg_integer(), {non_neg_integer(), term()}) -> ok.
promise(ServerRef, PropN, HighestAcceptedVal) ->
    gen_server:cast(ServerRef, {promise, PropN, HighestAcceptedVal}).

%% =============================================================================
%% gen_fsm API
%% =============================================================================

init([N, Acceptors]) ->
    {ok, #state{init_n=N, n=N, acceptors=Acceptors}}.

% Phase1
handle_cast({propose, Val},
        #state{init_n=InitN, n=N, acceptors=Acceptors}=State) ->
    NewN = (N div ?PAXOS_MOD + 1) * ?PAXOS_MOD + InitN,
    lists:foreach(
        fun(Acceptor) ->
                paxos_acceptor:prepare(Acceptor, NewN, self())
        end,
        Acceptors
    ),
    {noreply, State#state{n=NewN, val={0, Val}, promises=0}};

% Phase2
handle_cast({promise, N, HighestAccepted},
        #state{n=N, promises=Promises, val=Val}=State) ->
    {HighestAcceptedN, HighestAcceptedVal} = HighestAccepted,
    {_, PrevN} = Val,
    NewVal = if
        HighestAcceptedN > PrevN andalso HighestAcceptedVal =/= undefined ->
            HighestAccepted;
        true ->
            Val
    end,
    accept_if_majority(State#state{promises=Promises+1, val=NewVal});

handle_cast({promise, N, Val}, #state{n=CurrN}=State) ->
    lager:debug("Old promise: ~p, ~p, current N: ~p", [N, Val, CurrN]),
    {noreply, State}.

handle_info(_, State) -> {stop, bad_info, State}.
handle_call(_, _, State) -> {stop, bad_call, State}.
terminate(_, _) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% =============================================================================
%% Helpers
%% =============================================================================

accept_if_majority(#state{promises=Promises, acceptors=Acceptors, n=N,
        val={_, Val}}=State) when Promises > length(Acceptors) div 2 ->
    lists:foreach(
        fun(Acceptor) ->
                paxos_acceptor:accept(Acceptor, N, Val)
        end,
        Acceptors
    ),
    {noreply, State};

accept_if_majority(State) ->
    {noreply, State}.
