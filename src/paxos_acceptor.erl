-module(paxos_acceptor).

-include("include/paxos.hrl").

-behaviour(gen_server).

%% API
-export([prepare/3, accept/3]).

%% gen_server API
-export([init/1, handle_info/2, handle_call/3, handle_cast/2,
        terminate/2, code_change/3]).

-record(state, {
        learners :: list(pid()),

        % Highest numbered prepare responded to. 0 = nothing
        prepn = 0 :: non_neg_integer(),

        % Highest numbered proposal accepted. 0 = nothing.
        propn = 0 :: non_neg_integer(),

        % Value of propn
        valn :: term()
    }).

%% =============================================================================
%% API
%% =============================================================================

prepare(ServerRef, NewN, Caller) ->
    gen_server:cast(ServerRef, {prepare, NewN, Caller}).

accept(ServerRef, NewN, NewVal) ->
    gen_server:cast(ServerRef, {accept, NewN, NewVal}).

%% =============================================================================
%% gen_server API
%% =============================================================================

init([Learners]) ->
    {ok, #state{learners=Learners}}.

handle_cast({prepare, N, Pid}, #state{propn=PropN, prepn=PrepN,
        valn=ValN}=State) when N > PrepN ->
    paxos_proposer:promise(Pid, N, {PropN, ValN}),
    {noreply, State#state{prepn=N}};

%% @TODO notify proposer to stop the round
handle_cast({prepare, N, _Pid}, #state{prepn=PrepN}=State) when N =< PrepN ->
    {noreply, State};

handle_cast({accept, NewN, NewVal}, #state{prepn=N, learners=Learners}=State)
        when NewN >= N ->
    lists:foreach(
        fun(Learner) ->
                paxos_learner:learn(Learner, {NewN, NewVal})
        end,
        Learners
    ),
    {noreply, State#state{propn=NewN, valn=NewVal}};

handle_cast({accept, NewN, _NewVal}, #state{prepn=N}=State) when NewN < N ->
    {noreply, State}.

handle_info(_, State) -> {stop, bad_info, State}.
handle_call(_, _, State) -> {stop, bad_call, State}.
terminate(_, _) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
