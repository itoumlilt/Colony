-module(paxos_learner).

-behaviour(gen_server).

%% API
-export([learn/2]).

%% gen_server API
-export([init/1, handle_info/2, handle_call/3, handle_cast/2,
        terminate/2, code_change/3]).

-record(state, {
        num_acceptors :: non_neg_integer(),

        % {N, Value} -> no. of acceptors that gave this pair
        learns = orddict:new() :: orddict:orddict(),

        % Executed when value is learnt
        learn_fun :: epaxos_types:learn_fun()
    }).


%% =============================================================================
%% API
%% =============================================================================

learn(ServerRef, {N, Val}) ->
    gen_server:cast(ServerRef, {learn, N, Val}).


%% =============================================================================
%% gen_server API
%% =============================================================================

init([NumAcceptors, LearnFun]) ->
    {ok, #state{num_acceptors=NumAcceptors, learn_fun=LearnFun}}.

handle_cast({learn, N, Val}, #state{
            learns=Learns,
            num_acceptors=NumAcceptors,
            learn_fun=Fun}=State) ->
    NewLearns = orddict:update_counter({N, Val}, 1, Learns),
    case orddict:fetch({N, Val}, NewLearns) of
        Num when Num > NumAcceptors div 2 ->
            Fun(Val),
            {stop, normal, State};
        _ ->
            {noreply, State#state{learns=NewLearns}}
    end.

handle_info(_, State) -> {stop, bad_info, State}.
handle_call(_, _, State) -> {stop, bad_call, State}.
terminate(_, _) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
