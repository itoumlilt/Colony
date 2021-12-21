-module(epaxos_eunit).

-include_lib("eunit/include/eunit.hrl").

-define(NUM_PROPOSERS, 3).
-define(NUM_ACCEPTORS, 3).
-define(NUM_LEARNERS, 3).

setup() ->
    error_logger:tty(false),
    ok = application:start(syntax_tools),
    ok = application:start(compiler),
    ok = application:start(lager),
    lager:set_loglevel(lager_console_backend, debug).

shutdown(_) ->
    ok = application:stop(lager),
    ok = application:stop(compiler),
    ok = application:stop(syntax_tools).

basic_test_() ->
    {setup,
        fun setup/0,
        fun shutdown/1,
        [
            fun basic_one_value/0
        ]
    }.

basic_one_value() ->
    Self = self(),
    LearnFun = fun(Val) ->
            lager:info("learned ~p~n", [Val]),
            Self ! {learnt, Val}
    end,
    Learners = epaxos:start_learners(?NUM_LEARNERS, ?NUM_ACCEPTORS, LearnFun),
    Acceptors = epaxos:start_acceptors(?NUM_ACCEPTORS, Learners),
    [P1, P2, P3] = epaxos:start_proposers(?NUM_PROPOSERS, Acceptors),
    paxos_proposer:propose(P1, a1),
    paxos_proposer:propose(P2, a2),
    paxos_proposer:propose(P3, a3),
    paxos_proposer:propose(P1, a4),
    paxos_proposer:propose(P2, a5),
    paxos_proposer:propose(P3, a6),
    timer:sleep(30),
    paxos_proposer:propose(P1, a7),
    Stuff = gather_stuff([], ?NUM_LEARNERS),
    % Assert only 1 value is learnt
    ?assertEqual(1, length(lists:usort(Stuff))).

gather_stuff(Acc, 0) ->
    Acc;
gather_stuff(Acc, N) ->
    receive
        {learnt, Val} ->
            gather_stuff([Val|Acc], N-1)
    end.
