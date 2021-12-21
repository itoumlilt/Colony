-module(epaxos).

-export([start_learners/3, start_proposers/2, start_acceptors/2]).

-spec start_acceptors(non_neg_integer(), [pid()]) -> [pid()].
start_acceptors(NumAcceptors, Learners) ->
    lists:map(
        fun(_) ->
                {ok, Pid} = gen_server:start_link(paxos_acceptor,
                    [Learners], []),
                Pid
        end,
        lists:seq(1, NumAcceptors)
    ).

-spec start_proposers(non_neg_integer(), [pid()]) -> [pid()].
start_proposers(NumProposers, Acceptors) ->
    lists:map(
        fun(N) ->
                {ok, Pid} = gen_server:start_link(paxos_proposer,
                    [N, Acceptors], []),
                Pid
        end,
        lists:seq(1, NumProposers)
    ).

-spec start_learners(non_neg_integer(), [pid()], epaxos_types:learn_fun()) ->
    [pid()].
start_learners(NumLearners, NumAcceptors, LearnFun) ->
    lists:map(
        fun(_) ->
                {ok, Pid} = gen_server:start(paxos_learner,
                    [NumAcceptors, LearnFun], []),
                Pid
        end,
        lists:seq(1, NumLearners)
    ).
