-module(epaxos_types).

% What can be proposed
-type payload() :: term().

% Executed by learner when the value is learnt
-type learn_fun() :: fun((payload()) -> ok).

-export_type([payload/0, learn_fun/0]).
