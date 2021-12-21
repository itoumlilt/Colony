
%% @doc module antidote_crdt_register_lww - An operation based last-writer-wins register
%% Each operation is assigned a timestamp, which is guaranteed to be greater than
%% the current timestamp.
%% The current value of the register is the value assigned with the greatest timestamp
%% or the empty binary if there was no assignment yet.

-module(antidote_crdt_register_lww).

-behaviour(antidote_crdt).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([ new/0,
          value/1,
          downstream/2,
          update/2,
          equal/2,
          to_binary/1,
          from_binary/1,
          is_operation/1,
          require_state_downstream/1
        ]).

-type antidote_crdt_register_lww() :: {non_neg_integer(), term()}.

-type antidote_crdt_register_lww_op() :: {assign, term(), non_neg_integer()}  | {assign, term()}.

new() ->
    {0, <<>>}.

value({_Time, Val}) ->
    Val.

-spec downstream(antidote_crdt_register_lww_op(), antidote_crdt_register_lww()) -> {ok, term()}.
downstream({assign, Value, Time}, {OldTime, _OldValue}) ->
    {ok, {max(Time, OldTime + 1), Value}};
downstream({assign, Value}, State) ->
    downstream({assign, Value, make_micro_epoch()}, State).

make_micro_epoch() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.


update(Effect, State) ->
    % take the state with maximum time, if times are equal use maximum state
    {ok, max(Effect, State)}.


require_state_downstream(_Operation) -> true.

is_operation({assign, _Value}) -> true;
is_operation({assign, _Value, _Time}) -> true;
is_operation(_Other) -> false.


equal(CRDT1, CRDT2) ->
    CRDT1 == CRDT2.

to_binary(CRDT) ->
    erlang:term_to_binary(CRDT).

from_binary(Bin) ->
    {ok, erlang:binary_to_term(Bin)}.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(test).

all_test() ->
    S0 = new(),
    {ok, Downstream} = downstream({assign, a}, S0),
    {ok, S1} = update(Downstream, S0),
    ?assertEqual(a, value(S1)).

-endif.
