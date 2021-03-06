%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(materializer).
-include("antidote.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([create_snapshot/1,
         update_snapshot/3,
         materialize_eager/3,
         check_operations/1,
         check_operation/1
        ]).

%% @doc Creates an empty CRDT
-spec create_snapshot(type()) -> snapshot().
create_snapshot(Type) ->
    Type:new().

%% @doc Applies an operation to a snapshot of a crdt. 
%%      This function yields an error if the crdt does not have a corresponding update operation.
-spec update_snapshot(type(), snapshot(), op()) -> {ok, snapshot()} | {error, reason()}.
update_snapshot(Type, Snapshot, Op) ->
    try
        Type:update(Op, Snapshot)
    catch
        Class:Exception ->
            {error, {materializer_unexpected_format, {Class, Exception}}}
    end.

%% @doc Applies updates in given order without any checks, errors are simply propagated.
-spec materialize_eager(type(), snapshot(), [op()]) -> snapshot() | {error, reason()}.
materialize_eager(_Type, Snapshot, []) ->
    Snapshot;
materialize_eager(Type, Snapshot, [Op | Rest]) ->
    case update_snapshot(Type, Snapshot, Op) of
        {error, Reason} ->
            {error, Reason};
        {ok, Result} ->
            materialize_eager(Type, Result, Rest)
    end.


%% @doc Check that in a list of operations, all of them are correctly typed.
-spec check_operations(list()) -> ok | {error, {type_check, term()}}.
check_operations([]) ->
    ok;
check_operations([Op | Rest]) ->
    case check_operation(Op) of
        true ->
            check_operations(Rest);
        false ->
            {error, {type_check, Op}}
    end.

%% @doc Check that an operation is correctly typed.
-spec check_operation(term()) -> boolean().
check_operation(Op) ->
    case Op of
        {update, {_, Type, Update}} ->
            (antidote_crdt:is_type(Type)) andalso
                Type:is_operation(Update);
        {read, {_, Type}} ->
            (antidote_crdt:is_type(Type));
        _ ->
            false
    end.

-ifdef(TEST).

%% @doc Testing update with pn_counter.
update_pncounter_test() ->
    Type = antidote_crdt_counter,
    Counter = create_snapshot(Type),
    ?assertEqual(0, Type:value(Counter)),
    Op = {increment, 1},
    {ok, Counter2} = update_snapshot(Type, Counter, Op),
    ?assertEqual(1, Type:value(Counter2)).

%% @doc Testing pn_counter with update log
materializer_counter_withlog_test() ->
    Type = antidote_crdt_counter,
    Counter = create_snapshot(Type),
    ?assertEqual(0, Type:value(Counter)),
    Ops = [{increment, 1},
           {increment, 1},
           {increment, 2},
           {increment, 3}
          ],
    Counter2 = materialize_eager(Type, Counter, Ops),
    ?assertEqual(7, Type:value(Counter2)).

%% @doc Testing counter with empty update log
materializer_counter_emptylog_test() ->
    Type = antidote_crdt_counter,
    Counter = create_snapshot(Type),
    ?assertEqual(0, Type:value(Counter)),
    Ops = [],
    Counter2 = materialize_eager(Type, Counter, Ops),
    ?assertEqual(0, Type:value(Counter2)).

%% @doc Testing non-existing crdt
materializer_error_nocreate_test() ->
    ?assertException(error, undef, create_snapshot(bla)).

%% @doc Testing crdt with invalid update operation
materializer_error_invalidupdate_test() ->
    Type = antidote_crdt_counter,
    Counter = create_snapshot(Type),
    ?assertEqual(0, Type:value(Counter)),
    Ops = [{non_existing_op_type, {non_existing_op, actor1}}],
    ?assertEqual({error,{materializer_unexpected_format,{error, function_clause}}}, materialize_eager(Type, Counter, Ops)).

%% @doc Testing that the function check_operations works properly
check_operations_test() ->
    Operations =
        [{read, {key1, antidote_crdt_counter}},
         {update, {key1, antidote_crdt_counter, increment}}
        ],
    ?assertEqual(ok, check_operations(Operations)),

    Operations2 = [{read, {key1, antidote_crdt_counter}},
        {update, {key1, antidote_crdt_counter, {{add, elem}, a}}},
        {update, {key2, antidote_crdt_counter, {increment, a}}},
        {read, {key1, antidote_crdt_counter}}],
    ?assertMatch({error, _}, check_operations(Operations2)).

-endif.
