%% @doc
%% An operation-based Observed-Remove Set CRDT.
%% As the data structure is operation-based, to issue an operation, one should
%% firstly call `generate_downstream/3' to get the downstream version of the 
%% operation and then call `update/2'.
%%
%% It provides five operations: add, which adds an element to a set; add_all,
%% adds a list of elements to a set; remove, which removes an element from a set;
%% remove_all that removes a list of elements from the set; update, that contains
%% a list of previous four commands.
%%
%% This file is adapted from riak_dt_orset, a state-based implementation of 
%% Observed-Remove Set.
%% The changes are as follows:
%% 1. `generate_downstream/3' is added, as this is necessary for op-based CRDTs.
%% 2. `merge/2' is removed.
%% 3. There is no tombstone of removed elements.
%%
%% @reference Marc Shapiro, Nuno Preguiça, Carlos Baquero, Marek Zawirski (2011) A comprehensive study of
%% Convergent and Commutative Replicated Data Types. http://hal.upmc.fr/inria-00555588/
%%
%% @end
-module(crdt_orset).

%% API
-export([new/0, value/1, generate_downstream/3, update/2, equal/2,
         to_binary/1, from_binary/1, value/2, precondition_context/1, stats/1, stat/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([orset/0, binary_orset/0, orset_op/0]).
-opaque orset() :: orddict:orddict().

-type binary_orset() :: binary(). %% A binary that from_binary/1 will operate on.

-type orset_op() :: {add, member()} | {remove, member()} |
                    {add_all, [member()]} | {remove_all, [member()]} |
                    {update, [orset_op()]}.

-type actor() :: riak_dt:actor().
-type member() :: term().

-spec new() -> orset().
new() ->
    orddict:new().

%% @doc   
%% without parameter: return all existing elements in the `orset()'  
%% {fragment, elem}: create and return a new `orset()' with all metadata 
%% of an element 
%% {tokens, elem}: returns all uniques tokens of an element in the set
-spec value(orset()) -> [member()].
value(ORDict) ->
    orddict:fetch_keys(ORDict).

-spec value(any(), orset()) -> [member()] | orddict:orddict().
value({fragment, Elem}, ORSet) ->
    case value({tokens, Elem}, ORSet) of
        [] ->
            orddict:new();
        Tokens ->
            orddict:store(Elem, Tokens, orddict:new())
    end;
value({tokens, Elem}, ORSet) ->
    case orddict:find(Elem, ORSet) of
        error ->
            orddict:new();
        {ok, Tokens} ->
            Tokens
    end;
value(_,ORSet) ->
    value(ORSet).

%% @doc generate downstream operations. 
%% If the operation is add or add_all, generate unique tokens for each element
%% If the operation is remove or remove_all, fetches all unique tokens for 
%% these elements existing in the `orset()'.
-spec generate_downstream(orset_op(), actor(), orset()) -> {ok, orset_op()}.
generate_downstream({add,Elem}, Actor, _ORDict) ->
    Token = unique(Actor),
    {ok, {add, {Elem,[Token]}}};
generate_downstream({add_all,Elems}, Actor, _ORDict0) ->
    DownstreamOp = lists:foldl(fun(Elem, Sum) ->
                                Token = unique(Actor),
                                Sum++[{Elem, [Token]}]
                                end, [], Elems),
    {ok, {add_all, DownstreamOp}};
generate_downstream({remove, Elem}, _Actor, ORDict) ->
    ToRemove = value({tokens, Elem}, ORDict),
    {ok, {remove, {Elem, ToRemove}}};
generate_downstream({remove_all,Elems}, _Actor, ORDict) ->
    ToRemove = lists:foldl(fun(Elem, Sum) -> Sum++[{Elem, value({tokens, Elem}, ORDict)}] end, [], Elems),
    {ok, {remove_all, ToRemove}}.


%% @doc apply downstream operations and update an `orset()'.
%% The first parameter denotes this operation is for adding or removing elements.
%% For add or add_all, the second element of the tuple is a list of elements and tokens to add
%% For remove or remove_all, the second element of the tuple is a list of elements and their tokens
%% to remove.
%% For update, the second element of the tuple is a list of updates to apply, each of which can
%% either be add, add_all or remove, remove_all. 
-spec update(orset_op(), orset()) -> {ok, orset()} |
                                              {error, {precondition ,{not_present, member()}}}.
update({add, {Elem, [Token|_]}}, ORDict) ->
    add_elem(Elem,Token,ORDict);
update({add_all,Elems}, ORDict0) ->
    OD = lists:foldl(fun(Elem,ORDict) ->
                {ok, ORDict1} = update({add,Elem},ORDict),
                ORDict1
            end, ORDict0, Elems),
    {ok, OD};
update({remove, Elem}, ORDict) ->
    remove_elem(Elem, ORDict);
update({remove_all,Elems}, ORDict0) ->
    remove_elems(Elems, ORDict0);
update({update, Ops}, ORDict) ->
    apply_ops(Ops, ORDict).


-spec equal(orset(), orset()) -> boolean().
equal(ORDictA, ORDictB) ->
    ORDictA == ORDictB. % Everything inside is ordered, so this should work

%% @doc the precondition context is a fragment of the CRDT that
%% operations with pre-conditions can be applied too.  In the case of
%% OR-Sets this is the set of adds observed.  The system can then
%% apply a remove to this context and merge it with a replica.
%% Especially useful for hybrid op/state systems where the context of
%% an operation is needed at a replica without sending the entire
%% state to the client.
-spec precondition_context(orset()) -> orset().
precondition_context(ORDict) ->
    orddict:fold(fun(Elem, Tokens, ORDict1) ->
            case minimum_tokens(Tokens) of
                []      -> ORDict1;
                Tokens1 -> orddict:store(Elem, Tokens1, ORDict1)
            end
        end, orddict:new(), ORDict).

-spec stats(orset()) -> [{atom(), number()}].
stats(ORSet) ->
    [ {S, stat(S, ORSet)} || S <- [element_count] ].

-spec stat(atom(), orset()) -> integer() | undefined.
stat(element_count, ORSet) ->
    orddict:size(ORSet);
stat(_, _) -> undefined.

-include("../deps/riak_dt/include/riak_dt_tags.hrl").
-define(TAG, ?DT_ORSET_TAG).
-define(V1_VERS, 1).

-spec to_binary(orset()) -> binary_orset().
to_binary(ORSet) ->
    %% @TODO something smarter
    <<?TAG:8/integer, ?V1_VERS:8/integer, (term_to_binary(ORSet))/binary>>.

from_binary(<<?TAG:8/integer, ?V1_VERS:8/integer, Bin/binary>>) ->
    %% @TODO something smarter
    binary_to_term(Bin).

%% Private
%% @doc add an element and its token to the `orset()'.
add_elem(Elem,Token,ORDict) ->
    case orddict:find(Elem,ORDict) of
        {ok, Tokens} ->
            case lists:member(Token, Tokens) of
                true ->
                    {ok, ORDict};
                false ->
                    {ok, orddict:store(Elem, Tokens++[Token], ORDict)}
            end;
        error ->
            {ok, orddict:store(Elem, [Token], ORDict)}
    end.

%% @doc remove all tokens of the element from the `orset()'.
remove_elem({Elem,RemoveTokens},ORDict) ->
    case orddict:find(Elem,ORDict) of
        {ok, Tokens} ->
            RestTokens = Tokens--RemoveTokens,
            case RestTokens of 
                [] ->
                    {ok, orddict:erase(Elem, ORDict)};
                _ -> 
                    {ok, orddict:store(Elem, Tokens--RemoveTokens, ORDict)}
            end;
        error ->
            case RemoveTokens of 
                [] ->
                    {ok, ORDict};
                _ ->
                    {error, {precondition, {not_present, Elem}}}
            end
    end.

remove_elems([], ORDict) ->
    {ok, ORDict};
remove_elems([Elem|Rest], ORDict) ->
    case remove_elem(Elem,ORDict) of
        {ok, ORDict1} -> remove_elems(Rest, ORDict1);
        Error         -> Error
    end.

apply_ops([], ORDict) ->
    {ok, ORDict};
apply_ops([Op | Rest], ORDict) ->
    case update(Op, ORDict) of
        {ok, ORDict1} -> apply_ops(Rest, ORDict1);
        Error -> Error
    end.

%% @doc generate a unique identifier (best-effort).
unique(_Actor) ->
    crypto:strong_rand_bytes(20).

minimum_tokens(Tokens) ->
    orddict:filter(fun(_Token, Removed) ->
            not Removed
        end, Tokens).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
new_test() ->
    ?assertEqual(orddict:new(), new()). 

add_test() ->
    Set1 = new(),
    {ok, DownstreamOp1} = generate_downstream({add, <<"foo">>}, 1, Set1),
    ?assertMatch({add, {<<"foo">>, _}}, DownstreamOp1),
    {ok, DownstreamOp2} = generate_downstream({add_all, [<<"li">>,<<"manu">>]}, 1, Set1),
    ?assertMatch({add_all, [{<<"li">>, _}, {<<"manu">>, _}]}, DownstreamOp2),
    {ok, Set2} = update(DownstreamOp1, Set1),
    {_, Elem1} = DownstreamOp1,
    ?assertEqual([Elem1], orddict:to_list(Set2)),
    {ok, Set3} = update(DownstreamOp2, Set1),
    {_, Elems2} = DownstreamOp2,
    ?assertEqual(Elems2, orddict:to_list(Set3)).

value_test() ->
    Set1 = new(),
    {ok, DownstreamOp1} = generate_downstream({add, <<"foo">>}, 1, Set1),
    ?assertEqual([], value(Set1)),
    {ok, Set2} = update(DownstreamOp1, Set1),
    ?assertEqual([<<"foo">>], value(Set2)),
    {ok, DownstreamOp2} = generate_downstream({add_all, [<<"foo">>, <<"li">>,<<"manu">>]}, 1, Set2),
    {ok, Set3} = update(DownstreamOp2, Set2),
    ?assertEqual([<<"foo">>, <<"li">>, <<"manu">>], value(Set3)),

    {_, {_, Token1}}=DownstreamOp1,
    {_, [{_, Token2}|_]}=DownstreamOp2,
    ?assertEqual(Token1, value({tokens, <<"foo">>}, Set2)),
    ?assertEqual(Token1++Token2, value({tokens, <<"foo">>}, Set3)),

    ?assertEqual(orddict:store(<<"foo">>, Token1++Token2, orddict:new()), value({fragment, <<"foo">>}, Set3)).

remove_test() ->
    Set1 = new(),
    %% Add an element then remove it
    {ok, Op1} = generate_downstream({add, <<"foo">>}, 1, Set1),
    {ok, Set2} = update(Op1, Set1),
    ?assertEqual([<<"foo">>], value(Set2)),
    {ok, Op2} = generate_downstream({remove, <<"foo">>}, 1, Set2),
    {ok, Set3} = update(Op2, Set2),
    ?assertEqual([], value(Set3)),

    %% Add many elements then remove part
    {ok, Op3} = generate_downstream({add_all, [<<"foo">>, <<"li">>,<<"manu">>]}, 1, Set1),
    {ok, Set4} = update(Op3, Set1),
    ?assertEqual([<<"foo">>, <<"li">>, <<"manu">>], value(Set4)),

    {ok, Op5} = generate_downstream({remove_all, [<<"foo">>, <<"li">>]},1 , Set4),
    {ok, Set5} = update(Op5, Set4),
    ?assertEqual([<<"manu">>], value(Set5)),
    
    %% Remove more than current have
    {ok, Op6} = generate_downstream({add_all, [<<"foo">>, <<"li">>,<<"manu">>]}, 1, Set1),
    {ok, Set6} = update(Op6, Set1),
    {ok, Op7} = generate_downstream({remove_all, [<<"manu">>, <<"test">>]}, 1, Set6),
    Result = update(Op7, Set6),
    ?assertMatch({ok, _}, Result).

    
concurrent_add_test() ->
    Set1 = new(),
    %% Add an element then remove it
    {ok, Op1} = generate_downstream({add, <<"foo">>}, 1, Set1),
    {ok, Set2} = update(Op1, Set1),
    ?assertEqual([<<"foo">>], value(Set2)),
    
    %% If remove is concurrent with the second add, will not remove the second added 
    {ok, Op2} = generate_downstream({remove, <<"foo">>}, 1, Set2),

    {ok, Op3} = generate_downstream({add, <<"foo">>}, 1, Set1),
    {ok, Set3} = update(Op3, Set2),
    ?assertEqual([<<"foo">>], value(Set3)),

    {ok, Set4} = update(Op2, Set3),
    ?assertEqual([<<"foo">>], value(Set4)),

    %% If remove follows two adds, remove will remove all
    {ok, Op4} = generate_downstream({remove, <<"foo">>}, 1, Set3),
    {ok, Set5} = update(Op4, Set3),
    ?assertEqual([], value(Set5)).
    
binary_test() ->
    ORSet1 = new(),
    BinaryORSet1 = to_binary(ORSet1),
    ORSet2 = from_binary(BinaryORSet1),
    ?assert(equal(ORSet1, ORSet2)),

    {ok, Op1} = generate_downstream({add, <<"foo">>}, 1, ORSet1),
    {ok, ORSet3} = update(Op1, ORSet1),
    BinaryORSet3 = to_binary(ORSet3),
    ORSet4 = from_binary(BinaryORSet3),
    ?assert(equal(ORSet3, ORSet4)).

-endif.
