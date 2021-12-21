#!/usr/bin/env escript

-define(IPADDR, {127,0,0,1}).
-define(ANTIDOTE_PORT, 8087).
-define(CODEMOVING_PORT, 8089).
-define(SetKey, <<"set">>).
-define(Bucket, <<"bucket">>).

-export([
	test_store_module/0
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").

load(Dep) ->
    Path = filename:dirname(escript:script_name()) ++ "./toto/codemoving/_build/default/lib/" ++ Dep ++ "/ebin",
    case code:add_pathz(Path) of
        true ->
            true;
        Err ->
            erlang:error({could_not_load, Path, Err})
    end.

test_store_module() ->
	{ok, Pid} = antidotec_pb_socket:start(?IPADDR, ?ANTIDOTE_PORT),
	ClientID = codemoving:connect(?IPADDR, ?CODEMOVING_PORT, Pid),
	{ok, _} = codemoving:store_module(ClientID, hello),
	{ok, Result} = codemoving:rpc(ClientID, hello, hello, []),
	io:format(Result),
	_Disconnected = antidotec_pb_socket:stop(Pid),
	codemoving:disconnect(ClientID).

create_set(AntidoteSocket) ->
	Set = {?SetKey, antidote_crdt_set_aw, ?Bucket},
    {ok, Tx3} = antidotec_pb:start_transaction(AntidoteSocket, ignore, {}),
    antidotec_pb:update_objects(AntidoteSocket, [{Set, add, <<"2">>}, {Set, add, <<"7">>}, {Set, add, <<"8">>}], Tx3),
    antidotec_pb:update_objects(AntidoteSocket, [{Set, add, <<"7">>}, {Set, add, <<"11">>}], Tx3),
    {ok, _} = antidotec_pb:commit_transaction(AntidoteSocket, Tx3).

test_read_set(AntidoteSocket) ->
	Set = {?SetKey, antidote_crdt_set_aw, ?Bucket},
	ClientID = codemoving:connect(?IPADDR, ?CODEMOVING_PORT, AntidoteSocket),
	{ok, Tx3} = codemoving:start_transaction(ClientID, ignore, {}, ignore),
    {ok, [Val]} = codemoving:read_objects(ClientID, [Set], Tx3),
    {ok, _} = codemoving:commit_transaction(ClientID, Tx3),
    io:format("Value read: ~p~n", [antidotec_set:value(Val)]),
	codemoving:disconnect(ClientID).

 %   ExpectedRes = {map, [
 %     {{<<"a">>, antidote_crdt_register_mv}, [<<"42">>]},
 %     {{<<"b">>, antidote_crdt_register_lww}, <<"X">>},
 %     {{<<"c">>, antidote_crdt_register_mv}, [<<"Paul">>]},
 %     {{<<"d">>, antidote_crdt_set_aw}, [<<"Apple">>, <<"Banana">>]},
 %     {{<<"e">>, antidote_crdt_set_rw}, [<<"Apple">>, <<"Banana">>]},
 %     {{<<"f">>, antidote_crdt_counter_pn}, 7},
 %     {{<<"g">>, antidote_crdt_map_go}, [
 %       {{<<"x">>, antidote_crdt_register_mv}, [<<"17">>]}
 %     ]},
 %     {{<<"h">>, antidote_crdt_map_rr}, [
 %       {{<<"x">>, antidote_crdt_register_mv}, [<<"15">>]}
 %     ]}
 %   ]},
 %   antidotec_pb:read_objects(AntidoteSocket, [Set], Tx3),
 %   ?assertEqual(ExpectedRes, Val),


main(_) ->
    [load(Dep) || Dep <- ["codemoving", "riak_pb", "antidote_pb", "protobuffs", "antidote_crdt"]],
    {ok, AntidoteSocket} = antidotec_pb_socket:start(?IPADDR, ?ANTIDOTE_PORT),
	test_store_module(),
	%create_set(AntidoteSocket),
	%test_read_set(AntidoteSocket),
    _Disconnected = antidotec_pb_socket:stop(AntidoteSocket).    