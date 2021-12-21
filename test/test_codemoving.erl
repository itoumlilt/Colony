-module(test_codemoving).


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").

-define(IPADDR, {127,0,0,1}).
-define(ANTIDOTE_PORT, 8087).
-define(CODEMOVING_PORT, 8089).
-define(SetKey, <<"set">>).
-define(Bucket, <<"bucket">>).

-export([
	test_read_set/0
]).

-export([%% suite/0,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0]).



end_per_suite(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

all() -> [test_store_module].

load(Dep) ->
    Path = filename:dirname(escript:script_name()) ++ "./_build/default/lib/" ++ Dep ++ "/ebin",
    case code:add_pathz(Path) of
        true ->
            true;
        Err ->
            erlang:error({could_not_load, Path, Err})
    end.

store_module_test() ->
	{ok, Pid} = antidotec_pb_socket:start(?IPADDR, ?ANTIDOTE_PORT),
	ClientID = codemoving:connect(?IPADDR, ?CODEMOVING_PORT, Pid),
	{ok, Res1} = codemoving:store_module(ClientID, hello),
	?assertEqual(Res1, "ok"),
	{ok, Res2} = codemoving:rpc(ClientID, hello, hello, []),
	_Disconnected = antidotec_pb_socket:stop(Pid),
	codemoving:disconnect(ClientID),
	?assertEqual(Res2, "Hello World").

create_set(AntidoteSocket) ->
	Set = {?SetKey, antidote_crdt_set_aw, ?Bucket},
    {ok, Tx3} = antidotec_pb:start_transaction(AntidoteSocket, ignore, {}),
    antidotec_pb:update_objects(AntidoteSocket, [{Set, add, <<"2">>}, {Set, add, <<"7">>}, {Set, add, <<"8">>}], Tx3),
    antidotec_pb:update_objects(AntidoteSocket, [{Set, add, <<"7">>}, {Set, add, <<"11">>}], Tx3),
    {ok, _} = antidotec_pb:commit_transaction(AntidoteSocket, Tx3).

test_read_set() ->
	AntidoteSocket = antidotec_pb_socket:start(?IPADDR, ?ANTIDOTE_PORT),
	create_set(AntidoteSocket),
	Set = {?SetKey, antidote_crdt_set_aw, ?Bucket},
	ClientID = codemoving:connect(?IPADDR, ?CODEMOVING_PORT, AntidoteSocket),
	{ok, Tx3} = codemoving:start_transaction(ClientID, ignore, {}, ignore),
    {ok, [Val]} = codemoving:read_objects(ClientID, [Set], Tx3),
    {ok, _} = codemoving:commit_transaction(ClientID, Tx3),
    io:format("Value read: ~p~n", [antidotec_set:value(Val)]),
	codemoving:disconnect(ClientID),
	antidotec_pb_socket:stop(AntidoteSocket).

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
