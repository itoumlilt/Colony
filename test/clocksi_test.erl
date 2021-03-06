%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
% This file is provided to you under the Apache License,
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
-module(clocksi_test).

-export([confirm/0, clocksi_test1/1, clocksi_test2/1, clocksi_test3/1, clocksi_test5/1]).

-include_lib("eunit/include/eunit.hrl").
-define(HARNESS, (rt_config:get(rt_harness))).

confirm() ->
    [Nodes] = rt:build_clusters([3]),
    lager:info("Nodes: ~p", [Nodes]),
    clocksi_test1(Nodes),
    clocksi_test2(Nodes),
    clocksi_test3(Nodes),
    clocksi_test5(Nodes),
    clocksi_tx_noclock_test(Nodes),
    clocksi_single_key_update_read_test(Nodes),
    clocksi_multiple_key_update_read_test(Nodes),
    clocksi_test4 (Nodes),
    clocksi_multiple_read_update_test(Nodes),
    clocksi_concurrency_test(Nodes),
    rt:clean_cluster(Nodes),
    pass.

%% @doc The following function tests that ClockSI can run a non-interactive tx
%%      that updates multiple partitions.
clocksi_test1(Nodes) ->
    FirstNode = hd(Nodes),
    lager:info("Test1 started"),
    Type = riak_dt_pncounter,
    %% Empty transaction works,
    Result0=rpc:call(FirstNode, antidote, clocksi_execute_tx,
                    [[]]),
    ?assertMatch({ok, _}, Result0),
    Result1=rpc:call(FirstNode, antidote, clocksi_execute_tx,
                    [[]]),
    ?assertMatch({ok, _}, Result1),

    % A simple read returns empty
    Result11=rpc:call(FirstNode, antidote, clocksi_execute_tx,
                    [
                     [{read, key1, Type}]]),
    ?assertMatch({ok, _}, Result11),
    {ok, {_, ReadSet11, _}}=Result11, 
    ?assertMatch([0], ReadSet11),

    %% Read what you wrote
    Result2=rpc:call(FirstNode, antidote, clocksi_execute_tx,
                    [
                      [{read, key1, Type},
                      {update, key1, Type, {increment, a}},
                      {update, key2, Type, {increment, a}},
                      {read, key1, Type}]]),
    ?assertMatch({ok, _}, Result2),
    {ok, {_, ReadSet2, _}}=Result2, 
    ?assertMatch([0,1], ReadSet2),

    %% Update is persisted && update to multiple keys are atomic
    Result3=rpc:call(FirstNode, antidote, clocksi_execute_tx,
                    [
                     [{read, key1, Type},
                      {read, key2, Type}]]),
    ?assertMatch({ok, _}, Result3),
    {ok, {_, ReadSet3, _}}=Result3,
    ?assertEqual([1,1], ReadSet3),

    %% Multiple updates to a key in a transaction works
    Result5=rpc:call(FirstNode, antidote, clocksi_execute_tx,
                    [
                     [{update, key1, Type, {increment, a}},
                      {update, key1, Type, {increment, a}}]]),
    ?assertMatch({ok,_}, Result5),

    Result6=rpc:call(FirstNode, antidote, clocksi_execute_tx,
                    [
                     [{read, key1, Type}]]),
    {ok, {_, ReadSet6, _}}=Result6,
    ?assertEqual(3, hd(ReadSet6)),
    pass.

%% @doc The following function tests that ClockSI can run an interactive tx.
%%      that updates multiple partitions.
clocksi_test2(Nodes) ->
    FirstNode = hd(Nodes),
    lager:info("Test2 started"),
    Type = riak_dt_pncounter,
    {ok,TxId}=rpc:call(FirstNode, antidote, clocksi_istart_tx, []),
    ReadResult0=rpc:call(FirstNode, antidote, clocksi_iread,
                         [TxId, abc, riak_dt_pncounter]),
    ?assertEqual({ok, 0}, ReadResult0),
    WriteResult=rpc:call(FirstNode, antidote, clocksi_iupdate,
                         [TxId, abc, Type, {increment, 4}]),
    ?assertEqual(ok, WriteResult),
    ReadResult=rpc:call(FirstNode, antidote, clocksi_iread,
                        [TxId, abc, riak_dt_pncounter]),
    ?assertEqual({ok, 1}, ReadResult),
    WriteResult1=rpc:call(FirstNode, antidote, clocksi_iupdate,
                          [TxId, bcd, Type, {increment, 4}]),
    ?assertEqual(ok, WriteResult1),
    ReadResult1=rpc:call(FirstNode, antidote, clocksi_iread,
                         [TxId, bcd, riak_dt_pncounter]),
    ?assertEqual({ok, 1}, ReadResult1),
    WriteResult2=rpc:call(FirstNode, antidote, clocksi_iupdate,
                          [TxId, cde, Type, {increment, 4}]),
    ?assertEqual(ok, WriteResult2),
    ReadResult2=rpc:call(FirstNode, antidote, clocksi_iread,
                         [TxId, cde, riak_dt_pncounter]),
    ?assertEqual({ok, 1}, ReadResult2),
    CommitTime=rpc:call(FirstNode, antidote, clocksi_iprepare, [TxId]),
    ?assertMatch({ok, _}, CommitTime),
    End=rpc:call(FirstNode, antidote, clocksi_icommit, [TxId]),
    ?assertMatch({ok, {_Txid, _CausalSnapshot}}, End),
    {ok,{_Txid, CausalSnapshot}} = End,
    ReadResult3 = rpc:call(FirstNode, antidote, clocksi_read,
                           [CausalSnapshot, abc, Type]),
    {ok, {_,[ReadVal],_}} = ReadResult3,
    ?assertEqual(ReadVal, 1),
    lager:info("Test2 passed"),
    pass.

%% @doc The following function tests that ClockSI can run an interactive tx.
%%      It tests the API operation that allows clients to run interactive txs
%%      explicitely calling prepare and commit.
clocksi_test3(Nodes) ->
    FirstNode = hd(Nodes),
    lager:info("Test2 started"),
    Type = riak_dt_pncounter,
    {ok,TxId}=rpc:call(FirstNode, antidote, clocksi_istart_tx, []),
    ReadResult0=rpc:call(FirstNode, antidote, clocksi_iread,
                         [TxId, abc, riak_dt_pncounter]),
    ?assertEqual({ok, 1}, ReadResult0),
    WriteResult=rpc:call(FirstNode, antidote, clocksi_iupdate,
                         [TxId, abc, Type, {increment, 4}]),
    ?assertEqual(ok, WriteResult),
    ReadResult=rpc:call(FirstNode, antidote, clocksi_iread,
                        [TxId, abc, riak_dt_pncounter]),
    ?assertEqual({ok, 2}, ReadResult),
    WriteResult1=rpc:call(FirstNode, antidote, clocksi_iupdate,
                          [TxId, bcd, Type, {increment, 4}]),
    ?assertEqual(ok, WriteResult1),
    ReadResult1=rpc:call(FirstNode, antidote, clocksi_iread,
                         [TxId, bcd, riak_dt_pncounter]),
    ?assertEqual({ok, 2}, ReadResult1),
    WriteResult2=rpc:call(FirstNode, antidote, clocksi_iupdate,
                          [TxId, cde, Type, {increment, 4}]),
    ?assertEqual(ok, WriteResult2),
    ReadResult2=rpc:call(FirstNode, antidote, clocksi_iread,
                         [TxId, cde, riak_dt_pncounter]),
    ?assertEqual({ok, 2}, ReadResult2),
    End=rpc:call(FirstNode, antidote, clocksi_full_icommit, [TxId]),
    ?assertMatch({ok, {_Txid, _CausalSnapshot}}, End),
    {ok,{_Txid, CausalSnapshot}} = End,
    ReadResult3 = rpc:call(FirstNode, antidote, clocksi_read,
                           [CausalSnapshot, abc, Type]),
    {ok, {_,[ReadVal],_}} = ReadResult3,
    ?assertEqual(ReadVal, 2),
    lager:info("Test3 passed"),
    pass.

%% @doc The following function tests that ClockSI can run an interactive tx.
%%      that updates only one partition. This type of txs use a only-one phase 
%%      commit.
clocksi_test5(Nodes) ->
    FirstNode = hd(Nodes),
    lager:info("Test2 started"),
    Type = riak_dt_pncounter,
    {ok,TxId}=rpc:call(FirstNode, antidote, clocksi_istart_tx, []),
    ReadResult0=rpc:call(FirstNode, antidote, clocksi_iread,
                         [TxId, abc, riak_dt_pncounter]),
    ?assertEqual({ok, 2}, ReadResult0),
    WriteResult=rpc:call(FirstNode, antidote, clocksi_iupdate,
                         [TxId, abc, Type, {increment, 4}]),
    ?assertEqual(ok, WriteResult),
    ReadResult=rpc:call(FirstNode, antidote, clocksi_iread,
                        [TxId, abc, riak_dt_pncounter]),
    ?assertEqual({ok, 3}, ReadResult),
    WriteResult1=rpc:call(FirstNode, antidote, clocksi_iupdate,
                          [TxId, abc, Type, {increment, 4}]),
    ?assertEqual(ok, WriteResult1),
    ReadResult1=rpc:call(FirstNode, antidote, clocksi_iread,
                         [TxId, abc, riak_dt_pncounter]),
    ?assertEqual({ok, 4}, ReadResult1),
    WriteResult2=rpc:call(FirstNode, antidote, clocksi_iupdate,
                          [TxId, abc, Type, {increment, 4}]),
    ?assertEqual(ok, WriteResult2),
    ReadResult2=rpc:call(FirstNode, antidote, clocksi_iread,
                         [TxId, abc, riak_dt_pncounter]),
    ?assertEqual({ok, 5}, ReadResult2),
    End=rpc:call(FirstNode, antidote, clocksi_full_icommit, [TxId]),
    ?assertMatch({ok, {_Txid, _CausalSnapshot}}, End),
    {ok,{_Txid, CausalSnapshot}} = End,
    ReadResult3 = rpc:call(FirstNode, antidote, clocksi_read,
                           [CausalSnapshot, abc, Type]),
    {ok, {_,[ReadVal],_}} = ReadResult3,
    ?assertEqual(ReadVal, 5),
    lager:info("Test5 passed"),
    pass.

%% @doc Test to execute transaction with out explicit clock time
clocksi_tx_noclock_test(Nodes) ->
    FirstNode = hd(Nodes),
    Key = itx,
    Type = riak_dt_pncounter,
    {ok,TxId}=rpc:call(FirstNode, antidote, clocksi_istart_tx, []),
    ReadResult0=rpc:call(FirstNode, antidote, clocksi_iread,
                         [TxId, Key, riak_dt_pncounter]),
    ?assertEqual({ok, 0}, ReadResult0),
    WriteResult0=rpc:call(FirstNode, antidote, clocksi_iupdate,
                          [TxId, Key, Type, {increment, 4}]),
    ?assertEqual(ok, WriteResult0),
    CommitTime=rpc:call(FirstNode, antidote, clocksi_iprepare, [TxId]),
    ?assertMatch({ok, _}, CommitTime),
    End=rpc:call(FirstNode, antidote, clocksi_icommit, [TxId]),
    ?assertMatch({ok, _}, End),
    ReadResult1 = rpc:call(FirstNode, antidote, clocksi_read,
                           [Key, riak_dt_pncounter]),
    {ok, {_, ReadSet1, _}}= ReadResult1,
    ?assertMatch([1], ReadSet1),

    FirstNode = hd(Nodes),
    WriteResult1 = rpc:call(FirstNode, antidote, clocksi_bulk_update,
                            [[{update, Key, Type, {increment, a}}]]),
    ?assertMatch({ok, _}, WriteResult1),
    ReadResult2= rpc:call(FirstNode, antidote, clocksi_read,
                          [Key, riak_dt_pncounter]),
    {ok, {_, ReadSet2, _}}=ReadResult2,
    ?assertMatch([2], ReadSet2),
    lager:info("Test3 passed"),
    pass.

%% @doc The following function tests that ClockSI can run both a single
%%      read and a bulk-update tx.
clocksi_single_key_update_read_test(Nodes) ->
    lager:info("Test3 started"),
    FirstNode = hd(Nodes),
    Key = k3,
    Type = riak_dt_pncounter,
    Result= rpc:call(FirstNode, antidote, clocksi_bulk_update,
                     [
                      [{update, Key, Type, {increment, a}},
                       {update, Key, Type, {increment, b}}]]),
    ?assertMatch({ok, _}, Result),
    {ok,{_,_,CommitTime}} = Result,
    Result2= rpc:call(FirstNode, antidote, clocksi_read,
                      [CommitTime, Key, riak_dt_pncounter]),
    {ok, {_, ReadSet, _}}=Result2,
    ?assertMatch([2], ReadSet),
    lager:info("Test3 passed"),
    pass.

%% @doc Verify that multiple reads/writes are successful.
clocksi_multiple_key_update_read_test(Nodes) ->
    Firstnode = hd(Nodes),
    Type = riak_dt_pncounter,
    Key1 = keym1,
    Key2 = keym2,
    Key3 = keym3,
    Ops = [{update,Key1, Type, {increment,a}},
           {update,Key2, Type, {{increment,10},a}},
           {update,Key3, Type, {increment,a}}],
    Writeresult = rpc:call(Firstnode, antidote, clocksi_bulk_update,
                           [Ops]),
    ?assertMatch({ok,{_Txid, _Readset, _Committime}}, Writeresult),
    {ok,{_Txid, _Readset, Committime}} = Writeresult,
    {ok,{_,[ReadResult1],_}} = rpc:call(Firstnode, antidote, clocksi_read,
                                        [Committime, Key1, riak_dt_pncounter]),
    {ok,{_,[ReadResult2],_}} = rpc:call(Firstnode, antidote, clocksi_read,
                                        [Committime, Key2, riak_dt_pncounter]),
    {ok,{_,[ReadResult3],_}} = rpc:call(Firstnode, antidote, clocksi_read,
                                        [Committime, Key3, riak_dt_pncounter]),
    ?assertMatch(ReadResult1,1),
    ?assertMatch(ReadResult2,10),
    ?assertMatch(ReadResult3,1),
    pass.

%% @doc The following function tests that ClockSI can excute a
%%      read-only interactive tx.
clocksi_test4(Nodes) ->
    lager:info("Test4 started"),
    FirstNode = hd(Nodes),
    lager:info("Node1: ~p", [FirstNode]),
    {ok,TxId1}=rpc:call(FirstNode, antidote, clocksi_istart_tx, []),

    lager:info("Tx Started, id : ~p", [TxId1]),
    ReadResult1=rpc:call(FirstNode, antidote, clocksi_iread,
                         [TxId1, abc, riak_dt_pncounter]),
    lager:info("Tx Reading..."),
    ?assertMatch({ok, _}, ReadResult1),
    lager:info("Tx Read value...~p", [ReadResult1]),
    CommitTime1=rpc:call(FirstNode, antidote, clocksi_iprepare, [TxId1]),
    ?assertMatch({ok, _}, CommitTime1),
    lager:info("Tx sent prepare, got commitTime=..., id : ~p", [CommitTime1]),
    End1=rpc:call(FirstNode, antidote, clocksi_icommit, [TxId1]),
    ?assertMatch({ok, _}, End1),
    lager:info("Tx Committed."),
    lager:info("Test 4 passed."),
    pass.

%% @doc Read an update a key multiple times.
clocksi_multiple_read_update_test(Nodes) ->
    Node = hd(Nodes),
    Key = get_random_key(),
    NTimes = 100,
    {ok,Result1} = rpc:call(Node, antidote, read,
                       [Key, riak_dt_pncounter]),
    lists:foreach(fun(_)->
                          read_update_test(Node, Key) end,
                  lists:seq(1,NTimes)),
    {ok,Result2} = rpc:call(Node, antidote, read,
                       [Key, riak_dt_pncounter]),
    ?assertEqual(Result1+NTimes, Result2),
    pass.

%% @doc Test updating prior to a read.
read_update_test(Node, Key) ->
    Type = riak_dt_pncounter,
    {ok,Result1} = rpc:call(Node, antidote, read,
                       [Key, Type]),
    {ok,_} = rpc:call(Node, antidote, clocksi_bulk_update,
                      [[{update, Key, Type, {increment,a}}]]),
    {ok,Result2} = rpc:call(Node, antidote, read,
                       [Key, Type]),
    ?assertEqual(Result1+1,Result2),
    pass.

get_random_key() ->
    random:seed(now()),
    random:uniform(1000).

%% @doc The following function tests how two concurrent transactions work
%%      when they are interleaved.
clocksi_concurrency_test(Nodes) ->
    lager:info("clockSI_concurrency_test started"),
    Node = hd(Nodes),
    %% read txn starts before the write txn's prepare phase,
    Key = conc,
    {ok, TxId1} = rpc:call(Node, antidote, clocksi_istart_tx, []),
    rpc:call(Node, antidote, clocksi_iupdate,
             [TxId1, Key, riak_dt_gcounter, {increment, ucl}]),
    rpc:call(Node, antidote, clocksi_iprepare, [TxId1]),
    {ok, TxId2} = rpc:call(Node, antidote, clocksi_istart_tx, []),
    Pid = self(),
    spawn( fun() ->
                   rpc:call(Node, antidote, clocksi_iupdate,
                            [TxId2, Key, riak_dt_gcounter, {increment, ucl}]),
                   rpc:call(Node, antidote, clocksi_iprepare, [TxId2]),
                   {ok,_}= rpc:call(Node, antidote, clocksi_icommit, [TxId2]),
                   Pid ! ok
           end),

    {ok,_}= rpc:call(Node, antidote, clocksi_icommit, [TxId1]),
     receive
         ok ->
             Result= rpc:call(Node,
                              antidote, read, [Key, riak_dt_gcounter]),
             ?assertEqual({ok, 2}, Result),
             pass
     end.
