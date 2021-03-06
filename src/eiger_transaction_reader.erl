-module(eiger_transaction_reader).

-include("antidote.hrl").

-record(state, {partition :: partition_id(),
                logid :: log_id(),
                last_read_opid :: empty | op_id(),
                pending_operations :: dict(),
                pending_commit_records :: list(),
                prev_stable_time :: non_neg_integer(),
                dcid :: dcid()
               }).

-export([init/2,
         get_next_transactions/1,
         get_update_ops_from_transaction/1
         ]).

-export_type([transaction/0]).

-type transaction() :: {txid(), commit_time(),
                        snapshot_time(), [operation()]}.

%% @doc Returns an iterator to read transactions from a partition
%%  transactions can be read using get_next_transactions
-spec init(Partition::partition_id(), DcId::dcid()) -> {ok, #state{}}.
init(Partition, DcId) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    GrossPreflists = riak_core_ring:all_preflists(Ring, ?N),
    [Preflist] = lists:filtermap(fun([H|_T]) ->
                                         case H of
                                             {Partition,_} ->
                                                 true;
                                             _ ->
                                                 false
                                         end
                                 end, GrossPreflists),
    LogId = [ P || {P, _Node} <- Preflist],
    {ok, #state{partition = Partition,
                logid = LogId,
                last_read_opid = empty,
                pending_operations = dict:new(),
                pending_commit_records = [],
                prev_stable_time = 0,
                dcid = DcId}}.

%% @doc get_next_transactions takes the iterator returned by init
%%  it returns new iterator and a list of committed transactions in
%%  commit time order. Transactions which are not committed will not be returned
%% TODO: Deal with transactions committed in other DCs
-spec get_next_transactions(State::#state{}) -> {#state{}, [transaction()]}.
get_next_transactions(State=#state{partition = Partition,
                                   logid = LogId,
                                   pending_operations = Pending,
                                   pending_commit_records = PendingCommitRecords,
                                   last_read_opid = Last_read_opid,
                                   dcid = DcId}
                     ) ->
    Node = {Partition, node()},
    %% No transactions will commit in future with commit time < stable_time
    %% So it is safe to read all transactions committed before stable_time
    {ok, NewOps} = read_next_ops(Node, LogId, Last_read_opid),
    case NewOps of
        [] -> Newlast_read_opid = Last_read_opid;
        _ -> Newlast_read_opid = get_last_opid(NewOps)
    end,

    {PendingOperations, Commitrecords} =
        add_to_pending_operations(Pending, PendingCommitRecords, NewOps, DcId),

    Txns = get_sorted_commit_records(Commitrecords),
    %% "Before" contains all transactions committed before stable_time
    {NewPendingOps, ListTransactions} =
        lists:foldl(
          fun(_Logrecord=#log_record{tx_id=TxId},
              {PendingOps, Transactions}) ->
                  {ok, Ops} = get_ops_of_txn(TxId,PendingOps),
                  Txn = construct_transaction(Ops),
                  NewTransactions = Transactions ++ [Txn],
                  NewPending = remove_txn_from_pending(TxId, PendingOps),
                  {NewPending, NewTransactions}
          end, {PendingOperations, []},
          Txns),
    NewState = State#state{pending_operations = NewPendingOps,
                           pending_commit_records = [],
                           last_read_opid = Newlast_read_opid},
    {NewState, ListTransactions}.

%% @doc returns all update operations in a txn in #clocksi_payload{} format
-spec get_update_ops_from_transaction(Transaction::transaction()) ->
                                             [#clocksi_payload{}].
get_update_ops_from_transaction(Transaction) ->
    {_TxId, {DcId, CommitTime}, VecSnapshotTime, Ops, _Deps, _TotalOps} = Transaction,
    Downstreamrecord =
        fun(_Operation=#operation{payload=Logrecord}) ->
                case Logrecord#log_record.op_type of
                    update ->
                        {Key, Type, Op} = Logrecord#log_record.op_payload,
                        _NewRecord = #clocksi_payload{
                                        key = Key,
                                        type = Type,
                                        op_param = Op,
                                        snapshot_time = VecSnapshotTime,
                                        commit_time = {DcId, CommitTime},
                                        txid =  Logrecord#log_record.tx_id
                                       };
                    _ ->
                        nothing
                end
        end,

    lists:foldl( fun(Op, ListsOps) ->
                         case Downstreamrecord(Op) of
                             nothing ->
                                 ListsOps;
                             Record ->
                                 ListsOps ++ [Record]
                         end
                 end, [], Ops).


%% ---- Internal function ----- %%

%% @doc construct_transaction: Returns a structure of type transaction()
%% from a list of update operations and prepare/commit records
-spec construct_transaction([operation()]) -> transaction().
construct_transaction(Ops) ->
    Commitoperation = lists:last(Ops),
    Commitrecord = Commitoperation#operation.payload,
    {CommitTime, VecSnapshotTime, Deps, TotalOps} = Commitrecord#log_record.op_payload,
    TxId = Commitrecord#log_record.tx_id,
    {TxId, CommitTime, VecSnapshotTime, Deps, Ops, TotalOps}.

read_next_ops(Node, LogId, Last_read_opid) ->
    case Last_read_opid of
        empty ->
            logging_vnode:read(Node, LogId);
        _ ->
            logging_vnode:read_from(Node, LogId, Last_read_opid)
    end.

get_ops_of_txn(TxId, PendingOps) ->
    dict:find(TxId, PendingOps).

remove_txn_from_pending(TxId, PendingOps) ->
    dict:erase(TxId, PendingOps).

get_last_opid(Ops) ->
    Last = lists:last(Ops),
    {_Logid, Operation} = Last,
    Operation#operation.op_number.

%%@doc return all txnIds in sorted order committed befor stabletime
get_sorted_commit_records(Commitrecords) ->
    %%sort txns
    CompareFun = fun(Commitrecord1, Commitrecord2) ->
                         {{_DcId, CommitTime1},_,_,_} = Commitrecord1#log_record.op_payload,
                         {{_DcId, CommitTime2},_,_,_} = Commitrecord2#log_record.op_payload,
                         CommitTime1 =< CommitTime2
                 end,
    lists:sort(CompareFun, Commitrecords).

%%@doc Add updates in writeset ot Pending operations to process downstream
add_to_pending_operations(Pending, Commitrecords, Ops, DcId) ->
    case Ops of
        [] ->
            {Pending,Commitrecords};
        _ ->
            lists:foldl(
              fun(Op, {ListPending, ListCommits}) ->
                      {_Logid, Operation} = Op,
                      Logrecord = Operation#operation.payload,
                      TxId = Logrecord#log_record.tx_id,
                      case Logrecord#log_record.op_type of
                          commit ->
                              {{Dc,_CT},_ST, _Deps, _TotalOps} = Logrecord#log_record.op_payload,
                              case Dc of
                                  DcId ->
                                      NewCommit = ListCommits ++ [Logrecord],
                                      NewPending =
                                          dict:append(
                                            TxId, Operation, ListPending);
                                  _ ->
                                      NewCommit=ListCommits,
                                      NewPending = dict:erase(TxId, ListPending)
                              end;
                          abort ->
                              NewCommit = ListCommits,
                              NewPending = dict:erase(TxId, ListPending);
                          _ ->
                              NewPending =
                                  dict:append(TxId, Operation, ListPending),
                              NewCommit=ListCommits
                      end,
                      {NewPending, NewCommit}
              end,
              {Pending, Commitrecords}, Ops)
    end.
