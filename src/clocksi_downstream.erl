-module(clocksi_downstream).

-include("antidote.hrl").

-ifdef(TEST).
-define(LOG_UTIL, mock_partition_fsm).
-else.
-define(LOG_UTIL, log_utilities).
-endif.

-export([generate_downstream_op/4]).

%% @doc Returns downstream operation for upstream operation
%%      input: Update - upstream operation
%%      output: Downstream operation or {error, Reason}
-spec generate_downstream_op(Key :: key(),  Type :: type(), Update :: op(), CoordState :: #tx_coord_state{}) ->
	{ok, op(), vectorclock()|{vectorclock(), vectorclock()}, boolean()} | {error, reason()}.
generate_downstream_op(Key, Type, Update, CoordState)->
	Transaction = CoordState#tx_coord_state.transaction,
	InternalReadSet = CoordState#tx_coord_state.internal_read_set,
	Preflist = ?LOG_UTIL:get_preflist_from_key(Key),
	IndexNode = hd(Preflist),
	WriteSet = case lists:keyfind(IndexNode, 1, CoordState#tx_coord_state.updated_partitions) of
		false->
			[];
		{IndexNode, WS}->
			WS
	end,
	Result=case orddict:find(Key, InternalReadSet) of
		{ok, {S, SCP}}->
			{{S, SCP}, true};
		error->
			case clocksi_vnode:read_data_item(IndexNode, Transaction, Key, Type, WriteSet) of
				{ok, {S, SCP}}->
					{{S, SCP}, false};
				{error, Reason}->
					{error, Reason}
			end
	end,
	case Result of
		{error, R}->
			{error, R}; %% {error, Reason} is returned here.
		{{Snapshot, SnapshotCommitParams}, KewWasRead}->
			NewSnapshot=case Type of
				antidote_crdt_bcounter->
					%% bcounter data-type.
					bcounter_mgr:generate_downstream(Key, Update, Snapshot);
				_->
					Type:downstream(Update, Snapshot)
			end,
			case NewSnapshot of
				{ok, FinalSnapshot}->
					{ok, FinalSnapshot, SnapshotCommitParams, KewWasRead};
				{error, Reason1}->
					{error, Reason1}
			end
	end.
