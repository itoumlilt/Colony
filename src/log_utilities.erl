-module(log_utilities).

-include("antidote.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-export([get_preflist_from_key/1,
         get_logid_from_key/1,
         remove_node_from_preflist/1,
         get_my_node/1,
         log_record_version/0,
         check_log_record_version/1
        ]).

%% @doc get_logid_from_key computes the log identifier from a key
%%      Input:  Key:    The key from which the log id is going to be computed
%%      Return: Log id
%%
-spec get_logid_from_key(key()) -> log_id().
get_logid_from_key(Key) ->
    %HashedKey = riak_core_util:chash_key({?BUCKET, term_to_binary(Key)}),
    PreflistAnn = get_preflist_from_key(Key),
    remove_node_from_preflist(PreflistAnn).

%% @doc get_preflist_from_key returns a preference list where a given
%%      key's logfile will be located.
-spec get_preflist_from_key(key()) -> preflist().
get_preflist_from_key(Key) ->
    ConvertedKey = convert_key(Key),
    get_primaries_preflist(ConvertedKey).

%% @doc get_primaries_preflist returns the preflist with the primary
%%      vnodes. No matter they are up or down.
%%      Input:  A hashed key
%%      Return: The primaries preflist
%%
-spec get_primaries_preflist(non_neg_integer()) -> preflist().
get_primaries_preflist(Key)->
    NumPartitions = dc_meta_data_utilities:get_num_partitions(),
    Pos = Key rem NumPartitions + 1,
    [dc_meta_data_utilities:get_partition_at_index(Pos)].

-spec get_my_node(partition_id()) -> node().
get_my_node(Partition) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    riak_core_ring:index_owner(Ring, Partition).

%% @doc remove_node_from_preflist: From each element of the input
%%      preflist, the node identifier is removed
%%      Input:  Preflist: list of pairs {Partition, Node}
%%      Return: List of Partition identifiers
%%
-spec remove_node_from_preflist(preflist()) -> [partition_id()].
remove_node_from_preflist(Preflist) ->
    F = fun({P,_}) -> P end,
    lists:map(F, Preflist).

%% @doc Convert key. If the key is integer(or integer in form of binary),
%% directly use it to get the partition. If it is not integer, convert it
%% to integer using hash.
-spec convert_key(key()) -> non_neg_integer().
convert_key(Key) ->
    case is_binary(Key) of
        true ->
            KeyInt = (catch list_to_integer(binary_to_list(Key))),
            case is_integer(KeyInt) of 
                true -> abs(KeyInt);
                false ->
                    HashedKey = riak_core_util:chash_key({?BUCKET, Key}),
                    abs(crypto:bytes_to_integer(HashedKey))
            end;
        false ->
            case is_integer(Key) of 
                true ->
                    abs(Key);
                false ->
                    HashedKey = riak_core_util:chash_key({?BUCKET, term_to_binary(Key)}),
                    abs(crypto:bytes_to_integer(HashedKey))
            end
    end.

-spec log_record_version() -> non_neg_integer().
log_record_version() -> ?LOG_RECORD_VERSION.


%% Check the version of the log record and convert
%% to a different version if necessary
%% Checked when loading the log from disk, or
%% when log messages are recieved from another DC
-spec check_log_record_version(#log_record{}) -> #log_record{}.
check_log_record_version(LogRecord) ->
    %% Only support one version for now
    ?LOG_RECORD_VERSION = LogRecord#log_record.version,
    LogRecord.

-ifdef(TEST).



%% @doc Testing remove_node_from_preflist
remove_node_from_preflist_test()->
    Preflist = [{partition1, node},
                {partition2, node},
                {partition3, node}],
    ?assertEqual([partition1, partition2, partition3],
                 remove_node_from_preflist(Preflist)).

%% @doc Testing convert key
convert_key_test()->
    ?assertEqual(1, convert_key(1)),
    ?assertEqual(1, convert_key(-1)),
    ?assertEqual(0, convert_key(0)),
    ?assertEqual(45, convert_key(<<"45">>)),
    ?assertEqual(45, convert_key(<<"-45">>)).

-endif.
