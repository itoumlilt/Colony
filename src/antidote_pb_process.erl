
-module(antidote_pb_process).

-ifdef(TEST).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("antidote.hrl").
-include_lib("kernel/include/logger.hrl").

-export([process/1]).

-spec from_bin(binary()) -> snapshot_time() | ignore | txid().
from_bin(Clock) ->
    case Clock of
        undefined -> ignore;
        _ -> binary_to_term(Clock)
    end.

-spec encode_clock(snapshot_time() | txid()) -> binary().
encode_clock(TxId) ->
    term_to_binary(TxId).

-spec process(antidote_pb_codec:request()) -> antidote_pb_codec:response_in().
process({start_transaction, Clock, Properties}) ->
    Response = antidote:start_transaction(from_bin(Clock), Properties),
    case Response of
        {ok, TxId} -> {start_transaction_response, {ok, encode_clock(TxId)}};
        {error, Reason} -> {start_transaction_response, {error, Reason}}
    end;

process({abort_transaction, TxId}) ->
    Response = antidote:abort_transaction(from_bin(TxId)),
    case Response of
        ok -> {operation_response, ok};
        {error, Reason} -> {operation_response, {error, Reason}}
            %% TODO: client initiated abort is not implemented yet
     end;

process({commit_transaction, TxId}) ->
    Response = antidote:commit_transaction(from_bin(TxId)),
    case Response of
        {ok, CommitTime} -> {commit_transaction_response, {ok, encode_clock(CommitTime)}};
        {error, Reason} -> {commit_transaction_response, {error, Reason}}
    end;

process({update_objects, Updates, TxId}) ->
    Response = antidote:update_objects(Updates, from_bin(TxId)),
    case Response of
        {error, Reason} -> {operation_response, {error, Reason}};
        ok -> {operation_response, ok}
    end;

process({static_update_objects, Clock, Properties, Updates}) ->
    Response = antidote:update_objects(from_bin(Clock), Properties, Updates),
    case Response of
        {ok, CommitTime} -> {commit_transaction_response, {ok, encode_clock(CommitTime)}};
        {error, Reason} -> {commit_transaction_response, {error, Reason}}
    end;

process({read_objects, Objects, TxId}) ->
    Response = antidote:read_objects(Objects, from_bin(TxId)),
    case Response of
        {ok, Results} -> {read_objects_response, {ok, lists:zip(Objects, Results)}};
        {error, Reason} -> {read_objects_response, {error, Reason}}
    end;


process({static_read_objects, Clock, Properties, Objects}) ->
    Response = antidote:read_objects(from_bin(Clock), Properties, Objects),
    case Response of
        {ok, Results, CommitTime} -> {static_read_objects_response, {lists:zip(Objects, Results), encode_clock(CommitTime)}};
        {error, Reason} -> {static_read_objects_response, {error, Reason}}
    end;

process({create_dc, NodeNames}) ->
    try
      ok = antidote_dc_manager:create_dc(NodeNames),
      {create_dc_response, ok}
    catch
     Error:Reason ->
       ?LOG_ERROR("Create DC failed ~p : ~p", [Error, Reason]),
       {create_dc_response, {error, aborted}}
    end;

process(get_connection_descriptor) ->
    try
       {ok, Descriptor} = antidote_dc_manager:get_connection_descriptor(),
       {get_connection_descriptor_response, {ok, term_to_binary(Descriptor)}}
    catch
      Error:Reason ->
        ?LOG_ERROR("Get Conection Descriptor failed ~p : ~p", [Error, Reason]),
        {get_connection_descriptor_response, {error, aborted}}
    end;

process({connect_to_dcs, BinDescriptors}) ->
    try
       Descriptors = [binary_to_term(D) || D <- BinDescriptors],
       ?LOG_INFO("Connection to DCs: ~p", [Descriptors]),
       ok = antidote_dc_manager:subscribe_updates_from(Descriptors),
       {connect_to_dcs_response, ok}
    catch
      Error:Reason ->
        ?LOG_ERROR("Connect to DCs failed ~p : ~p", [Error, Reason]),
        {connect_to_dcs_response, {error, aborted}}
    end.