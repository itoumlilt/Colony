-module(antidote_pb_dc).

-ifdef(TEST).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(riak_api_pb_service).

-include_lib("riak_pb/include/antidote_pb.hrl").
-include("antidote.hrl").

-export([init/0,
         decode/2,
         encode/1,
         process/2,
         process_stream/3
        ]).

%% @doc init/0 callback. Returns the service internal start
%% state.
init() ->
    {}.

%% @doc decode/2 callback. Decodes an incoming message.
decode(Code, Bin) ->
    Msg = riak_pb_codec:decode(Code, Bin),
    case Msg of
        #apbcreatedc{} ->
            {ok, Msg, {"antidote.createdc",<<>>}};
        apbgetconnectiondescriptor ->
            {ok, Msg, {"antidote.getconnectiondescriptor",<<>>}};
        #apbconnecttodcs{} ->
            {ok, Msg, {"antidote.connecttodcs",<<>>}}
    end.

%% @doc encode/1 callback. Encodes an outgoing response message.
encode(Message) ->
    {ok, riak_pb_codec:encode(Message)}.

process(#apbcreatedc{nodes = Nodes}, State) ->
    NodeNames = lists:map(fun(Node) ->
                            list_to_atom(Node)
                          end, Nodes),
    try
      ok = antidote_dc_manager:create_dc(NodeNames),
      {reply, antidote_pb_codec:encode(operation_response, ok), State}
    catch
     Error:Reason -> %% Some error, return unsuccess. TODO: correct error response
       lager:info("Create DC Failed ~p : ~p", [Error, Reason]),
       {reply, antidote_pb_codec:encode(operation_response, {error, create_dc_failed}), State}
    end;

process(apbgetconnectiondescriptor, State) ->
    try
       {ok, Descriptor} = antidote_dc_manager:get_connection_descriptor(),
       {reply, #apbgetconnectiondescriptorresponse{success=true, descriptor = term_to_binary(Descriptor)}, State}
    catch
      Error:Reason -> %% Some error, return unsuccess. TODO: correct error response
        lager:info("Get Conection Descriptor ~p : ~p", [Error, Reason]),
        {reply, #apbgetconnectiondescriptorresponse{success=false}, State}
    end;

process(#apbconnecttodcs{descriptors = BinDescriptors}, State) ->
    Descriptors = lists:map(fun(BinDesc) ->
                              binary_to_term(BinDesc)
                            end, BinDescriptors),
    try
       ok = antidote_dc_manager:subscribe_updates_from(Descriptors),
       {reply, antidote_pb_codec:encode(operation_response, ok), State}
    catch
      Error:Reason -> %% Some error, return unsuccess. TODO: correct error response
        lager:info("Connect to DCs Failed ~p : ~p", [Error, Reason]),
        {reply, antidote_pb_codec:encode(operation_response, {error, connect_to_dcs_failed}), State}
    end.

%% @doc process_stream/3 callback. This service does not create any
%% streaming responses and so ignores all incoming messages.
process_stream(_, _, State) ->
    {ignore, State}.
