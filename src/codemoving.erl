-module(codemoving).
-include("code_unloading.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").

-export([
	connect/3,
	disconnect/1,
	store_module/2,
	rpc/4
]).

-export([
	start_transaction/4,
	read_objects/3,
	commit_transaction/2
]).


%%%===================================================================
%%% API reserved to the EdgeAnt client
%%%===================================================================

connect(IpAddr, Port, AntidoteSocket) ->
	case gen_tcp:connect(IpAddr, Port, [binary, {active,false}]) of
		{ok, Socket} -> 
			{{dc_provided_id, dcid}, Socket, AntidoteSocket};
		Error -> Error
	end.

store_module(ClientId, ModuleName) ->
	Socket = extract_socket(ClientId),
	{Module, Binary, Filename} = code:get_object_code(ModuleName),
	Code = #'CmStoreModule'{
		module = atom_to_binary(Module, utf8),
        filepath = list_to_binary(Filename),
        binary = Binary
	},
	Msg = code_unloading:encode_msg(Code),
	MsgCode = codemoving_messages:msg_code(store_module),
	ok = gen_tcp:send(Socket, <<MsgCode:8, Msg/binary>>),
	receive_message(Socket).

rpc(ClientId, Module, Function, Args) ->
	Socket = extract_socket(ClientId),
	RPC = #'CmRemoteProcedureCall'{
		module = atom_to_binary(Module, utf8),
		function = atom_to_binary(Function, utf8),
		args = Args
	},
	Msg = code_unloading:encode_msg(RPC),
	MsgCode = codemoving_messages:msg_code(rpc),
	ok = gen_tcp:send(Socket, <<MsgCode:8, Msg/binary>>),
	receive_message(Socket).

disconnect(ClientId) ->
	Socket = extract_socket(ClientId),
	gen_tcp:close(Socket).


%%%===================================================================
%%% API available by both the EdgeAnt client and the server executor
%%%===================================================================

%% Client-side execution of start_transaction just create a transaction
%% with antidote_pb module. InterestSet is only needed for a server-side
%% execution so it should be ignore here.
start_transaction(ClientId, Snapshot, Properties, _InterestSet) ->
	AntidoteSocket = extract_antidote_socket(ClientId),
	antidotec_pb:start_transaction(AntidoteSocket, Snapshot, Properties).

read_objects(ClientId, Keys, TransactionId) ->
	AntidoteSocket = extract_antidote_socket(ClientId),
	antidotec_pb:read_objects(AntidoteSocket, Keys, TransactionId).

commit_transaction(ClientId, TransactionId) ->
	AntidoteSocket = extract_antidote_socket(ClientId),
	antidotec_pb:commit_transaction(AntidoteSocket, TransactionId).

%%%===================================================================
%%% Internal
%%%===================================================================

extract_antidote_socket(ClientId) ->
	{_, _, Socket} = ClientId,
	Socket.

extract_socket(ClientId) ->
	{_, Socket, _} = ClientId,
	Socket.

receive_message(Socket) ->
	{ok, EncodedResponse} = gen_tcp:recv(Socket, 0),
	Response = code_unloading:decode_msg(EncodedResponse, 'Response'),
	Message = case Response#'Response'.type of
				'ATOM' -> binary_to_atom(Response#'Response'.message, utf8);
				'STRING' -> binary_to_list(Response#'Response'.message);
				'TUPLE' -> list_to_tuple(binary_to_list(Response#'Response'.message))
			  end,
	case Response#'Response'.error of
		true -> {error, Message};
		false -> {ok, Message}
	end.