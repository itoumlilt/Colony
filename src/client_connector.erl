
%% InterDC publisher - holds a ZeroMQ PUB socket and makes it available for Antidote processes.
%% This vnode is used to publish interDC transactions.

-module(client_connector).
-include("code_unloading.hrl").

-define(DEFAULT_PORT, 8089).
-behaviour(gen_server).

-export([
  init/1,
  start_link/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% State
-record(state, {
	listenSocket,
	acceptedSocket
}). %% socket :: erlzmq_socket()

%%%% Server methods ---------------------------------------------------------+

start_link(ListenSocket) -> gen_server:start_link(?MODULE, ListenSocket, []).

init(ListenSocket) ->
	gen_server:cast(self(), accept),
    {ok, #state{listenSocket = ListenSocket}}.

handle_cast(accept, State) ->
	{ok, AcceptedSocket} = gen_tcp:accept(State#state.listenSocket),
	client_connector_sup:start_acceptor(),
	{noreply, State#state{acceptedSocket = AcceptedSocket}}.

handle_info({tcp, Socket, _BinaryMsg}, State) ->
	<<Code:8, Msg/binary>> = _BinaryMsg,
	case Code of
		200 -> store_module(Socket, Msg);
		201 -> rpc(Socket, Msg)
	end,
	{noreply, State};

handle_info({zmq, _Socket, _BinaryMsg, _Flags}, State) ->
	<<Length:32, Code:8, Msg:32>> = _BinaryMsg,
	io:format("Code: ~p, Length: ~p, Message: ~p~n", [Code, Length, Msg]),
	erlzmq:send(_Socket, <<"Message Received">>),
	%io:format("Receive something ~p\n", [{State#state.listenSocket, _Socket, _Flags}]),
	%Code = code_unloading:decode_msg(_BinaryMsg, 'Code'),
	%Module = binary_to_atom(Code#'Code'.module, unicode),
	%File = binary_to_list(Code#'Code'.filepath),
	%Function = binary_to_atom(Code#'Code'.function, unicode),
	%_Args = Code#'Code'.args,
	%Binary = Code#'Code'.binary,
	%code:purge(Module),
	%code:load_binary(Module, File, Binary),
	%erlzmq:send(_Socket, <<>>),
	%spawn(fun() -> Module:Function() end),
	{noreply, State};

handle_info({tcp_closed, _Socket}, S) ->
	{stop, normal, S};

handle_info({tcp_error, _Socket, _}, S) ->
	{stop, normal, S};

handle_info(Msg, State) ->
	io:format("Handle  info: ~p~n", [Msg]),
	{noreply, State}.

handle_call(_Message, _From, State) -> {noreply, State}.

terminate(_Reason, _State) -> 
	io:format("Client connector terminate: ~p~n", [_Reason]),
	ok.

%handle_call(_Request, _From, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%% Internal ---------------------------------------------------------------+

store_module(Socket, Msg) ->
	Code = code_unloading:decode_msg(Msg, 'CmStoreModule'),
	Module = binary_to_atom(Code#'CmStoreModule'.module, unicode),
	File = binary_to_list(Code#'CmStoreModule'.filepath),
	Binary = Code#'CmStoreModule'.binary,
	case code:load_binary(Module, File, Binary) of
		{module, Module} -> send_message(Socket, "", false);
		{error, Reason} -> send_message(Socket, atom_to_list(Reason), true)
	end.

rpc(Socket, Msg) ->
	RPC = code_unloading:decode_msg(Msg, 'CmRemoteProcedureCall'),
	Module = binary_to_atom(RPC#'CmRemoteProcedureCall'.module, utf8),
	Function = binary_to_atom(RPC#'CmRemoteProcedureCall'.function, utf8),
	Args = RPC#'CmRemoteProcedureCall'.args,
	Res = Module:Function(Args),
	send_message(Socket, Res, false).

send_message(Socket, Msg, Error) when is_tuple(Msg) ->
	io:format("Response is tuple: ~p~n", [Msg]),
	send_message(Socket, Error, 'ATOM',
				list_to_binary(tuple_to_list(atom_to_binary(Msg, utf8))));	

send_message(Socket, Msg, Error) when is_atom(Msg) ->
	io:format("Response is atom: ~p~n", [Msg]),
	send_message(Socket, Error, 'ATOM', atom_to_binary(Msg, utf8));

send_message(Socket, Msg, Error) when is_list(Msg) ->
	io:format("Response is string: ~p~n", [Msg]),
	send_message(Socket, Error, 'STRING', list_to_binary(Msg)).

send_message(Socket, Error, Type, Msg) ->
	Response = #'Response'{
		error = Error,
		type = Type,
		message = Msg
	},
	EncodedResponse = code_unloading:encode_msg(Response),
	inet:setopts(Socket, [{active, once}]),
	gen_tcp:send(Socket, EncodedResponse).

 