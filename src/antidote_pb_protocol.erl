
-module(antidote_pb_protocol).
% This module handles the protocol buffer protocol.
% It provides callbacks used by the ranch library.

-behaviour(ranch_protocol).

-include_lib("kernel/include/logger.hrl").

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

init(Ref, Socket, Transport, _Opts) ->
  ok = ranch:accept_ack(Ref),
  % Each message starts with 4 byte denoting the length of the
  % package. The setting {packet, 4} tells the socket library
  % to use this encoding (it is one of the builtin protocols of Erlang)
  ok = Transport:setopts(Socket, [{packet, 4}]),
  loop(Socket, Transport).

% Receive-Respond loop for handling connections:
loop(Socket, Transport) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      handle(Socket, Transport, Data),
      loop(Socket, Transport);
    {error, closed} ->
      ok = Transport:close(Socket);
    {error, timeout} ->
      ?LOG_ERROR("Socket ~p timed out", [Socket]),
      ok = Transport:close(Socket);
    {error, Reason} ->
      ?LOG_ERROR("Socket error: ~p", [Reason]),
      ok = Transport:close(Socket)
  end.


% handles a single request
-spec handle(_Socket, _Transport, binary()) -> ok.
handle(Socket, Transport, Msg) ->
  DecodedMessage = antidote_pb_codec:decode_request(Msg),
  try
    Response = antidote_pb_process:process(DecodedMessage),
    PbMessage = antidote_pb_codec:encode_response(Response),
    ok = Transport:send(Socket, PbMessage),
    ok
  catch
    ExceptionType:Error:StackTrace ->
      % log errors and reply with error message:
      ?LOG_ERROR("Error ~p: ~p~n~p~nWhen handling request ~p~n", [ExceptionType, Error, StackTrace, DecodedMessage]),
      % when formatting the error message, we use a maximum depth of 9001.
      % This should be big enough to include useful information, but avoids sending a lot of data
      MessageStr = erlang:iolist_to_binary(io_lib:format("~P: ~P~n~P~n", [ExceptionType, 9001, Error, 9001, StackTrace, 9001])),
      Message = antidote_pb_codec:encode_response({error_response, {unknown, MessageStr}}),
      ok = Transport:send(Socket, Message),
      ok
  end.
