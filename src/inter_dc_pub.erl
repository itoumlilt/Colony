%% InterDC publisher - holds a ZeroMQ PUB socket and makes it available for Antidote processes.
%% This vnode is used to publish interDC transactions.

-module(inter_dc_pub).
-behaviour(gen_server).
-include("antidote.hrl").
-include("inter_dc_repl.hrl").

%% API
-export([
  broadcast/1,
  get_address/0,
  get_address_list/0]).

%% Server methods
-export([
  init/1,
  start_link/0,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% State
-record(state, {socket}). %% socket :: erlzmq_socket()

%%%% API --------------------------------------------------------------------+

-spec get_address() -> socket_address().
get_address() ->
  %% TODO check if we do not return a link-local address
  {ok, List} = inet:getif(),
  {Ip, _, _} = hd(List),
  Port = application:get_env(antidote, pubsub_port, ?DEFAULT_PUBSUB_PORT),
  {Ip, Port}.

-spec get_address_list() -> [socket_address()].
get_address_list() ->
    {ok, List} = inet:getif(),
    Port = application:get_env(antidote, pubsub_port, ?DEFAULT_PUBSUB_PORT),
    [{Ip1, Port} || {Ip1, _, _} <- List, Ip1 /= {127, 0, 0, 1}].

-spec broadcast(#interdc_txn{}) -> ok.
broadcast(Txn) ->
  case catch gen_server:call(?MODULE, {publish, inter_dc_txn:to_bin(Txn)}) of
    {'EXIT', _Reason} -> lager:warning("Failed to broadcast a transaction."); %% this can happen if a node is shutting down.
    Normal -> Normal
  end.

%%%% Server methods ---------------------------------------------------------+

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {_, Port} = get_address(),
  Socket = zmq_utils:create_bind_socket(pub, false, Port),
  lager:info("Publisher started on port ~p", [Port]),
  {ok, #state{socket = Socket}}.

handle_call({publish, Message}, _From, State) -> {reply, erlzmq:send(State#state.socket, Message), State}.

terminate(_Reason, State) -> erlzmq:close(State#state.socket).
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
