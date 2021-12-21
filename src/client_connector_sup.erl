-module(client_connector_sup).

-behaviour(supervisor).
-define(DEFAULT_PORT, 8089).
-define(CONNECTOR_POOL_SIZE, 20).

%% API
-export([start_link/0,
		start_acceptor/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
	{ok, ListenSocket} = gen_tcp:listen(?DEFAULT_PORT, [binary, {active,once}]),
	io:format("Client connector started on port ~p~n", [?DEFAULT_PORT]),
	spawn_link(fun empty_listeners/0),
	{ok, {{simple_one_for_one, 60, 3600},
	[{acceptor,
	{client_connector, start_link, [ListenSocket]},
	temporary, 1000, worker, [client_connector]}
	]}}.

start_acceptor() ->
	supervisor:start_child(?MODULE, []).

%% Start with 20 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
	[start_acceptor() || _ <- lists:seq(1, ?CONNECTOR_POOL_SIZE)],
	ok.