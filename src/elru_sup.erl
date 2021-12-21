%%%-------------------------------------------------------------------
%% @doc elru top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(elru_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("elru.hrl").
%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ELRU_SRV = ?CHILD(elru_srv,worker,[]),
    {ok, { {one_for_all, 1, 5}, [ELRU_SRV]} }.

%%====================================================================
%% Internal functions
%%====================================================================
