-module(basho_bench_app).
-include_lib("kernel/include/logger.hrl").

-behaviour(application).

%% API
-export([start/0,
         stop/0,
         is_running/0,
         halt_or_kill/0]).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% API
%%===================================================================

start() ->
    %% Redirect all SASL logging into a text file
    case application:get_env(basho_bench,app_run_mode) of
       {ok, included} ->
          %%Make sure sasl and crypto is available
          true=lists:keymember(sasl,1,application:which_applications()),
          true=lists:keymember(crypto,1,application:which_applications()),

          %% Start up our application
          application:start(basho_bench);
       NotInc when NotInc == {ok, standalone} orelse NotInc == undefined ->
          application:load(sasl),
          %% Make sure crypto is available
          ensure_started([sasl, crypto, bear]),

          %% Start up our application -- mark it as permanent so that the node
          %% will be killed if we go down
          application:start(basho_bench, permanent)
    end.

stop() ->
    application:stop(basho_bench).

is_running() ->
    application:get_env(basho_bench_app, is_running) == {ok, true}.

halt_or_kill() ->
    %% If running standalone, halt and kill node.  Otherwise, just
    %% kill top supervisor.
    case application:get_env(basho_bench,app_run_mode) of
        {ok, included} ->
            exit(whereis(basho_bench_sup),kill);
        _ ->
            init:stop()
    end.

%% ===================================================================
%% Application callbacks
%%===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = basho_bench_sup:start_link(),
    application:set_env(basho_bench_app, is_running, true),
    ok = basho_bench_stats:run(),
    ok = basho_bench_measurement:run(),
    ok = basho_bench_worker:run(basho_bench_sup:workers()),
    {ok, Pid}.


stop(_State) ->
    %% intentionally left in to show where worker profiling start/stop calls go.
    %% eprof:stop_profiling(),
    %% eprof:analyze(total),
    %% eprof:log("bb.eprof"),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started(Applications) when is_list(Applications) ->
  [ensure_started(Application) || Application <- Applications];

ensure_started(Application) ->
  case application:start(Application) of
    ok ->
      ok;
    {error, {already_started, Application}} ->
      ok;
    Error ->
      throw(Error)
  end.
