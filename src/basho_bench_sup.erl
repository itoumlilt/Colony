-module(basho_bench_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         workers/0,
         stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

-include("basho_bench.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

workers() ->
    [Pid || {_Id, Pid, worker, [basho_bench_worker]} <- supervisor:which_children(?MODULE)].

stop_child(Id) ->
    ok = supervisor:terminate_child(?MODULE, Id),
    ok = supervisor:delete_child(?MODULE, Id).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% Get the number concurrent workers we're expecting and generate child
    %% specs for each

    %% intentionally left in to show where worker profiling start/stop calls go.
    %% eprof:start(),
    %% eprof:start_profiling([self()]),
    Workers = worker_specs(basho_bench_config:get(concurrent), []),
    MeasurementDriver =
        case basho_bench_config:get(measurement_driver, undefined) of
            undefined -> [];
            _Driver -> [?CHILD(basho_bench_measurement, worker)]
        end,

    {ok, {{one_for_one, 5, 10},
        [?CHILD(basho_bench_stats, worker)] ++
        Workers ++
        MeasurementDriver
    }}.

%% ===================================================================
%% Internal functions
%% ===================================================================

worker_specs(0, Acc) ->
    Acc;
worker_specs(Count, Acc) ->
    Id = list_to_atom(lists:concat(['basho_bench_worker_', Count])),
    Spec = {Id, {basho_bench_worker, start_link, [Id, Count]},
            permanent, 5000, worker, [basho_bench_worker]},
    worker_specs(Count-1, [Spec | Acc]).
