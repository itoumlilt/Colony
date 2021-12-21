
%%@doc: This module periodically collects different metrics (currently only staleness)
%%  and updates exometer metrics. Monitoring tools can then read it from exometer.

-module(antidote_stats_collector).

-behaviour(gen_server).
%% Interval to collect metrics
-define(INTERVAL, 10000). %% 10 sec
%% Metrics collection will be started after INIT_INTERVAL after application startup.
-define(INIT_INTERVAL, 10000). %% 10 seconds

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    init_metrics(),
    Timer = erlang:send_after(?INIT_INTERVAL, self(), periodic_update),
    {ok, Timer}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({update, Metric}, State) ->
    Val = antidote_stat:get_value(Metric),
    exometer:update(Metric, Val),
    {noreply, State};

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(periodic_update, OldTimer) ->
    erlang:cancel_timer(OldTimer),
    update([staleness]),
    Timer = erlang:send_after(?INTERVAL, self(), periodic_update),
    {noreply, Timer}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update(Metric) ->
    Val = antidote_stats:get_value(Metric),
    exometer:update(Metric, Val).

init_metrics() ->
    Metrics = antidote_stats:stats(),
    lists:foreach(fun(Metric) ->
                          exometer:new(Metric, histogram, [{time_span, timer:seconds(60)}])
                  end, Metrics).
