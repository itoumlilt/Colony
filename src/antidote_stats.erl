
%%@doc: This module has methods to calculate different metrics. Currently only
%%      metric staleness is supported.
-module(antidote_stats).

%% API
-export([get_value/1, stats/0]).

%% List of configured metrics
stats() ->
    [[staleness]].

get_value(Metric) ->
    calculate(Metric).

%% Calculate staleness by comparing the current stable snapshot and current local
%% time. Staleness is local time - minimum(entry in stable snapshot)
%% Return staleness in millisecs
calculate([staleness]) ->
    {ok, SS} = dc_utilities:get_stable_snapshot(),
    CurrentClock = to_microsec(os:timestamp()),
    Staleness = dict:fold(fun(_K, C, Max) ->
                                   max(CurrentClock - C, Max)
                           end, 0, SS),
    Staleness/(1000); %% To millisecs

calculate(_) ->
    {error, metric_not_found}.

to_microsec({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.
