-module(antidote_error_monitor).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_info/2, terminate/2]).

init(_Args) ->
  {ok, []}.

handle_event({error, _Gleader, {_Pid, _Format, _Data}}, State) ->
  prometheus_counter:inc(antidote_error_count),
  {ok, State};

handle_event({error_report, _Gleader, {_Pid, std_error, _Report}}, State) ->
  prometheus_counter:inc(antidote_error_count),
  {ok, State};

handle_event({error_report, _Gleader, {_Pid, _Type, _Report}}, State) ->
  prometheus_counter:inc(antidote_error_count),
  {ok, State};

handle_event(_, State) ->
  {ok, State}.

handle_info(_, State) ->
  {ok, State}.

terminate(_Args, _State) ->
  ok.
