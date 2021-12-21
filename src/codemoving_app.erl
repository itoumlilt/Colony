-module(codemoving_app).

-behaviour(application).
-include("code_unloading.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    codemoving_sup:start_link().

stop(_State) ->
    ok.
