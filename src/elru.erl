-module(elru).
-export([start/0, stop/0, new/1, add/3]).

start() ->
    application:start(elru).
stop() -> 
    application:stop(elru).

new(Size) ->
    elru_srv:new(Size).

add(Module, Func, List_Args) ->
    elru_srv:add(Module, Func, List_Args).