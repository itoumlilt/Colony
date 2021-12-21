-module(hello).
-define(NAME, "Mario").

-export([hello/1]).

hello([]) ->
	"Hello World".
