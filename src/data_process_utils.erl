-module(data_process_utils).
-define(BLOCK_SIZE, 512).

-export([
		readFile/1,
		groupBy/2,
		groupByKey/1]).

readFile(Path) ->
	case file:open(Path, [read, raw, {read_ahead, ?BLOCK_SIZE}]) of
		{ok, IODevice} -> readLines(IODevice, []);
		Error -> Error
	end.

readLines(IODevice, Acc) ->
	case file:read_line(IODevice) of
		{ok, Line} -> readLines(IODevice, [string:trim(Line)|Acc]);
		eof -> 
			file:close(IODevice),
			lists:reverse(Acc);
		Error -> Error
	end.

groupBy(F, L) ->
	lists:foldl(fun({K,V}, D) -> dict:append(K, V, D) end,
				maps:new(),
				[ {F(X), X} || X <- L ]).

groupByKey(L) -> 
	lists:foldl(fun({K, V}, D) -> dict:append(K,V,D) end,
				dict:new(),
				L).