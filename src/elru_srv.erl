-module(elru_srv).
-behaviour(gen_server).
-include("elru.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, new/1, add/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new(Size) ->
    gen_server:call(?MODULE, {new, Size}).

add(Module, Func, List_Args) ->
    gen_server:call(?MODULE, {add, Module, Func, List_Args}).

init([]) ->
    {ok, {}}.

handle_call({new , MaxSize}, _From, _State) ->
    {reply, ok, {ets:new(elru, [set]),0 ,MaxSize}};
handle_call({add, Module, Func, List_Args}, _From, {Id, Count, MaxSize}) ->
    Bin = erlang:term_to_binary([Module, Func, List_Args]),
    Hash = crypto:hash(md5, Bin),
    case ets:lookup(Id, Hash) of
        [{Key, {_ValCount,Value}}] ->
            ets:delete(Id, Key),
            ets:insert(Id, {Key,{Count, Value}}),
            {reply, Value, {Id,Count + 1 ,MaxSize}};
        _ -> 
            Res = Module:Func(List_Args),
            ets:insert(Id, {Hash, {Count, Res}}),
            gen_server:cast(?MODULE, {check_size}),
            {reply, Res, {Id, Count + 1, MaxSize}}
    end.

handle_cast({check_size}, {Id, Count, MaxSize}) ->
    Size = ets:info(Id, size),
    if 
        Size > MaxSize ->
            List = ets:tab2list(Id),
            Key = minimum(List),
            ets:delete(Id, Key),
            {noreply, {Id, Count, MaxSize}};
        true -> 
            {noreply, {Id, Count, MaxSize}}
    end.

minimum([H|T]) ->
    minimum(H,T).
minimum({Key,{Count,Val}}, [{NewKey,{NewCount,NewVal}}|T]) ->
    case Count < NewCount of
        true -> minimum({Key,{Count,Val}},T);
        false -> minimum({NewKey,{NewCount,NewVal}},T)
    end;
minimum({Key,{_Count,_}}, []) ->
    Key. 