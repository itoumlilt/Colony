-module(lru_cache_graph).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

-define(MAX_EDGE_COUNT, 4).

-record(vertex, {
          key :: any()
         }).
-record(edge, {
          left :: #vertex{},
          right :: #vertex{}
         }).

%% escript Entry point
main(_Args) ->
    A = #vertex{key=1},
    B = #vertex{key=2},
    C = #vertex{key=3},
    D = #vertex{key=4},
    E = #vertex{key=5},
    Cache = #{ 1 => one,
               2 => two,
               3 => three,
               4 => four,
               5 => five
             },
    {_, Graph} = lists:foldl(fun (Vertex, {Edge, Graph}) ->
                                 NewEdge = Edge#edge{left=Vertex},
                                 {#edge{right=Vertex}, [NewEdge|Graph]}
                         end, {#edge{right=E}, []}, [D, C, B, A]),
    io:format("graph=~p~n~n", [Graph]),

    {one, Cache, Graph} = get(1, Cache, Graph),
    io:format("graph=~p~n~n", [Graph]),

    {99, Cache2, Graph2} = get(9, Cache, Graph),
    io:format("graph=~p~n~n", [Graph2]),

    {99, Cache3, Graph3} = get(hello, Cache2, Graph2),
    io:format("graph=~p~n~n", [Graph3]),

    {two, Cache3, Graph4} = get(2, Cache3, Graph3),
    io:format("graph=~p~n~n", [Graph4]),

    {four, Cache, Graph5} = get(4, Cache, Graph),
    io:format("graph=~p~n~n", [Graph5]),

    {five, Cache, Graph6} = get(5, Cache, Graph),
    io:format("graph=~p~n~n", [Graph6]),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-spec get(any(), map(), [#edge{}]) -> {any(), map(), [#edge{}]}.
get(Key, Map, Graph) ->
    case maps:get(Key, Map, undefined) of
        undefined ->
            %% load from storage
            FakeValue = 99,
            {NewMap, NewGraph} = put(Key, FakeValue, Map, Graph),
            {FakeValue, NewMap, NewGraph};
        Value ->
            {NewGraph, _ToDrop} = reorder_graph(Key, Graph), % _ToDrop is empty in this case
            {Value, Map, NewGraph}
    end.

-spec put(any(), any(), map(), [#edge{}]) -> {map(), [#edge{}]}.
put(Key, Value, Cache, Graph) ->
    {NewGraph, ToDrop} = reorder_graph(Key, Graph),
    NewCache = lists:foldl(fun (#vertex{key=K}, C) ->
                                   maps:remove(K, C)
                           end, Cache, ToDrop),
    {NewCache#{Key => Value}, NewGraph}.

-spec reorder_graph(any(), [#edge{}]) -> {[#edge{}], [#vertex{}]}.
reorder_graph(_Key, Graph) when length(Graph) > ?MAX_EDGE_COUNT ->
    %% only support adding one key at a time right now
    erlang:error({bad_graph, Graph});
reorder_graph(Key, Graph) ->
    NewFirstVertex = #vertex{key=Key},
    NewFirstEdge = #edge{left=NewFirstVertex},
    rebuild_edges(NewFirstEdge, Graph).

-spec rebuild_edges(#edge{}, [#edge{}]) -> {[#edge{}], [#vertex{}]}.
rebuild_edges(FirstEdge=#edge{left=FirstVertex}, Edges) ->
    Vertexes = lists:map(fun (#edge{left=L, right=R}) ->
                                 [L, R]
                         end, Edges),
    NewVertexes = lists:filter(fun (V) ->
                                       V =/= FirstVertex
                               end, lists:flatten(Vertexes)),
    {_, Graph} = lists:foldl(fun (V, Acc={#edge{left=V},_}) ->
                                     Acc;
                                 (V, {Edge, Graph}) ->
                                     NewEdge = Edge#edge{right=V},
                                     {#edge{left=V}, [NewEdge|Graph]}
                             end, {FirstEdge, []}, NewVertexes),
    trim_graph(lists:reverse(Graph)).

-spec trim_graph([#edge{}]) -> {[#edge{}], [#vertex{}]}.
trim_graph(Graph) ->
    {TrimedGraph, Rest} = lists:split(?MAX_EDGE_COUNT, Graph),
    ToDrop = lists:filtermap(fun (#edge{right=R}) ->
                                     {true, R}
                       end, Rest),
    {TrimedGraph, ToDrop}.
