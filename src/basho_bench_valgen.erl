-module(basho_bench_valgen).

-export([new/2,
         dimension/2]).

-include("basho_bench.hrl").
-include_lib("kernel/include/logger.hrl").


%% ====================================================================
%% API
%% ====================================================================

new({fixed_bin, Size}, Id)
  when is_integer(Size), Size >= 0 ->
    Source = init_source(Id),
    fun() -> data_block(Source, Size) end;
new({fixed_bin, Size, Val}, _Id)
  when is_integer(Size), Size >= 0, is_integer(Val), Val >= 0, Val =< 255 ->
    Data = list_to_binary(lists:duplicate(Size, Val)),
    fun() -> Data end;
new({fixed_char, Size}, _Id)
  when is_integer(Size), Size >= 0 ->
    fun() -> list_to_binary(lists:map(fun (_) -> rand:uniform(95)+31 end, lists:seq(1,Size))) end;
new({exponential_bin, MinSize, Mean}, Id)
  when is_integer(MinSize), MinSize >= 0, is_number(Mean), Mean > 0 ->
    Source = init_source(Id),
    fun() -> data_block(Source, MinSize + trunc(basho_bench_stats:exponential(1 / Mean))) end;
new({uniform_bin, MinSize, MaxSize}, Id)
  when is_integer(MinSize), is_integer(MaxSize), MinSize < MaxSize ->
    Source = init_source(Id),
    Diff = MaxSize - MinSize,
    fun() -> data_block(Source, MinSize + rand:uniform(Diff)) end;
new({function, Module, Function, Args}, Id)
  when is_atom(Module), is_atom(Function), is_list(Args) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            erlang:apply(Module, Function, [Id] ++ Args);
        _Error ->
            ?FAIL_MSG("Could not find valgen function: ~p:~p\n", [Module, Function])
    end;
new({uniform_int, MaxVal}, _Id)
  when is_integer(MaxVal), MaxVal >= 1 ->
    fun() -> rand:uniform(MaxVal) end;
new({uniform_int, MinVal, MaxVal}, _Id)
  when is_integer(MinVal), is_integer(MaxVal), MaxVal > MinVal ->
    fun() -> rand:uniform(MinVal, MaxVal) end;
new(Other, _Id) ->
    ?FAIL_MSG("Invalid value generator requested: ~p\n", [Other]).

dimension({fixed_bin, Size}, KeyDimension) ->
    Size * KeyDimension;
dimension(_Other, _) ->
    0.0.



%% ====================================================================
%% Internal Functions
%% ====================================================================

init_source(Id) ->
    init_source(Id, basho_bench_config:get(?VAL_GEN_BLOB_CFG, undefined)).

init_source(Id, undefined) ->
    if Id == 1 -> ?LOG_DEBUG("random source\n", []);
       true    -> ok
    end,
    SourceSz = basho_bench_config:get(?VAL_GEN_SRC_SIZE, 1048576),
    {?VAL_GEN_SRC_SIZE, SourceSz, crypto:strong_rand_bytes(SourceSz)};
init_source(Id, Path) ->
    {Path, {ok, Bin}} = {Path, file:read_file(Path)},
    if Id == 1 -> ?LOG_DEBUG("path source ~p ~p\n", [size(Bin), Path]);
       true    -> ok
    end,
    {?VAL_GEN_BLOB_CFG, size(Bin), Bin}.

data_block({SourceCfg, SourceSz, Source}, BlockSize) ->
    case SourceSz - BlockSize > 0 of
        true ->
            Offset = rand:uniform(SourceSz - BlockSize),
            <<_:Offset/bytes, Slice:BlockSize/bytes, _Rest/binary>> = Source,
            Slice;
        false ->
            ?LOG_WARNING("~p is too small ~p < ~p\n",
                  [SourceCfg, SourceSz, BlockSize]),
            Source
    end.
