-module(basho_bench_config).

-export([load/1,
         normalize_ips/2,
         set/2,
         get/1, get/2]).

-include("basho_bench.hrl").
-include_lib("kernel/include/logger.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

load(Files) ->
    TermsList =
        [ case file:consult(File) of
              {ok, Terms} ->
                  Terms;
              {error, Reason} ->
                  ?FAIL_MSG("Failed to parse config file ~s: ~p\n", [File, Reason])
          end || File <- Files ],
    load_config(lists:append(TermsList)).

%% @doc Normalize the list of IPs and Ports.
%%
%% E.g.
%%
%% ["127.0.0.1", {"127.0.0.1", 8091}, {"127.0.0.1", [8092,8093]}]
%%
%% => [{"127.0.0.1", DefaultPort},
%%     {"127.0.0.1", 8091},
%%     {"127.0.0.1", 8092},
%%     {"127.0.0.1", 8093}]
normalize_ips(IPs, DefultPort) ->
    F = fun(Entry, Acc) ->
                normalize_ip_entry(Entry, Acc, DefultPort)
        end,
    lists:foldl(F, [], IPs).

set(Key, Value) ->
    ok = application:set_env(basho_bench, Key, Value).

get(Key) ->
    case application:get_env(basho_bench, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            erlang:error("Missing configuration key", [Key])
    end.

get(Key, Default) ->
    case application:get_env(basho_bench, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.


%% ===================================================================
%% Internal functions
%% ===================================================================

load_config([]) ->
    ok;
load_config([{Key, Value} | Rest]) ->
    ?MODULE:set(Key, Value),
    load_config(Rest);
load_config([ Other | Rest]) ->
    ?LOG_WARNING("Ignoring non-tuple config value: ~p\n", [Other]),
    load_config(Rest).

normalize_ip_entry({IP, Ports}, Normalized, _) when is_list(Ports) ->
    [{IP, Port} || Port <- Ports] ++ Normalized;
normalize_ip_entry({IP, Port}, Normalized, _) ->
    [{IP, Port}|Normalized];
normalize_ip_entry(IP, Normalized, DefaultPort) ->
    [{IP, DefaultPort}|Normalized].
