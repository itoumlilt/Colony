
%% @doc
%% A helper for operation-based flags, enable wins flag and disable wins flag.

%% @end
-module(antidote_crdt_flag_helper).


%% Callbacks
-export([ from_binary/1,
          is_operation/1,
          require_state_downstream/1,
          unique/0
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([tokens/0, binary_flag/0, op/0]).

-type binary_flag() :: binary(). %% A binary that from_binary/1 will operate on.

-type op() ::
      {enable, {}}
    | {disable, {}}
    | {reset, {}}.

-type token() :: term().
-type tokens() :: [token()].

%% @doc generate a unique identifier (best-effort).
unique() ->
    crypto:strong_rand_bytes(20).

-define(TAG, 77).
-define(V1_VERS, 1).

from_binary(<<?TAG:8/integer, ?V1_VERS:8/integer, Bin/binary>>) ->
    %% @TODO something smarter
    {ok, binary_to_term(Bin)}.

is_operation({enable, {}}) -> true;
is_operation({disable, {}}) -> true;
is_operation({reset, {}}) -> true;
is_operation(_) -> false.

require_state_downstream(_) -> true.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

-endif.
