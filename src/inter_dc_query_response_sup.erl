-module(inter_dc_query_response_sup).

-behavior(supervisor).

-include("antidote.hrl").

-export([start_link/1]).

-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

generate_module_name(N) ->
    list_to_atom(atom_to_list(?MODULE) ++ "-" ++ integer_to_list(N)).

generate_worker_spec(N) ->
    Module = generate_module_name(N),
    {Module,
     {inter_dc_query_response, start_link, [N]},
     permanent, 5000, worker, [inter_dc_query_response]}.

%% @doc Start the log response readers
init(Num) ->
    Pool = [generate_worker_spec(N) || N <- lists:seq(1, Num)],
    {ok, {{one_for_one, 5, 10}, Pool}}.
