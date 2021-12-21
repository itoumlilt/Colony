-module(ec_interactive_tx_coord_sup).
-author("Christopher Meiklejohn <christopher.meiklejohn@gmail.com>").

-behavior(supervisor).

-include("antidote.hrl").

-export([start_fsm/1,
         start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_fsm(Args) ->
    _Res = random:seed(dc_utilities:now()),
    Random = random:uniform(?NUM_SUP),
    Module = generate_module_name(Random),
    supervisor:start_child(Module, Args).

generate_module_name(N) ->
    list_to_atom(atom_to_list(?MODULE) ++ "-" ++ integer_to_list(N)).

generate_supervisor_spec(N) ->
    Module = generate_module_name(N),
    {Module,
     {ec_interactive_tx_coord_worker_sup, start_link, [Module]},
      permanent, 5000, supervisor, [ec_interactive_tx_coord_worker_sup]}.

%% @doc Starts the coordinator of a ec interactive transaction.
init([]) ->
    Pool = [generate_supervisor_spec(N) || N <- lists:seq(1, ?NUM_SUP)],
    {ok, {{one_for_one, 5, 10}, Pool}}.
