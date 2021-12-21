-module(ec_static_tx_coord_worker_sup).
-author("Christopher Meiklejohn <christopher.meiklejohn@gmail.com>").

-behavior(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

%% @doc Starts the coordinator of a ec static transaction.
init([]) ->
    Worker = {ec_static_tx_coord_fsm,
              {ec_static_tx_coord_fsm, start_link, []},
               temporary, 5000, worker, [ec_static_tx_coord_fsm]},
    {ok, {{simple_one_for_one, 5, 10}, [Worker]}}.
