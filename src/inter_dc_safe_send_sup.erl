%% @doc Supervise the fsm.
-module(inter_dc_safe_send_sup).
-behavior(supervisor).


-export([start_fsm/1,
         start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

init([]) ->
    Worker = {inter_dc_safe_send_fsm,
              {inter_dc_safe_send_fsm, start_link, []},
              permanent, 5000, worker, [inter_dc_safe_send_fsm]},
    {ok, {{one_for_one, 5, 10}, [Worker]}}.
