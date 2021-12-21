%% @doc Supervise the fsm.
-module(inter_dc_communication_process_updates_fsm_sup).
-behavior(supervisor).

-export([start_fsm/1,
         start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).


init([]) ->
    Worker = {inter_dc_communication_process_updates_fsm,
              {inter_dc_communication_process_updates_fsm, start_link, []},
              transient, 5000, worker, [inter_dc_communication_process_updates_fsm]},
    {ok, {{simple_one_for_one, 5, 10}, [Worker]}}.
