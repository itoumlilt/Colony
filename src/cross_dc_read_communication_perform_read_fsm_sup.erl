%% @doc Supervise the fsm.
-module(cross_dc_read_communication_perform_read_fsm_sup).
-behavior(supervisor).

-export([start_fsm/1,
         start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).


init([]) ->
    Worker = {cross_dc_read_communication_perform_read_fsm,
              {cross_dc_read_communication_perform_read_fsm, start_link, []},
              transient, 5000, worker, [cross_dc_read_communication_perform_read_fsm]},
    {ok, {{simple_one_for_one, 5, 10}, [Worker]}}.
