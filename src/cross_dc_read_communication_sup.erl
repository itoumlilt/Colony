%% @doc : The supervisor in charge of all the socket acceptors.
%%  Supervisor starts up a pool of listeners which can accept incoming
%%  connection request from other DCs

-module(cross_dc_read_communication_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
    Listener = {cross_dc_read_communication_recvr,
                {cross_dc_read_communication_recvr, start_link, [Port]}, % pass the socket!
                permanent, 1000, worker, [cross_dc_read_communication_recvr]},

    SupWorkers = {cross_dc_read_communication_perform_read_fsm_sup,
		  {cross_dc_read_communication_perform_read_fsm_sup, start_link, []},
		  permanent, 1000, supervisor, [cross_dc_read_communication_perform_read_fsm_sup]},
    SupWorkers1 = {cross_dc_read_communication_fsm_sup,
		  {cross_dc_read_communication_fsm_sup, start_link, []},
		  permanent, 1000, supervisor, [cross_dc_read_communication_fsm_sup]},
    {ok, {{one_for_one, 60, 3600}, [Listener, SupWorkers, SupWorkers1]}}.
