-module(ec_readitem_sup).
-behavior(supervisor).

-export([start_fsm/2,
         start_link/0]).

-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_fsm(Partition,Id) ->
    supervisor:start_child(?MODULE, [Partition,Id]).

init([]) ->
    {ok, {{simple_one_for_one, 5, 10},
	  [{ec_readitem_fsm,
	    {ec_readitem_fsm, start_link, []},
	    transient, 5000, worker, [ec_readitem_fsm]}]
	 }}.
