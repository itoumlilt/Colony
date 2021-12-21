%% @doc Supervise the fsm.
-module(collect_sent_time_sup).
-behavior(supervisor).

-export([start_link/2,start_fsm/1]).
-export([init/1]).


start_link(DcId, StartTimestamp) ->
    supervisor:start_link(?MODULE, [DcId, StartTimestamp]).


start_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

init([DcList,_TS]) ->
     Workers = lists:foldl(fun(DcId, Acc) ->
				  Acc ++ [{{collect_sent_time_fsm,DcId},
					   {collect_sent_time_fsm, start_link, [DcId, 0]},
					   permanent, 1000, worker, [collect_sent_time_fsm]}]
			  end, [], DcList),
    {ok, {{one_for_one, 5, 10}, Workers}}.


