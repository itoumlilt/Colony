-module(antidote_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% PB Services
-define(SERVICES, [{antidote_pb_txn, 107, 128}]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case antidote_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, logging_vnode}]),
            ok = riak_core_node_watcher:service_up(logging, self()),
            %%ClockSI layer

            ok = riak_core:register([{vnode_module, clocksi_vnode}]),
            ok = riak_core_node_watcher:service_up(clocksi, self()),

            ok = riak_core:register([{vnode_module, materializer_vnode}]),
            ok = riak_core_node_watcher:service_up(materializer, self()),

            ok = riak_core:register([{vnode_module, inter_dc_log_sender_vnode}]),
            ok = riak_core_node_watcher:service_up(logsender, self()),

            ok = riak_core:register([{vnode_module, inter_dc_sub_vnode}]),
            ok = riak_core_node_watcher:service_up(inter_dc_sub, self()),

            ok = riak_core:register([{vnode_module, inter_dc_dep_vnode}]),
            ok = riak_core_node_watcher:service_up(inter_dc_dep, self()),

            ok = riak_core_ring_events:add_guarded_handler(antidote_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(antidote_node_event_handler, []),
            ok = riak_api_pb_service:register(?SERVICES),

	    _IsRestart = inter_dc_manager:check_node_restart(),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
