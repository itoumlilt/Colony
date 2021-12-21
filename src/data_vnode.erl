%% @doc : This vnode is responsible for being responsible

-module(data_vnode).
-behaviour(riak_core_vnode).
-include("antidote.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
	 get_socket/3,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-record(state, {partition,
                dcid,
		socket_dict
	       }).


start_vnode(I) ->
    {ok, Pid} = riak_core_vnode_master:get_vnode_pid(I, ?MODULE),
    {ok, Pid}.

%% riak_core_vnode call backs
init([Partition]) ->
    DcId = dc_utilities:get_my_dc_id(),
    {ok, #state{partition=Partition,
                dcid=DcId,
		socket_dict=dict:new()
                }}.


get_socket(DcAddress,Port,Key) ->
    DocIdx = riak_core_util:chash_key({?BUCKET, term_to_binary(Key)}),
    Preflist = riak_core_apl:get_primary_apl(DocIdx, 1, materializer),
    [{NewPref,_}] = Preflist,
    {ok,Socket}=riak_core_vnode_master:sync_command(NewPref,{get_socket,DcAddress,Port},data_vnode_master),
    Socket.


handle_command({get_socket,DcAddress,Port},_Sender,State=#state{socket_dict=SocketDict}) ->
    Result = case dict:find({DcAddress,Port},SocketDict) of
		 {ok,Val} ->
		     case erlang:port_info(Val) of
			 undefined ->
			     error;
			 _ ->
			     {ok,Val}
		     end;
		 error ->
		     error
	     end,
    Result2 = case Result of
		  error ->
		      lager:info("creating a new data node connection"),
		      case gen_tcp:connect(DcAddress, Port,
					   [{active,false},binary, {packet,2}], ?CONNECT_TIMEOUT) of
			  { ok, Socket} ->
			      NewDict = dict:store({DcAddress,Port},Socket,SocketDict),
			      {ok,Socket};
			  {error, _Reason} ->
			      lager:error("Couldnot connect to remote DC"),
			      NewDict = SocketDict,
			      error
		      end;
		  {ok,Val1} ->
		      NewDict = SocketDict,
		      {ok,Val1}
	      end,
    {reply,Result2,State#state{socket_dict=NewDict}}.



handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
