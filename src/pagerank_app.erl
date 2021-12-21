%%%-------------------------------------------------------------------
%% @doc pagerank public API
%% @end
%%%-------------------------------------------------------------------

-module(pagerank_app).

-behaviour(application).
-define(PATH_USER, "/tmp/facebook/facebook_users_prop.csv").
-define(PATH_EDGE, "/tmp/facebook/facebook_edges_prop.csv").
-define(ADDRESS, "localhost").
-define(PORT, 8087).
-define(UPDATE_NUMBER_BY_TRANSACTION, 18000).

%%===============
%% USER KEYS
%%===============

-define(USER_BUCKET, <<"pagerank_user_bucket">>).
-define(EDGE_BUCKET, <<"pagerank_edge_bucket">>).
-define(FIRSTNAME, <<"firstname">>).
-define(NAME, <<"name">>).
-define(AGE, <<"age">>).
-define(RELATIONS, <<"relations">>).

%%===============
%% EDGE KEYS
%%===============

-define(SOURCE, <<"source">>).
-define(DESTINATION, <<"destination">>).
-define(TYPE, <<"relation_type">>).
-define(EXCHANGED_MESSAGES, <<"exchanged_messages">>).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([populate_database/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	{ok, Pid} = pagerank_sup:start_link(),
	{ok, Pid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

populate_database() ->	
	Users = data_process_utils:readFile(?PATH_USER),
	Edges = data_process_utils:readFile(?PATH_EDGE),
	{ok, Socket} = antidotec_pb_socket:start(?ADDRESS, ?PORT),
	io:format("Create users~n"),
    {ok, Tx} = antidotec_pb:start_transaction(Socket, ignore, {}),
	antidotec_pb:update_objects(Socket,
						[create_user_update(create_user(User)) || User <- Users],
								Tx),
	{ok, _} = antidotec_pb:commit_transaction(Socket, Tx),
	io:format("Create edges~n"),
	EdgeIds = lists:seq(0, length(Edges) - 1),
	Relations = [create_edge(Edge, ID) || {Edge, ID} <- lists:zip(Edges, EdgeIds)],
	io:format("Create edges updates~n"),
	EdgeUpdates = create_edge_updates(Relations),
	%% Requires because antidote can not handle two much operation in one transaction
	io:format("Send edges updates~n"),
	send_big_updates(Socket, EdgeUpdates).
    
%%% UserInfo is a string that takes the form "Id, First name, Name, Age"
create_user(UserInfo) ->
	[Id , Firstname , Name , Age] = string:split(UserInfo, ",", all),
	#{id => Id, ?FIRSTNAME => Firstname, ?NAME => Name, ?AGE => list_to_integer(Age)}.

create_user_update(#{id := Id,
			?FIRSTNAME := Firstname,
			?NAME := Name,
			?AGE := Age}) ->
	UserKey = {list_to_binary(Id), antidote_crdt_map_go, ?USER_BUCKET},
    {UserKey, update, [
        {{?FIRSTNAME, antidote_crdt_register_lww}, {assign, list_to_binary(Firstname)}},
        {{?NAME, antidote_crdt_register_lww}, {assign, list_to_binary(Name)}},
        {{?AGE, antidote_crdt_counter_pn}, {increment , Age}}
	]}.

%%% EdgeInfo is a string that takes the form
%% "Source (UserID), Destination (UserId), RelationType, Number of exchanged messages"
%% EdgeId is an integer
create_edge(EdgeInfo, EdgeId) ->
	[Source, Destination, Type, MsgNumber] = string:split(EdgeInfo, ",", all),
	#{id => integer_to_list(EdgeId),
	  source => Source,
	  destination => Destination,
	  type => Type,
	  exchangedMessages => MsgNumber}.

create_edge_updates(Edges) ->
	create_edge_updates(Edges, []).

create_edge_updates([], Acc) -> Acc;

create_edge_updates([#{id := Id,
					source := Source,
					destination := Destination,
					type := Type,
					exchangedMessages := MsgNumber}| Tail], Acc) ->
	SourceKey = {list_to_binary(Source), antidote_crdt_map_go, ?USER_BUCKET},
	DestinationKey = {list_to_binary(Destination), antidote_crdt_map_go, ?USER_BUCKET},
	EdgeKey = {list_to_binary(Id), antidote_crdt_map_go, ?EDGE_BUCKET},
	EdgeUpdate = {EdgeKey, update, [
			 {{?SOURCE, antidote_crdt_register_lww}, {assign, list_to_binary(Source)}},
			 {{?DESTINATION, antidote_crdt_register_lww}, {assign, list_to_binary(Destination)}},
			 {{?TYPE, antidote_crdt_register_lww}, {assign, list_to_binary(Type)}},
			 {{?EXCHANGED_MESSAGES, antidote_crdt_counter_pn}, {increment, list_to_integer(MsgNumber)}}
	]},
	SourceUpdate = {SourceKey, update, [  
			 {{?RELATIONS, antidote_crdt_set_aw}, {add, list_to_binary(Id)}}
	]},
	DestinationUpdate = {DestinationKey, update, [
			 {{?RELATIONS, antidote_crdt_set_aw}, {add, list_to_binary(Id)}}
	]},
	create_edge_updates(Tail, [EdgeUpdate , SourceUpdate , DestinationUpdate | Acc]).

send_big_updates(Socket, UpdateList) ->
	send_big_updates(Socket, UpdateList, length(UpdateList)).


send_big_updates(_Socket, [], _Length) -> ok;

send_big_updates(Socket, UpdateList, Length) when Length < ?UPDATE_NUMBER_BY_TRANSACTION ->
	[Head|_Tail] = UpdateList,
	io:format("Send ~p~n", [Head]),
	{ok, Tx} = antidotec_pb:start_transaction(Socket, ignore, {}),
	antidotec_pb:update_objects(Socket,
								UpdateList,
								Tx),
	{ok, _} = antidotec_pb:commit_transaction(Socket, Tx);

send_big_updates(Socket, UpdateList, Length) ->
	{Updates, Rest} = lists:split(?UPDATE_NUMBER_BY_TRANSACTION, UpdateList),
	[Head|_Tail] = Updates,
	io:format("Send 2 ~p~n", [Head]),
	{ok, Tx} = antidotec_pb:start_transaction(Socket, ignore, {}),
	antidotec_pb:update_objects(Socket,
								Updates,
								Tx),
	{ok, _} = antidotec_pb:commit_transaction(Socket, Tx),
	send_big_updates(Socket, Rest, Length - ?UPDATE_NUMBER_BY_TRANSACTION).