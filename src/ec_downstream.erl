-module(ec_downstream).

-include("antidote.hrl").

-export([generate_downstream_op/5]).

%% @doc Returns downstream operation for upstream operation
%%      input: Update - upstream operation
%%      output: Downstream operation or {error, Reason}
-spec generate_downstream_op(Node :: term(), Key :: key(),
    Type :: type(), Update :: op(), list()) ->
  {ok, op()} | {error, atom()}.
generate_downstream_op(Node, Key, Type, Update, WriteSet) ->
  {Op, Actor} = Update,
  case ec_vnode:read_data_item(Node, Key, Type, WriteSet) of
    {ok, Snapshot} ->
      case Type:update(Op, Actor, Snapshot) of
        {ok, NewState} ->
          {ok, {merge, NewState}};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.