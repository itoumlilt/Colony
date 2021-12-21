-module(eiger_downstream).

-include("antidote.hrl").

-export([generate_downstream_op/1]).

generate_downstream_op(Update) ->
    {_Key, Type, {Op, Actor}} = Update,
    case Type of
        crdt_pncounter ->
            {ok, OpParam} = Type:generate_downstream(Op, Actor, useless),
            {update, OpParam};
        _ ->
            {error, type_not_supported_in_eiger}
    end.
