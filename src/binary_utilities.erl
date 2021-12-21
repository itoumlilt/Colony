-module(binary_utilities).
-include("antidote.hrl").
-include("inter_dc_repl.hrl").

-export([
	 check_message_version/1,
	 check_version_and_req_id/1
	]).

%% Check a binary message version for inter_dc messages 
%% performed by inter_dc_query
-spec check_message_version(<<_:?VERSION_BITS,_:_*8>>) -> <<_:_*8>>.
check_message_version(<<Version:?VERSION_BYTES/binary,Rest/binary>>) ->
    %% Only support one version now
    ?MESSAGE_VERSION = Version,
    Rest.

%% Check a binary message version and the message id for inter_dc messages 
%% performed by inter_dc_query
-spec check_version_and_req_id(<<_:?MESSAGE_HEADER_BIT_LENGTH,_:_*8>>) -> {<<_:?REQUEST_ID_BIT_LENGTH>>,binary()}.
check_version_and_req_id(Binary) ->
    <<ReqId:?REQUEST_ID_BYTE_LENGTH/binary,Rest/binary>> = check_message_version(Binary),
    {ReqId,Rest}.
