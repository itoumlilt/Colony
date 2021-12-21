-module(codemoving_messages).

-export([
	msg_code/1,
	code_func/1
]).

msg_code(store_module) -> 200;
msg_code(rpc) -> 201.

code_func(200) -> store_module;
code_func(201) -> rpc.