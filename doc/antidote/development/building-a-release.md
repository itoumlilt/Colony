# Building a Release

Use 

	make rel
	
to build a release. 

It can be found under `_build/default/rel/antidote`.


Using a Release
-----------

After building a release with `make rel`, Antidote can be started with the generated executable file.
The release executable is located at `_build/default/bin/rel/antidote/releases/bin/antidote`.
It should be started with the node name and cookie configured. 
Setting the cookie is optional, setting the node name is mandatory.

	# start antidote with given short node name as a foreground process
	NODE_NAME=testing ./antidote foreground
	
	# start antidote with given long node name with an erlang interactive console attached
	NODE_NAME=testing ./antidote console
	
	
Dynamic Release Configuration
-----------

The release executable is dynamically configurable with OS environment variables.
The default configuration can be found in the config file folder `config`.
Only `NODE_NAME`, `COOKIE`, `ROOT_DIR_PREFIX`, `LOGGER_DIR_PREFIX` and `DATA_DIR_PREFIX` are expected to be provided via OS environment variables.
All other configuration has to be set via [erlang application environment arguments](http://erlang.org/doc/man/erl.html#flags) `-Application Par Val`, 
where variable expansion can be used in the `Val` part.

Example:



	# start antidote with a unified directory, set logging level to warning
	# assume NODE_NAME, ROOT_PREFIX_DIR (`antidote/`), P1, P2, P3, P4, and P5 OS variables 
	# have been exported before
	# modify appliation configurations (see sys.config.src and other .config files for options)
	# logging level and all ports
	# disable certification checks for concurrent transactions
	./antidote foreground \
	-kernel logging_level warning \
	-riak_core handoff_port $P1 \
	-ranch pb_port $P2 \
	-antidote pubsub_port $P3 \
	-antidote logreader_port $P4 \
	-antidote_stats metrics_port $P5 \
	-antidote txn_check false

