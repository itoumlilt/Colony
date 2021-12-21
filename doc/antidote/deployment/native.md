# Deploying Antidote Natively

Antidote is currently supported on the Linux platform and needs Erlang 21 or greater as a runtime dependency.

To get a release, [setup your environment](https://github.com/AntidoteDB/AntidoteDB-documentation/blob/master/development/setup.md) and build Antidote by following the [building a release guide](https://github.com/AntidoteDB/AntidoteDB-documentation/blob/master/development/building-a-release.md).

Copy the release folder to the deploy target and start the release binary `bin/antidote` with the node name configured via the OS variable `NODE_NAME` (long).
Other possible configurations can be found in the [Configuring Antidote](https://github.com/AntidoteDB/AntidoteDB-documentation/blob/master/architecture/configuration.md) section.

Example:

    $ pwd
    .../antidote/_build/default/rel/antidote
    $ ls
    bin erts-10.5 lib releases
    
    % start antidote with long name and (data and log) directory in the foreground
    $ NODE_NAME=antidote@127.0.0.1 ROOT_DIR_PREFIX=antidote@127.0.0.1/ bin/antidote foreground
    
    % start antidote with long name and (data and log) directory in the background and attach a remote console to it
    $ NODE_NAME=antidote@127.0.0.1 ROOT_DIR_PREFIX=antidote@127.0.0.1/ bin/antidote start
    $ NODE_NAME=antidote@127.0.0.1 bin/antidote remote_console
    
    % stop antidote
    $ NODE_NAME=antidote@127.0.0.1 bin/antidote stop
    

Clients can now interact with Antidote via the default port configuration (see [Configuration](https://github.com/AntidoteDB/AntidoteDB-documentation/blob/master/architecture/configuration.md)).

Logs and data can be found in the respective directories in the directory denoted by `ROOT_DIR_PREFIX` (default: no prefix, current working folder).
