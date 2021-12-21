# Basho-bench for Antidote

The basho_bench for benchmarking antidote is available in the repository [AntidoteDB/Benchmarks](https://github.com/AntidoteDB/Benchmarks).

This benchmark is build on the original benchmark for riak core called [basho_bench](https://github.com/basho/basho_bench).
Documentation on the benchmark for riakKV can be found at [basho_bench_documentation](https://docs.riak.com/riak/kv/latest/using/performance/benchmarking). It also holds further information about the capabilities of basho_bench.


There are two additional files, that make the original basho_bench compatible with Antidote:
- [antidote_pb.config](https://github.com/AntidoteDB/Benchmarks/blob/master/examples/antidote_pb.config)
- [basho_bench_driver_antidote_pb.erl](https://github.com/AntidoteDB/Benchmarks/blob/master/src/basho_bench_driver_antidote_pb.erl)

The configuration file is given as a starting parameter to the basho_bench. The section for [Configuration](#configuration) will cover how to configure the benchmark.  
The driver makes the workloads that were configured to be benchmarked for the specified targets. The section [Driver](#driver) will cover this in more detail.

## Configuration
An exemplary configuration file is [antidote_pb.config](https://github.com/AntidoteDB/Benchmarks/blob/master/examples/antidote_pb.config). It also contains more detailed commentary on each possible setting.  
The example configuration uses the [basho_bench_driver_antidote_pb.erl](https://github.com/AntidoteDB/Benchmarks/blob/master/src/basho_bench_driver_antidote_pb.erl) driver.  

### Target:

The following two lines of the configuration file specify which Antidote nodes protocol buffer interfaces the basho_bench node should target.

 - `{antidote_pb_ips, ['127.0.0.1']}`. 
 - `{antidote_pb_port, [8087]}`.

Make sure the given ip-addresses are actually accessible from the node on which the benchmark is running.
The port 8087 is the set port for protocol buffer messages inside the antidote node. Unless port-mappings or other redirection of traffic is made to access the antidote node this may stay the same.  
It is possible to specify multiple ip-addresses and a single port, as well as a single ip-address and multiple ports, which is useful if you want to benchmark a local Antidote cluster.

### Driver:

- `{driver, basho_bench_driver_antidote_pb}`.

This line specifies the main erlang file, that specifies which operations my be configured. Here the name of this driver file is given, as defined in the src directory. It is possible to write your own drivers. See [Driver](#driver) for further information.

### Operations:

This line configures which operations the driver should run and which weight each operation receives.

- `{operations, [{op1, 4}, {op2, 1}]}.`

This line will make the basho_bench run operations `op1` and `op2`, where out of 5 operations `op1` will be executed 4 times, while `op2` will be executed 1 time. The possible operations are defined in the driver.  

Possible configurations, when using the [basho_bench_driver_antidote_pb.erl](https://github.com/AntidoteDB/Benchmarks/blob/master/src/basho_bench_driver_antidote_pb.erl) are the following:
`txn`, `update_only_txn`, `read_only_txn`, `append`, `read`.

Also defined in the configuration file are parameters for transactions, that are <b>not</b> `read` or `append`, e.g.:

- `{num_reads, 10}.` The number of reads done for each transaction, that do reads.
- `{num_updates, 10}.` The number of updates done for each transaction, that do updates.

- `{sequential_reads, false}.` If set to `true`, the client will send each read (of a total `{num_reads, X}`) in a different antidote:read_objects call. When set to `false`, all (`{num_reads, X}`) reads will be sent in a single read_objects call, which is faster, as Antidote will process them in parallel.
- `{sequential_writes, false}.` The same as `{sequential_reads, Boolean}.` but with updates.

#### Operation `txn`
A general transaction, that first performs reads to a number of objects defined by the `{num_reads, X}` parameter. Then it updates `{num_updates, X}`.

#### Operation `update_only_txn`
This transaction will only perform update operations, by calling the static update_objects interface of antidote. The number of operations is defined by the `{num_updates, X}` parameter in the config file.

#### Operation `read_only_txn`
This transaction will only perform read operations in an antidote's read/only transaction. The number of operations is defined by the `{num_reads, X}` parameter in the config file.

#### Operation `append`
This command will run a transaction with a single update, and no reads.

#### Operation `read`
This command will run a transaction with a single read, and no updates.

## Driver

The driver is the erlang code file specified in the configuration file to generate load. The standard driver used for benchmarking Antidote is [basho_bench_driver_antidote_pb.erl](https://github.com/AntidoteDB/Benchmarks/blob/master/src/basho_bench_driver_antidote_pb.erl).
It is also possible to write a custom driver, that specifies different operations.

### Custom Driver

As specified in [basho_bench_doc](https://docs.riak.com/riak/kv/latest/using/performance/benchmarking/index.html#custom-driver), the driver has to implement the following interface:

``` Erlang
% Create the worker
% ID is an integer
new(ID) -> {ok, State} or {error, Reason}.

% Run an operation
run(Op, KeyGen, ValueGen, State) -> {ok, NewState} or {error, Reason, NewState}.
```

The callbacks for `driver:run/4` may be:

- `{ok, NewState}` — operation completed successfully
- `{error, Reason, NewState}` — operation failed but the driver can continue processing (i.e. recoverable error)
- `{stop, Reason}` — operation failed; driver can’t/won’t continue processing
- `{'EXIT', Reason}` — operation failed; driver crashed

### Custom Driver with State

An example for a custom driver with state is [FMKe](https://github.com/goncalotomas/FMKe). It is based on [lasp-bench](https://github.com/lasp-lang/lasp-bench), which is an alternative fork from basho_bench. The principle for creating a custom driver is the same.
