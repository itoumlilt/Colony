---
description: This guide will walk you through the core features of AntidoteDB.
---


# Quick Start Guide

We will use Docker to run AntidoteDB nodes. Please refer to the complete installation guide below if you wish to build the nodes from source:

{% page-ref page="overview/installation.md" %}

In this guide, you will accomplish the following steps:

* Start two AntidoteDB nodes as single-node data centers and connect them together
* Start and commit a transaction
* Use the Bounded Counter data type


### Requirements <a id="requirements"></a>

* Install the latest version of Docker \([Docker Installation Guide](https://docs.docker.com/engine/installation/)\)
* Install the latest version of [docker-compose](https://docs.docker.com/compose/)


### Create a cluster <a id="create-a-cluster"></a>

> If you prefer to skip the steps to build the cluster manually, there is a Docker Compose file available at: https://github.com/mweberUKL/antidote\_dev/tree/master/docker\_dcs.

* Download the deployment setup for a 2 data center single shard setup ([docker-compose file](https://github.com/AntidoteDB/docker-antidote/blob/master/compose-files/dc2n1))

* Run `docker-compose up`

We now have an AntidoteDB setup with two DCs (i.e. data is replicated once).


### Connecting to AntidoteDB

 In two separate terminals, connect to the console of each node:

```bash
docker exec -it antidote1 /antidote/bin/antidote remote_console
docker exec -it antidote2 /antidote/bin/antidote remote_console
```

> Alternatively, you can use docker-compose to connect

```bash
docker-compose exec antidote1 /antidote/bin/antidote remote_console
```


### Interacting with AntidoteDB <a id="interacting-with-antidotedb"></a>

Start a new transaction and increment the value of a bounded counter. By default, if the counter does not exist, one is created automatically.

```erlang
{ok, TxId} = antidote:start_transaction(ignore, []).
antidote:update_objects([{ {counter_key, antidote_crdt_counter_b, test_bucket}, increment, {10, client1}}], TxId).
antidote:commit_transaction(TxId).
# output: {ok, ...}
```

 The counter can be decremented in the replica where it was created:

```erlang
{ok, TxId1} = antidote:start_transaction(ignore, []).
antidote:update_objects([{ {counter_key, antidote_crdt_counter_b, test_bucket}, decrement, {1, client1}}], TxId1).
antidote:commit_transaction(TxId1).
# output: {ok, ...}
```

 And the value of the counter can be read on the other replica \(use the other terminal\):

```erlang
{ok, TxId} = antidote:start_transaction(ignore, []).
{ok, [Obj]} = antidote:read_objects([{counter_key, antidote_crdt_counter_b, test_bucket}], TxId).
antidote_crdt_counter_b:permissions(Obj).
# output: 9
```

 But cannot be decremented:

```erlang
{error, _} = antidote:update_objects([{ {counter_key, antidote_crdt_counter_b, test_bucket}, decrement, {1, client2}}], TxId).
# output: {error,{aborted, ...}}
```

 At this point, the transaction aborts and the system will automatically fetch permissions from another replica. After the resources are transfered between nodes, it is possible to decrement the counter.

```erlang
{ok, TxId1} = antidote:start_transaction(ignore, []).
antidote:update_objects([{ {counter_key, antidote_crdt_counter_b, test_bucket}, decrement, {1, client2}}], TxId1).
antidote:commit_transaction(TxId1).
# output: {ok, ...}
```
