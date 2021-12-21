---
description: Docker-compose setups for creating AntidoteDB clusters
---

# Deploying Antidote with Docker-Compose

There are multiple examples on how to deploy Antidote with compose files in the [Antidote Docker repository](https://github.com/AntidoteDB/docker-antidote/tree/master/compose-files).

The cluster and data center management is done with [Antidote Connect](https://github.com/AntidoteDB/antidote-connect).

Some notes:

* Erlang nodes use short names in a docker network
* Only the protocol buffer client port has to be exposed to the outside
* If monitoring is used outside the docker network, the metrics port needs to be exposed, too

