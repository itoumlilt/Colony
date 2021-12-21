# Failure Modes in Antidote

## Supported

### DC Death / Network Partition

Currently the fault type supported by Antidote is only killing an entire DC, 
then restarting it will load the state back from disk and restart all connections .

### Shard Death

If a shard inside a DC crashes, it restarts and fetches missing updates from a remote DC.


## Unsupported

### Process Death

If a single process within the node dies and restarts it is not guaranteed that the state is loaded correctly 
(see [Issue #218](https://github.com/AntidoteDB/antidote/issues/218)
and [Issue #193](https://github.com/AntidoteDB/antidote/issues/218)).

