# Materializer in Antidote

The materializer caches materialized versions of key in memory.


The are two per-key caches.

An operation cache where the most recent operations are stored, 
and a snapshot cache which stores the latest materialised views (snapshots) of a CRDT.

When the materializer receives an operation, it stores in its operation cache. 
When it receives a read request, it checks whether there are suitable materialized views available in the snapshot cache 
and whether there are operations to be applied in the operation cache. 
If so, it materializes a new version of the CRDT that satisfies the request by applying the potentially missing operations 
to the corresponding snapshot, thus creating a new snapshot. 

In the event that cached snapshots or operations exist for a requested key, 
but none satisfying the read request parameters (i.e. snapshot time), 
the materializer replies with an {error, no_snapshot} message.


## Garbage Collection

As there are object views (snapshots) and operations, the growing number of any of them needs to trigger the GC mechanism.

**Snapshot (or read-triggered) GC:**

In case the number of snapshots stored for a key grows up to a value “snapshot_threshold”, 
keep just the latest “snapshot_min” snapshots. 
Proposed initial values (for no particular reason) are 10 and 2 respectively. 
Note that the snapshot generation is done on reads, that’s why this mechanism is triggered only on reads.

**Operation (or write triggered) GC:**

In case the number of update operations stored for a key grows up to a number “ops_threshold”, 
keep just the latest “ops_min” operations by creating a snapshot of the object applying and then removing the oldest ops_threshold-ops_min ops. 
Proposed an initial values (for no particular reason) are 50 and 5 respectively. 
Note that the snapshot generation could possibly trigger the Snapshot GC mechanism.
