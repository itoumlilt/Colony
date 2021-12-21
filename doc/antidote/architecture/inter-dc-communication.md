# Communication Between Data Centers in Antidote (Inter-DC)

Antidote currently uses ZeroMQ for efficient communication between data centers.

There are two mechanisms once a DC is registered to listen to another DC.

First, the DC creates a new SUB socket and connects it to the PUB socket on the remote DC.
Second, it also creates a REQ socket and connects it to the REP socket on the remote DC. 
This REQ/REP socket pair is for fetching all transactions lost in case of a pub/sub failure.

If a (node inside a) DC crashes, it needs to reconnect to the exact same PUB sockets and same REP sockets. 
The internals will take care of the lost messages as soon as the newly recovered node receives a ping. 
It will then figure out which transactions were lost, re-download them via the REQ/REP and resume the normal communication via PUB/SUB.
