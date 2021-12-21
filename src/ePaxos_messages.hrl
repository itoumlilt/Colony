%% Sent from Proposer to Acceptor
-record(prepare_message, {paxos_id, proposal_id}).
-record(accept_message, {paxos_id, proposal_id, value}).

%% Sent from Acceptor to Proposer
-record(promise_message, {acceptor_ref, paxos_id, proposal_id, accepted_proposal, accepted_value}).
-record(accepted_message, {acceptor_ref, paxos_id, proposal_id}).
