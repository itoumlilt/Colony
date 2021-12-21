# Colony

*Note: [DEPRECATED]* This repo is not maintained anymore, a production ready implementation of Colony can be found in the Concordant platform repository: [https://github.com/concordant](https://github.com/concordant).

Based on [AntidoteDB](https://github.com/itoumlilt/antidote), this is a research prototype for the Colony paper [published at Middleware'22](https://dl.acm.org/doi/abs/10.1145/3464298.3493405).

## abstract

Edge applications, such as gaming, cooperative engineering, or in-the-field information sharing, enjoy immediate response, autonomy and availability by distributing and replicating data at the edge. However, application developers and users demand the highest possible consistency guarantees, and specific support for group collaboration. To address this challenge, Colony guarantees Transactional Causal Plus Consistency (TCC+) globally, strengthened to Snapshot Isolation within edge groups. To help with scalability, fault tolerance and security, its logical communication topology is forest-like, with replicated roots in the core cloud, but with the flexibility to migrate a node or a group. Despite this hybrid approach, applications enjoy the same semantics everywhere in the topology. Our experiments show that local caching and peer groups improve throughput and response time significantly, performance is not affected in offline mode, and that migration is seamless.

## getting started

Colony requires Erlang 18 or greater.
