# Overview

## What is the Reckon Ecosystem?

The Reckon Ecosystem is a collection of five Erlang/OTP packages that together provide a complete **event sourcing and CQRS infrastructure** for BEAM applications.

Unlike traditional approaches that bolt event sourcing onto existing databases (PostgreSQL + Commanded, EventStoreDB + gRPC clients), Reckon runs entirely within the BEAM VM. Your event store is an Erlang process. Your consensus protocol is Raft, implemented by Ra. Your storage layer is Khepri — RabbitMQ's next-generation metadata store.

This means:

- **No external services to operate** — No database clusters, no message brokers
- **BEAM-native supervision** — Event store processes are supervised by OTP
- **Distribution built-in** — Raft consensus replicates events across nodes
- **Hot code upgrades** — Update your event store without downtime

## The Five Packages

### The Foundation: reckon_gater

`reckon_gater` defines the shared types — event records, snapshot records, subscription records — and the store interface that all other packages agree on. It's the contract layer.

### The Store: reckon_db

`reckon_db` is the event store itself. It implements the store interface from reckon_gater using Khepri/Ra for persistence and distribution. Events are appended to streams, streams are replicated via Raft, and subscriptions deliver events to consumers.

### The Framework: evoq

`evoq` is a pure CQRS/ES framework. It provides behaviours for aggregates, projections, process managers, and more. It has no opinion about storage — it works through adapter interfaces. This separation means your domain code never couples to a specific database.

### The Bridge: reckon_evoq

`reckon_evoq` connects evoq to reckon_db. It implements evoq's store interface using reckon_gater's API. When you dispatch a command through evoq, reckon_evoq ensures the resulting events are persisted in reckon_db. When you subscribe to events, reckon_evoq translates between evoq's subscription model and reckon_db's delivery mechanism.

### The Accelerator: reckon_nifs

`reckon_nifs` provides optional Rust NIFs for performance-critical operations like CRC32 checksums, hashing, compression, and serialization. When present, reckon_db uses them automatically for hot-path operations.

## Design Philosophy

### Separation of Concerns

The most important architectural decision in Reckon is the separation of **framework** (evoq) from **storage** (reckon_db). Your domain code — aggregates, commands, events, projections — uses evoq's behaviours. It never mentions reckon_db. This means:

1. You can test aggregates without a running event store
2. You can swap storage backends without changing domain code
3. The framework evolves independently from storage

### Shared Types via reckon_gater

Rather than having reckon_db define types that evoq must understand, or vice versa, the shared types live in reckon_gater. Both reckon_db and reckon_evoq depend on reckon_gater — neither depends on each other directly.

### Vertical Integration for Applications

When you build an application, you add `reckon_db` (storage) and `reckon_evoq` (adapter). The adapter brings `evoq` as a transitive dependency. Your application code uses evoq's API, and reckon_evoq transparently connects it to reckon_db.

```
Your Application
    ├── uses: evoq (CQRS framework)
    ├── depends: reckon_evoq (adapter)
    │     ├── depends: evoq
    │     └── depends: reckon_gater
    └── depends: reckon_db (event store)
          └── depends: reckon_gater
```

## Where Reckon is Used

### Hecate

[Hecate](https://github.com/hecate-social/hecate-ecosystem) is an AI-powered developer studio that uses Reckon for all its domain services. The daemon runs 19+ evoq-powered applications — each with its own embedded reckon_db instance, managing ventures, divisions, designs, plans, and more through event sourcing.

### Macula

[Macula](https://github.com/macula-io/macula-ecosystem) is a distributed application platform. Reckon provides the persistence layer for applications that run on Macula's HTTP/3 mesh network.

## Next Steps

- [Getting Started](getting-started.md) — Install and build your first aggregate
- [Architecture](architecture.md) — Deep-dive into how the packages work together
- Individual package guides: [reckon_db](reckon-db.md), [evoq](evoq.md), [reckon_gater](reckon-gater.md), [reckon_evoq](reckon-evoq.md), [reckon_nifs](reckon-nifs.md)
