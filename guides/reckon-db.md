# reckon_db — BEAM-native Event Store

## Overview

reckon_db is a distributed event store built entirely in Erlang/OTP. It uses [Khepri](https://github.com/rabbitmq/khepri) for tree-structured data storage and [Ra](https://github.com/rabbitmq/ra) for Raft consensus. Events are stored in append-only streams and replicated across cluster nodes.

**Version:** 5.11.11 | **License:** Apache 2.0

> **Since the 2.1 wave:** reckon_db now provides multi-stream consistency — Dynamic Consistency Boundary (DCB) conditional appends and Command Context Consistency (CCC) payload-indexed conditions (see [DCB &amp; CCC](dcb-and-ccc.md)) — and **continuous cluster self-healing** (a per-store healer that rejoins a drifted/split replica to quorum, with a wedge-proof coordinator and drift/heal telemetry).

- [GitHub](https://github.com/reckon-db-org/reckon-db) | [HexDocs](https://hexdocs.pm/reckon_db)

## Installation

```erlang
%% rebar.config
{deps, [{reckon_db, "~> 5.9"}]}.
```

## Key Concepts

### Streams

A stream is an ordered, append-only sequence of events identified by a stream ID. Each aggregate instance typically maps to one stream:

```
stream_id: "order-018f6a7b8c9d4e5f60718293a4b5c6d9"
  ├── version 0: {order_placed_v1, ...}
  ├── version 1: {payment_received_v1, ...}
  └── version 2: {order_shipped_v1, ...}
```

### Events

Events are wrapped in the `#event{}` record (defined in reckon_gater):

```erlang
#event{
    event_id,       %% UUID
    event_type,     %% Binary, e.g. <<"order_placed_v1">>
    stream_id,      %% Binary, e.g. <<"order-018f6a7b8c9d4e5f60718293a4b5c6d9">>
    version,        %% Non-negative integer (0-based)
    data,           %% Map — your business event payload
    metadata,       %% Map — correlation_id, causation_id, etc.
    tags,           %% [Binary] — for cross-stream queries
    timestamp       %% Integer — epoch milliseconds
}
```

### Subscriptions

reckon_db supports two subscription modes:

- **Catch-up**: Replay all events from a given position, then switch to live
- **Live**: Only receive new events as they're appended

Subscriptions are durable — they track their position and resume from where they left off.

### Snapshots

For aggregates with long event histories, snapshots cache the aggregate state at a point in time. On replay, the store loads the latest snapshot and replays only events after it.

## Starting a Store

### Embedded Mode (Single Node)

A store is named in the `reckon_db` application environment and starts with the
application. In `config/sys.config`:

```erlang
[
 {reckon_db, [
     {stores, [
         {my_store, [{data_dir, "data/my_store"}, {mode, single}]}
     ]}
 ]}
].
```

`application:ensure_all_started(reckon_db)` (or listing `reckon_db` in your
application's `applications`) then starts `my_store`. A store can also be
started at runtime with `reckon_db_sup:start_store/1`, which takes a store name
already in that configuration or a `#store_config{}` record
(`reckon_db/include/reckon_db.hrl`), not a map.

### Clustered Mode

A clustered store uses `{mode, cluster}` and needs a cluster secret, set as
`{reckon_db, [{cluster_secret, <<"...">>}]}` or the `RECKON_DB_CLUSTER_SECRET`
environment variable. Without one, discovery stays off and the store logs why.
The full set of cluster keys is in reckon-db's
[Configuration](https://github.com/reckon-db-org/reckon-db#configuration)
section.

## Core Operations

Every call below takes the store name first. A stream id is `<prefix>-<32 hex>`
([Stream Ids](https://hexdocs.pm/reckon_gater/stream_ids.html)).

### Append Events

```erlang
OrderId = <<"order-018f6a7b8c9d4e5f60718293a4b5c6d9">>,
Events = [
    #{event_type => <<"order_placed_v1">>,
      data => #{total => 4999},
      metadata => #{correlation_id => <<"req-abc">>},
      tags => [<<"customer:42">>]}
],
{ok, 0} = reckon_db_streams:append(my_store, OrderId, -1, Events).
```

The third argument is the version you expect the stream to be at: `-1` says
the stream must not exist yet, and the call returns the stream's new version.

### Read Events

```erlang
%% Up to 100 events, forward from version 0
{ok, Read} = reckon_db_streams:read(my_store, OrderId, 0, 100, forward).

%% The stream's current version
0 = reckon_db_streams:get_version(my_store, OrderId).
```

Each event comes back as an `#event{}` record (`reckon_gater_types.hrl`), with
the `data`, `metadata` and `tags` you appended.

### Subscribe

```erlang
{ok, _Key} = reckon_db_subscriptions:subscribe(
    my_store, stream, OrderId, <<"order_watcher">>, #{subscriber => self()}).

%% Every event appended to the stream from now on arrives as
receive
    {events, [Event | _]} -> handle(Event)
end.
```

Without the `subscriber` option the subscription is recorded but nothing is
sent to your process. The other subscription types are `event_type`,
`event_pattern` and `event_payload`; see reckon-db's API reference.

### Snapshots

```erlang
%% Save the aggregate's state as of version 0
ok = reckon_db_snapshots:save(my_store, OrderId, 0, #{total => 4999}).

%% Load the latest snapshot
{ok, Snapshot} = reckon_db_snapshots:load(my_store, OrderId).
```

### Cross-Stream Queries

```erlang
%% Every event carrying the tag, across all streams
{ok, Tagged} = reckon_db_streams:read_by_tags(my_store, [<<"customer:42">>], any, 100).
```

Without a declared tag index this scans the whole store and logs a hint saying
so; declare the index in the store's configuration to make it proportional to
the matches.

## Architecture

### Storage Layer

```
reckon_db
  └── Khepri (tree-structured data store)
        └── Ra (Raft consensus)
              └── Erlang processes (one Ra server per store)
```

Khepri provides tree-structured storage where streams are organized as paths. Ra handles replication — writes are proposed to the Raft leader, replicated to followers, and committed when a majority acknowledges.

### Supervision Tree

```
reckon_db_sup (per store instance)
  ├── Ra server (Raft consensus)
  ├── Khepri store
  ├── Subscription manager
  └── Snapshot manager
```

Each store runs its own supervision subtree. You can have multiple independent stores in one application (e.g., one per domain service).

## Performance Considerations

- **Write throughput**: Bounded by Raft consensus (majority acknowledgment)
- **Read throughput**: Reads from local Khepri (no consensus needed for stale reads)
- **Snapshot frequency**: Configure based on event volume — every 100-1000 events is typical
- **reckon_nifs**: Drop in for faster checksums, hashing, and serialization on hot paths

## Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| reckon_gater | ~> 3.10 | Shared types and store interface |
| khepri | 0.17.2 | Tree-structured storage |
| telemetry | 1.3.0 | Metrics and instrumentation |
| reckon_nifs | (optional) | Rust NIF acceleration |

## Related Guides

- [Architecture](architecture.md) — How reckon_db fits in the ecosystem
- [reckon_gater](reckon-gater.md) — Shared types used by reckon_db
- [reckon_evoq](reckon-evoq.md) — Adapter that connects evoq to reckon_db
