# reckon_db — BEAM-native Event Store

## Overview

reckon_db is a distributed event store built entirely in Erlang/OTP. It uses [Khepri](https://github.com/rabbitmq/khepri) for tree-structured data storage and [Ra](https://github.com/rabbitmq/ra) for Raft consensus. Events are stored in append-only streams and replicated across cluster nodes.

**Version:** 1.2.3 | **License:** Apache 2.0

- [GitHub](https://github.com/reckon-db-org/reckon-db) | [HexDocs](https://hexdocs.pm/reckon_db)

## Installation

```erlang
%% rebar.config
{deps, [{reckon_db, "1.2.3"}]}.
```

## Key Concepts

### Streams

A stream is an ordered, append-only sequence of events identified by a stream ID. Each aggregate instance typically maps to one stream:

```
stream_id: "order-order-123"
  ├── version 0: {order_placed_v1, ...}
  ├── version 1: {payment_received_v1, ...}
  └── version 2: {order_shipped_v1, ...}
```

### Events

Events are wrapped in the `reckon_event` record (defined in reckon_gater):

```erlang
#reckon_event{
    event_id,       %% UUID
    event_type,     %% Binary, e.g. <<"order_placed_v1">>
    stream_id,      %% Binary, e.g. <<"order-order-123">>
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

```erlang
%% In your supervisor
#{id => my_store,
  start => {reckon_db_sup, start_store, [#{
      name => my_store,
      data_dir => "/var/lib/my_app/events"
  }]},
  type => supervisor}
```

### Clustered Mode

```erlang
#{id => my_store,
  start => {reckon_db_sup, start_store, [#{
      name => my_store,
      data_dir => "/var/lib/my_app/events",
      cluster => #{
          nodes => ['node1@host1', 'node2@host2', 'node3@host3']
      }
  }]},
  type => supervisor}
```

## Core Operations

### Append Events

```erlang
Events = [
    #{event_type => <<"order_placed_v1">>,
      data => #{order_id => <<"ord-1">>, total => 4999},
      metadata => #{correlation_id => <<"req-abc">>}}
],
{ok, Written} = reckon_db:append(my_store, <<"order-ord-1">>, Events).
```

### Read Events

```erlang
%% Read all events from a stream
{ok, Events} = reckon_db:read_stream(my_store, <<"order-ord-1">>).

%% Read from a specific version
{ok, Events} = reckon_db:read_stream(my_store, <<"order-ord-1">>, #{from => 5}).
```

### Subscribe

```erlang
%% Catch-up subscription (starts from beginning)
{ok, SubId} = reckon_db:subscribe(my_store, <<"order-ord-1">>, self(), #{
    from => 0
}).

%% Live subscription (only new events)
{ok, SubId} = reckon_db:subscribe(my_store, <<"order-ord-1">>, self(), #{
    from => latest
}).

%% Receive events
receive
    {reckon_event, SubId, Event} -> handle(Event)
end.
```

### Snapshots

```erlang
%% Save snapshot
ok = reckon_db:save_snapshot(my_store, <<"order-ord-1">>, #{
    version => 100,
    state => AggregateState
}).

%% Load latest snapshot
{ok, Snapshot} = reckon_db:load_snapshot(my_store, <<"order-ord-1">>).
```

### Cross-Stream Queries

```erlang
%% Query events by tag
{ok, Events} = reckon_db:query_by_tag(my_store, <<"realm:io.macula">>).
```

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
| reckon_gater | ~> 1.1.1 | Shared types and store interface |
| khepri | 0.17.2 | Tree-structured storage |
| telemetry | 1.3.0 | Metrics and instrumentation |
| reckon_nifs | (optional) | Rust NIF acceleration |

## Related Guides

- [Architecture](architecture.md) — How reckon_db fits in the ecosystem
- [reckon_gater](reckon-gater.md) — Shared types used by reckon_db
- [reckon_evoq](reckon-evoq.md) — Adapter that connects evoq to reckon_db
