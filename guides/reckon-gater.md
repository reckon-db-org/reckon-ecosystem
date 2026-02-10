# reckon_gater — Gateway &amp; Shared Types

## Overview

reckon_gater is the shared foundation of the Reckon ecosystem. It defines the canonical record types, store interface, and gateway capabilities that all other packages depend on. Think of it as the API contract — reckon_db implements it, reckon_evoq consumes it, and your application code uses the types.

**Version:** 1.1.1 | **License:** Apache 2.0

- [GitHub](https://github.com/reckon-db-org/reckon-gater) | [HexDocs](https://hexdocs.pm/reckon_gater)

## Installation

```erlang
{deps, [{reckon_gater, "1.1.1"}]}.
```

Note: You typically don't add reckon_gater directly — it comes as a transitive dependency of reckon_db and reckon_evoq.

## Shared Types

### Event Record

The canonical event record used across the ecosystem:

```erlang
-include_lib("reckon_gater/include/reckon_types.hrl").

#reckon_event{
    event_id        :: binary(),
    event_type      :: binary(),
    stream_id       :: binary(),
    version         :: non_neg_integer(),
    data            :: map() | binary(),
    metadata        :: map(),
    tags            :: [binary()] | undefined,
    timestamp       :: integer(),
    epoch_us        :: integer(),
    data_content_type     :: binary(),
    metadata_content_type :: binary()
}
```

### Snapshot Record

```erlang
#reckon_snapshot{
    stream_id  :: binary(),
    version    :: non_neg_integer(),
    state      :: term(),
    timestamp  :: integer()
}
```

### Subscription Record

```erlang
#reckon_subscription{
    subscription_id :: binary(),
    stream_id       :: binary(),
    subscriber      :: pid(),
    from            :: non_neg_integer() | latest,
    status          :: active | paused | cancelled
}
```

## Store Interface

reckon_gater defines the behaviour that event stores must implement:

```erlang
-behaviour(reckon_store).

-callback append(Store, StreamId, Events) -> {ok, Written} | {error, Reason}.
-callback read_stream(Store, StreamId) -> {ok, Events} | {error, Reason}.
-callback read_stream(Store, StreamId, Opts) -> {ok, Events} | {error, Reason}.
-callback subscribe(Store, StreamId, Pid, Opts) -> {ok, SubId} | {error, Reason}.
-callback save_snapshot(Store, StreamId, Snapshot) -> ok | {error, Reason}.
-callback load_snapshot(Store, StreamId) -> {ok, Snapshot} | {error, not_found}.
```

This interface is implemented by reckon_db and consumed by reckon_evoq. By programming to the interface (not the implementation), the adapter remains decoupled from the store.

## Gateway Capabilities

Beyond type definitions, reckon_gater provides:

### Load Balancing

Route requests across multiple store instances for read scalability.

### UCAN Security

Integrate capability-based authorization tokens for securing store access in distributed deployments.

### Telemetry Integration

Standard telemetry events for monitoring:

```erlang
%% Events emitted:
[reckon, gater, request, start]
[reckon, gater, request, stop]
[reckon, gater, request, exception]
```

## Why a Separate Package?

The common question: why not put these types in reckon_db?

1. **Prevents circular dependencies** — reckon_evoq needs the types but shouldn't depend on reckon_db
2. **Clean interface** — The store contract is separate from the implementation
3. **Independent versioning** — Types can evolve independently from storage

## Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| telemetry | 1.3.0 | Instrumentation |

reckon_gater has minimal dependencies by design — it's the foundation that everything else builds on.

## Related Guides

- [reckon_db](reckon-db.md) — The store that implements this interface
- [reckon_evoq](reckon-evoq.md) — The adapter that consumes these types
- [Architecture](architecture.md) — How the packages connect
