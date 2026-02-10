# Architecture

## Package Architecture

The Reckon ecosystem is organized as five packages with clear dependency boundaries.

### Dependency Graph

```
Level 0 (no Reckon deps):
  reckon_nifs     — Rust NIFs (standalone, optional)
  evoq            — CQRS framework (standalone)

Level 1 (shared types):
  reckon_gater    — Canonical types + store interface

Level 2 (depends on gater):
  reckon_db       — Event store implementation

Level 3 (depends on gater + evoq):
  reckon_evoq     — Adapter bridging evoq to reckon_db
```

The critical insight: **reckon_evoq depends on evoq + reckon_gater, NOT on reckon_db directly.** This means the adapter couples to the API contract (reckon_gater), not the implementation (reckon_db).

## Write-Side Architecture

### Command Dispatch Flow

```
1. Application code dispatches a command
   └── reckon_evoq:dispatch(Store, StreamId, Aggregate, Cmd)

2. reckon_evoq loads current aggregate state
   └── Reads events from reckon_db via reckon_gater API
   └── Replays events through Aggregate:apply/2

3. evoq executes the command
   └── Aggregate:execute(CurrentState, Command)
   └── Returns {ok, [Events]} or {error, Reason}

4. reckon_evoq persists new events
   └── Appends to stream in reckon_db
   └── Events wrapped in evoq_event envelope

5. Subscriptions are notified
   └── reckon_db delivers events to subscribers
   └── Catch-up + live delivery
```

### The evoq_event Envelope

Every domain event is wrapped in an envelope that adds operational metadata:

```erlang
#evoq_event{
    event_id        :: binary(),           %% Unique UUID
    event_type      :: binary(),           %% e.g., <<"user_registered_v1">>
    stream_id       :: binary(),           %% e.g., <<"user-user123">>
    version         :: non_neg_integer(),  %% Stream position (0-based)
    data            :: map(),              %% YOUR business event payload
    metadata        :: map(),              %% Correlation, causation, context
    tags            :: [binary()],         %% Cross-stream query tags
    timestamp       :: integer(),          %% Event creation time
    epoch_us        :: integer()           %% Microsecond precision
}
```

Your aggregate only produces the `data` field. The envelope is added by the infrastructure.

### Stream Organization

Events are organized into streams. Each aggregate instance gets its own stream:

```
Stream: "order-order-123"
  ├── Event 0: order_placed_v1
  ├── Event 1: order_confirmed_v1
  ├── Event 2: order_shipped_v1
  └── Event 3: order_delivered_v1

Stream: "order-order-456"
  ├── Event 0: order_placed_v1
  └── Event 1: order_cancelled_v1
```

Stream IDs follow the convention: `{aggregate_type}-{aggregate_id}`

## Read-Side Architecture

### Projection Flow

```
1. reckon_db stores new events

2. Subscription delivers events to projection
   └── Catch-up: replays from position 0 on startup
   └── Live: delivers new events in real-time

3. Projection transforms event into read model
   └── evoq_projection:handle_event/2
   └── Updates SQLite, ETS, or any read store

4. Query reads from the read model
   └── Fast, denormalized, no joins
   └── All calculations done in projection
```

### CQRS Separation

```
    WRITE SIDE                          READ SIDE
    ──────────                          ─────────
    Command                             Query
       ↓                                   ↑
    Aggregate                           Read Model
       ↓                                   ↑
    Domain Event                        Projection
       ↓                                   ↑
    Event Store  ─── subscription ──→  Event Consumer
```

The write side and read side share NOTHING except the event stream. This enables:

- Independent scaling (more read replicas without touching writes)
- Optimized read models (denormalized tables, materialized views)
- Multiple read models from the same events
- Temporal queries (rebuild read models from any point in time)

## Distributed Architecture

### Raft Consensus (reckon_db)

reckon_db uses Khepri/Ra for distributed storage. Ra implements the Raft consensus protocol — the same one used in RabbitMQ for quorum queues.

```
Node A (Leader)          Node B (Follower)       Node C (Follower)
┌──────────────┐        ┌──────────────┐        ┌──────────────┐
│  reckon_db   │        │  reckon_db   │        │  reckon_db   │
│              │        │              │        │              │
│  Ra/Khepri   │──────→ │  Ra/Khepri   │──────→ │  Ra/Khepri   │
│  (Leader)    │  Raft  │  (Follower)  │  Raft  │  (Follower)  │
└──────────────┘        └──────────────┘        └──────────────┘
```

Writes go to the leader, are replicated to a majority, then acknowledged. Reads can happen on any node (with configurable consistency).

### Embedded Mode

For simpler deployments, reckon_db also runs in embedded mode — a single-node event store within your application:

```erlang
%% Each domain service starts its own embedded store
{reckon_db_sup, start_store, [#{
    name => my_domain_store,
    data_dir => "/var/lib/my_app/my_domain"
}]}
```

This is how Hecate uses Reckon — each domain service (setup_venture, design_division, etc.) runs its own embedded reckon_db instance.

## Aggregate Design

### The evoq_aggregate Behaviour

```erlang
-behaviour(evoq_aggregate).

%% REQUIRED callbacks:
-callback execute(State :: map(), Command :: map()) ->
    {ok, [Event :: map()]} | {error, Reason :: term()}.

-callback apply(State :: map(), Event :: map()) ->
    NewState :: map().
```

### State Comes First

This is critical: evoq calls `execute(State, Command)` with **State as the first argument**. This matches Erlang convention (state/accumulator first) and enables pattern matching on state:

```erlang
%% Guard: only active aggregates can accept domain commands
execute(#{lifecycle_state := active} = State, #{command_type := <<"place_order_v1">>} = Cmd) ->
    %% Process the command...
    {ok, Events};

execute(#{lifecycle_state := Other}, #{command_type := <<"place_order_v1">>}) ->
    {error, {not_active, Other}}.
```

### Bit Flags for Status

evoq provides `evoq_bit_flags` for compact, performant status tracking:

```erlang
-define(INITIATED,  1).   %% 2^0
-define(ACTIVE,     2).   %% 2^1
-define(PAUSED,     4).   %% 2^2
-define(COMPLETED,  8).   %% 2^3
-define(ARCHIVED,  16).   %% 2^4

%% Check flags
evoq_bit_flags:has(Status, ?ACTIVE)          %% true/false
evoq_bit_flags:has_any(Status, [?PAUSED, ?COMPLETED])
evoq_bit_flags:set(Status, ?COMPLETED)
evoq_bit_flags:unset(Status, ?ACTIVE)
```

## Event Versioning

Events are versioned in their type name:

```erlang
event_type => <<"order_placed_v1">>
event_type => <<"order_placed_v2">>   %% Added new fields
```

Projections handle multiple versions:

```erlang
handle_event(#{event_type := <<"order_placed_v1">>} = E, State) ->
    upgrade_and_project(E, State);
handle_event(#{event_type := <<"order_placed_v2">>} = E, State) ->
    project_v2(E, State).
```

Old events are never modified. New versions add fields. This preserves the immutable nature of the event log.

## Cross-Stream Queries

Tags enable querying events across streams:

```erlang
%% Tag events during creation
Event = #{
    event_type => <<"order_placed_v1">>,
    tags => [<<"realm:io.macula">>, <<"region:eu-west">>],
    ...
}

%% Query by tag
{ok, Events} = reckon_db:query_by_tag(Store, <<"realm:io.macula">>).
```

## Next Steps

- [evoq Guide](evoq.md) — Framework behaviours and patterns
- [reckon_db Guide](reckon-db.md) — Store configuration and operations
- [reckon_evoq Guide](reckon-evoq.md) — Adapter configuration
