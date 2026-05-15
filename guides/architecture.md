# Architecture

## Package Architecture

The Reckon ecosystem is organized as six packages with clear dependency boundaries.

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

Level 4 (depends on gater + db):
  reckon_gateway  — gRPC façade for polyglot clients
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

### The reckon_event Envelope

Every domain event is wrapped in the canonical `#reckon_event{}` record (defined in `reckon-gater`, header `reckon_gater/include/reckon_gater_types.hrl`). The envelope adds operational metadata:

```erlang
#reckon_event{
    event_id              :: binary(),           %% UUIDv7
    event_type            :: binary(),           %% e.g., <<"user_registered_v1">>
    stream_id             :: binary(),           %% e.g., <<"user-user123">>
    version               :: non_neg_integer(),  %% Stream position (0-based)
    data                  :: map() | binary(),   %% YOUR business event payload
    metadata              :: map(),              %% Correlation, causation, context
    tags                  :: [binary()] | undefined, %% Cross-stream query tags
    timestamp             :: integer(),          %% Event creation time (epoch ms)
    epoch_us              :: integer(),          %% Microsecond precision
    data_content_type     :: binary(),           %% Defaults to application/json
    metadata_content_type :: binary()
}
```

Your aggregate only produces the `data` field. The envelope is added by the infrastructure.

> **Tamper-resistance note** (2.1+): the record gained `prev_event_hash`, `mac`, and (reserved) `signature` fields in `reckon-gater` 2.1.0. Populated on integrity-enabled stores; default `undefined` for legacy events. See *On-Disk Format and Tamper Resistance* below.

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

## On-Disk Format and Tamper Resistance

> **Status as of the 2.1 wave** (`reckon-gater` 2.1.0, `reckon-db` 2.1.0, `evoq` 1.15.0, `reckon-evoq` 2.1.0, `reckon-gateway` 0.2.0): tamper-evident events and snapshots are now available as an opt-in per-store feature. Stores configured with `integrity` enabled write HMAC-protected, chain-hashed events and verify them on every read surface.

The on-disk event store is built on Khepri (tree-structured storage) and Ra (Raft consensus). Both layers protect against **corruption** via WAL CRCs — but CRCs do not protect against **intentional tampering**, since an attacker can recompute them trivially. The 2.1 wave addresses the gap above the storage primitive.

### Per-store opt-in

Tamper resistance is **off by default**. To enable on a store:

```erlang
#store_config{
    %% ... existing fields ...
    integrity = #{
        enabled => true,
        key_source => {env_var, <<"RECKON_DB_KEY_MY_STORE">>}
        %% or: {sealed_file, "/path/to/key"}  (mode 0600 required)
    }
}.
```

The key is 32 random bytes (HMAC-SHA256). Loaded into `persistent_term` at store startup; cleared on shutdown. Misconfiguration (missing env, bad base64, insecure file mode, wrong size) is **fail-fast** — the store refuses to start rather than silently advertise integrity it cannot deliver.

### What gets stored

The `#reckon_event{}` record (in `reckon-gater`) carries three integrity fields:

| Field | Purpose | When populated |
|-------|---------|----------------|
| `prev_event_hash :: binary() \| undefined` | SHA-256 chain hash of the predecessor event in the stream. For version 0, the genesis value (32 zero bytes). Verifiable WITHOUT the HMAC key — projections and external (gateway) consumers can chain-check independently. | Events written on integrity-enabled stores |
| `mac :: {KeyId, MacBytes} \| undefined` | HMAC-SHA256 over canonical bytes of the event, domain-tagged `evt\|`. Symmetric secret bound to the per-store key. Verified at the storage boundary only; **never** propagated to external consumers. | Events written on integrity-enabled stores |
| `signature :: binary() \| undefined` | Reserved for Ed25519 cross-trust-domain authenticity in a future release. Not populated in 2.1.x. | Not populated in 2.1.x |

The `#reckon_snapshot{}` record carries two analogous fields:

| Field | Purpose |
|-------|---------|
| `anchor_hash :: binary() \| undefined` | Chain hash of the event at the snapshot's version, captured at save time. Re-verified at load time to detect both snapshot tampering AND post-snapshot stream tampering. |
| `mac :: {KeyId, MacBytes} \| undefined` | HMAC-SHA256 over the snapshot record, domain-tagged `snap\|`. The domain tag prevents an attacker who recovers one MAC from replaying it onto a different protocol element. |

### Canonical encoding

Both the chain hash and the MAC are computed over `term_to_binary/2` with the `deterministic` flag (OTP 26+), which sorts map keys lexicographically before encoding so the same record always produces byte-identical output across nodes and OTP minor versions. The algorithm identifier exposed to clients is `"sha256-deterministic-etf-v1"` — bumped when the canonical encoding or hash function changes.

### Verify-at-read enforcement

Verification runs at every read surface:

- **Storage reads** (`reckon_db_streams:read/6`): forward-direction reads verify each event's MAC and chain link against a running tip. Failure surfaces as `{error, {integrity_violation, _}}`. Non-retriable, distinct from `wrong_expected_version`. New `Opts` map supports `verify => skip_legacy | strict | skip_all` — default `skip_legacy` returns pre-2.1 legacy events untouched while strictly checking integrity-bearing events.
- **Snapshot loads**: `reckon_db_snapshots:load/2,3` recompute the chain hash from the underlying event at load time and verify against the recorded `anchor_hash`. This catches stream tampering that occurred *after* the snapshot was saved, even when the snapshot itself is intact. Failed snapshots are refused; the aggregate falls back to full replay from the per-stream `chain_start_version` watermark.
- **Aggregate rebuild** (via `evoq` 1.15.0): the dispatcher recognises `{error, {integrity_violation, _}}` as terminal; the retry loop does not engage. `evoq_aggregate:is_integrity_violation/1` is the public classifier.
- **Subscription catch-up**: every integrity-bearing event passes a per-event MAC check before delivery; a tampered event halts replay and sends `{subscription_error, {integrity_violation, _}}` to the subscriber.
- **Gateway egress** (`reckon-gateway` 0.2.0): the `prev_event_hash` field is exposed on the wire; the `mac` field is **never** transmitted (it is a symmetric secret). Polyglot clients verify chain continuity with their own SHA-256 implementation; the `GetServerInfo` RPC advertises the algorithm identifier and key ID.

### Migration story — `chain_start_version` watermark

Streams that existed before integrity was enabled keep their legacy events untouched. The first integrity-bearing append to such a stream records a per-stream watermark at `[metadata, integrity, chain_start, StreamId]` set to the version of that first integrity event. Events below the watermark are legacy; events at or above must carry integrity fields. The `skip_legacy` read mode handles the mixed case transparently and emits `[reckon, db, read, legacy_event_returned]` telemetry so operators can monitor remediation progress.

### What this catches

| Attack | Detected at | How |
|--------|-------------|-----|
| Per-event field mutation (data, metadata, type, tags, timestamp) | Read time | MAC mismatch |
| Forging an event under a different key | Read time | MAC mismatch |
| Mutating a stored MAC | Read time | MAC mismatch |
| Deleting a middle event | Read time | Chain mismatch on the successor |
| Inserting a forged event | Read time | MAC mismatch (forged event signed under different key) |
| Swapping two adjacent events | Read time | Chain or MAC mismatch |
| Tampering a snapshot's state | Snapshot load | Snapshot MAC mismatch |
| Tampering a stream event *after* a snapshot | Snapshot load | Snapshot anchor mismatch (the headline property — defeats re-signing attacks) |
| Subscription delivery of a tampered event | Catch-up | Per-event MAC check, subscriber receives `subscription_error` |

### What this does NOT catch (current limitations)

- ~~**Backward-direction reads** bypass chain verification in 2.1.0.~~ **Closed in reckon-db 2.1.1** — backward reads verify the same chain as forward reads; only the result-ordering of the returned events differs by direction.
- **Cross-stream catch-up** does per-event MAC only — no chain walk, since cross-stream reads sort by `epoch_us` and have no single chain.
- **Operator-level Khepri tampering** can plant any value; the system *detects* this on the next read but does not prevent it. Trust boundaries around the BEAM process (filesystem permissions, dm-verity, FDE, container immutability) remain operationally relevant.
- **An attacker who controls both the central orchestrator AND code execution at the storage node** has the HMAC key and can produce events that verify. No architectural mitigation suffices at that point; the operator is in incident-response territory regardless.
- **External authenticity** — clients of the gateway can verify chain continuity but cannot independently authenticate events (the MAC is server-side only). A future Ed25519 signature on the schema (`signature` field) is reserved for this; not populated in 2.1.x.

### Trust boundaries explicitly

Capability tokens (UCAN-flavoured, in `reckon-gater`) and tamper resistance are **independent orthogonal layers**. Capability authorises WHO may invoke the API; integrity authenticates WHAT was written. A capability holder appending via the API produces integrity-bearing events; an actor with direct Khepri write access (bypassing the API) can plant unverifiable values that surface as `integrity_violation` on the next read.

### Key management posture in 2.1.0

Single symmetric HMAC key per store; key ID slot reserved in the `mac` tuple format (`{1, MacBytes}` always in 2.1). Rotation arrives in a follow-up release; the format is forward-compatible.

### Reference design

The full design — threat model, layer-by-layer implementation plan, key management evolution path, deferred-scope list — lives in [`reckon-db/plans/PLAN_TAMPER_RESISTANCE.md`](https://codeberg.org/reckon-db-org/reckon-db/src/branch/main/plans/PLAN_TAMPER_RESISTANCE.md).

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
- [reckon_gateway Guide](reckon-gateway.md) — gRPC façade for polyglot clients
