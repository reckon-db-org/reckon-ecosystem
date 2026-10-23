# reckon_evoq — Integration Adapter

## Overview

reckon_evoq is the adapter that connects evoq's CQRS framework to reckon_db's event store. It translates between evoq's dispatch/subscribe API and reckon_gater's store interface, enabling seamless event persistence and delivery.

**Version:** 2.9.0 | **License:** Apache 2.0

- [GitHub](https://github.com/reckon-db-org/reckon-evoq) | [HexDocs](https://hexdocs.pm/reckon_evoq)

## Installation

```erlang
%% rebar.config
{deps, [
    {reckon_db, "~> 5.11"},     %% Event store
    {reckon_evoq, "~> 2.9"}     %% Adapter (brings evoq as transitive dep)
]}.
```

## Key Design Decision

reckon_evoq depends on:
- **evoq** (~> 1.22) — The CQRS framework
- **reckon_gater** (~> 3.7) — The shared types and store interface

It does **NOT** depend on reckon_db directly. This is intentional — the adapter couples to the API contract (reckon_gater), not the implementation (reckon_db). Your application adds reckon_db separately.

```
Your Application
  ├── reckon_db (storage implementation)
  └── reckon_evoq (adapter)
        ├── evoq (framework)
        └── reckon_gater (types/interface)
```

## Configuration

reckon_evoq has no API of its own to call: it is the module evoq uses for
storage once you point evoq at it. In `config/sys.config`, beside the store:

```erlang
{evoq, [
    {store_id, my_store},
    {event_store_adapter, reckon_evoq_adapter},
    {snapshot_store_adapter, reckon_evoq_adapter},
    {subscription_adapter, reckon_evoq_adapter}
]}
```

## Core Operations

### Command Dispatch

You dispatch through evoq; the adapter does the storage underneath:

```erlang
OrderId = <<"order-018f6a7b8c9d4e5f60718293a4b5c6d9">>,
Command = evoq_command:new(place_order, order_aggregate, OrderId,
    #{command_type => place_order, items => [#{sku => <<"WIDGET-1">>, qty => 3}]}),
{ok, Version, Events} = evoq_router:dispatch(Command).
```

The aggregate id is also the stream id, so it must be `<prefix>-<32 hex>`
([Stream Ids](https://hexdocs.pm/reckon_gater/stream_ids.html)). evoq hands
`order_aggregate:execute/2` the payload, which is why the payload carries
`command_type`. The [Getting Started](getting-started.md) guide builds a
complete aggregate and runs it.

### What Dispatch Does

1. **Load state**: on first use, `Aggregate:init/1`, then the stream's events
   (after the latest snapshot, if any) are replayed through `Aggregate:apply/2`.
2. **Execute command**: `Aggregate:execute(State, Payload)`.
3. **Persist events**: the returned events are appended to the stream, with
   everything except `event_type` stored as the event's data.
4. **Return**: `{ok, Version, Events}`, or `{error, Reason}` if `execute/2`
   refused the command.

If the command is rejected by the aggregate, no events are persisted.

### Reacting to Events

Inside evoq, events reach your code through event handlers, projections and
process managers, which subscribe through this adapter for you: see evoq's
[projections](https://hexdocs.pm/evoq/projections.html) and
[event handlers](https://hexdocs.pm/evoq/event_handlers.html) guides. To watch
a stream directly, use the store's subscription API
(`reckon_db_subscriptions:subscribe/5` with `#{subscriber => self()}`; see the
[reckon_db guide](reckon-db.md#subscribe)).

### Snapshots

evoq saves a snapshot through the adapter every 100 events by default
(`{evoq, [{snapshot_every, N}]}`), and loads the latest one before replaying
when an aggregate starts. An aggregate takes part by exporting the optional
`snapshot/1` and `from_snapshot/1` callbacks.

## Supervisor Integration

The store starts with the `reckon_db` application, from its `stores`
configuration, so your own supervisor only starts your desks:

```erlang
-module(manage_orders_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{id => place_order_sup,
          start => {place_order_sup, start_link, []},
          type => supervisor}
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.
```

Each domain service names its **own** store in that configuration. Stores are not shared across domains.

## Telemetry

reckon_evoq emits no telemetry of its own: dispatch and aggregate events come
from evoq, and store operations from reckon_db and reckon_gater. evoq's
dispatch events are `[evoq, dispatch, start]`, `[evoq, dispatch, stop]` and
`[evoq, dispatch, exception]`; per-aggregate ones include
`[evoq, aggregate, execute, stop]` (all in evoq's `evoq_telemetry.hrl`).

```erlang
ok = telemetry:attach(<<"dispatch-metrics">>, [evoq, dispatch, stop],
    fun my_metrics:handle_dispatch/4, #{}).
```

## Error Handling

| Result | Meaning | Action |
|-------|---------|--------|
| `{error, Reason}` from your `execute/2` | The aggregate refused the command (`unknown_command`, `below_zero`, ...) | Nothing was stored; the reason is yours to define |
| `{error, {invalid_stream_id, Reason, Id}}` (reckon-db 5.12 and older; newer releases add the rule as a fourth element) | The aggregate id is not a valid stream id | Use `<prefix>-<32 hex>`, e.g. `reckon_gater_stream_id:new(order)` |
| `{error, wrong_expected_version}` | Another writer appended to the stream first, and evoq's retry did not resolve it | evoq rebuilds the aggregate and retries once by itself; if this reaches you, retry the dispatch |

A brand-new aggregate is not an error: evoq starts it from `init/1`.

## Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| evoq | ~> 1.22 | CQRS framework (dispatch, aggregate behaviours) |
| reckon_gater | ~> 3.7 | Shared types (event records, store interface) |
| telemetry | ~> 1.3 | Instrumentation |

These are reckon_evoq's own floors (its `rebar.config`); with `~>` they float to
the newest minor, so a fresh build today takes evoq 1.29 and reckon_gater 3.14.

> **2.0.0 migration note:** reckon-evoq 2.0.0 consumes the renamed reckon-gater 2.0.0 API (`reckon_gater_api:*`, `reckon_gater_types.hrl`). Apps that go through `reckon_evoq_adapter` via `evoq_dispatcher` upgrade as a drop-in. Apps that reached directly into `esdb_gater_api` must do their own migration per reckon-gater 2.0.0's release notes.

## Related Guides

- [evoq](evoq.md) — The framework that reckon_evoq adapts
- [reckon_db](reckon-db.md) — The store that reckon_evoq connects to
- [reckon_gater](reckon-gater.md) — The shared types used by the adapter
- [Architecture](architecture.md) — How the adapter fits in the ecosystem
