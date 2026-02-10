# reckon_evoq — Integration Adapter

## Overview

reckon_evoq is the adapter that connects evoq's CQRS framework to reckon_db's event store. It translates between evoq's dispatch/subscribe API and reckon_gater's store interface, enabling seamless event persistence and delivery.

**Version:** 1.1.3 | **License:** Apache 2.0

- [GitHub](https://github.com/reckon-db-org/reckon-evoq) | [HexDocs](https://hexdocs.pm/reckon_evoq)

## Installation

```erlang
%% rebar.config
{deps, [
    {reckon_db, "1.2.3"},       %% Event store
    {reckon_evoq, "1.1.3"}      %% Adapter (brings evoq as transitive dep)
]}.
```

## Key Design Decision

reckon_evoq depends on:
- **evoq** (~> 1.2) — The CQRS framework
- **reckon_gater** (~> 1.1.1) — The shared types and store interface

It does **NOT** depend on reckon_db directly. This is intentional — the adapter couples to the API contract (reckon_gater), not the implementation (reckon_db). Your application adds reckon_db separately.

```
Your Application
  ├── reckon_db (storage implementation)
  └── reckon_evoq (adapter)
        ├── evoq (framework)
        └── reckon_gater (types/interface)
```

## Core Operations

### Command Dispatch

```erlang
%% Dispatch a command to an aggregate
Cmd = #{
    command_type => <<"place_order_v1">>,
    order_id => <<"ord-123">>,
    items => [#{sku => <<"WIDGET-1">>, qty => 3}]
},

StreamId = <<"order-ord-123">>,
Result = reckon_evoq:dispatch(my_store, StreamId, order_aggregate, Cmd).
%% => {ok, [#{event_type => <<"order_placed_v1">>, ...}]}
```

### What Dispatch Does

1. **Load state** — Read events from reckon_db for the stream, replay through `Aggregate:apply/2`
2. **Execute command** — Call `Aggregate:execute(State, Command)`
3. **Persist events** — Append returned events to the stream in reckon_db
4. **Return** — `{ok, Events}` or `{error, Reason}`

If the command is rejected by the aggregate, no events are persisted.

### Event Subscriptions

```erlang
%% Subscribe to events on a stream
{ok, SubId} = reckon_evoq:subscribe(my_store, <<"order-ord-123">>, self(), #{
    from => 0   %% catch-up from beginning
}).

%% Or subscribe to all events (all streams)
{ok, SubId} = reckon_evoq:subscribe_all(my_store, self(), #{from => 0}).

%% Receive events
receive
    {reckon_event, SubId, #evoq_event{} = Event} ->
        %% Process the event
        handle_event(Event)
end.
```

### Subscription Modes

| Mode | Option | Behavior |
|------|--------|----------|
| Catch-up | `#{from => 0}` | Replay all events, then switch to live |
| Live | `#{from => latest}` | Only new events |
| Resume | `#{from => Position}` | Start from a saved position |

### Snapshots

```erlang
%% Save aggregate state snapshot
reckon_evoq:save_snapshot(my_store, <<"order-ord-123">>, #{
    version => CurrentVersion,
    state => AggregateState
}).

%% Snapshots are loaded automatically during dispatch
%% (load snapshot → replay events after snapshot → execute command)
```

## Supervisor Integration

In your domain service supervisor:

```erlang
-module(manage_orders_sup).
-behaviour(supervisor).

init([]) ->
    StoreConfig = #{
        name => manage_orders_store,
        data_dir => "/var/lib/my_app/manage_orders"
    },
    Children = [
        %% Start embedded event store
        #{id => event_store,
          start => {reckon_db_sup, start_store, [StoreConfig]},
          type => supervisor},

        %% Start domain-specific desks (vertical slices)
        #{id => place_order_sup,
          start => {place_order_sup, start_link, []},
          type => supervisor}
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.
```

Each domain service starts its **own** embedded reckon_db instance. Stores are not shared across domains.

## Telemetry

reckon_evoq emits telemetry events for monitoring:

```erlang
%% Events:
[reckon, evoq, dispatch, start]        %% Command dispatch begins
[reckon, evoq, dispatch, stop]         %% Command dispatch completes
[reckon, evoq, dispatch, exception]    %% Command dispatch fails
[reckon, evoq, subscribe, start]       %% Subscription starts
[reckon, evoq, event, delivered]       %% Event delivered to subscriber
```

Attach handlers for metrics:

```erlang
:telemetry.attach("dispatch-metrics", [reckon, evoq, dispatch, stop],
    fun handle_dispatch/4, #{}).
```

## Error Handling

| Error | Meaning | Action |
|-------|---------|--------|
| `{error, unknown_command}` | Aggregate doesn't handle this command type | Check command_type spelling and aggregate execute/2 clauses |
| `{error, {not_active, State}}` | Lifecycle guard rejected command | Send lifecycle command first (start, resume) |
| `{error, stream_not_found}` | No events for this stream | This is normal for new aggregates — initial state is `#{}` |
| `{error, optimistic_concurrency}` | Another writer modified the stream | Retry the dispatch |

## Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| evoq | ~> 1.2 | CQRS framework (dispatch, aggregate behaviours) |
| reckon_gater | ~> 1.1.1 | Shared types (event records, store interface) |
| telemetry | ~> 1.3 | Instrumentation |

## Related Guides

- [evoq](evoq.md) — The framework that reckon_evoq adapts
- [reckon_db](reckon-db.md) — The store that reckon_evoq connects to
- [reckon_gater](reckon-gater.md) — The shared types used by the adapter
- [Architecture](architecture.md) — How the adapter fits in the ecosystem
