# evoq — CQRS/ES Framework

## Overview

evoq is a pure, backend-agnostic CQRS and Event Sourcing framework for Erlang/OTP. It provides behaviours for aggregates, projections, process managers, and more — without any opinion about how events are stored. This separation means your domain code is testable, portable, and decoupled from infrastructure.

**Version:** 1.2.1 | **License:** Apache 2.0

- [GitHub](https://github.com/reckon-db-org/evoq) | [HexDocs](https://hexdocs.pm/evoq)

## Installation

```erlang
{deps, [{evoq, "1.2.1"}]}.
```

Note: evoq is typically pulled in as a transitive dependency of reckon_evoq.

## Core Behaviours

### evoq_aggregate

The aggregate is the core building block. It receives commands and produces events.

```erlang
-module(order_aggregate).
-behaviour(evoq_aggregate).

-export([execute/2, apply/2]).

%% CRITICAL: State comes FIRST in both callbacks!

%% execute/2 — Validate command, produce events
execute(#{status := pending} = _State,
        #{command_type := <<"confirm_order_v1">>,
          order_id := OrderId}) ->
    {ok, [#{event_type => <<"order_confirmed_v1">>,
            order_id => OrderId,
            confirmed_at => erlang:system_time(millisecond)}]};

execute(#{status := Status}, #{command_type := <<"confirm_order_v1">>}) ->
    {error, {invalid_status, Status}};

execute(_State, _Cmd) ->
    {error, unknown_command}.

%% apply/2 — Update state from event (MUST be pure, no side effects)
apply(State, #{event_type := <<"order_confirmed_v1">>}) ->
    State#{status => confirmed};

apply(State, _Event) ->
    State.
```

**Callback signatures:**

| Callback | Arguments | Returns |
|----------|-----------|---------|
| `execute/2` | `(State, Command)` | `{ok, [Events]}` or `{error, Reason}` |
| `apply/2` | `(State, Event)` | `NewState` |

### evoq_projection

Projections consume events and build read models:

```erlang
-module(orders_projection).
-behaviour(evoq_projection).

-export([handle_event/2]).

handle_event(#{event_type := <<"order_confirmed_v1">>,
               order_id := Id,
               confirmed_at := At}, State) ->
    %% Update SQLite read model
    esqlite3:exec(Db,
        "UPDATE orders SET status = 'confirmed', confirmed_at = ? WHERE id = ?",
        [At, Id]),
    State;

handle_event(_Event, State) ->
    State.
```

### evoq_process_manager

Process managers orchestrate across aggregates. They subscribe to events and dispatch commands to other aggregates:

```erlang
-module(on_order_confirmed_reserve_inventory).

handle_event(#{event_type := <<"order_confirmed_v1">>,
               order_id := OrderId,
               items := Items}) ->
    %% Dispatch command to inventory aggregate
    Cmd = #{command_type => <<"reserve_items_v1">>,
            order_id => OrderId,
            items => Items},
    {dispatch, <<"inventory-", OrderId/binary>>, inventory_aggregate, Cmd};

handle_event(_) ->
    ignore.
```

## Bit Flags

evoq includes `evoq_bit_flags` for compact, CPU-efficient status tracking:

```erlang
-include_lib("evoq/include/evoq_bit_flags.hrl").

%% Define flags as powers of 2
-define(INITIATED,  1).
-define(ACTIVE,     2).
-define(PAUSED,     4).
-define(COMPLETED,  8).
-define(ARCHIVED,  16).

%% Operations
Status = evoq_bit_flags:set(0, ?INITIATED),          %% 1
Status2 = evoq_bit_flags:set(Status, ?ACTIVE),        %% 3
evoq_bit_flags:has(Status2, ?ACTIVE),                  %% true
evoq_bit_flags:has(Status2, ?PAUSED),                  %% false
evoq_bit_flags:has_any(Status2, [?ACTIVE, ?PAUSED]),   %% true
Status3 = evoq_bit_flags:unset(Status2, ?INITIATED),   %% 2
```

### Why Bit Flags?

- **1 integer** replaces N booleans
- **CPU-native** bitwise operations
- **SQL-friendly** — `WHERE status & 2 = 2` (is active?)
- **Network-efficient** — Single integer over the wire
- **Event sourcing friendly** — Compact in event payloads

## Event Naming Convention

| Element | Format | Example |
|---------|--------|---------|
| Event type | `{subject}_{past_verb}_v{N}` | `<<"order_confirmed_v1">>` |
| Command type | `{verb}_{subject}_v{N}` | `<<"confirm_order_v1">>` |
| Module name | `{event_type}.erl` | `order_confirmed_v1.erl` |

Events are **facts** (past tense). Commands are **intentions** (imperative). Never use CRUD verbs (created, updated, deleted).

## Testing

Aggregates are pure functions — test them without infrastructure:

```erlang
-module(order_aggregate_test).
-include_lib("eunit/include/eunit.hrl").

confirm_pending_order_test() ->
    State = #{status => pending, order_id => <<"ord-1">>},
    Cmd = #{command_type => <<"confirm_order_v1">>, order_id => <<"ord-1">>},
    {ok, [Event]} = order_aggregate:execute(State, Cmd),
    ?assertEqual(<<"order_confirmed_v1">>, maps:get(event_type, Event)),

    NewState = order_aggregate:apply(State, Event),
    ?assertEqual(confirmed, maps:get(status, NewState)).

reject_confirmed_order_test() ->
    State = #{status => confirmed},
    Cmd = #{command_type => <<"confirm_order_v1">>, order_id => <<"ord-1">>},
    ?assertMatch({error, {invalid_status, confirmed}},
                 order_aggregate:execute(State, Cmd)).
```

This is one of the biggest benefits of evoq's design. No mocks, no test databases, no setup/teardown.

## Lifecycle Protocol

For long-lived processes, evoq supports a standard lifecycle:

```
pending → active → paused → active → completed
                                    ↘ archived
```

Each aggregate implements lifecycle commands:

```erlang
execute(#{lifecycle_state := pending}, #{command_type := <<"start_..._v1">>} = Cmd) ->
    {ok, [#{event_type => <<"..._started_v1">>, ...}]};

execute(#{lifecycle_state := active}, #{command_type := <<"pause_..._v1">>} = Cmd) ->
    {ok, [#{event_type => <<"..._paused_v1">>, ...}]};

execute(#{lifecycle_state := paused}, #{command_type := <<"resume_..._v1">>} = Cmd) ->
    {ok, [#{event_type => <<"..._resumed_v1">>, ...}]};

execute(#{lifecycle_state := active}, #{command_type := <<"complete_..._v1">>} = Cmd) ->
    {ok, [#{event_type => <<"..._completed_v1">>, ...}]}.
```

Domain commands are guarded to only work in `active` state:

```erlang
execute(#{lifecycle_state := active}, #{command_type := <<"domain_cmd_v1">>} = Cmd) ->
    %% Normal processing
    ...;
execute(#{lifecycle_state := Other}, #{command_type := <<"domain_cmd_v1">>}) ->
    {error, {not_active, Other}}.
```

## Dependencies

evoq has no Reckon dependencies. It depends only on:

| Package | Version | Purpose |
|---------|---------|---------|
| telemetry | 1.3.0 | Instrumentation |

## Related Guides

- [Getting Started](getting-started.md) — Build your first aggregate
- [Architecture](architecture.md) — How evoq fits in the ecosystem
- [reckon_evoq](reckon-evoq.md) — Connecting evoq to reckon_db
