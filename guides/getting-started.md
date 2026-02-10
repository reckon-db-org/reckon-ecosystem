# Getting Started

## Prerequisites

- **Erlang/OTP 26+** (OTP 27 recommended)
- **Rebar3** (build tool)
- **Rust toolchain** (only if using reckon_nifs)

## Installation

### 1. Create a new project

```bash
rebar3 new app my_app
cd my_app
```

### 2. Add dependencies

Edit `rebar.config`:

```erlang
{deps, [
    %% Core event store
    {reckon_db, "1.2.3"},

    %% CQRS adapter (brings evoq as transitive dependency)
    {reckon_evoq, "1.1.3"}
]}.
```

### 3. Fetch and compile

```bash
rebar3 get-deps
rebar3 compile
```

## Your First Aggregate

Let's build a simple counter aggregate that tracks a numeric value.

### Define the aggregate

```erlang
-module(counter_aggregate).
-behaviour(evoq_aggregate).

-export([execute/2, apply/2]).

%% Handle commands
execute(_State, #{command_type := <<"increment_counter_v1">>,
                  counter_id := CounterId,
                  amount := Amount}) ->
    {ok, [#{event_type => <<"counter_incremented_v1">>,
            counter_id => CounterId,
            amount => Amount}]};

execute(_State, #{command_type := <<"decrement_counter_v1">>,
                  counter_id := CounterId,
                  amount := Amount}) ->
    {ok, [#{event_type => <<"counter_decremented_v1">>,
            counter_id => CounterId,
            amount => Amount}]};

execute(_State, _Unknown) ->
    {error, unknown_command}.

%% Apply events to state
apply(State, #{event_type := <<"counter_incremented_v1">>,
               amount := Amount}) ->
    Value = maps:get(value, State, 0),
    State#{value => Value + Amount};

apply(State, #{event_type := <<"counter_decremented_v1">>,
               amount := Amount}) ->
    Value = maps:get(value, State, 0),
    State#{value => Value - Amount}.
```

### Key points

1. **`execute(State, Command)`** — State comes first! This is the evoq callback convention.
2. **Commands are maps** with a `command_type` key for dispatch.
3. **Events are maps** with an `event_type` key. Events are returned as a list from `execute/2`.
4. **`apply(State, Event)`** — Rebuilds aggregate state from events. Must be pure (no side effects).

### Start the store

In your application's supervisor:

```erlang
init([]) ->
    StoreSpec = #{
        name => my_app_store,
        data_dir => "/var/lib/my_app/events"
    },
    Children = [
        #{id => event_store,
          start => {reckon_db_sup, start_store, [StoreSpec]},
          type => supervisor}
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.
```

### Dispatch a command

```erlang
%% Create a command
Cmd = #{
    command_type => <<"increment_counter_v1">>,
    counter_id => <<"counter-1">>,
    amount => 5
},

%% Dispatch via reckon_evoq
StreamId = <<"counter-counter-1">>,
Result = reckon_evoq:dispatch(my_app_store, StreamId, counter_aggregate, Cmd).
%% => {ok, [Event1]}
```

### Subscribe to events

```erlang
%% Subscribe to all events on a stream
{ok, Sub} = reckon_evoq:subscribe(my_app_store, <<"counter-counter-1">>, self()),

%% Receive events
receive
    {reckon_event, Event} ->
        io:format("Got event: ~p~n", [Event])
end.
```

## Building a Projection

Projections transform events into read models optimized for queries:

```erlang
-module(counter_projection).
-behaviour(evoq_projection).

-export([handle_event/2]).

handle_event(#{event_type := <<"counter_incremented_v1">>,
               counter_id := Id,
               amount := Amount}, State) ->
    %% Update your read model (ETS, SQLite, etc.)
    Current = ets:lookup_element(counters, Id, 2, 0),
    ets:insert(counters, {Id, Current + Amount}),
    State;

handle_event(#{event_type := <<"counter_decremented_v1">>,
               counter_id := Id,
               amount := Amount}, State) ->
    Current = ets:lookup_element(counters, Id, 2, 0),
    ets:insert(counters, {Id, Current - Amount}),
    State;

handle_event(_Event, State) ->
    State.
```

## Testing Aggregates

One of the biggest benefits of evoq's design: aggregates are pure functions. Test them without a running store:

```erlang
-module(counter_aggregate_test).
-include_lib("eunit/include/eunit.hrl").

increment_test() ->
    State = #{},
    Cmd = #{command_type => <<"increment_counter_v1">>,
            counter_id => <<"test">>,
            amount => 10},
    {ok, [Event]} = counter_aggregate:execute(State, Cmd),
    ?assertEqual(<<"counter_incremented_v1">>, maps:get(event_type, Event)),
    ?assertEqual(10, maps:get(amount, Event)),

    %% Apply and verify state
    NewState = counter_aggregate:apply(State, Event),
    ?assertEqual(10, maps:get(value, NewState)).

unknown_command_test() ->
    State = #{},
    Cmd = #{command_type => <<"unknown_v1">>},
    ?assertEqual({error, unknown_command}, counter_aggregate:execute(State, Cmd)).
```

```bash
rebar3 eunit
```

## Next Steps

- [Architecture](architecture.md) — Understand how the packages work together
- [evoq Guide](evoq.md) — Deep-dive into the CQRS framework
- [reckon_db Guide](reckon-db.md) — Event store configuration and operations
