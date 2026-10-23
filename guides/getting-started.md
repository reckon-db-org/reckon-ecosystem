# Getting Started

By the end of this page you have a CQRS aggregate running on a real Reckon
store: a command goes in, an event is stored, and you read the event back.
Every block below is what you type; the outputs are what the stack returns.

## Prerequisites

- **Erlang/OTP 28.** This is the release the stack is built and tested on.
  OTP 29 is not supported yet.
- **rebar3** 3.27 or newer.
- A Rust toolchain is optional. reckon-db ships its NIFs in-tree and falls back
  to pure Erlang when there is no Cargo, printing one warning per NIF at build
  time.

## 1. Create a project

```bash
rebar3 new app my_app
cd my_app
```

Replace `rebar.config` with:

```erlang
{erl_opts, [debug_info]}.

{deps, [
    %% The event store
    {reckon_db, "~> 5.11"},

    %% The evoq adapter for reckon-db; brings evoq along
    {reckon_evoq, "~> 2.9"}
]}.

{shell, [
    {config, "config/sys.config"},
    {apps, [my_app]}
]}.
```

and list the stack in `src/my_app.app.src`, so starting your application
starts the store and evoq:

```erlang
{applications, [kernel, stdlib, reckon_db, evoq, reckon_evoq]},
```

## 2. Configure the store and evoq

Create `config/sys.config`:

```erlang
[
 {reckon_db, [
     {stores, [
         {my_store, [{data_dir, "data/my_store"}, {mode, single}]}
     ]}
 ]},
 {evoq, [
     {store_id, my_store},
     {event_store_adapter, reckon_evoq_adapter},
     {snapshot_store_adapter, reckon_evoq_adapter},
     {subscription_adapter, reckon_evoq_adapter}
 ]}
].
```

The `reckon_db` block makes the application start one store, `my_store`, on a
single node. The `evoq` block tells evoq to keep its events in that store,
through the reckon-evoq adapter. A clustered store needs `{mode, cluster}` and
a cluster secret; see the [reckon-db guide](reckon-db.md).

## 3. Write an aggregate

An aggregate decides what happens when a command arrives (`execute/2`) and
folds what happened into its state (`apply/2`). Create
`src/counter_aggregate.erl`:

```erlang
-module(counter_aggregate).
-behaviour(evoq_aggregate).

-export([init/1, execute/2, apply/2]).

%% A fresh counter, before any event has happened to it.
init(_CounterId) ->
    {ok, #{value => 0}}.

%% evoq hands execute/2 the command's PAYLOAD, so the payload names the
%% command it carries.
execute(_State, #{command_type := increment_counter, amount := Amount})
  when Amount > 0 ->
    {ok, [#{event_type => <<"counter_incremented_v1">>, amount => Amount}]};
execute(#{value := Value}, #{command_type := decrement_counter, amount := Amount})
  when Amount > 0, Amount =< Value ->
    {ok, [#{event_type => <<"counter_decremented_v1">>, amount => Amount}]};
execute(_State, #{command_type := decrement_counter}) ->
    {error, below_zero};
execute(_State, _Payload) ->
    {error, unknown_command}.

%% Fold one event into the state. The same function rebuilds the state
%% from the stored events when the aggregate is loaded again.
apply(#{value := Value} = State,
      #{event_type := <<"counter_incremented_v1">>, amount := Amount}) ->
    State#{value => Value + Amount};
apply(#{value := Value} = State,
      #{event_type := <<"counter_decremented_v1">>, amount := Amount}) ->
    State#{value => Value - Amount}.
```

What each callback receives:

1. **`init(AggregateId)`** builds the state of an aggregate nothing has happened
   to yet. evoq calls it before the first command, and before replaying a
   stream when the aggregate is loaded again.
2. **`execute(State, Payload)`** gets the state first and the command's payload
   second, never the command record. That is why the payload carries
   `command_type`. It returns the events to store, or `{error, Reason}` to refuse
   the command; a refused command stores nothing.
3. **An event is a flat map**: `event_type` plus the facts that happened.
   evoq stores everything except `event_type` as the event's data, so
   `#{event_type => ..., amount => 5}` is stored with data `#{amount => 5}`.
4. **`apply(State, Event)`** receives that same flat map, right after
   `execute/2` and again on every replay. Keep it pure: state and event in,
   state out.

## 4. Send a command, read the event back

```bash
rebar3 shell
```

Every aggregate has an id, and that id is also the id of the stream its events
are stored in. The store accepts `<prefix>-<32 hex>` only (see
[Stream Ids](https://hexdocs.pm/reckon_gater/stream_ids.html)), and
`reckon_gater_stream_id:new/1` makes one:

```erlang
CounterId = reckon_gater_stream_id:new(counter).
%% <<"counter-01a166248da377418fbcdad44dec51b4">>
```

A command names its type, the aggregate module, the aggregate id and the
payload. `evoq_router:dispatch/1` runs it and returns the new stream version
and the events it stored:

```erlang
evoq_router:dispatch(evoq_command:new(increment_counter, counter_aggregate, CounterId,
    #{command_type => increment_counter, amount => 5})).
%% {ok,0,[#{event_type => <<"counter_incremented_v1">>,amount => 5}]}

evoq_router:dispatch(evoq_command:new(decrement_counter, counter_aggregate, CounterId,
    #{command_type => decrement_counter, amount => 2})).
%% {ok,1,[#{event_type => <<"counter_decremented_v1">>,amount => 2}]}

evoq_router:dispatch(evoq_command:new(decrement_counter, counter_aggregate, CounterId,
    #{command_type => decrement_counter, amount => 9})).
%% {error,below_zero}
```

The third command was refused by `execute/2` (the counter is at 3), so nothing
was stored. Read the stream straight from the store:

```erlang
{ok, Events} = reckon_db_streams:read(my_store, CounterId, 0, 100, forward).
```

`Events` is a list of two `#event{}` records, versions 0 and 1, with data
`#{amount => 5}` and `#{amount => 2}`. Quit the shell, start it again, and send
another command to the same id: evoq replays the two stored events through
`apply/2`, so the counter starts at 3, not 0.

## 5. Test the aggregate without a store

The callbacks are plain functions, so they test without a running store. Create
`test/counter_aggregate_tests.erl`:

```erlang
-module(counter_aggregate_tests).
-include_lib("eunit/include/eunit.hrl").

increment_test() ->
    {ok, State0} = counter_aggregate:init(<<"counter-018f6a7b8c9d4e5f60718293a4b5c6dd">>),
    {ok, [Event]} = counter_aggregate:execute(State0,
        #{command_type => increment_counter, amount => 10}),
    ?assertEqual(<<"counter_incremented_v1">>, maps:get(event_type, Event)),
    ?assertEqual(#{value => 10}, counter_aggregate:apply(State0, Event)).

no_decrement_below_zero_test() ->
    {ok, State0} = counter_aggregate:init(<<"counter-018f6a7b8c9d4e5f60718293a4b5c6dd">>),
    ?assertEqual({error, below_zero},
        counter_aggregate:execute(State0, #{command_type => decrement_counter, amount => 1})).
```

```bash
rebar3 eunit
```

## Next Steps

- [Projections](https://hexdocs.pm/evoq/projections.html) in the evoq docs:
  turn the stored events into a read model.
- [Architecture](architecture.md): how reckon-db, reckon-gater, evoq and
  reckon-evoq fit together.
- [evoq Guide](evoq.md): the CQRS framework in depth.
- [reckon_db Guide](reckon-db.md): store configuration and operations.
