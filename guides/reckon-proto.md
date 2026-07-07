# reckon_proto — The gRPC Wire Contract

## Overview

reckon_proto is the **single source of truth** for how the outside world talks to ReckonDB. It is the set of `.proto` files that define every gRPC service, RPC, and message shape. The [gateway](reckon-gateway.md) generates its server stubs from these files; every polyglot client ([Go](polyglot-clients.md), [.NET](polyglot-clients.md), [Python](polyglot-clients.md)) generates its client stubs from the same files. Nothing hand-writes protobuf.

**Version:** 0.8.0 | **License:** Apache 2.0

- [Codeberg](https://codeberg.org/reckon-db-org/reckon-proto)

> **Versioning:** SemVer at the **wire-contract level**. A client and a gateway on the same minor line interoperate. Breaking wire changes bump the minor while the contract is pre-1.0; expect stabilisation toward 1.0.0.

## What's in it

The contract is split by concern, one `.proto` per service plus a shared types file:

| File | Defines |
|------|---------|
| `reckon_shared.proto` | Common message types (events, versions, error shapes) shared across services |
| `reckon_streams.proto` | `StreamService` — append, read forward/backward/streaming, list, delete, read by event type / tags / metadata, global read |
| `reckon_subscriptions.proto` | `SubscriptionService` — persistent server-streaming subscriptions |
| `reckon_snapshots.proto` | `SnapshotService` — record / read / delete / list snapshots |
| `reckon_dcb.proto` | `DcbService` — DCB conditional appends and CCC payload-indexed reads (`CccReadByPayload`, `CccReadByPayloadHash`) |
| `reckon_temporal.proto` | `TemporalService` — `ReadUntil`, `ReadRange`, `VersionAt` |
| `reckon_schema.proto` | `SchemaService` — register / unregister / get / list schemas, `UpcastEvents` |
| `reckon_stores.proto` | `StoresService` — `ListStores`, `GetStore`, `WatchStores` |
| `reckon_admin.proto` | `AdminService` — store stats, stream info, scavenge (+ dry-run / matching), projection/stream links, catalogue reload |
| `reckon_health.proto` | `HealthService` — `Check`, `VerifyClusterConsistency`, `VerifyMembershipConsensus`, `CheckRaftLogConsistency`, memory level/stats, server info |

There is **no** `CausationService` — event lineage and correlation are an application concern, deliberately kept out of the wire contract.

## DCB and CCC on the wire

`DcbService` is where multi-stream consistency reaches polyglot clients. A DCB append conditions the write on a **tag-filter context query** (the absence of events matching a boolean tag / `event_type` expression); CCC extends the boundary to **payload-indexed** conditions. See [DCB &amp; CCC](dcb-and-ccc.md) for the model, and the client guides for idiomatic bindings.

`0.8.0` added `CccReadByPayload` and `CccReadByPayloadHash` to `DcbService`, plus the `TagFilter.event_type_match` leaf for `event_type`-scoped context queries.

## Toolchain

reckon_proto is a [buf](https://buf.build/) module (`buf.yaml`, `buf.gen.yaml`) and also ships generated Erlang stubs in `src/` (via `rebar.config`) so BEAM consumers can depend on it directly:

```erlang
%% reckon_gateway consumes the generated Erlang server stubs
{reckon_proto, {git, "https://codeberg.org/reckon-db-org/reckon-proto.git", {tag, "v0.8.0"}}}.
```

Clients pin the contract per their ecosystem:
- **reckon-py** vendors the `.proto` files under `proto/` and generates Python stubs.
- **reckon-dotnet** pins `reckon-proto` as a **git submodule** and generates C#.
- **reckon-go** generates Go stubs into `genproto/gatewayv1`.

Pin all of them to a matching contract tag so wire compatibility is explicit.

## See also

- [reckon_gateway](reckon-gateway.md) — the server that implements this contract
- [Polyglot Clients](polyglot-clients.md) — the generated clients
- [DCB &amp; CCC](dcb-and-ccc.md) — the consistency model behind `DcbService`
