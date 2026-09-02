# reckon_gateway — gRPC Façade for ReckonDB

## Overview

reckon_gateway is a gRPC server that wraps the full ReckonDB API. It lets polyglot clients — Go, .NET, Python, anything that speaks gRPC — use ReckonDB as their event store without running on the BEAM. It serves the [reckon_proto](reckon-proto.md) contract and runs in two modes: **catalogue mode** (the default), which federates one or many remote reckon_db clusters over Erlang distribution and hosts no stores itself; and **embedded mode** (`RECKON_GATEWAY_STORE_ENABLED=true`), which boots a local store. Either way it shares the exact same semantics as a native BEAM client.

**Version:** 0.27.0 | **License:** Apache 2.0

- [GitHub](https://github.com/reckon-db-org/reckon-gateway)

> **Status:** 0.27.0. The proto contract lives in [reckon_proto](reckon-proto.md) (SemVer at the wire level); expect minor breaks until 1.0.0. The gateway exposes DCB conditional appends and CCC payload-indexed reads (`DcbService`), tamper-resistance on the wire (`RecordedEvent.prev_event_hash` — the `mac` is never transmitted), and cluster self-healing diagnostics on `HealthService`. Requires store BEAMs on **reckon-db ~> 5.7** + **reckon-gater ~> 3.10**.

## Quick Start

### Docker

```bash
docker build -t reckon-gateway .
docker run -p 50051:50051 -v reckon-data:/app/data reckon-gateway
```

### From Source

```bash
git clone https://github.com/reckon-db-org/reckon-gateway.git
cd reckon-gateway
rebar3 grpc gen     # Generate gRPC stubs from .proto files
rebar3 compile
rebar3 shell        # Listens on :50051 by default
```

## Services

| Service | Proto File | Purpose |
|---------|-----------|---------|
| **StreamService** | `reckon_streams.proto` | Append, read (forward/backward/streaming), list, delete; read by event type / tags / metadata / global |
| **SubscriptionService** | `reckon_subscriptions.proto` | Persistent subscriptions with server-streaming delivery |
| **SnapshotService** | `reckon_snapshots.proto` | Save / load aggregate state snapshots |
| **DcbService** | `reckon_dcb.proto` | DCB conditional appends + CCC payload-indexed reads (`CccReadByPayload`, `CccReadByPayloadHash`) |
| **TemporalService** | `reckon_temporal.proto` | Time-based / time-travel reads (`ReadUntil`, `ReadRange`, `VersionAt`) |
| **SchemaService** | `reckon_schema.proto` | Event schema registration and upcasting |
| **AdminService** | `reckon_admin.proto` | Store inspection, scavenging, projection/stream links, catalogue reload |
| **StoresService** | `reckon_stores.proto` | Cluster topology discovery + watch |
| **HealthService** | `reckon_health.proto` | Health checks, cluster consistency / membership consensus / Raft log verification, memory pressure, server info |

> The full contract (message shapes, every RPC) lives in [reckon_proto](reckon-proto.md). There is no `CausationService` — event lineage is an application concern, not a gateway service.

## Example: Go Client

```go
conn, _ := grpc.Dial("localhost:50051", grpc.WithInsecure())
client := gatewayv1.NewStreamServiceClient(conn)

resp, _ := client.AppendEvents(ctx, &gatewayv1.AppendEventsRequest{
    StoreId:         "default_store",
    StreamId:        "user-123",
    ExpectedVersion: -1, // NO_STREAM
    Events: []*gatewayv1.ProposedEvent{{
        EventType: "user_registered_v1",
        Data:      []byte(`{"name":"Alice","email":"alice@example.com"}`),
    }},
})
```

The expected-version constants on the wire match the BEAM-side `?ANY_VERSION` / `?NO_STREAM` / `?STREAM_EXISTS` defines from `reckon_gater_types.hrl`.

## Configuration

The gateway respects standard reckon-db / reckon-gater configuration. Key knobs:

| Setting | Default | Purpose |
|---------|---------|---------|
| `listen_port` | `50051` | gRPC listener port |
| `listen_ip` | `{0, 0, 0, 0}` | gRPC bind address |
| Data dir | (via reckon-db config) | Where streams persist on disk |

## When to Use the Gateway

**Use reckon_gateway when:**
- Clients are not on the BEAM (Go services, Python workers, .NET apps)
- You want a network boundary in front of the event store
- You are integrating with infrastructure that already speaks gRPC

**Skip the gateway when:**
- Both sides are BEAM applications — go through `reckon_gater` / `reckon_evoq` directly for lower latency and no serialization overhead.

## Dependencies

| Package | Purpose |
|---------|---------|
| `reckon_gater` | Shared types, store interface |
| `reckon_db` | Event store implementation |
| `grpc` | gRPC server runtime |
| `telemetry` | Metrics |

## Related Guides

- [reckon_db](reckon-db.md) — Event store the gateway exposes
- [reckon_gater](reckon-gater.md) — Shared types and behaviours
- [Architecture](architecture.md) — How the gateway sits at the edge of the stack
