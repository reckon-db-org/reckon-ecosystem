# reckon_gateway — gRPC Façade for ReckonDB

## Overview

reckon_gateway is a gRPC server that wraps the full ReckonDB API. It lets polyglot clients — Go, .NET, Rust, Python, anything that speaks gRPC — use ReckonDB as their event store without running on the BEAM. Internally it consumes `reckon-db` and `reckon-gater`, so it shares the exact same semantics as a native BEAM client.

**Version:** 0.1.0 | **License:** Apache 2.0

- [Codeberg](https://codeberg.org/reckon-db-org/reckon-gateway)

> **Status:** 0.1.0. The proto contracts are stabilising; expect minor breaks until 1.0.0.

## Quick Start

### Docker

```bash
docker build -t reckon-gateway .
docker run -p 50051:50051 -v reckon-data:/app/data reckon-gateway
```

### From Source

```bash
git clone https://codeberg.org/reckon-db-org/reckon-gateway.git
cd reckon-gateway
rebar3 grpc gen     # Generate gRPC stubs from .proto files
rebar3 compile
rebar3 shell        # Listens on :50051 by default
```

## Services

| Service | Proto File | Purpose |
|---------|-----------|---------|
| **StreamService** | `reckon_streams.proto` | Append, read, list, delete event streams |
| **SubscriptionService** | `reckon_subscriptions.proto` | Persistent subscriptions with server-streaming delivery |
| **SnapshotService** | `reckon_snapshots.proto` | Save / load aggregate state snapshots |
| **TemporalService** | `reckon_temporal.proto` | Time-based event queries |
| **CausationService** | `reckon_causation.proto` | Event lineage / correlation tracking |
| **SchemaService** | `reckon_schema.proto` | Event schema registration and upcasting |
| **AdminService** | `reckon_admin.proto` | Store inspection, scavenging, stream links |
| **HealthService** | `reckon_health.proto` | Health checks, cluster diagnostics, memory pressure |

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
