# Polyglot Clients

## Overview

ReckonDB is a BEAM-native event store, but you don't need to run on the BEAM to use it. The [gateway](reckon-gateway.md) exposes the whole store surface over gRPC, and a family of idiomatic clients — generated from the [reckon_proto](reckon-proto.md) contract — let Go, .NET, and Python applications append and read events, run subscriptions, take snapshots, and use DCB/CCC, all over one gRPC connection.

Every client follows the same shape: **one connection to one gateway endpoint**, with per-service sub-clients bound to a store. They generate their stubs from `reckon_proto` at build time and speak gRPC (TLS by default) at runtime. Keep the client and gateway on the same wire minor line.

## reckon-go

**Version:** 0.9.0 — [GitHub](https://github.com/reckon-db-org/reckon-go)

The import path ends in `reckon-go`; the Go package is `reckon`.

```go
import reckon "github.com/reckon-db-org/reckon-go"

c, err := reckon.Connect(ctx, "gateway.example.org:50051") // TLS, system roots (the default)
if err != nil { /* ... */ }
defer c.Close()

stores, err := c.Stores().List(ctx)
```

One `reckon.Client` wraps one gRPC connection. Per-service sub-clients share it:

| Sub-client | Method | Purpose |
|---|---|---|
| `stores` | `c.Stores()` | cluster topology discovery + watch |
| `streams` | `c.Streams(store)` | append + read events on a stream |
| `subscriptions` | `c.Subscriptions(store)` | live + persistent subscriptions |
| `snapshots` | `c.Snapshots(store)` | per-stream snapshots |
| `dcb` | `c.Dcb(store)` | DCB writes/reads + CCC payload reads |
| `schema` | `c.Schema(store)` | schema registration + upcasting |
| `temporal` | `c.Temporal(store)` | wall-clock / time-travel reads |
| `admin` | `c.Admin(store)` | scavenge, projection links, store stats |
| `health` | `c.Health()` | gateway-wide gRPC health snapshot |

`Stores()` and `Health()` are gateway-wide and take no store; the rest are cheap to construct and bind to a store id.

## reckon-dotnet

**Version:** 0.1.0 — [GitHub](https://github.com/reckon-db-org/reckon-dotnet)

Idiomatic async .NET client. Pins `reckon_proto` as a git submodule and generates C# from it.

```csharp
await using var client = await ReckonClient.ConnectAsync("gateway.example.org:50051");
var overview = await client.Health.OverviewAsync();
Console.WriteLine($"node={overview.Node} status={overview.Status}");
```

## reckon-py

**Version:** 0.1.0 — [GitHub](https://github.com/reckon-db-org/reckon-py)

Python client for scripting, data workflows, and integration. Vendors the `.proto` files and generates Python stubs.

```bash
pip install reckon  # from the repo's packaging; see the README
```

## reckon-lazy (lazyreckon)

**Version:** 0.4.0 — [GitHub](https://github.com/reckon-db-org/reckon-lazy)

`lazyreckon` is a terminal UI for operators and developers: browse stores, drill into streams, and inspect events without writing code. It is built on **reckon-go**, so it connects to the same gateway the same way.

```bash
lazyreckon --gateway gateway.example.org:50051
```

## Choosing a client

- **Go** — the most complete surface (all sub-services); also the substrate for lazyreckon.
- **.NET / Python** — v0.1.0, covering the core surface; grow with the contract.
- Any other gRPC-capable language can generate its own stubs from [reckon_proto](reckon-proto.md) directly — these clients are conveniences, not gatekeepers.

## See also

- [reckon_proto](reckon-proto.md) — the contract every client generates from
- [reckon_gateway](reckon-gateway.md) — the server they connect to
- [DCB &amp; CCC](dcb-and-ccc.md) — multi-stream consistency, available through the `dcb` sub-client
