# reckon_nifs — Rust NIF Acceleration

## Overview

reckon_nifs provides optional Rust NIFs (Native Implemented Functions) that accelerate performance-critical operations in the Reckon stack. As of 2.0.0 the NIFs are organised into **seven layer-qualified crates** that target either `reckon-db` (server-side) or `reckon-gater` (client / gateway-side). When loaded, the consuming modules detect them via `persistent_term` and switch from the pure-Erlang fallback to the NIF path.

**Version:** 2.0.1 | **License:** Apache 2.0

- [Codeberg](https://codeberg.org/reckon-db-org/reckon-nifs)

## Installation

reckon_nifs is distributed via Git because it requires a Rust toolchain to compile:

```erlang
%% rebar.config
{deps, [
    {reckon_nifs, {git, "https://codeberg.org/reckon-db-org/reckon-nifs.git", {branch, "main"}}}
]}.
```

### Prerequisites

- **Rust toolchain** — Install via [rustup](https://rustup.rs/)
- **Erlang/OTP 26+** — For NIF compatibility

## The NIF Crates

### Server-side (consumed by `reckon-db`)

| Crate | Purpose | Approx. Speedup |
|-------|---------|-----------------|
| `reckon_db_hash_nif` | xxHash64, xxHash3, partition / stream-partition hashing for routing | 10–15× |
| `reckon_db_crypto_nif` | Ed25519 signature verify, SHA-256, base64 SHA-256, secure compare (used by capability verification) | 3–5× |
| `reckon_db_archive_nif` | LZ4 / Zstd compression and decompression for archive files | 5–8× |
| `reckon_db_aggregate_nif` | Vectorised reductions over event slices | 5–10× |
| `reckon_db_filter_nif` | Regex / pattern matching against event payloads | 3–5× |
| `reckon_db_graph_nif` | Graph algorithms (petgraph) for causation and lineage queries | 5–10× |

### Client / gateway-side (consumed by `reckon-gater`)

| Crate | Purpose | Approx. Speedup |
|-------|---------|-----------------|
| `reckon_gater_crypto_nif` | Base58 encode / decode, UCAN resource pattern matching, DID utilities | 5–10× |

## How Loading Works

Each consuming module checks for NIF availability at runtime via `persistent_term`:

```erlang
%% Example pattern in reckon-db's reckon_db_hash_nif.erl
xxhash64(Data) ->
    case persistent_term:get(reckon_db_hash_nif_loaded, false) of
        true  -> nif_xxhash64(Data);      %% NIF accelerated
        false -> erlang_xxhash64(Data)    %% Pure Erlang fallback
    end.
```

When the `reckon_nifs` application starts (`reckon_nifs_loader`), it loads each `.so` and sets the corresponding `persistent_term` key, enabling the fast path. If the NIF for a given crate fails to load, the fallback path remains active — applications keep running, just slower.

## Verification

After starting your application:

```erlang
1> application:ensure_all_started(reckon_nifs).
{ok, [reckon_nifs]}

2> persistent_term:get(reckon_db_hash_nif_loaded, false).
true

3> persistent_term:get(reckon_db_crypto_nif_loaded, false).
true
```

## What the NIFs Do NOT Do Today

Although the `reckon_db_crypto_nif` crate exposes Ed25519 verification and SHA-256 hashing, **those operations are not currently wired into the event write/read path**. They are used only for capability-token / identity verification by `reckon_gater`. The event store relies on Khepri/Ra's WAL CRC for corruption detection — there is no event-level HMAC, hash chain, or signature on the on-disk representation today. See the architecture guide's *Tamper resistance* note for the roadmap if you need it.

## When to Use

**Use reckon_nifs when:**
- You're processing high event volumes (1000+ events/sec)
- Event payloads are large (10 KB+)
- Latency matters for your use case
- Your build environment has the Rust toolchain available

**Skip reckon_nifs when:**
- Development / testing environments (pure Erlang is fine)
- Low-volume applications
- Constrained CI systems where Rust compilation isn't available

## Building from Source

```bash
git clone https://codeberg.org/reckon-db-org/reckon-nifs.git
cd reckon-nifs
rebar3 compile   # Compiles Erlang + triggers rustler Rust build for each crate
rebar3 eunit
```

Each crate under `native/` is built by rustler via `rebar.config` pre-compile hooks.

## Dependencies

reckon_nifs has no Erlang runtime dependencies of its own. It only requires the Rust toolchain at compile time.

## Related Guides

- [reckon_db](reckon-db.md) — The server-side event store that consumes the `reckon_db_*` NIFs
- [reckon_gater](reckon-gater.md) — Consumes the `reckon_gater_crypto_nif` crate
- [Architecture](architecture.md) — Where NIFs fit in the stack
