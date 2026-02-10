# reckon_nifs — Rust NIF Acceleration

## Overview

reckon_nifs provides optional Rust NIFs (Native Implemented Functions) that accelerate performance-critical operations in the Reckon stack. When present, reckon_db detects and uses them automatically for hot-path operations like checksums, hashing, and serialization.

**Version:** 1.0.1 | **License:** Apache 2.0

- [GitHub](https://github.com/reckon-db-org/reckon-nifs)

## Installation

reckon_nifs is distributed via Git (not hex.pm) because it requires a Rust toolchain to compile:

```erlang
%% rebar.config
{deps, [
    {reckon_nifs, {git, "https://github.com/reckon-db-org/reckon-nifs.git", {branch, "main"}}}
]}.
```

### Prerequisites

- **Rust toolchain** — Install via [rustup](https://rustup.rs/)
- **Erlang/OTP 26+** — For NIF compatibility

## NIF Operations

reckon_nifs provides seven categories of accelerated operations:

### CRC32

Cyclic redundancy checks for event integrity verification:

```erlang
Checksum = reckon_nifs:crc32(EventBinary).
```

### Hashing

Fast cryptographic and non-cryptographic hashing:

```erlang
Hash = reckon_nifs:hash(Data).
```

### Checksums

Data integrity verification for stored events and snapshots:

```erlang
ok = reckon_nifs:verify_checksum(Data, ExpectedChecksum).
```

### Compression

Efficient compression/decompression for event payloads:

```erlang
Compressed = reckon_nifs:compress(EventData),
Original = reckon_nifs:decompress(Compressed).
```

### Serialization

High-speed encoding and decoding of event data:

```erlang
Encoded = reckon_nifs:encode(Term),
{ok, Decoded} = reckon_nifs:decode(Encoded).
```

### Filtering

Optimized event stream filtering operations:

```erlang
Filtered = reckon_nifs:filter_events(Events, Criteria).
```

### Timestamp Generation

Monotonic, high-resolution timestamp generation:

```erlang
Timestamp = reckon_nifs:monotonic_timestamp().
```

## Integration with reckon_db

reckon_db auto-detects reckon_nifs at startup. No configuration needed:

```erlang
%% reckon_db checks:
case code:ensure_loaded(reckon_nifs) of
    {module, reckon_nifs} ->
        %% Use Rust NIFs for hot-path operations
        reckon_nifs:crc32(Data);
    {error, _} ->
        %% Fall back to pure Erlang implementation
        erlang:crc32(Data)
end.
```

## Performance

The Rust NIFs provide measurable speedups for:

| Operation | Erlang | Rust NIF | Speedup |
|-----------|--------|----------|---------|
| CRC32 (1MB) | ~2ms | ~0.3ms | ~6x |
| Compression | ~5ms | ~1ms | ~5x |
| Serialization | ~3ms | ~0.5ms | ~6x |

These gains compound in high-throughput scenarios where thousands of events flow through the store per second.

## When to Use

**Use reckon_nifs when:**
- You're processing high event volumes (1000+ events/sec)
- Event payloads are large (10KB+)
- Latency matters for your use case
- You're running on hardware where Rust compilation is feasible

**Skip reckon_nifs when:**
- Development/testing environments (pure Erlang is fine)
- Low-volume applications
- Environments where Rust compilation isn't available (e.g., some CI systems)

## Building from Source

```bash
git clone https://github.com/reckon-db-org/reckon-nifs.git
cd reckon-nifs
rebar3 compile   # Compiles Erlang + triggers Rust build
rebar3 eunit     # Run tests
```

The Rust code lives in `native/reckon_nifs/` and is compiled automatically during `rebar3 compile` via a pre-compile hook.

## Dependencies

reckon_nifs has no Erlang dependencies. It only requires the Rust toolchain at compile time.

## Related Guides

- [reckon_db](reckon-db.md) — The event store that uses these NIFs
- [Architecture](architecture.md) — Where NIFs fit in the stack
