# Changelog

All notable changes to this project will be documented in this file.

## [0.4.0] - 2026-07-06

### Tracks the July 2026 wave + adds the wire contract and polyglot client tier

The ecosystem docs were frozen at the 2.1 / 1.15 / 0.2 wave (May 2026) while the
stack moved several major versions and grew a whole polyglot client family. This
release resynchronises everything.

**Versions corrected** (README, dependency graph, and every guide header):

- `reckon-db` 2.1.0 → **5.9.0** — adds DCB + CCC conditional consistency and
  continuous cluster self-healing
- `reckon-gater` 2.1.0 → **3.10.0** — DCB/CCC `tag_filter` types
- `evoq` 1.15.0 → **1.23.0** — Decisions (DCB/CCC) via `evoq_decision`
- `reckon-evoq` 2.1.0 → **2.7.0**
- `reckon-nifs` 2.0.0 → **2.0.1**
- `reckon-gateway` 0.2.0 → **0.27.0** — catalogue-mode federation + optional
  embedded store

**New: wire contract + polyglot clients** (previously undocumented):

- `reckon-proto` **0.8.0** — the gRPC `.proto` contract, single source of truth
- `reckon-go` **0.9.0**, `reckon-dotnet` **0.1.0**, `reckon-py` **0.1.0** —
  idiomatic clients generated from the contract
- `reckon-lazy` **0.4.0** — the `lazyreckon` TUI, built on reckon-go

### Added

- **guides/reckon-proto.md** — the wire contract, service/RPC catalogue,
  versioning, and how gateway + clients consume it.
- **guides/polyglot-clients.md** — Go, .NET, Python, and lazyreckon.
- **guides/dcb-and-ccc.md** — multi-stream consistency (DCB tag-filter boundaries
  and CCC payload-indexed conditions) across reckon_db, evoq, and the gateway.
- README **Polyglot Clients** section + a second package table for the contract
  and client tier.

### Changed

- **assets/dependency-graph.svg** — corrected every version label; added Level 5
  (reckon_proto wire contract) and Level 6 (polyglot clients), with build-time
  stub-generation edges and runtime gRPC-connect edges.
- **README.md** — feature list now covers DCB/CCC and self-healing; per-package
  capabilities and install snippets updated; loose (`~> X.Y`) constraints.
- **guides/reckon-gateway.md** — service table corrected: removed the
  non-existent `CausationService`, added `DcbService` and `StoresService`,
  documented catalogue vs embedded modes.
- **guides/architecture.md** — status note moved from the 2.1 wave to the current
  wave; DCB/CCC + self-healing referenced.
- **reckon-db / reckon-gater / evoq / reckon-evoq / reckon-nifs / getting-started**
  guides — version headers and dependency snippets updated to the current wave.

## [0.3.0] - 2026-05-15

### Tracks the 2.1 / 1.15 / 0.2 release wave

This release synchronises the ecosystem documentation with the
tamper-resistance work shipped across five packages:

- `reckon-gater` **2.1.0** (hex) — schema additions + integrity primitives
- `reckon-db` **2.1.0** (hex) — Layers 2–5: write-time integrity,
  verify-at-read, snapshot anchor, subscription catch-up verification
- `evoq` **1.15.0** (hex) — `prev_event_hash` propagation,
  `integrity_violation` non-retriable classification (Layer 6)
- `reckon-evoq` **2.1.0** (hex) — chain hash forwarding through the adapter
- `reckon-gateway` **0.2.0** (git tag + Docker; hex blocked by git deps) —
  wire-format additions + `GetServerInfo` RPC (Layer 7)

### Changed

- **README.md** — package version table updated; install snippets
  bumped to the 2.1 wave; each row now flags its tamper-resistance
  contribution.
- **guides/architecture.md** — *On-Disk Format and Tamper Resistance*
  section rewritten from a roadmap stub to a full reference of the
  shipped feature: per-store opt-in configuration, schema fields,
  canonical encoding, verify-at-read enforcement across all
  surfaces, `chain_start_version` migration story, an attack/detection
  matrix, and an explicit limitations list. Points at
  [`reckon-db/plans/PLAN_TAMPER_RESISTANCE.md`](https://codeberg.org/reckon-db-org/reckon-db/src/branch/main/plans/PLAN_TAMPER_RESISTANCE.md)
  for the full design and the deferred-scope list.
- **guides/evoq.md** — *Notable Changes* extended with 1.15.0
  entry covering the `prev_event_hash` propagation and the
  integrity-violation classifier.
- **guides/reckon-evoq.md**, **guides/reckon-db.md**,
  **guides/reckon-gater.md**, **guides/reckon-gateway.md**,
  **guides/getting-started.md** — version bumps + dependency
  constraint updates (`~> 2.1` / `~> 1.15`) to match the new wave.

### Known Gaps (carried, not closed in 0.3.0)

- **`assets/dependency-graph.svg`** and **`assets/ecosystem-overview.svg`**
  still reflect pre-2.1 visual state. Carried over from the 0.2.0
  changelog; not regressed, just unfinished. Updating an SVG by
  hand is meaningfully more work than updating text, and the
  package-table in README is the primary version-of-truth.

## [0.2.0] - 2026-05-14

### Added

- **`guides/reckon-gateway.md`** — new guide for the gRPC façade package (`reckon_gateway` 0.1.0). Documents services, configuration, polyglot client usage.
- **Tamper-resistance section** in `guides/architecture.md` explaining the current on-disk integrity properties (Khepri/Ra WAL CRC) versus what the event record carries (nothing) and the dormant cryptographic NIFs that are available but unused on the event path.

### Changed

- **Versions bumped across README + guides** to reflect the current state of all packages:
  - `reckon_db` 1.2.4 → **2.0.0** (esdb_* → reckon_db_* module rename)
  - `reckon_gater` 1.1.2 → **2.0.1** (esdb_* → reckon_gater_* module rename + local-node worker preference fix)
  - `reckon_evoq` 1.1.4 → **2.0.0** (consumes renamed reckon-gater 2.0.0 API)
  - `reckon_nifs` 1.0.1 → **2.0.0** (NIF crates renamed to `reckon_{db,gater}_*` scheme)
  - `evoq` 1.3.1 → **1.14.4** (multiple `wrong_expected_version` and rebuild-from-events fixes)
- **`guides/reckon-nifs.md` rewritten** to reflect the actual layout: seven layer-qualified Rust crates (`reckon_db_hash_nif`, `reckon_db_crypto_nif`, `reckon_db_archive_nif`, `reckon_db_aggregate_nif`, `reckon_db_filter_nif`, `reckon_db_graph_nif`, `reckon_gater_crypto_nif`) rather than the previous single-crate fiction. Documents the `persistent_term`-based loading mechanism.
- **`guides/architecture.md` event-envelope section** corrected to use `#reckon_event{}` (from `reckon-gater`) instead of the obsolete `#evoq_event{}` description. Field list now matches the canonical record in `reckon_gater_types.hrl`.
- **`guides/evoq.md`** — added summary of the 1.4.x → 1.14.x pipeline fixes (recognition of all three `wrong_expected_version` shapes, empty-stream rebuild version `-1`, header rename to `reckon_gater_types.hrl`).
- **`guides/reckon-gater.md`** — added a migration note pointing at the 2.0.0 module rename.
- **README** describes the ecosystem as **six** packages, includes a `reckon_gateway` entry in the package table and install order, and points at the Codeberg URLs as canonical (with GitHub flagged as a read-only mirror).

### Known Gaps (carried, not closed in 0.2.0)

- **`assets/dependency-graph.svg`** still shows only five packages. Needs a 6th node for `reckon_gateway` and arrows from `reckon_gateway` → `reckon_db` and `reckon_gateway` → `reckon_gater`. Listed here so it does not silently drift.
- **`assets/ecosystem-overview.svg`** likewise predates the gateway and the 2.0.0 renames.

## [0.1.0] - 2026-02-10

### Added

- Initial ecosystem documentation
- Architecture overview with dependency graph
- Package guides for reckon_db, reckon_gater, evoq, reckon_nifs, and reckon_evoq
- SVG diagrams: ecosystem overview, data flow, dependency graph, logo
- Getting started guide
- Architecture deep-dive
