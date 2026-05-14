# Changelog

All notable changes to this project will be documented in this file.

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
