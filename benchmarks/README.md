# reckon-ecosystem Benchmarks

Paired and comparative benchmarks spanning multiple layers of the reckon stack.

For single-layer benchmarks see each layer's own `benchmarks/` directory:
- `reckon-db/benchmarks/` — storage engine
- `reckon-gater/benchmarks/` — gateway API
- `evoq/benchmarks/` — CQRS framework
- `reckon-evoq/benchmarks/` — adapter

---

## What lives here

- **`paired/`** — named pairs of slices designed to run back-to-back on the same VM so layer-overhead deltas are directly measurable.
- **`slices/`** — local slice implementations that the paired runs consume. Named `pair_*` by convention.
- **`src/paired_join.erl`** — joins two slice results into a delta report.
- **`scripts/run_paired.sh`** — the paired-run orchestrator.

## Paired runs

| Pair | Base | Compare | Answers |
|---|---|---|---|
| `reckon_db_vs_gater` | `pair_storage_bare` | `pair_storage_via_gater` | Gater's per-op overhead above bare storage |
| `gater_vs_evoq` | `pair_storage_via_gater` | (evoq slice) | Evoq's per-op overhead above gater |
| `bare_vs_full_stack` | `pair_storage_bare` | (evoq slice) | Full-stack overhead above bare storage |

Only `reckon_db_vs_gater` is scaffolded as of initial commit. The others follow the same pattern.

## Running a paired bench

```
./scripts/run_paired.sh --pair reckon_db_vs_gater --profile local-dev
```

Output lands in `results/<run-id>/` with:
- `base.json` — base slice full report
- `compare.json` — compare slice full report
- `paired.json` — joined delta report
- console line summarising throughput + p99 deltas

## Methodology

Single source of truth: [reckon-bench-harness/METHODOLOGY.md](https://github.com/reckon-db-org/reckon-bench-harness/blob/main/METHODOLOGY.md).

Paired-specific rule: **base and compare slices MUST share scenario parameters**. The joiner assumes same event size, same parallelism, same duration. Mismatched scenarios produce meaningless deltas.

## Publication policy

Paired numbers leave this repository only under the rules in METHODOLOGY.md § Publication Policy. Local-dev paired runs are validation-only.

## License

Apache-2.0.
