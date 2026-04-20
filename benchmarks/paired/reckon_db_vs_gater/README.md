# reckon_db_vs_gater

**Question answered:** How much per-operation overhead does the gater API layer add above bare storage?

Runs two slices back-to-back on the same VM, same store, same hardware profile:
- **base:** `pair_storage_bare` — direct `reckon_db_streams:append/4`
- **compare:** `pair_storage_via_gater` — `reckon_gater_api:append_events/3`

The delta is the gater's overhead.

## Run

```
./scripts/run_paired.sh --pair reckon_db_vs_gater --profile local-dev
```

Results land in `results/<run-id>/` with `base.json`, `compare.json`, and `paired.json`.

## Interpretation

Small deltas (~5% or less) mean the gater is a thin pass-through — the marketing story is "no meaningful overhead."

Large deltas (>15%) mean the gater has real cost, which becomes a tuning opportunity or a caveat that must be honestly included in publications.

On local-dev the run-to-run variance is large enough that only directional signals matter. Publish-quality deltas require a tuned hardware profile and ≥100k samples per side.
