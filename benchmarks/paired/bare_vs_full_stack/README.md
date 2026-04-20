# bare_vs_full_stack

**Question answered:** How much overhead does the FULL reckon stack (gater + evoq) add on top of bare reckon-db storage?

- **base:** `pair_storage_bare` — `reckon_db_streams:append/4`
- **compare:** `pair_dispatch_via_evoq` — `evoq_dispatcher:dispatch/2`

This is the headline "cost of the stack" number. If it exceeds what evaluators can stomach, the recommendation becomes "use reckon-db directly." If it's reasonable, "use the full stack" is defensible.

The delta equals approximately the SUM of `reckon_db_vs_gater` + `gater_vs_evoq` deltas — with some non-linearity from the combined layer interactions.

## Run

```
./scripts/run_paired.sh --pair bare_vs_full_stack --profile hetzner-cx32
./scripts/run_paired.sh --pair bare_vs_full_stack --profile hetzner-cx32 --scenario baseline
```
