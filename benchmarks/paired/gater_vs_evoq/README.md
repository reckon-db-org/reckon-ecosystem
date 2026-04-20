# gater_vs_evoq

**Question answered:** How much per-operation overhead does evoq (CQRS framework) add above bare gater API calls?

- **base:** `pair_storage_via_gater` — `reckon_gater_api:append_events/3`
- **compare:** `pair_dispatch_via_evoq` — `evoq_dispatcher:dispatch/2` through `reckon_evoq_adapter`

The delta is the evoq dispatch-pipeline overhead — middleware + aggregate lookup + execute + idempotency cache — minus whatever the gater already costs (both sides pay that cost equally).

## Run

```
./scripts/run_paired.sh --pair gater_vs_evoq --profile hetzner-cx32
./scripts/run_paired.sh --pair gater_vs_evoq --profile hetzner-cx32 --scenario baseline
```
