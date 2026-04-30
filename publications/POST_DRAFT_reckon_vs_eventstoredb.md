# DRAFT — Reckon vs EventStoreDB: an honest benchmark on €4/month hardware

**Status: DRAFT. Not for publication until the checklist at the end is green.**

---

## TL;DR

(Fill in once numbers exist. One sentence, one claim, one metric. Example shape:)

> On a €4.89/month Hetzner CX32, Reckon sustains **X appends/sec** at **Y ms p99**, at **Z µs CPU per event** — on the same hardware EventStoreDB sustains **A / B / C**. Both systems are within the measurement noise floor on raw throughput; Reckon's differentiator is **zero external infrastructure** and **native BEAM clustering**.

If that framing doesn't survive the actual numbers, rewrite the TL;DR. **Do not massage numbers to fit a pre-decided message.**

---

## What we measured

### Workload shape

- Stream: single, append-only
- Writer: one process appending serially
- Event size: 256 bytes (small audit-trail event)
- Expected-version check: `ANY_VERSION` (no version enforcement on append)

### Metrics

- **Throughput**: appends per second, sustained over 60 seconds after warmup
- **Latency distribution**: p50, p90, p95, p99, p99.9, p99.99
- **Per-operation cost**: CPU-ms, memory-MB-s, disk bytes per append — the cost metric nobody else publishes honestly

### Hardware

**Single profile for this post: Hetzner Cloud CX32** — 4 vCPU AMD EPYC dedicated, 8 GB RAM, 80 GB NVMe SSD, Ubuntu 24.04 LTS, kernel 6.8+, OTP 27.

Why this profile: matches the dominant European cloud price/performance point for compliance-conscious buyers under NIS2 / DORA — the audience that is actually evaluating event stores for regulated workloads.

Full hardware spec + tuning script committed at:
[`reckon-bench-harness/hardware_profiles/hetzner-cx32/`](https://github.com/reckon-db-org/reckon-bench-harness/tree/main/hardware_profiles/hetzner-cx32)

### Methodology

- Warmup until per-second throughput coefficient of variation < 5% over 5 buckets
- Measurement runs for min(60s, 100k operations), whichever is longer
- Percentiles computed from full sample list, not histograms
- 20-run noise floor measured and documented per profile
- Comparative numbers only published after a pre-commitment to publish the "we lost" version honestly

Full methodology: [`reckon-bench-harness/METHODOLOGY.md`](https://github.com/reckon-db-org/reckon-bench-harness/blob/main/METHODOLOGY.md).

---

## The numbers

### Reckon (direct storage)

| Metric | Value |
|---|---|
| Throughput | TBD ops/s |
| p50 | TBD ms |
| p99 | TBD ms |
| p99.9 | TBD ms |
| CPU-ms / op | TBD |
| Disk bytes / op | TBD |

### EventStoreDB

| Metric | Value | Config |
|---|---|---|
| Throughput | TBD ops/s | ESDB version: TBD |
| p50 | TBD ms | cluster size: TBD |
| p99 | TBD ms | write strategy: TBD |
| p99.9 | TBD ms | |

### Delta

| Metric | Reckon vs ESDB | Notes |
|---|---|---|
| Throughput | TBD % | |
| p99 | TBD % | |
| CPU cost / op | TBD % | |

---

## Interpretation

### What the numbers say

(Write after the numbers exist. Straight reading of the data. No spin.)

### What the numbers do NOT say

- Not a claim about *all* workload shapes. Single-stream, small events, serial writer. Fan-out scenarios (`append_many_streams`) and large-payload scenarios (`sweep_event_size`) have their own numbers, in future posts.
- Not a claim about operational simplicity "in general" — only about "fewer moving parts in your deployment diagram." The section below quantifies what "fewer moving parts" means concretely.

### What this actually matters for

Compliance-driven event sourcing deployments where:
- The event store is a legal record (NIS2 / DORA audit trail)
- The hosting must be EU-based and self-hostable
- The operational cost per event is under budget scrutiny
- The team is BEAM-fluent OR willing to treat the event store as a black-box gRPC service

Reckon loses on mature ecosystem, tooling, language-client breadth. Reckon wins on zero-external-infra, BEAM-native clustering, and operational-cost-per-event on small hardware.

---

## The "not fastest" story

If Reckon does NOT win on raw throughput, the post's framing becomes:

> "Reckon is within **X%** of EventStoreDB's throughput, at **Y%** of the operational complexity and **Z%** of the per-event CPU cost — on hardware that costs EUR 4.89/month."

The differentiator is not "fastest." The differentiator is:

1. **No external infrastructure.** ReckonDB runs inside your BEAM release. EventStoreDB is a separate cluster to provision, monitor, upgrade, back up.
2. **Native BEAM clustering.** `libcluster` + Khepri/Ra. No out-of-band service discovery.
3. **Operational cost per event.** On a €4.89/month CX32, Reckon's CPU-ms-per-event figure is **Z µs**, which dominates cost-efficiency conversations even when throughput is lower.

Only publish the "not fastest" post if numbers 1-3 are concretely better than EventStoreDB on the same hardware. If they are not, do not publish. Optimise first.

---

## Reproduce it yourself

Every number in this post comes from:

```bash
# Single layer — direct storage:
cd reckon-db/benchmarks
./scripts/bench.sh \
    --slice append_single_stream \
    --scenario baseline \
    --profile hetzner-cx32

# Paired — Reckon bare vs through gater:
cd reckon-ecosystem/benchmarks
./scripts/run_paired.sh \
    --pair reckon_db_vs_gater \
    --profile hetzner-cx32
```

Full run IDs, raw JSON outputs, and hardware verification logs for every number in this post are committed at:
[`reckon-ecosystem/publications/results/reckon_vs_eventstoredb/`](#) (LINK TBD — create directory when post ships).

**If you cannot reproduce, the numbers are wrong.** File an issue with your `run.json`.

---

## Pre-publication checklist

- [ ] All placeholder `TBD` replaced with measured values
- [ ] Numbers collected on a tuned `hetzner-cx32` with `verify.sh` passing
- [ ] Noise floor for `hetzner-cx32` measured and filed in `hardware_profiles/hetzner-cx32/noise_floor.md`
- [ ] All reported deltas exceed the noise floor by ≥ 2x
- [ ] EventStoreDB configured by someone who knows the product (or at least: follows the official perf guide, not the default docker-compose image)
- [ ] "We lost" version of the TL;DR drafted and reviewed
- [ ] Second reviewer has independently reproduced the Reckon numbers
- [ ] Raw result JSONs committed under `publications/results/<post-name>/`
- [ ] `PRECOMMIT.md` in that results directory: **what we would have published if the numbers had gone the other way**
- [ ] Nothing in the post is a claim that cannot be defended under adversarial review from an EventStoreDB engineer

If any box above is unchecked, the post does not ship.
