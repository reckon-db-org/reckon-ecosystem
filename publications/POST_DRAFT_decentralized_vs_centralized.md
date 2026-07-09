# DRAFT — Throughput is the wrong question: what a decentralized event store actually optimizes

**Status: DRAFT. Ships once the "live figures" slots are filled from a deployed
run of the parksim demo (see the gate at the end).**

Companion to [POST_DRAFT_reckon_vs_eventstoredb.md](POST_DRAFT_reckon_vs_eventstoredb.md).
That post answers "is Reckon fast enough?" on one box. This post answers a
prior question: for a *decentralized* event store, is throughput even the metric
you should be grading on? Both posts can be true at once. This one carries no
benchmark numbers, only structural claims plus a live existence proof.

---

## TL;DR

A centralized event store benchmarks throughput because the central cluster
*is* the scarce resource: everything funnels through it, and the only way to
scale is to make the middle bigger. events/sec defines the product because the
middle is the bottleneck.

A decentralized store has no middle, and therefore no central throughput
ceiling. Each node carries only its own local load (a parking facility does a
few events/sec, the entire workload of that node). Throughput stops being the
scarce resource. What becomes scarce, and therefore what must be engineered and
measured, is a different set of non-functional requirements: autonomy,
sovereignty, convergence, trust without an operator, self-healing, and
footprint.

Grading a decentralized store by central throughput is a category error, like
rating a sailboat by engine horsepower. This post makes the alternative
scorecard concrete, and proves it on four mini-PCs running a live robotaxi
fleet.

---

## Two topologies, two scarce resources

| | Centralized store | Decentralized store (Reckon + Macula) |
|---|---|---|
| Where events live | all at the center | at their origin; only *facts* travel |
| Scaling move | grow the central cluster | add autonomous nodes (federate) |
| The bottleneck | the central log | there isn't one; each node is independent |
| Headline metric | throughput (events/sec) | autonomy · sovereignty · convergence |
| Partition | producers block, store unavailable | each node keeps serving locally |
| Right-to-erasure | chase copies through the pipeline | erase locally, it is gone |
| Trust | trust the operator | tamper-evident chain, no central authority |
| Failure domain | central cluster is a SPOF | any node dies, the store self-heals |
| Footprint | a datacenter tier | a €150 mini-PC at the edge |
| Governance | one owner, one jurisdiction | plural, sovereign, commons |

The last three rows are why the throughput number, taken alone, misleads. A
store optimized for the right column *spends* throughput to buy those
properties, on purpose.

## Why centralized stores benchmark throughput

It is the honest thing for them to measure. If your architecture routes every
event to one cluster, then that cluster's sustained append rate is the ceiling
on the whole system, and per-event cost on that cluster is the dominant line in
the bill. EventStoreDB, Kafka, and friends publish appends/sec because for their
topology it is the number that governs everything downstream. Our companion
benchmark post plays that game on their field, honestly, and reports the loss if
we lose.

## Why a decentralized store has no throughput ceiling

Aggregate throughput across a federation scales *past* any central cluster: a
million edge nodes at ten events/sec each is ten million events/sec with no
central bottleneck. But that is a million independent partial-order streams, not
one globally ordered log. So the honest framing is not "we are faster." It is:
throughput is no longer the constraint, so we stopped optimizing for it and
started optimizing for the constraints that a federation actually hits.

Reckon is built for that profile already: consensus per store (Raft, for safety
not speed), offline operation, a hard boundary between internal domain events
and published integration facts, tamper-evidence, and continuous self-healing
(`reckon_db_store_healer`, since 5.8; native rejoin with no roll-time split,
since 5.11). Every one of those is a choice that trades some throughput away to
buy a decentralized NFR.

## The honest trade-offs (this post must say this)

The claim is not "decentralized is better." It is that the two topologies
optimize different things and the problem dictates the choice.

- **Per-append latency is worse.** A Raft quorum round-trip per write is slower
  than appending to a single local log. Consensus safety and partition
  tolerance are bought with write latency.
- **Operational complexity is real.** N autonomous clusters plus convergence is
  harder to reason about than one central thing. We have the scars: a boot-race
  re-formation bug that split clusters at deploy time, fixed in reckon_db 5.11.
- **No global total order.** If you need one ordered firehose into a central
  analytics sink, that is a centralized store's job. Buy the right tool.

Right tool for: edge, federated, multi-owner, sovereign, regulated-data
workloads. Wrong tool for: central-firehose-to-analytics, global total order,
maximum single-stream throughput.

---

## The existence proof: parksim on four mini-PCs

We run a live robotaxi + parking simulation ("parksim") as four sovereign
tenants (Leuven, Brussels, Ghent, Antwerp) on four Intel Celeron J4105 mini-PCs
(beam00–03). Each tenant is an independent three-node Raft cluster; a gateway
federates a *view* but sits in no tenant's data path. It is the decentralized
profile in miniature, and it is the thing the two claims below are measured on.

### 1. The scorecard replaces the speedometer

The admin dashboard used to lead with **Fleet Event Ingest (events/sec)**, the
centralized yardstick, on which parksim looks weak by design. It now leads with
a **Decentralization Scorecard** built from the same live snapshot:

- **Sovereign stores** — count of independent Raft clusters (no shared log).
- **Fault tolerance** — quorate store count and the fleet's weakest `can_lose`
  ("survives losing any N nodes per store").
- **Edge footprint** — total event data on disk across nodes, and mean node CPU:
  proof it runs on mini-PCs, not a datacenter.
- **Facts-only egress** — domain events stay local; only integration facts leave
  the node.

Each tile carries the "vs central" contrast. This is the reframe made literal:
the metric a central store is *structurally incapable* of scoring well on is the
one we put first.

### 2. Charging as a dense, federated, decentralized process

"A few events/sec" is not a weakness, it is the workload. But we also modeled a
genuinely dense process to show the difference between "sparse because
milestone-based" and "sparse because centralized-and-throttled." EV charging is
the highest-frequency, most economically central process in a real fleet, so we
made it a first-class vertical slice: `charge_requested → charging_started →
charging_progressed* (per-SoC milestone) → charging_completed → energy_settled`.

The federation twist is the point. A `grid_price_changed` integration fact
propagates across the mesh (published by each region, no central energy
controller). A process manager on every edge reacts to the local price and
decides charge-now vs defer. The aggregate charging behavior of the whole fleet
*emerges* from fact propagation: when the grid is expensive, edges defer
non-critical charging and the off-peak share of energy climbs, with nothing
scheduling them centrally. The realm dashboard shows the federated result
(kWh, cost, off-peak share, live grid price) assembled purely from each
operator's `energy/<region>/summary` fact.

### 3. The runbook: three things a central store cannot do

Scripted, repeatable, watched live on the dashboard (full runbook:
`hecate-parksim/plans/RUNBOOK_DECENTRALIZED_NFR_DEMO.md`):

- **Partition autonomy.** Cut one site off. The two-site majority keeps
  committing; the isolated site refuses unsafe writes; on reconnect it rejoins
  natively and catches up. A central store's disconnected producer is simply
  dead.
- **Sovereignty and erasure.** Rider PII stays in the edge store; only aggregate
  facts cross the mesh. Erase a rider locally and there is no central copy to
  chase (ties to our NLnet right-to-erasure work).
- **Self-heal.** Kill a node; the store returns to full strength unattended
  (5.11 native rejoin), no operator runbook.

### Live figures (from the deployed fleet, 2026-07-09)

| Claim | Figure | Source |
|---|---|---|
| Events per charging session (density) | **~8** (4 SoC milestones + request/start/complete/settle), vs **1** `battery_charged` before | `charging-*` streams: 48 `charging_progressed` / 12 `charging_completed` = 4.0 milestones/session |
| Sovereign stores · weakest `can_lose` | **4 independent Raft clusters · can_lose 1** (all quorate) | `/v1/stores/parksim_<t>_store/cluster` |
| Price signal reaches each region | **yes** — region-specific tariffs stamped on sessions (leuven 36, brussels/ghent 41, antwerp 40 c/kWh) | `energy_settled.tariff_cents_per_kwh` |
| Tamper-evidence | every event hash-chained (`prev_event_hash`) | raw event |
| Off-peak share of energy, price-aware ON | **needs a full sim-day** — first 48 sessions all fell in one (peak) clock band, so 0% is not yet representative | `energy_settled.off_peak` |
| Off-peak share, price signal ignored (control) | TBD (control run, defer disabled) | same |
| Partition autonomy: writes committed while cut off | TBD | Scenario 1 |

No fake numbers. The density, sovereignty, price-propagation and tamper-evidence
figures are measured on the live fleet. The off-peak *share* — the headline of
the emergent-coordination claim — is still collecting: it only becomes
meaningful once the simulated day rotates through off-peak windows, and the ON
vs OFF control comparison is a deliberate second run. The qualitative and
structural claims above hold today.

---

## When the centralized store wins

If you need one globally ordered stream, or maximum single-stream throughput
into a central sink, or you have no tolerance for higher per-append latency, a
centralized store is the right tool and this whole post does not apply to you.
Read the companion benchmark post to see whether Reckon is nonetheless *fast
enough* on your hardware.

## Relationship to the throughput benchmark

Both posts are honest and both are true. The benchmark post says: on one box,
here is Reckon's appends/sec and per-event cost versus EventStoreDB, loss
reported if we lose. This post says: for the federated, sovereign,
regulated-data workloads Reckon is actually for, that number is not the one that
decides the architecture. If a reader only ever needs the central-firehose
shape, the benchmark post is the relevant one. If they are building for the
edge, this one is.

## Reproduce it yourself

The demo is scripted end to end:

```
# Dashboard: http://host00.lab:8080/admin/  (Decentralization Scorecard)
#            macula.io/clankercab            (federated City energy strip)

# Partition autonomy (Scenario 1):
cd macula-demo/infrastructure
scripts/demo-partition.sh beam03.lab cut      # isolate a site
scripts/demo-partition.sh beam03.lab restore  # native rejoin
```

Full scenarios, exact commands, and the "vs central" contrast per step:
`hecate-parksim/plans/RUNBOOK_DECENTRALIZED_NFR_DEMO.md`.

---

## Pre-publication gate

- [ ] parksim charging process deployed to the live fleet (Phase 2)
- [ ] Every `TBD` in the live-figures table replaced with a measured value
- [ ] Off-peak share measured twice: price-aware scheduling ON vs OFF (the
      control run) — the delta is the federation claim; if there is no delta,
      cut the "emergent coordination" claim rather than keep it
- [ ] One dashboard screenshot each: the Scorecard and the City energy strip
- [ ] Every structural claim ("no central data path", "facts-only egress")
      re-verified against the shipped code, not this draft's memory of it
- [ ] Nothing here is a claim that cannot be defended against an EventStoreDB
      engineer who has read the benchmark post

If any box is unchecked, the post does not ship.
