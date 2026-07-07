# DCB &amp; CCC — Multi-stream Consistency

## The problem single-stream concurrency can't solve

Classic event sourcing guards consistency with **optimistic concurrency on one stream**: you append with an `expected_version`, and the store rejects the write if the stream moved under you. That is enough for invariants scoped to a single aggregate instance.

It is **not** enough for invariants that span many streams:

- **Uniqueness** — "no two users may register the same email" (each user is its own stream).
- **Allocation** — "at most N seats sold for this event" (each booking is its own stream).
- **Rate limits** — "at most K actions per principal per window."

There is no single stream whose version you can lock to enforce these. You need to condition the write on facts drawn from *across* streams. That is what DCB and CCC provide.

## DCB — Dynamic Consistency Boundary

A **Dynamic Consistency Boundary** is a **conditional append on a tag-filter context query**. Instead of "append if this stream is still at version V," a DCB append says "append if **no events match this filter**." The filter is a boolean expression over:

- **tag leaves** — events carrying a given cross-stream tag, and
- **`event_type` leaves** — events of a given type,

combined with full boolean algebra. The boundary is *dynamic*: it is defined by the query, not by a fixed stream. If a concurrent write introduces a matching event between your read and your append, the append is rejected — exactly the guarantee uniqueness/allocation need.

**Where it lives:**
- **reckon_db** implements the conditional append and the tag-filter evaluation (`reckon-db` guides `dcb.md`, `dcb_raft_design.md`).
- **reckon_gater** carries the `tag_filter` type shared across the stack.
- **evoq** exposes it to domain code via the **`evoq_decision`** behaviour — a cross-cutting consistency boundary that locks on the *absence* of events matching a tag-filter rather than a single stream's version (`evoq` guide `decisions.md`).
- **reckon_gateway** exposes it to polyglot clients via **`DcbService`** (see [reckon_proto](reckon-proto.md)); the Go client reaches it through `c.Dcb(store)`.

## CCC — Command Context Consistency

Tags are structural — you attach them at write time. Sometimes the boundary you need is defined by **the event's payload itself**: "no event whose `email` field equals this value," "no booking whose `seat` matches." **Command Context Consistency** extends the boundary from tags to **payload-indexed conditions**.

reckon_db supports this with two moving parts:

1. **Index declarations** — a store declares a payload index so the field is queryable:
   - `{payload, Key}` — index a single payload key.
   - `{payload_hash, [Keys]}` — index a hash over a set of keys (composite conditions).
2. **Condition filters** used in the conditional append / read:
   - `{payload_match, ...}` — match on an indexed payload key.
   - `{payload_hash_match, ...}` — match on an indexed composite hash.

evoq lets a Decision scope its boundary on these payload fields (`payload_match` / `payload_hash_match`) rather than only on tags. Crucially, it **fails loud when the index is undeclared** — you cannot silently get a boundary that isn't actually enforced by an index. (`evoq` guide `decisions.md#ccc-payload-conditions`.)

For polyglot clients, `DcbService` exposes payload reads directly: **`CccReadByPayload`** and **`CccReadByPayloadHash`** (added in [reckon_proto](reckon-proto.md) 0.8.0).

## DCB vs CCC at a glance

| | DCB | CCC |
|---|---|---|
| Boundary defined by | tags + `event_type`, boolean algebra | payload fields (single key or composite hash) |
| Requires | nothing beyond tagging events | a declared payload index on the store |
| evoq surface | `evoq_decision` (tag-filter) | `evoq_decision` with `payload_match` / `payload_hash_match` |
| Gateway surface | `DcbService` conditional append | `DcbService.CccReadByPayload` / `CccReadByPayloadHash` |
| Failure mode | append rejected if a matching event exists | as DCB, plus **loud failure** if the index is undeclared |

## When to reach for which

- Start with a **single-stream** `expected_version` whenever the invariant is local to one aggregate. It's the cheapest guard.
- Use **DCB** for cross-stream invariants you can express with tags/event-types (uniqueness by a tagged value, allocation caps counted by a tagged event).
- Use **CCC** when the value you must guard on lives in the event payload and isn't practical to hoist into a tag — declare the payload index, then condition on it.

## See also

- [reckon_db](reckon-db.md) — storage-side DCB/CCC (deep guides `dcb.md`, `ccc.md` in the reckon-db repo)
- [evoq](evoq.md) — the `evoq_decision` behaviour (`decisions.md` in the evoq repo)
- [reckon_proto](reckon-proto.md) / [reckon_gateway](reckon-gateway.md) — `DcbService` on the wire
- [Polyglot Clients](polyglot-clients.md) — using DCB/CCC from Go/.NET/Python
