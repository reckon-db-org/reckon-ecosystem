# Publications

Draft posts and published posts built from reckon-ecosystem benchmark data.

## Structure

```
publications/
├── POST_DRAFT_<name>.md      # in-progress draft, TBD numbers
├── POST_<name>.md            # published, numbers filled in
├── results/<name>/           # raw JSON + PRECOMMIT.md per published post
└── archive/                  # retired posts (kept for history)
```

## Rules

1. **Drafts stay drafts until every `TBD` has real data.** Fake numbers — even placeholder "~100 ops/s" — are not allowed in drafts.
2. **Every published post requires a `PRECOMMIT.md`** in its results directory: the "what would we publish if we lost" version, written and reviewed before the comparative run happens.
3. **Every published post must include raw JSONs** the reader can diff, recompute from, and challenge.
4. **Every comparative number requires a second reviewer** to have independently reproduced it.

## Active drafts

| Draft | Topic | Gate |
|---|---|---|
| [POST_DRAFT_reckon_vs_eventstoredb.md](POST_DRAFT_reckon_vs_eventstoredb.md) | Reckon vs EventStoreDB on hetzner-cx32 | Needs tuned CX32 numbers; noise floor measured |

## Published

_None yet._
