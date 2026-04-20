#!/usr/bin/env bash
#
# Run a paired comparison: two slices back-to-back on the same store,
# same VM, same hardware profile. Emits a joined delta report.
#
# Usage:
#   ./scripts/run_paired.sh --pair <pair-name> --profile <profile> [--scenario <name>]
#
# Each pair is a directory under `paired/`. It must contain a
# `manifest.eterm' file that names the two slices and the scenario:
#
#   #{
#     base    => {pair_storage_bare,      smoke},
#     compare => {pair_storage_via_gater, smoke}
#   }.

set -euo pipefail

PAIR=""
PROFILE="local-dev"
SCENARIO_OVERRIDE=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --pair)     PAIR="$2";              shift 2 ;;
        --profile)  PROFILE="$2";           shift 2 ;;
        --scenario) SCENARIO_OVERRIDE="$2"; shift 2 ;;
        *)
            printf 'unknown argument: %s\n' "$1" >&2
            exit 2 ;;
    esac
done

if [[ -z "$PAIR" ]]; then
    printf 'required: --pair <pair-name>\n' >&2
    exit 2
fi

BENCH_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$BENCH_DIR"

MANIFEST="paired/${PAIR}/manifest.eterm"
if [[ ! -f "$MANIFEST" ]]; then
    printf 'no manifest: %s\n' "$MANIFEST" >&2
    exit 3
fi

read -r BASE_SLICE BASE_SCEN CMP_SLICE CMP_SCEN < <(
    erl -noshell -eval "
      {ok, [M]} = file:consult(\"${MANIFEST}\"),
      {B, BS} = maps:get(base, M),
      {C, CS} = maps:get(compare, M),
      io:format(\"~s ~s ~s ~s~n\", [B, BS, C, CS]),
      halt()."
)

if [[ -n "$SCENARIO_OVERRIDE" ]]; then
    BASE_SCEN="$SCENARIO_OVERRIDE"
    CMP_SCEN="$SCENARIO_OVERRIDE"
fi

RUN_ID="$(date -u +%Y-%m-%dT%H:%M:%SZ)_$(git rev-parse --short HEAD 2>/dev/null || echo nogit)_${PROFILE}_${PAIR}"
RESULTS_DIR="results/${RUN_ID}"
mkdir -p "$RESULTS_DIR"

printf '== paired run: %s\n' "$PAIR"
printf '   base:    %s/%s\n' "$BASE_SLICE" "$BASE_SCEN"
printf '   compare: %s/%s\n' "$CMP_SLICE"  "$CMP_SCEN"
printf '   results: %s\n' "$RESULTS_DIR"

rebar3 as bench compile >/dev/null

# Each side runs in a fresh escript VM against a pristine data dir.
# The 3-second wait lets khepri/ra fully release file handles before
# we wipe and re-seed the directory for the next side.
./scripts/wipe_data.sh >/dev/null

printf '\n=== base ===\n'
./scripts/run_slice.escript \
    "$BASE_SLICE" \
    "slices/${BASE_SLICE}/scenarios/${BASE_SCEN}.eterm" \
    "${RESULTS_DIR}/base.json" \
    "$PROFILE"

sleep 3
./scripts/wipe_data.sh >/dev/null
sleep 1

printf '\n=== compare ===\n'
./scripts/run_slice.escript \
    "$CMP_SLICE" \
    "slices/${CMP_SLICE}/scenarios/${CMP_SCEN}.eterm" \
    "${RESULTS_DIR}/compare.json" \
    "$PROFILE"

printf '\n=== join ===\n'
# Include all ebin dirs so paired_join is loadable.
PATHS=""
for d in _build/bench/lib/*/ebin _build/bench/checkouts/*/ebin _build/default/lib/*/ebin; do
    [[ -d "$d" ]] && PATHS="$PATHS $d"
done
erl -noshell -pa $PATHS \
    -eval "paired_join:join(\"${RESULTS_DIR}/base.json\", \"${RESULTS_DIR}/compare.json\", \"${RESULTS_DIR}/paired.json\"), halt()."
