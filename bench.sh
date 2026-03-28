#!/usr/bin/env bash
set -euo pipefail

N_RUNS="${1:-10}"
PIPE="./zig-out/bin/pipe"
FIB="samples/fibonacci.pipe"
FIB_PY="samples/fibonacci.py"

echo "==> Building pipe..."
zig build

echo ""
echo "==> Verifying outputs match..."
OUT_VM=$(${PIPE} "${FIB}")
OUT_INTERP=$(${PIPE} --interp "${FIB}")
OUT_PY=$(python3 "${FIB_PY}")

if [[ "${OUT_VM}" != "${OUT_INTERP}" || "${OUT_VM}" != "${OUT_PY}" ]]; then
  echo "ERROR: outputs differ!"
  echo "  VM:     ${OUT_VM}"
  echo "  interp: ${OUT_INTERP}"
  echo "  python: ${OUT_PY}"
  exit 1
fi
echo "All three produce: ${OUT_VM}"

echo ""
if command -v hyperfine &>/dev/null; then
  echo "==> Benchmarking with hyperfine (${N_RUNS} runs each)..."
  hyperfine \
    --runs "${N_RUNS}" \
    --warmup 2 \
    --export-markdown bench-results.md \
    "${PIPE} ${FIB}" \
    "${PIPE} --interp ${FIB}" \
    "python3 ${FIB_PY}"
  echo ""
  echo "Results saved to bench-results.md"
else
  echo "==> hyperfine not found — falling back to manual timing (${N_RUNS} runs each)..."
  echo ""

  run_timed() {
    local label="$1"; shift
    local total=0
    for i in $(seq 1 "${N_RUNS}"); do
      local t
      t=$(TIMEFORMAT='%R'; { time "$@" > /dev/null; } 2>&1)
      total=$(awk "BEGIN { print ${total} + ${t} }")
    done
    local avg
    avg=$(awk "BEGIN { printf \"%.3f\", ${total} / ${N_RUNS} }")
    printf "  %-40s avg %ss over %s runs\n" "${label}" "${avg}" "${N_RUNS}"
  }

  run_timed "pipe VM   (${FIB})"       ${PIPE} "${FIB}"
  run_timed "pipe interp (${FIB})"     ${PIPE} --interp "${FIB}"
  run_timed "python3 (${FIB_PY})"      python3 "${FIB_PY}"
fi
