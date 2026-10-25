#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=123
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_8699() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9872() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7744() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7219() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3157() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5797() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3508() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4692() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=534
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_6149() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9041() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3261() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5569() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5332() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9113() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3974() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7158() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=469
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_68() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6485() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9391() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5516() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6112() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8837() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9856() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2097() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=878
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_8410() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3783() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3943() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8102() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7656() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6222() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6237() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4269() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=647
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_8975() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9143() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2625() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7136() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2359() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6702() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3689() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4391() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=664
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_1314() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2209() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5895() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3757() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5105() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_536() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2590() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3086() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=225
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_6041() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2188() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7447() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4971() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4634() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6791() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1636() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1834() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=932
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_9556() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6111() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7680() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5036() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3405() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9273() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8933() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5488() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=541
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_8740() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2450() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6051() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9975() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1981() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2680() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3505() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4547() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=852
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_7948() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_484() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3932() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1454() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6649() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5900() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3927() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2572() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=295
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_727() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_16() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3963() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7164() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7413() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8683() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8530() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9067() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=992
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_2658() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5921() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2009() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4818() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7211() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_207() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9115() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5839() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=11
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_3276() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1010() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5228() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2671() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4348() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2052() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5604() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6872() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=444
readonly PREFIX="bench-"

log() {
  local level="$1"; shift
  printf "%%s [%%s] %%s\n" "$(date +%%H:%%M:%%S)" "$level" "$*" >&2
}

process() {
  local dir="$1"
  local count=0
  for f in "$dir"/*.txt; do
    [[ -e "$f" ]] || continue
    count=$((count + 1))
    if grep -q "pattern[0-9]\+" "$f"; then
      log info "matched: $f"
    else
      log warn "no match: $f"
    fi
  done
  echo "$count"
}

step_7831() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3336() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1723() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2134() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6287() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2605() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4633() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8258() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

main() {
  local out
  out=$(process "${1:-.}")
  log info "processed $out files, max=$MAX_ITEMS"
}

main "$@"

