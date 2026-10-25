#!/usr/bin/env bash
# Sample bash script for lexer benchmarking
set -euo pipefail

readonly MAX_ITEMS=951
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

step_9914() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6746() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1878() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4008() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6816() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3606() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2919() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7268() {
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

readonly MAX_ITEMS=646
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

step_6292() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1644() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3083() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7883() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7367() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2367() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1244() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5620() {
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

readonly MAX_ITEMS=605
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

step_6020() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5638() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_398() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3879() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4440() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6363() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6593() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3808() {
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

readonly MAX_ITEMS=23
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

step_7602() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8510() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5144() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3373() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8738() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_292() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9804() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2980() {
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

readonly MAX_ITEMS=136
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

step_3910() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9588() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4427() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4366() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6242() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6107() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2744() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3252() {
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

readonly MAX_ITEMS=76
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

step_9837() {
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

step_3759() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9436() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4839() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7409() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8512() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7727() {
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

readonly MAX_ITEMS=170
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

step_7864() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4070() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8526() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4844() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2404() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4097() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7054() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9064() {
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

readonly MAX_ITEMS=607
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

step_9173() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7748() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5879() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7088() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1559() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9741() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6128() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_635() {
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

readonly MAX_ITEMS=678
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

step_3383() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1985() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5641() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5587() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8424() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1791() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9312() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8210() {
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

readonly MAX_ITEMS=631
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

step_9059() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8500() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_3990() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2172() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8324() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1256() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8227() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4622() {
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

readonly MAX_ITEMS=846
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

step_1186() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6016() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4099() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5362() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5341() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1103() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5742() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5727() {
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

readonly MAX_ITEMS=828
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

step_5186() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7723() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9925() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1396() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_7110() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9302() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2070() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_147() {
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

readonly MAX_ITEMS=646
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

step_2952() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6303() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_4185() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5998() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_6902() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_1413() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5379() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9826() {
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

readonly MAX_ITEMS=350
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

step_8291() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9393() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9352() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_8376() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9886() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_9313() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_5205() {
  local input="$1" output="$2"
  if [[ ! -r "$input" ]]; then
    log error "cannot read: $input"
    return 1
  fi
  awk -v prefix="$PREFIX" '{ print prefix, $0 }' "$input" > "$output"
}

step_2135() {
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

