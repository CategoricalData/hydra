#!/usr/bin/env bash
#
# Pure-bash per-target freshness check + digest refresh — the host-free
# promotion of `digest-check` (#416). Replaces the subcommands that every
# assemble / test freshness gate dragged a Haskell toolchain to run:
#
#   digest.sh fresh         --inputs <file> --output-dir <dir> --output-digest <file>
#                           [--keep-paths-from <manifest>]
#   digest.sh refresh       --inputs <file> --output-dir <dir> --output-digest <file>
#   digest.sh refresh-input --package <pkg> --dist-json-root <dir>
#
# `refresh-input` rewrites a package's INPUT digest from current on-disk state:
# it scans the package's source files, extracts each declared namespace via a
# CLOSED host-neutral idiom set (5 regexes — 2 Haskell / 1 Java / 2 Python),
# routes each to its owning package via the on-disk per-package manifest.json
# (plain membership; see 416-namespace-extraction-classification.md), hashes the
# owned sources + the package's JSON content (jsonContent:* entries), computes
# the selfHash, and preserves the existing dependencyHashes. The namespace
# extraction is (a) agnostic-via-data: the extension→regex idiom set and the
# manifest membership are DATA, not branch-on-language logic (#416 neutrality
# ruling). Hydra.Digest.discoverModuleNameFiles is the TEST-TIME oracle.
#
# CONTRACT (see 416-digest-check-classification.md): the on-disk digest.json is
# NOT byte-reproduced. The Haskell writer uses a width-adaptive AST
# pretty-printer; both digest paths are gitignored build caches, never
# byte-diffed across commits. The contract is SEMANTIC: this writer emits JSON
# that the Haskell `fresh` reader (and bin/lib/batch-cache.sh, already a stdlib
# json.load reader) parse to the same InputDigest / OutputDigest value, and
# each side reads the other's file. The conformance test (bin/test-digest-conformance.sh)
# asserts semantic equivalence + identical fresh/refresh verdicts against the
# `digest-check` oracle, NOT `diff` of bytes.
#
# The typed digest.json shapes (hydra.build.format, #512):
#   InputDigest  { digestFormatVersion, moduleFormatVersion, selfHash?,
#                  dependencyHashes:[{key,value}], moduleHashes:[{key,value}] }
#   OutputDigest { digestFormatVersion, moduleFormatVersion, generator,
#                  generation:{generatorId,mode,host,hydraVersion?,revision?,timestamp?},
#                  selfHash?, dependencyHashes:[{key,value}],
#                  inputs:[{key,value:{kind,hash}}], outputs:[{key,value:{kind,hash}}] }
# Hydra maps serialize as arrays of {"key","value"} objects; jq reads/writes them as such.

set -euo pipefail

DIGEST_FORMAT_VERSION=2
MODULE_FORMAT_VERSION=1

err() { echo "digest.sh: $*" >&2; }

usage() {
  cat >&2 <<'EOF'
Usage:
  digest.sh fresh   --inputs <file> --output-dir <dir> --output-digest <file> [--keep-paths-from <manifest>]
  digest.sh refresh --inputs <file> --output-dir <dir> --output-digest <file>

  fresh:   exit 0 if cache hit (skip work), exit 1 if miss (do work). On an
           otherwise-hit with extra (orphan) files present, deletes them,
           refreshes the digest, and reports a hit (#393 reconcile).
  refresh: walk <output-dir>, hash every file, write <output-digest> with
           paths relative to <output-dir>, plus the generation record.
  refresh-input: rewrite <dir>/<pkg>/build/main/digest.json from the current
           source files (declared-namespace scan) + JSON content (#469).
EOF
}

# ---------------------------------------------------------------------------
# Hashing. sha256sum of raw file bytes == Data.Digest.Pure.SHA.sha256 (verified
# byte-for-byte against recorded digests). Emits the bare 64-char lowercase hex.
hash_file() {
  # $1 = path. Fail loudly if unreadable (callers treat absence as a miss, but a
  # present-yet-unreadable file is a real error).
  sha256sum "$1" | cut -d' ' -f1
}

# Recursively list every regular file under a dir, skipping dot-entries — the
# same walk as Hydra.Digest.listFilesRecursive (dotfiles and dot-dirs pruned).
# Emits absolute-or-as-given paths, one per line, sorted for determinism.
list_files_recursive() {
  local root="$1"
  [ -d "$root" ] || return 0
  # -not -path '*/.*' prunes any component beginning with '.', matching the
  # Haskell walk which returns [] for any entry with a leading '.'.
  find "$root" -type f -not -path '*/.*' | LC_ALL=C sort
}

# Normalise a path the way System.FilePath.normalise does for our purposes:
# collapse '//' -> '/' and drop a single trailing '/'. (We never construct '..'
# or './' segments here, so the full normalise algorithm isn't needed.)
normalise() {
  local p="$1"
  # collapse repeated slashes
  while [[ "$p" == *//* ]]; do p="${p//\/\//\/}"; done
  # strip a trailing slash (but keep a bare "/")
  [[ "$p" != "/" && "$p" == */ ]] && p="${p%/}"
  printf '%s' "$p"
}

# path relative to base, mirroring Hydra.Digest.makeRelativeTo: if base is a
# prefix (base + '/'), strip it; otherwise return path unchanged.
make_relative_to() {
  local base="$1" path="$2" prefix
  if [[ -n "$base" && "${base: -1}" == "/" ]]; then prefix="$base"; else prefix="$base/"; fi
  if [[ "$path" == "$prefix"* ]]; then
    printf '%s' "${path#"$prefix"}"
  else
    printf '%s' "$path"
  fi
}

# ---------------------------------------------------------------------------
# JSON reads (semantic, via jq). A missing/unreadable/legacy-format digest
# yields the empty value — callers treat any read failure as a cache miss, the
# same tolerance as the Haskell typed reader (any Left -> miss, never an error).

# Read a {key,value} array field into TSV "key<TAB>value" lines.
# $1 = file, $2 = jq field name (e.g. moduleHashes, dependencyHashes).
read_kv_map() {
  local file="$1" field="$2"
  [ -f "$file" ] || return 0
  jq -r --arg f "$field" '
    (.[$f] // []) | .[] | [.key, (.value|tostring)] | @tsv
  ' "$file" 2>/dev/null || return 0
}

# Read the {key, value:{kind,hash}} entries of an output digest into
# "key<TAB>hash" lines. $1 = file, $2 = "inputs"|"outputs".
read_entry_hashes() {
  local file="$1" field="$2"
  [ -f "$file" ] || return 0
  # `| tostring` coerces the hash to a string so a null/absent `value.hash` on a
  # corrupt or legacy entry can't error @tsv (which rejects a non-scalar row and
  # would truncate the stream). A missing hash then compares unequal → cache miss,
  # the safe direction.
  jq -r --arg f "$field" '
    (.[$f] // []) | .[] | [.key, ((.value.hash // .value) | tostring)] | @tsv
  ' "$file" 2>/dev/null || return 0
}

# Read a scalar top-level field, defaulting to "" (or a supplied default).
read_scalar() {
  local file="$1" field="$2" dflt="${3:-}"
  [ -f "$file" ] || { printf '%s' "$dflt"; return 0; }
  jq -r --arg f "$field" --arg d "$dflt" '.[$f] // $d' "$file" 2>/dev/null || printf '%s' "$dflt"
}

# ---------------------------------------------------------------------------
# The generator stamp + generation record, sourced entirely from the
# HYDRA_GENERATOR_STAMP / HYDRA_GENERATION_* env vars the shell assembler already
# exports (export_generation_env in assemble-common.sh). No host logic — this is
# pure serialization of values bash already computed.
generator_stamp() {
  local s="${HYDRA_GENERATOR_STAMP:-}"
  if [ -n "$s" ]; then printf '%s' "$s"; else printf 'v0-unstamped'; fi
}

# Emit the generation:{...} object as JSON. Mirrors Hydra.Digest.generationRecord:
# mode defaults to published; host/version/revision/timestamp from env; the
# shim => revision-present invariant is enforced (fail loudly, matching the
# Haskell `error`).
generation_json() {
  local gid mode host ver rev ts
  gid="$(generator_stamp)"
  mode="${HYDRA_GENERATION_MODE:-published}"
  [ "$mode" = "shim" ] || mode="published"
  host="${HYDRA_GENERATION_HOST:-}"
  ver="${HYDRA_GENERATION_HYDRA_VERSION:-}"
  rev="${HYDRA_GENERATION_REVISION:-}"
  ts="${HYDRA_GENERATION_TIMESTAMP:-}"
  if [ "$mode" = "shim" ] && [ -z "$rev" ]; then
    err "generation: mode=shim requires HYDRA_GENERATION_REVISION (invariant: shim => revision present)"
    exit 1
  fi
  # Optional fields (hydraVersion/revision/timestamp) are omitted when empty,
  # matching the Maybe-encoding of the Haskell writer.
  jq -n \
    --arg gid "$gid" --arg mode "$mode" --arg host "$host" \
    --arg ver "$ver" --arg rev "$rev" --arg ts "$ts" '
    {generatorId:$gid, mode:$mode, host:$host}
    + (if $ver != "" then {hydraVersion:$ver} else {} end)
    + (if $rev != "" then {revision:$rev} else {} end)
    + (if $ts  != "" then {timestamp:$ts}  else {} end)
  '
}

# ---------------------------------------------------------------------------
# refresh: hash every file under output-dir (excluding the output digest itself),
# copy the input digest's module hashes into `inputs` (kind:other) and its
# selfHash/deps into the recorded-* slots, and write the OutputDigest.
do_refresh() {
  local input_digest="$1" output_dir="$2" output_digest="$3"

  local gen generation self
  gen="$(generator_stamp)"
  generation="$(generation_json)"

  # LARGE arrays (deps/inputs/outputs) are written to TEMP FILES and read back
  # with jq --slurpfile, NEVER passed via --argjson. A big package (e.g.
  # dist/java/hydra-kernel = 800+ output files) produces a JSON array far larger
  # than ARG_MAX; passing it on the jq command line fails with "Argument list too
  # long" (#416 CI regression, run 33978457591). --slurpfile reads from a file, so
  # it has no argv-size limit. Only the tiny scalars stay as --arg/--argjson.
  local tmpd; tmpd="$(mktemp -d)"
  # shellcheck disable=SC2064
  trap "rm -rf '$tmpd'" RETURN

  # Input side (tolerant: absent/legacy -> empty). selfHash + deps carry into
  # the recorded-* slots for #347 transitive invalidation; moduleHashes become
  # the `inputs` map with kind:other (matching doRefresh's DigestEntry KindOther).
  self="$(read_scalar "$input_digest" selfHash "")"
  read_kv_map "$input_digest" dependencyHashes | jq -R -s '
    split("\n") | map(select(length>0) | split("\t") | {key:.[0], value:.[1]})
  ' > "$tmpd/deps.json"
  read_kv_map "$input_digest" moduleHashes | jq -R -s '
    split("\n") | map(select(length>0) | split("\t")
      | {key:.[0], value:{kind:"other", hash:.[1]}})
  ' > "$tmpd/inputs.json"

  # Output side: walk output-dir, hash each file, store relative paths, EXCLUDE
  # the output digest file itself (normalised compare, matching doRefresh).
  local digest_norm; digest_norm="$(normalise "$output_digest")"
  local f rel h fn
  : > "$tmpd/outputs.tsv"
  while IFS= read -r f; do
    [ -n "$f" ] || continue
    fn="$(normalise "$f")"
    [ "$fn" = "$digest_norm" ] && continue
    rel="$(make_relative_to "$output_dir" "$f")"
    h="$(hash_file "$f")"
    printf '%s\t%s\n' "$rel" "$h" >> "$tmpd/outputs.tsv"
  done < <(list_files_recursive "$output_dir")
  jq -R -s '
    split("\n") | map(select(length>0) | split("\t")
      | {key:.[0], value:{kind:"targetFile", hash:.[1]}})
  ' "$tmpd/outputs.tsv" > "$tmpd/outputs.json"

  mkdir -p "$(dirname "$output_digest")"
  # --slurpfile binds each file's (single) JSON value as the [0] element of an
  # array, hence the `[0]` indexing below.
  jq -n \
    --argjson dfv "$DIGEST_FORMAT_VERSION" \
    --argjson mfv "$MODULE_FORMAT_VERSION" \
    --arg gen "$gen" \
    --argjson generation "$generation" \
    --arg self "$self" \
    --slurpfile deps "$tmpd/deps.json" \
    --slurpfile inputs "$tmpd/inputs.json" \
    --slurpfile outputs "$tmpd/outputs.json" '
    {digestFormatVersion:$dfv, moduleFormatVersion:$mfv, generator:$gen, generation:$generation}
    + (if $self != "" then {selfHash:$self} else {} end)
    + {dependencyHashes:$deps[0], inputs:$inputs[0], outputs:$outputs[0]}
  ' > "$output_digest"

  local ninputs noutputs ndeps
  ninputs="$(jq 'length' "$tmpd/inputs.json")"
  noutputs="$(jq 'length' "$tmpd/outputs.json")"
  ndeps="$(jq 'length' "$tmpd/deps.json")"
  echo "  digest.sh: wrote $output_digest ($ninputs inputs, $noutputs outputs, $ndeps deps)"
}

# ---------------------------------------------------------------------------
# fresh: cache-hit iff all inputs match, generator matches, selfHash + deps
# match, and every recorded output exists with a matching hash. On an
# otherwise-hit with orphan files present, reconcile (#393) and still report hit.
do_fresh() {
  local input_digest="$1" output_dir="$2" output_digest="$3" keep_manifest="${4:-}"

  # Unreadable input or output digest => cache miss (never an error).
  if [ ! -f "$input_digest" ]; then
    echo "  digest.sh: input digest unreadable ($input_digest); cache miss"; return 1
  fi
  if [ ! -f "$output_digest" ]; then
    echo "  digest.sh: output digest unreadable ($output_digest); cache miss"; return 1
  fi
  # A digest that doesn't parse as our shape (legacy/corrupt) => miss.
  if ! jq -e '.digestFormatVersion' "$output_digest" >/dev/null 2>&1; then
    echo "  digest.sh: output digest unreadable (bad format); cache miss"; return 1
  fi

  # 1. Recorded inputs (from output digest, hash only) must equal current
  #    inputs (input digest moduleHashes). Compare as sorted key\thash sets.
  local recorded_inputs current_inputs
  recorded_inputs="$(read_entry_hashes "$output_digest" inputs | LC_ALL=C sort)"
  current_inputs="$(read_kv_map "$input_digest" moduleHashes | LC_ALL=C sort)"
  if [ "$recorded_inputs" != "$current_inputs" ]; then
    echo "  digest.sh: input mismatch; cache miss"; return 1
  fi

  # 2. Generator stamp must match.
  local current_gen recorded_gen
  current_gen="$(generator_stamp)"
  recorded_gen="$(read_scalar "$output_digest" generator "v0-unstamped")"
  if [ "$current_gen" != "$recorded_gen" ]; then
    echo "  digest.sh: generator stamp mismatch ($recorded_gen vs $current_gen); cache miss"; return 1
  fi

  # 3. #347 transitive: package selfHash + deps must match.
  local input_self recorded_self input_deps recorded_deps
  input_self="$(read_scalar "$input_digest" selfHash "")"
  recorded_self="$(read_scalar "$output_digest" selfHash "")"
  if [ "$input_self" != "$recorded_self" ]; then
    echo "  digest.sh: package selfHash mismatch ($recorded_self vs $input_self); cache miss"; return 1
  fi
  input_deps="$(read_kv_map "$input_digest" dependencyHashes | LC_ALL=C sort)"
  recorded_deps="$(read_kv_map "$output_digest" dependencyHashes | LC_ALL=C sort)"
  if [ "$input_deps" != "$recorded_deps" ]; then
    echo "  digest.sh: package deps mismatch; cache miss"; return 1
  fi

  # 4. Every recorded output file must exist and hash to the recorded value.
  #    Paths are relative to output-dir.
  local key hash abs actual
  while IFS=$'\t' read -r key hash; do
    [ -n "$key" ] || continue
    abs="$output_dir/$key"
    if [ ! -f "$abs" ]; then
      echo "  digest.sh: output files missing or modified; cache miss"; return 1
    fi
    actual="$(hash_file "$abs")"
    if [ "$actual" != "$hash" ]; then
      echo "  digest.sh: output files missing or modified; cache miss"; return 1
    fi
  done < <(read_entry_hashes "$output_digest" outputs)

  # 5. #393 orphan reconcile. Every recorded file is present and correct and all
  #    inputs match — but extra files may linger (a renamed-away namespace dir).
  #    The recorded output set is the authoritative keep-set. Build it (normalised
  #    relative paths), plus the protect-set: the output digest itself (if it
  #    lives under output-dir) and any keep-paths-from manifest entries under
  #    output-dir (overlay files, #511). Delete orphans, prune empty dirs, refresh.
  reconcile_and_report "$input_digest" "$output_dir" "$output_digest" "$keep_manifest"
}

# Perform the #393 reconcile inside a confirmed-hit. Emits the hit message
# (with reconcile note if orphans were removed). Always returns 0 (a hit).
reconcile_and_report() {
  local input_digest="$1" output_dir="$2" output_digest="$3" keep_manifest="${4:-}"

  # keep-set: recorded output rel paths, normalised.
  local -A keep=()
  local key hash n
  while IFS=$'\t' read -r key hash; do
    [ -n "$key" ] || continue
    n="$(normalise "$key")"
    keep["$n"]=1
  done < <(read_entry_hashes "$output_digest" outputs)

  # protect-set: full relative paths AND bare basenames (the Haskell reconcile
  # matches on both, so protection holds even when a path can't be prefix-stripped).
  local -A protect=()
  local pd; pd="$(normalise "$(make_relative_to "$output_dir" "$output_digest")")"
  protect["$pd"]=1
  protect["$(basename "$pd")"]=1

  # keep-paths-from manifest: lines "<dir>\t<relToDir>". Keep only entries that
  # genuinely live UNDER output-dir (make_relative_to leaves the path unchanged —
  # still with its own root — when it isn't under base; drop absolute / '..').
  if [ -n "$keep_manifest" ] && [ -f "$keep_manifest" ]; then
    local d relsrc full relout
    # `|| [ -n "$d" ]`: read returns non-zero at EOF-without-trailing-newline, so
    # a manifest whose last line has no final '\n' would otherwise drop that last
    # entry — leaving a legitimately-kept overlay file (#511) unprotected and thus
    # deletable by the reconcile. This reads a caller-supplied file directly (the
    # only such loop; the find/jq/here-string loops always end in a newline).
    while IFS=$'\t' read -r d relsrc || [ -n "$d" ]; do
      [ -n "$d" ] || continue
      [ -n "$relsrc" ] || continue
      full="$(normalise "$d/$relsrc")"
      relout="$(normalise "$(make_relative_to "$output_dir" "$full")")"
      [ -n "$relout" ] || continue
      [ "${relout:0:1}" = "/" ] && continue
      [ "${relout:0:2}" = ".." ] && continue
      protect["$relout"]=1
      protect["$(basename "$relout")"]=1
    done < "$keep_manifest"
  fi

  # Find orphans: on-disk files whose normalised rel path isn't in keep and
  # isn't protected (by full rel OR basename).
  local orphans=() f rel base
  while IFS= read -r f; do
    [ -n "$f" ] || continue
    rel="$(normalise "$(make_relative_to "$output_dir" "$f")")"
    base="$(basename "$rel")"
    [ -n "${keep[$rel]:-}" ] && continue
    [ -n "${protect[$rel]:-}" ] && continue
    [ -n "${protect[$base]:-}" ] && continue
    orphans+=("$f")
  done < <(list_files_recursive "$output_dir")

  if [ "${#orphans[@]}" -eq 0 ]; then
    echo "  digest.sh: cache hit; skipping work"
    return 0
  fi

  echo "  digest.sh: ${#orphans[@]} orphan output file(s) present; reconciling (#393)"
  local o
  for o in "${orphans[@]}"; do
    echo "    - $o"
    rm -f "$o" 2>/dev/null || true
  done
  prune_empty_dirs "$output_dir"
  # Rewrite the digest from the now-clean dir so the orphan doesn't reappear.
  do_refresh "$input_digest" "$output_dir" "$output_digest"
  echo "  digest.sh: cache hit after reconcile; skipping work"
  return 0
}

# Remove empty subdirectories under dir (depth-first); dir itself is left alone.
# Best-effort, matching Hydra.Digest.pruneEmptyDirs.
prune_empty_dirs() {
  local dir="$1"
  [ -d "$dir" ] || return 0
  # -mindepth 1 leaves $dir itself; -depth processes children before parents so
  # a dir emptied by a deeper prune is itself removed. rmdir only removes empties.
  find "$dir" -mindepth 1 -depth -type d -not -path '*/.*' -exec rmdir {} + 2>/dev/null || true
}

# ---------------------------------------------------------------------------
# refresh-input support (#416 promotion of doRefreshInput).
#
# PACKAGES_ROOT: source packages live here, resolved from HYDRA_ROOT_DIR if set,
# else from this script's location (bin/digest.sh → repo root → packages).
packages_root() {
  if [ -n "${HYDRA_ROOT_DIR:-}" ]; then
    printf '%s/packages' "$HYDRA_ROOT_DIR"
  else
    local self_dir; self_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    printf '%s/packages' "$(cd "$self_dir/.." && pwd)"
  fi
}

# Extract the declared namespace from a source file via the CLOSED idiom set.
# Mirrors Hydra.Digest.extractNs (Haskell .hs) + extractNativeNs (.java/.py).
# The extension→pattern choice is DATA; emits the namespace or nothing.
#   Haskell (2): `^\s*ns = ModuleName "..."` ; `moduleName = .?ModuleName "..."`
#   Java   (1): `ModuleName NS = new ModuleName("...")`
#   Python (2): `^_?NS = ModuleName("...")`
extract_declared_ns() {
  local fp="$1" ext="${1##*.}" ns=""
  case "$ext" in
    hs)
      # idiom 1: top-level or where-indented `ns = ModuleName "..."`
      ns="$(grep -oE '^[[:space:]]*ns = ModuleName "[^"]+"' "$fp" 2>/dev/null | head -1 \
            | grep -oE '"[^"]+"' | head -1 | tr -d '"')"
      if [ -z "$ns" ]; then
        # idiom 2: inline `moduleName = (ModuleName "...")` (the '.' matches an
        # optional '(' in the Haskell regex `.?ModuleName`)
        ns="$(grep -oE 'moduleName = .?ModuleName "[^"]+"' "$fp" 2>/dev/null | head -1 \
              | grep -oE '"[^"]+"' | head -1 | tr -d '"')"
      fi
      ;;
    java)
      # `ModuleName NS = new ModuleName("...")` — the space before NS avoids the
      # dependency fields whose names end in _NS (SYNTAX_NS, CORE_NS, ...).
      ns="$(grep -oE 'ModuleName NS = new ModuleName\("[^"]+"\)' "$fp" 2>/dev/null | head -1 \
            | grep -oE '"[^"]+"' | head -1 | tr -d '"')"
      ;;
    py)
      # `^_?NS = ModuleName("...")` — top-level, optional leading underscore.
      ns="$(grep -oE '^_?NS = ModuleName\("[^"]+"\)' "$fp" 2>/dev/null | head -1 \
            | grep -oE '"[^"]+"' | head -1 | tr -d '"')"
      ;;
  esac
  # Always exit 0: an empty result is a normal "no declaration here", and callers
  # capture this via `ns="$(extract_declared_ns …)"` — a non-zero exit there is
  # FATAL under `set -e` (command-substitution failure in an assignment), which
  # would abort the whole discovery scan on the first no-namespace file.
  printf '%s' "$ns"
  return 0
}

# Emit "ns<TAB>file" for every discoverable source, reproducing
# Hydra.Digest.discoverModuleNameFiles: scan Haskell DSL sources AND native
# java/python coder sources (NOT scala — scala is native-authored but unscanned,
# matching the Haskell scanNativePackage which only walks java + python dirs).
# NATIVE-FIRST precedence: on a namespace collision, the native pair wins (the
# Haskell M.union lists native pairs first / left-biased). We emit native pairs
# first here; the consumer keeps the first hash seen per namespace.
discover_module_name_files() {
  local root; root="$(packages_root)"
  [ -d "$root" ] || return 0
  local pkg_dir pkg f ns
  # EMISSION ORDER mirrors Hydra.Digest exactly:
  #   M.union (M.fromList nativePairs) (M.fromList hsPairs)
  # M.fromList is LAST-wins within each list; M.union is left-biased (native beats
  # hs on collision). The consumer (do_refresh_input) takes LAST-wins per namespace,
  # so we emit HASKELL FIRST then NATIVE LAST: within each scan the last duplicate
  # wins (== fromList), and native (emitted last) overrides any hs collision (==
  # left-biased union). Emitting native-first with first-wins would agree on the
  # native-vs-hs axis but DISAGREE with fromList on an intra-scan duplicate.
  #
  # Haskell DSL sources first.
  for pkg_dir in "$root"/*/; do
    local hdir="${pkg_dir}src/main/haskell/Hydra/Sources"
    [ -d "$hdir" ] || continue
    while IFS= read -r f; do
      [ -n "$f" ] || continue
      ns="$(extract_declared_ns "$f")"
      # Guard: a no-namespace file yields empty ns; the `&&` must not be the loop
      # body's final status (it would be 1 under `set -e` and abort the whole
      # scan). Use an if so the body always ends 0.
      if [ -n "$ns" ]; then printf '%s\t%s\n' "$ns" "$f"; fi
    done < <(find "$hdir" -type f -name '*.hs' -not -path '*/.*' | LC_ALL=C sort)
  done
  # Native (.java/.py) sources last — they own hydra.<lang>.* over any stale hs.
  for pkg_dir in "$root"/*/; do
    for sub in java python; do
      local ext; [ "$sub" = java ] && ext=java || ext=py
      local ndir="${pkg_dir}src/main/$sub/hydra/sources"
      [ -d "$ndir" ] || continue
      while IFS= read -r f; do
        [ -n "$f" ] || continue
        ns="$(extract_declared_ns "$f")"
        if [ -n "$ns" ]; then printf '%s\t%s\n' "$ns" "$f"; fi
      done < <(find "$ndir" -type f -name "*.$ext" -not -path '*/.*' | LC_ALL=C sort)
    done
  done
}

# Membership set of namespaces owned by <pkg>, from its on-disk manifest.json:
# mainModules ∪ mainDslModules ∪ testModules (plain membership — the derived-name
# expansion is unnecessary for the refresh-input call set; see the classification
# doc). Emits one namespace per line.
package_owned_namespaces() {
  local dist_json_root="$1" pkg="$2"
  local man="$dist_json_root/$pkg/src/main/json/manifest.json"
  [ -f "$man" ] || return 0
  jq -r '[ (.mainModules // [])[], (.mainDslModules // [])[], (.testModules // [])[] ]
         | unique | .[]' "$man" 2>/dev/null || return 0
}

# Hash every *.json under <dist_json_root>/<pkg>/src/main/json and emit
# "jsonContent:<rel><TAB><hash>" lines (#469). Mirrors hashPackageJsonContent.
hash_package_json_content() {
  local dist_json_root="$1" pkg="$2"
  local json_root="$dist_json_root/$pkg/src/main/json"
  [ -d "$json_root" ] || return 0
  local f rel h
  while IFS= read -r f; do
    [ -n "$f" ] || continue
    rel="$(make_relative_to "$json_root" "$f")"
    h="$(hash_file "$f")"
    printf 'jsonContent:%s\t%s\n' "$rel" "$h"
  done < <(find "$json_root" -type f -name '*.json' -not -path '*/.*' | LC_ALL=C sort)
}

# selfHash = sha256 over sorted "ns\thash\n" lines (mirror computeSelfHash).
# Reads "key<TAB>hash" lines on stdin.
compute_self_hash() {
  LC_ALL=C sort | while IFS=$'\t' read -r k h; do
    [ -n "$k" ] || continue
    printf '%s\t%s\n' "$k" "$h"
  done | sha256sum | cut -d' ' -f1
}

do_refresh_input() {
  local pkg="$1" dist_json_root="$2"
  local dpath="$dist_json_root/$pkg/build/main/digest.json"

  # Owned namespace set (membership test) for routing.
  local owned_tsv; owned_tsv="$(package_owned_namespaces "$dist_json_root" "$pkg")"
  # A set for O(1) membership.
  local -A owned=()
  local ns
  while IFS= read -r ns; do if [ -n "$ns" ]; then owned["$ns"]=1; fi; done <<< "$owned_tsv"

  # Source hashes: discover (ns,file), keep those routed to <pkg>, hash the file.
  # LAST-wins per namespace (no short-circuit): discover_module_name_files emits
  # haskell-first then native-last, so overwriting reproduces
  # `M.union (fromList native) (fromList hs)` exactly — last duplicate within a
  # scan wins (== fromList), and native (last) overrides hs on a cross-scan
  # collision (== left-biased union).
  local -A src_hash=()
  local f h
  while IFS=$'\t' read -r ns f; do
    [ -n "$ns" ] || continue
    [ -n "${owned[$ns]:-}" ] || continue          # route == pkg
    [ -f "$f" ] || continue
    h="$(hash_file "$f")"
    src_hash["$ns"]="$h"
  done < <(discover_module_name_files)

  # JSON content hashes.
  local json_tsv; json_tsv="$(hash_package_json_content "$dist_json_root" "$pkg")"

  # Assemble the full module-hash TSV (src + jsonContent) for selfHash + emit.
  local all_tsv=""
  for ns in "${!src_hash[@]}"; do
    all_tsv+="${ns}"$'\t'"${src_hash[$ns]}"$'\n'
  done
  all_tsv+="$json_tsv"$'\n'

  # selfHash over the full set. No `grep -v '^$'` prefilter: compute_self_hash
  # already skips blank lines (its `[ -n "$k" ] || continue`), and an empty
  # package would make grep exit 1 → under pipefail+set -e that would abort the
  # capture (the same footgun class as extract_declared_ns). Let sort/sha256
  # handle the empty stream (yields the empty-input sha, matching the oracle).
  local self_hash; self_hash="$(printf '%s' "$all_tsv" | compute_self_hash)"

  # Large arrays via TEMP FILES + jq --slurpfile, never --argjson (ARG_MAX;
  # same fix as do_refresh — a big package's moduleHashes would blow the jq
  # command line). refresh-input's real call set is small, but keep the pattern
  # consistent + robust.
  local tmpd; tmpd="$(mktemp -d)"
  # shellcheck disable=SC2064
  trap "rm -rf '$tmpd'" RETURN

  # Preserve existing dependencyHashes from the current digest (a single-package
  # input refresh does not change cross-package deps).
  if [ -f "$dpath" ]; then
    jq -c '.dependencyHashes // []' "$dpath" 2>/dev/null > "$tmpd/deps.json" || echo '[]' > "$tmpd/deps.json"
    [ -s "$tmpd/deps.json" ] || echo '[]' > "$tmpd/deps.json"
  else
    echo '[]' > "$tmpd/deps.json"
  fi

  # moduleHashes array from the full TSV.
  printf '%s' "$all_tsv" | jq -R -s '
    split("\n") | map(select(length>0) | split("\t") | {key:.[0], value:.[1]})
  ' > "$tmpd/modules.json"

  mkdir -p "$(dirname "$dpath")"
  jq -n \
    --argjson dfv "$DIGEST_FORMAT_VERSION" \
    --argjson mfv "$MODULE_FORMAT_VERSION" \
    --arg self "$self_hash" \
    --slurpfile deps "$tmpd/deps.json" \
    --slurpfile modules "$tmpd/modules.json" '
    {digestFormatVersion:$dfv, moduleFormatVersion:$mfv}
    + (if $self != "" then {selfHash:$self} else {} end)
    + {dependencyHashes:$deps[0], moduleHashes:$modules[0]}
  ' > "$dpath"

  local nsrc njson
  nsrc="${#src_hash[@]}"
  njson="$(printf '%s' "$json_tsv" | grep -c . || true)"
  echo "  digest.sh refresh-input: wrote $dpath ($nsrc src + $njson json = $((nsrc + njson)) entries)"
}

# ---------------------------------------------------------------------------
main() {
  [ $# -ge 1 ] || { usage; exit 1; }
  local cmd="$1"; shift
  local inputs="" output_dir="" output_digest="" keep_from=""
  local package="" dist_json_root=""
  while [ $# -gt 0 ]; do
    case "$1" in
      --inputs)          inputs="$2"; shift 2 ;;
      --output-dir)      output_dir="$2"; shift 2 ;;
      --output-digest)   output_digest="$2"; shift 2 ;;
      --keep-paths-from) keep_from="$2"; shift 2 ;;
      --package)         package="$2"; shift 2 ;;
      --dist-json-root)  dist_json_root="$2"; shift 2 ;;
      *) err "unknown argument: $1"; usage; exit 1 ;;
    esac
  done

  case "$cmd" in
    fresh|refresh)
      if [ -z "$inputs" ] || [ -z "$output_digest" ]; then
        err "missing required --inputs or --output-digest"; exit 1
      fi
      if [ -z "$output_dir" ]; then err "--output-dir is required"; exit 1; fi
      ;;
    refresh-input)
      if [ -z "$package" ] || [ -z "$dist_json_root" ]; then
        err "refresh-input requires --package and --dist-json-root"; exit 1
      fi
      ;;
  esac

  case "$cmd" in
    fresh)         do_fresh         "$inputs" "$output_dir" "$output_digest" "$keep_from" ;;
    refresh)       do_refresh       "$inputs" "$output_dir" "$output_digest" ;;
    refresh-input) do_refresh_input "$package" "$dist_json_root" ;;
    *) err "unknown subcommand: $cmd"; usage; exit 1 ;;
  esac
}

main "$@"
