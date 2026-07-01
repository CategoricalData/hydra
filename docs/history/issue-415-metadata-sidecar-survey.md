# Issue #415 survey: metadata sidecar + freshness across the #370 seam

**Date:** 2026-07-01
**Branch:** `bug_415_metadata_sidecar_survey`
**Surveyed by:** Claude (opusplan) on behalf of the coordinator

This document records the state of the codebase *after* prerequisites
#412, #413, #414, and #370 closed, maps what each of #415's four
sub-items still requires, and recommends an implementation ordering
and branch shape for the implementer.

---

## Prerequisites: what landed

| Issue | Title | State | Notes |
|-------|-------|-------|-------|
| #412 | Digest metadata rename (`digestFormatVersion`/`moduleFormatVersion`) | CLOSED | Both fields present in `writeDigest`/`serializeDigestV2`. `docs/json-format.md` updated. |
| #413 | Structured `generation` provenance record | CLOSED | **Partially implemented — see finding below.** |
| #414 | Fail-loud decoder + shell failure propagation | CLOSED | `set -euo pipefail` in scripts; Haskell decoder assumed strictened per issue intent. |
| #370 | Transition to external versioned hosts | CLOSED | `component_identity` in `bin/lib/assemble-common.sh` branches on published vs local; `hydra-packages.py host-version` resolves version pins. |

### Critical finding: #413's `generation` record is not serialized

Issue #413 specified a structured record to replace the flat `generator` string in digest files:

```
generation:
  generatorId:  <hash | version-pin>   # GATING — the cache key
  mode:         published | shim        # informational — provenance class
  host:         <string>                # which host produced this
  hydraVersion: <string?>               # release version (published mode only)
  revision:     <string?>               # commit SHA (shim mode only)
  timestamp:    <string?>               # when (NON-deterministic)
```

What actually exists in the codebase today:

- `Digest.hs:532` — `digestGenerator :: String` — a flat string field, unchanged from before #413.
- `serializeDigestV2` / `parseDigestV2` — emit/parse `"generator": "<stamp>"` — the flat string.
  No `generation` object, no `mode`, no `host`, no `hydraVersion`, no `revision`, no `timestamp`.
- The `digestsMatch` docstring (lines 589–596) *references* the `generation` record's informational
  fields as fields that "must be omitted" from freshness gating — but that's aspirational commentary,
  not implemented code.
- `component_identity` in `assemble-common.sh` (lines 112–130) correctly implements the
  `generatorId` semantic: returns `host:<pkg>:<ver>` for a published host and a SHA-256 content
  hash for local/shim. But this stamp is written into the flat `generator` string, not into a
  structured `generation` object.

**Conclusion:** #413 was closed with the `generatorId` gating behavior correct (via
`component_identity`), but the structured record with `mode`/`host`/`hydraVersion`/`revision`
was never serialized. The coordinator will file or reopen an issue for this gap separately.

---

## The core gap in #415

Published packages include `manifest.json` and `package.json` but NOT `digest.json`.

| Sidecar | Path | Tracked in git? | Shipped? | Has `moduleFormatVersion`? |
|---------|------|-----------------|----------|---------------------------|
| `manifest.json` | `dist/json/<pkg>/src/main/json/manifest.json` | YES | YES | NO — has `manifestFormatVersion` only |
| `package.json` | `packages/<pkg>/package.json` | YES | YES | NO |
| `digest.json` | `dist/json/<pkg>/build/<set>/digest.json` | NO (`dist/**/build/` gitignored) | NO | YES — but unreachable |

A post-#370 external consumer of a published package cannot currently read `moduleFormatVersion`
or any generation provenance at all.

---

## Sub-item 1: Ship the metadata sidecar

**What it is:** Surface `moduleFormatVersion` (and eventually provenance) in the shipped artifact
so external consumers can detect encoding version and provenance without reading the gitignored
digest sidecar.

### Files and line ranges

- `heads/haskell/src/main/haskell/Hydra/Generation.hs`
  - `writeManifestJson` (line 1458–1478): writes the monolithic `dist/json/manifest.json`.
    Emits `manifestFormatVersion: 1`, `kernelModules`, `mainModules`, `testModules`, `dslModules`.
    No `moduleFormatVersion`.
  - `writePerPackageManifestsJson` (line 1500–1541): writes per-package
    `dist/json/<pkg>/src/main/json/manifest.json`.
    Emits `manifestFormatVersion: 1`, `mainModules`, `mainDslModules`, `mainEncodingModules`,
    `testModules`, `package`.
    No `moduleFormatVersion`.

### Design sketch

1. Add `("moduleFormatVersion", Json.ValueNumber 1)` to the field list in both
   `writeManifestJson` and `writePerPackageManifestsJson`, keeping alphabetical field order
   (per coding style).
2. Update `docs/json-format.md` §Sidecar and metadata files → `manifest.json` table to
   document the new field.
3. Update `manifestFormatVersion` bump rules: adding a new field is a consumer-visible change,
   so bump `manifestFormatVersion` from 1 to 2 (or document it as additive/backward-compatible
   if the field is optional for existing readers).
4. Update `readManifestField` / `readManifestFieldOrEmpty` call sites in
   `heads/haskell/src/exec/bootstrap-from-json/Main.hs` (lines 287–349) if the new field
   needs to be read back — for skew detection (sub-item 3), yes.
5. Update `DigestSpec.hs` / snapshot tests if manifest format is tested.

### Dependencies

None. This is the foundational sub-item — all other sub-items that need consumer-visible
format version depend on this one landing first.

### Rough size

1 focused commit for the `manifest.json` writer changes + 1 commit for the doc update.
Total: 2 WIP commits, likely < 50 lines of Haskell.

### Risks/open questions

- **Bump `manifestFormatVersion` or not?** Adding `moduleFormatVersion` is a new field;
  old readers that ignore unknown fields won't break. But the field is consumer-facing and
  its absence was the stated gap. Safest: bump `manifestFormatVersion` to 2 and document
  the additive change. Decision needed.
- **What value to ship?** Today `moduleFormatVersion` is a compile-time constant (`1`).
  If it ever changes, the Haskell kernel hardcodes it. A named constant in `Digest.hs`
  or `Generation.hs` would make it a single source of truth.

---

## Sub-item 2: `component_identity` version-pin branch (the #347 T-side)

**What it is:** The T-side of the Merkle stamp — when a published host is consumed, the
generator stamp for that host should be the pinned version string (`host:<pkg>:<ver>`),
not a content hash. This makes the cache stable across local source edits when the published
host hasn't changed.

### Current state

This is **already implemented.** `component_identity` in `bin/lib/assemble-common.sh`
(lines 112–130) already does exactly this:

```bash
component_identity() {
    local pkg="$1"
    local ver
    if ver=$("$HYDRA_ROOT_DIR/bin/lib/hydra-packages.py" host-version "$pkg" 2>/dev/null) \
            && [ -n "$ver" ]; then
        printf 'host:%s:%s' "$pkg" "$ver"
        return
    fi
    # ... fall back to local content hash
}
```

`PUBLISHED_HOSTS` in `hydra-packages.py` (lines 59–70) lists:
`hydra-kernel`, `hydra-haskell`, `hydra-java`, `hydra-jvm`, `hydra-python`, `hydra-scala`,
`hydra-lisp`, `hydra-typescript`.

However, `hydra.json` currently has all three active hosts overridden to `local`:

```json
"hostOverrides": {"java": "local", "python": "local", "haskell": "local"}
```

So in the 0.17.0 development cycle, `component_identity` always falls through to the
content-hash path — the published-host branch is dormant.

### Design sketch

The mechanism is complete. What remains is not a code change but an operational gate:
when 0.17.0 hosts are published and `hostOverrides` entries are removed (or set to
a version), `component_identity` will automatically return the version pin. No code
changes needed to "implement" sub-item 2 — it activates itself when publishing resumes.

The **informational gap** is that the `mode` discriminator (which branch was taken) is
not written to any digest or shipped sidecar. That's a consequence of #413's partial
implementation (see above) and is addressed by the generation record work.

### Files and line ranges

- `bin/lib/assemble-common.sh` lines 112–130 — `component_identity` (no change needed).
- `bin/lib/hydra-packages.py` lines 59–70 — `PUBLISHED_HOSTS` (no change needed).
- `hydra.json` — `hostOverrides` is the operational toggle (no code change; operational).

### Dependencies

Depends on published hosts existing (operational). No code dependency on sub-items 1, 3, or 4.

### Rough size

0 code commits (already implemented). If the `generation.mode` informational field is
wanted (from #413), that's part of the generation record work (a separate tracked item).

### Risks/open questions

- The version-pin branch has never been exercised in production (all overrides are `local`
  during 0.17.0 development). An integration test that exercises the `host:<pkg>:<ver>` path
  would catch regressions. No such test exists today.

---

## Sub-item 3: Skew detection (fail loud on `moduleFormatVersion` disagreement)

**What it is:** When `bootstrap-from-json` reads a published package's sidecar, compare
the sidecar's `moduleFormatVersion` against the local kernel's expected version. Fail loud
(not degrade silently) if they disagree.

### Current state

No skew detection exists. `bootstrap-from-json/Main.hs` reads `manifest.json` fields
(lines 287–349) but only reads module-name lists (`mainModules`, `testModules`,
`mainDslModules`, `mainEncodingModules`, `dslModules`). It never reads `moduleFormatVersion`.

The local kernel writes `moduleFormatVersion: 1` into the local `digest.json` (which is
gitignored), but never compares that against a shipped sidecar's version claim.

### Design sketch

1. After sub-item 1 lands (so `manifest.json` carries `moduleFormatVersion`), add a
   `readManifestFormatVersion :: FilePath -> IO (Maybe Int)` helper to `Generation.hs`
   (near `readManifestField`, line 1574).
2. In `bootstrap-from-json/Main.hs`, after loading a package's `manifest.json`, call
   `readManifestFormatVersion pkgDir` and compare against a `expectedModuleFormatVersion :: Int`
   constant (currently `1`, sourced from `Digest.hs` or a new shared constant).
3. If the loaded version differs from expected: fail with a named error, e.g.:
   ```
   bootstrap-from-json: moduleFormatVersion mismatch for hydra-kernel:
     expected 1, got 2 (upgrade the local kernel or pin an older host version).
   ```
4. If `manifest.json` is missing the field (legacy or pre-sub-item-1 package): warn but
   do not abort — the field was absent before sub-item 1 lands, so strict failure would
   break existing packages in transition. Treat absent as version 1 (the only version
   that has ever existed).
5. Branch on `generation.mode` if it's implemented: a `mode: shim` artifact is expected
   to disagree with the last published host's format version and must not trigger skew
   detection. For now (pre-#413 structured record), this branch is moot — there is no
   mode field to read.

### Files and line ranges

- `heads/haskell/src/main/haskell/Hydra/Generation.hs` — add
  `readManifestFormatVersion` (near line 1575).
- `heads/haskell/src/exec/bootstrap-from-json/Main.hs` — add skew check after manifest
  load (near lines 287–293). Add `expectedModuleFormatVersion` constant.
- `heads/haskell/src/test/haskell/Hydra/DigestSpec.hs` — add test for version-mismatch
  path (or a new `ManifestSpec.hs`).

### Dependencies

**Depends on sub-item 1** (shipping `moduleFormatVersion` in `manifest.json`). Without
that field in the shipped sidecar, there is nothing to compare. Sub-item 3 cannot land
independently.

### Rough size

2–3 WIP commits: (a) `readManifestFormatVersion` helper, (b) skew check in
`bootstrap-from-json`, (c) tests.

### Risks/open questions

- **Fail-loud vs. warn for missing field:** During the transition period (published packages
  built before sub-item 1), `moduleFormatVersion` will be absent from `manifest.json`. Hard
  failure would break the build for those packages. Recommend: treat absent as `1` (the
  implicit version) for one release cycle, then make the field required.
- **Where is the "local kernel's expected version" defined?** Today it's the literal `1`
  baked into `serializeDigest`. Should be a named constant shared between the sidecar writer
  and the skew checker. Add `currentModuleFormatVersion :: Int` to `Digest.hs` or a new
  `Hydra.FormatVersion` module.
- **Shim exemption:** A `mode: shim` artifact is "ahead of" the last published host and may
  encode data the published host can't decode. Skew detection must not false-positive on shims.
  This requires the `generation.mode` field (unimplemented per #413 finding). Until the
  structured record lands, shim-produced artifacts will trip skew detection if their
  `moduleFormatVersion` ever disagrees. Risk is low today (version is still 1 everywhere),
  but the invariant should be documented.

---

## Sub-item 4: Compat test (vN reads vN+1 and vice versa)

**What it is:** A test that a parser for format version N either correctly reads version
N+1 data or fails with a clear, named version-mismatch error (not a silent decode failure
or a crash with no diagnostic).

### Current state

No compat test infrastructure exists. `DigestSpec.hs` tests digest read/write round-trips
but not version-mismatch paths. `TestSuiteSpec.hs` runs the cross-module test suite.
Neither tests what happens when a decoder encounters data at a format version it doesn't
expect.

### Design sketch

1. Add a `moduleFormatVersion: 2` fixture (a synthetic `manifest.json` with an
   incremented version field; the module JSON files themselves don't need to change
   for a format-version-only test).
2. In `DigestSpec.hs` (or a new `ManifestSpec.hs`), write a test that:
   - Reads the v2 fixture with a v1 reader.
   - Asserts that the reader either (a) returns a typed error (preferred after sub-item 3)
     or (b) currently degrades gracefully — establishing the contract so a regression
     introduces explicit failure rather than silent misparse.
3. Write a symmetric test: a v1 manifest read by code that expects v2 should fail with
   a version-mismatch error (once sub-item 3 adds the check).

### Files and line ranges

- `heads/haskell/src/test/haskell/Hydra/DigestSpec.hs` or a new
  `heads/haskell/src/test/haskell/Hydra/ManifestSpec.hs`.
- A new test fixture directory (e.g., `heads/haskell/src/test/resources/compat/`) for
  synthetic manifest JSON files at synthetic versions.

### Dependencies

**Depends on sub-items 1 and 3.** The compat test is only meaningful once:
- Sub-item 1 ships `moduleFormatVersion` in `manifest.json` (so version is visible), and
- Sub-item 3 adds the skew check (so the reader has something to test).

The test fixture can be written ahead of time, but the "reader fails with a clear error"
assertion requires the skew check from sub-item 3.

### Rough size

1–2 WIP commits: (a) fixture + test skeleton, (b) assertions once sub-item 3 lands.

### Risks/open questions

- **What does "vN+1" mean?** Today `moduleFormatVersion` is `1`; version `2` doesn't
  exist. The compat test must synthesize a future-version fixture. The test should document
  that it is testing the *error path* for an unknown version, not an actual v2 encoding.
- **Symmetric direction (vN+1 reader, vN data):** Forward-compat (a new reader reading old
  data) is simpler — old data is just missing new fields, which the reader should tolerate.
  The interesting failure mode is a v1 reader trying to parse genuinely-different v2 data.
  Both directions should be tested, but the v1-reads-v2 direction is higher priority.

---

## Existence-only freshness gates: audit

**Issue #415 item:** "Ensure no existence-only freshness checks remain; all gates
content/version-addressed."

### Findings

The v2 `digest-check fresh` path (`heads/haskell/src/exec/digest-check/Main.hs`) does NOT
have existence-only gates. The `doFresh` function checks:
1. Input digest exists (file existence — unavoidable bootstrap check, not a content gate).
2. Output digest exists (same).
3. Per-namespace input hashes all match recorded inputs (content-addressed).
4. Generator stamp matches (content-addressed via `component_identity`).
5. `selfHash` matches (content-addressed).
6. `ppDeps` matches (content-addressed).
7. Per-output-file: exists AND hash matches (lines 234–245, content-addressed, not existence-only).

The `verifyOutputsExist` function in `Digest.hs` (line 613) checks both file existence and
hash equality — it is not existence-only (the name is slightly misleading).

The v1 `readDigest` / `writeDigest` path (used for per-package input digests) stores hashes
but does not verify them at read time — it is an existence-based read gate at the per-package
level. However, the v2 `assemble_check_fresh` layer above it delegates to `digest-check fresh`,
which is fully content-addressed.

**Conclusion:** No significant existence-only freshness gates remain in the active code path.
The `assemble_check_fresh` → `digest-check fresh` chain is content-addressed end-to-end.
This sub-item of #415 appears complete; no additional work needed.

---

## Dependency graph

```
Sub-item 2 (version-pin branch) — already implemented; activates operationally
     (no code dependency)

Sub-item 1 (ship moduleFormatVersion in manifest.json)
     ↓
Sub-item 3 (skew detection in bootstrap-from-json)
     ↓
Sub-item 4 (compat test: vN reads vN+1)
```

Also floating, separate:
- **#413 generation record**: implement `data Generation` + serialization + shim exemption
  in skew detection. Coordinator will file or reopen.

---

## Recommended ordering

1. **Sub-item 2** is already done operationally. No code work needed.
2. **Sub-item 1** first: add `moduleFormatVersion` to `manifest.json` in
   `writeManifestJson` and `writePerPackageManifestsJson`. Small, self-contained,
   unlocks everything else.
3. **Sub-item 3** second: add skew detection in `bootstrap-from-json`. Depends on 1.
4. **Sub-item 4** last: compat test. Depends on 1 and 3.
5. **Generation record (#413 gap)** in parallel or after: implement `data Generation`,
   wire `mode` into serialization, add shim-exemption to sub-item 3's skew check.

---

## Recommended branch shape

**Two branches, not one:**

- **Branch A** — `feature_415a_ship_format_version`: sub-items 1 + 3 + 4 (the core loop).
  These are tightly coupled (1 enables 3; 3 enables 4) and together close the stated
  "fail loud across the external-host seam" story without needing the generation record.
  Estimated 4–6 focused commits. Good candidate for a single PR.

- **Branch B** — `feature_413b_generation_record`: implement the structured `generation`
  record from #413's spec (`data Generation`, serialization, `mode` field, shim exemption
  wired into Branch A's skew check). This is logically separate and can be developed and
  reviewed independently. It also has a non-trivial design question: how to thread `mode`
  down from `component_identity` into the Haskell digest writer (the stamp is computed
  in shell; the digest is written in Haskell). Worth a standalone branch and issue.

Rationale for splitting: the generation record requires cross-layer coordination
(shell → Haskell) and is blocked on a separate issue being filed. The core format-version
visibility story (Branch A) is straightforward and can ship without it.
