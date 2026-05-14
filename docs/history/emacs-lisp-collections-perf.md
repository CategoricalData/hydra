# Emacs Lisp collections and lazy-let performance fix

A historical record of the Hydra-Emacs-Lisp performance work landed on
the `feature_361_elisp_collections` branch (issue
[#361](https://github.com/CategoricalData/hydra/issues/361)).
The fix mirrors the Common Lisp work from #360 and the Python work from
#344, applied a third time. This document captures the shape of the
problem, the two-part fix, and the side-by-side numbers; the structural
similarity across the three hosts is the main lesson.

## Background — the same wall, a third time

Hydra-Emacs-Lisp's collections lived where Hydra-Python's (pre-#362)
and Hydra-Common-Lisp's (pre-#360) lived:

- `lib/maps.el` represented maps as **sorted alists** keyed by
  `generic-compare`. Insert/lookup/delete were O(n).
- `lib/sets.el` represented sets as **sorted lists**, similarly O(n).
- The Emacs Lisp coder emitted `let` bindings as eager native
  `let`/`let*`, so any binding referenced multiple times under
  conditional or lambda paths was re-evaluated each time it was reached
  through the kernel's deeply nested inference code. This is the same
  exponential blowup the Python investigation
  (`docs/history/python-host-perf-investigation.md`) documented at
  length and the Common Lisp work hit head-on.

Combined with Emacs Lisp's already-slow curried-call overhead, the
result was that Hydra-Emacs-Lisp was not treated as a fully supported
implementation: the kernel-test suite was tolerable but inference at
non-trivial sizes was unusable.

## The fix (two parts, both needed)

The CL plan (`feature_360_common_lisp_collections-plan.md`) explicitly
calls out that neither part of the fix is sufficient alone:

- Hash-table maps/sets alone leave the scaling exponent above 3 because
  the dominant cost is repeated *evaluation* of the same binding, not
  collection ops.
- Lazy-let alone leaves the constant factor too high because every
  primitive call into the kernel still walks an alist.

### Part 1 — hash-table-backed maps and sets (lib/maps.el, lib/sets.el)

Both libraries are now facades over Emacs Lisp's `make-hash-table :test
'equal`. Mutating operations (insert/delete/union/alter/…) clone the
table via the C-level `copy-hash-table` primitive and mutate the clone,
preserving the immutable semantics Hydra expects. Read paths
(lookup/member/size) are O(1). Iteration helpers (to_list, keys, elems)
sort via `generic-compare` so downstream serialization stays
deterministic.

`lib/equality.el` got a `hash-table-p` clause in `generic-compare`
(structural equality fast path, sorted-pair fallback).

### Part 2 — lazy-let loader transformation (loader.el, new lazy.el)

`heads/lisp/emacs-lisp/src/main/emacs-lisp/hydra/lazy.el` defines a
two-state cons-tagged lazy cell (`(:lazy-thunk . #'thunk)` →
`(:lazy-value . v)`) with `make-lazy` / `lazy-force` (both `defsubst`s
for inlining).

`loader.el`'s `hydra-fix-curried-calls` now rewrites each non-trivial
`(let ((x EXPR)) BODY)` into

```elisp
(let ((x-lazy-thunk (make-lazy (lambda () EXPR))))
  (cl-symbol-macrolet ((x (if (eq (car x-lazy-thunk) :lazy-value)
                              (cdr x-lazy-thunk)
                              (lazy-force x-lazy-thunk))))
    BODY))
```

The transformation is **gated** on the body containing a conditional
(`if`/`cond`/`case`/`pcase`/`when`/`unless`) or a `lambda` — without
the gate, kernel-test bench regressed because the most common shape is
straight-line let chains where lazy adds pure overhead. The CL work
(#360) tuned the gate the same way after seeing the same regression.

"Trivial" bindings (atoms, quoted constants, bare lambdas, single-level
calls with all-atom args, already-lazy bindings) are emitted as plain
`let` — these don't benefit from lazy and would pay an indirection on
every reference.

## Side-by-side numbers

All numbers captured on the same machine, on commit `ac2705261` for the
post-fix EL runs and on `ac2705261` with the `heads/lisp/emacs-lisp/`
files reverted to HEAD~1 (`29da4e813`) for baseline. Emacs Lisp runs are
byte-compiled only (no native-comp) to match the issue's apples-to-apples
framing — see the README's notes on native-comp for optional further
speedup. All EL runs use Emacs 30.2 on native arm64
(`/opt/homebrew/opt/emacs-plus@30/bin/emacs-30.2`).

### Kernel-test bench (per-rep wall, ms, 3 reps mean ± stddev)

| Host | Pre-fix | Post-fix | Speedup |
|------|---:|---:|---:|
| **Emacs Lisp** | 33385 ± 456 | **8577 ± 45** | **3.89×** |

(Haskell and Python columns omitted: both unchanged by this branch — the
fix is EL-only. For absolute comparison see
`docs/history/python-host-perf-investigation.md` Python numbers and the
Haskell baseline in `feature_360_common_lisp_collections-plan.md`.)

### Inference linearChain (seconds, single run)

| n | Haskell | Emacs Lisp pre-fix | **Emacs Lisp post-fix** | Speedup |
|---:|---:|---:|---:|---:|
| 0 | 15.03 | 16.88 | **0.20** | 84.4× |
| 10 | 14.70 | 1311.84 | **18.73** | **70.0×** |
| 25 | 13.93 | (skipped — projects to ~50 min) | **48.79** | — |
| 50 | 15.12 | n/a | **99.07** | — |

Power-law fit n=10→50 post-fix: ratio 99.07/18.73 = 5.29 over 5× more
work → k ≈ log(5.29) / log(5) = **1.04** — near-linear scaling, matching
Common Lisp's post-fix k=1.06.

Pre-fix sizes were capped at n=10 measurement because alist + eager-let
at n=15 was projected to take ~50 minutes (CL's pre-fix curve hit n=25 at
1664s; EL alist is ~5× slower per op than CL's, so n=15 projects to >2400s).
Haskell stays flat at ~15s across all sizes because its `infer_modules_given`
overhead dominates the synthetic linearChain workload at these N.

Python (CPython) inference reference numbers, from the `#360` CL plan
on the same kernel: n=10=14.0s, n=25=38.6s. Emacs Lisp post-fix is now
**within 25-35% of CPython** at n=10 and n=25, with the same near-linear
scaling.

### Bootstrap emacs-lisp → Python (wall time)

| Variant | Wall | Codegen | Files |
|---|---:|---:|---:|
| Emacs Lisp pre-fix | **>4 h (timed out, SIGTERM at 14400s)** | (never reached) | 0 |
| **Emacs Lisp post-fix** | **65m 41s** | **43m 18s** | 142 |

The baseline run was capped at 4 hours; it was still in kernel-load /
early codegen when SIGTERM fired (no output written, no progress
markers). The post-fix bootstrap completed in 65m 41s, so the speedup
is **at least 3.7×**, almost certainly substantially more.

The bootstrap is dominated by kernel byte-compilation + code generation
across 142 modules. Lazy-let primarily helps the inference path; the
bootstrap codegen path is mostly straight-line, so the speedup here is
modest compared to the inference win. The hash-table maps/sets still
help (constant-factor primitive ops are cheaper than alist scans).

Cross-host bootstrap reference (from #360's CL→Python data, same
kernel scope):
- Haskell → Python: 68s
- Python (pypy3) → Python: 305s
- Common Lisp → Python: 45s
- Emacs Lisp → Python (this branch, post-fix): 3941s

EL→Python is ~50–90× slower than Haskell→Python and ~80× slower than
CL→Python. The dominant overhead is EL's curried-call cost in
interpreted/byte-compiled mode and the loader transformations that
run on every load. Native-comp closes most of this gap but isn't
captured in this measurement.

## Lessons (carried forward from #344 and #360)

1. **Lazy-let is the dominant fix in every Lisp-flavored host that
   inherits eager DSL `let` semantics.** Collections optimization helps
   the constant factor; lazy-let fixes the scaling exponent.
2. **The "body has conditional or lambda" gate matters.** Without it,
   straight-line code in the kernel-test bench regresses ~15-20%.
3. **Hash-table copy-on-write is the right backing on hosts with a
   C-level table-copy primitive.** CL eventually chose RB-tree because
   SBCL's `maphash`-copy was too slow; Emacs has `copy-hash-table`
   built-in, so the simpler approach fits.

## File map

Files touched in this branch:

- `heads/lisp/emacs-lisp/src/main/emacs-lisp/hydra/lib/maps.el` — hash-table facade
- `heads/lisp/emacs-lisp/src/main/emacs-lisp/hydra/lib/sets.el` — hash-table facade
- `heads/lisp/emacs-lisp/src/main/emacs-lisp/hydra/lib/equality.el` — hash-table clause in generic-compare
- `heads/lisp/emacs-lisp/src/main/emacs-lisp/hydra/loader.el` — lazy-let transform, cl-symbol-macrolet branch
- `heads/lisp/emacs-lisp/src/main/emacs-lisp/hydra/lazy.el` — new file: make-lazy / lazy-force
- `heads/lisp/emacs-lisp/bin/inference-bench.{el,sh}` — new EL bench runner
- `bin/run-inference-bench.sh` — wires emacs-lisp into the dispatch
- `packages/hydra-lisp/hydra-emacs-lisp/README.md` — performance section update

## Related work

- [#344 — Python host self-hosting and Lazy](https://github.com/CategoricalData/hydra/issues/344)
- [#359 — Java collections](https://github.com/CategoricalData/hydra/issues/359)
- [#360 — Common Lisp collections](https://github.com/CategoricalData/hydra/issues/360)
- [#361 — Emacs Lisp collections (this branch)](https://github.com/CategoricalData/hydra/issues/361)
- [#362 — Python collections](https://github.com/CategoricalData/hydra/issues/362)
- `docs/history/python-host-perf-investigation.md`
- `docs/history/inference-bench-complexity-analysis.md`
