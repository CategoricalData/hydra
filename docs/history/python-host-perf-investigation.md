# Python host performance investigation

A historical record of the multi-session investigation that brought
Hydra-Python from "unusable for term-level workloads" to "competitive with
the Haskell and Java hosts."
The goal of this document is twofold:

1. Capture what was tried, what worked, and what didn't,
   so the next person looking at Python-host perf doesn't have to re-derive it.
2. Surface the insights that are likely to apply to other Hydra hosts
   (Scala, Lisp, Rust, Go, etc.) when they hit similar walls.

## Timeline summary

The investigation spanned several sessions on the
`feature_344_self_hosting_coders` branch.

| Phase | What was wrong | What fixed it |
|-------|---------------|---------------|
| Pre-#362 | Python host worked but the kernel test bench had no persistent collections — `hydra.lib.{lists,maps,sets}` were thin wrappers around `tuple`/`dict`/`frozenset` | Baseline. Bootstrap python→python ~6 minutes |
| #362 lands (`feature_362_python_collections`) | Replaced lib collections with hand-rolled persistent classes (RB-tree `PersistentMap`, singly-linked `ConsList`) for Haskell-semantics parity | Bootstrap regressed to **15+ minutes**, `hydra.lib.maps` benchmark degraded too |
| Multiple optimization attempts on the RB-tree | Hash caching, transient `from_pairs`, slot access, `is`-shortcut compare | Reduced gen time ~25% but still ~2× pre-#362 baseline |
| `immutables.Map` HAMT facade | C-extension HAMT used in `contextvars` | CPython bench beats pre-#362 by 15%; **PyPy still 4× slower** because PyPy lacks the C wheel |
| Lazy emission for inline let bindings (`hydra.python.util.Lazy`) | Generated Python eagerly evaluated walrus-let bindings, causing exponential re-evaluation in inference | Inference n=67 went from "doesn't terminate in 5h" to **34.6s on CPython, 20.4s on PyPy** |
| Site 3 v3 (top-level vs inner thunks) | Inner zero-arg thunks were emitted as `@lru_cache(1) def name(): ...; name()` (heavy `functools.update_wrapper` overhead per definition) | Test codegen 47% faster |
| `comparisonToExpr` serde fix | The Python-syntax serializer dropped the RHS of comparison nodes; `from_ == X` rendered as `from_`. Inline `cases _Enum` dispatches always picked the first branch | 9 broken pytest cases fixed; correct enum dispatch in inline contexts |
| **dict/frozenset/tuple facades** (winner) | `PersistentMap.union` was 48% of codegen profile time, doing `len(other)` membership checks per call (181M `hash()` ops). All hand-rolled persistent collections paid Python-level overhead per op | `PersistentMap`/`PersistentSet`/`ConsList` became thin facades over native `dict`/`frozenset`/`tuple`. Bootstrap **3m 54s** (better than pre-#362). Inference n=67 PyPy **6.1s** (within 1.6× of Java) |

## Final numbers (HEAD as of investigation close)

Same hardware, same day, fresh measurements.

### Bootstrap python→python (wall time)

| Config | Wall | Codegen total |
|--------|-----:|--------------:|
| Pre-#362 staging | 6m 3s | ~291s |
| #362 + Lazy (D baseline) | 14m 32s | 676s |
| #362 + Lazy + Site 3 + serde fix | 10m 34s | 454s |
| **HEAD (above + dict/frozenset/tuple facades)** | **3m 54s** | **~150s** |

### Bench (kernel-test, 5 reps, mean ± stdev, ms)

| Config | CPython | PyPy |
|---|---:|---:|
| Pre-#362 | 935.4 ± 11.8 | 2087.5 ± 131.8 |
| #362 only | 856.1 ± 52.8 | 2552.2 ± 96.0 |
| #362 + Lazy | 904.4 ± 38.4 | 2577.2 ± 211.0 |
| #362 + Lazy + Site 3 + serde fix | 816.2 ± 32.1 | 2484.6 ± 129.2 |
| **HEAD** | **934.3 ± 7.5** | **2477.5 ± 166.7** |

CPython bench is back at pre-#362 level.
PyPy bench is +18% slower than pre-#362 — the price for the codegen win.

### Inference scaling (utils.py first N defs)

| Config | n=7 | n=16 | n=32 | n=67 |
|---|---:|---:|---:|---:|
| #362 only (CPython) | 103.5s | 141.4s | killed | doesn't terminate |
| #362 + Lazy CPython | 5.5s | 10.4s | 16.8s | 34.6s |
| #362 + Lazy PyPy | 5.9s | 6.4s | 9.7s | 20.4s |
| **HEAD CPython** | **4.5s** | **8.6s** | **13.5s** | **27.5s** |
| **HEAD PyPy** | **2.6s** | **2.2s** | **3.0s** | **6.1s** |
| Java reference (n=67) | — | — | — | 3.88s |

PyPy is now within 1.6× of Java for the kernel's largest module. CPython improved to within ~7× of Java.

### Bootstrap triad (haskell, java, python × haskell, java, python)

```
  ┌───────────────┬─────────────────────┬──────────────────────┬──────────────────────┐
  │ Host \ Target │ Haskell             │ Java                 │ Python               │
  ├───────────────┼─────────────────────┼──────────────────────┼──────────────────────┤
  │ Haskell       │ gen: 27.0s          │ gen: 9.6s            │ gen: 7.5s            │
  ├───────────────┼─────────────────────┼──────────────────────┼──────────────────────┤
  │ Java          │ gen: 12.1s          │ gen: 5.4s            │ gen: 5.8s            │
  ├───────────────┼─────────────────────┼──────────────────────┼──────────────────────┤
  │ Python        │ gen: 41.4s          │ gen: 48.4s           │ gen: 1m 10s          │
  └───────────────┴─────────────────────┴──────────────────────┴──────────────────────┘
```

All 22581 tests passing across the matrix.
Python host gen times are in the same order of magnitude as Java host
across all targets. The diagonal (host = target) is well-behaved everywhere.

## What changed in code

Two commits did the perf work:

1. `331dd14c6` — Site 3 v3 (`encodeTermAssignment` topLevel-aware) +
   `comparisonToExpr` serde fix (proper RHS emission) +
   `encodeUnionEliminationInline` fix (emit `EnumType.VARIANT` not
   `EnumTypeVariant`).

2. `a75ce964e` — Replace `PersistentMap`, `PersistentSet`, `ConsList` impls
   with thin facades over `dict`, `frozenset`, `tuple`.
   Removes `immutables>=0.21` runtime dependency.
   6 files, +228/−342 lines.

The combined diff against the pre-fix branch tip is about 11 files,
+1k/−700 lines (counting DSL sources, generated dist, head impls, tests).

## Insights worth remembering

These are the things that, if forgotten, will cost a future maintainer hours.

### 1. The Hydra codegen pipeline is allocation-heavy

The Python coder rewrites every kernel module's term tree multiple times.
On the order of millions of small persistent containers are created per
module. **Anything in the inner loop of a collection operation will dominate
the entire pipeline**, even if it looks like a one-liner. A 100ns extra per
allocation × 10M allocations = 1 second per module × 250 modules = 4
minutes.

This is why O(n) copy semantics on dict/frozenset/tuple beat structurally
shared HAMT/RB-tree implementations: the typical input is small (5–100
entries), so the constant factor wins. Asymptotic structural sharing matters
for long-lived large maps, not transient build-up-then-throw-away ones.

### 2. Profile before guessing

Many sessions of this investigation were spent on micro-optimizations
(hash caching, slot access, transient builders) that produced ~25% wins
but never got close to closing the gap.
A 78s cProfile run showed the answer in 30 seconds:

```
   ncalls  tottime  cumtime  function
   115413   38.406    77.729  PersistentMap.union
181059425    8.610     8.613  builtins.hash
```

`PersistentMap.union` was 48% of total profile time. The fix (replace its
N-element membership-checked loop with `{**other, **self}`) dropped its self
time from 38.4s to 0.34s, and bootstrap fell from 10m to 4m.

When in doubt:

```bash
python -m cProfile -o /tmp/prof.out my_script.py
python -c "import pstats; pstats.Stats('/tmp/prof.out').sort_stats('tottime').print_stats(40)"
```

The `tottime` (self time, exclusive of callees) column is the right
sort. `cumtime` is misleading for recursive paths like `Lazy.get`.

### 3. The "facade" pattern decouples API from implementation

Hydra's collection types live under `hydra.python.util/` and are *imported
by name from generated code*. Generated Python emits
`ConsList.of(1, 2, 3)`, `PersistentMap.from_pairs(...)`, etc. The class
contract — what methods exist, what types they return, what abstract base
class they implement — is the API surface; the underlying storage is an
implementation detail.

This let us swap the storage three times during the investigation
(RB-tree → `immutables.Map` HAMT → native `dict`/`frozenset`/`tuple`)
without touching a single line of generated code or any DSL source.
**This is a pattern other Hydra hosts should adopt.** Java already has
`hydra.util.PersistentMap` / `Lazy` / etc. as named classes; Scala/Lisp
should follow.

### 4. Eager Python evaluation kills lazy `let`

Hydra DSL's `let` bindings have lazy semantics: a binding is evaluated only
on first use, and only once. The Python coder originally emitted them as
walrus assignments (`name := <expr>`), which Python evaluates **eagerly**.
For a pattern like:

```haskell
"x" <~ expensive_computation $
maybes.maybe noValueExpr withValueExpr (var "x")
```

Eager emission caused `expensive_computation` to run unconditionally even
when `maybes.maybe` selected `noValueExpr`. With recursive nesting this
became exponential — typed inference of `hydra.python.utils` (67 defs, ~21
levels of nesting) didn't terminate in 5 hours.

The fix was a one-shot memoizer (`hydra.python.util.Lazy`, ~45 lines of
Python mirroring `hydra.util.Lazy<>` from Hydra-Java), and a coder-side
emit pass that wraps inline let-binding RHS in `Lazy(lambda: <expr>)` and
use sites in `name.get()` (or `name()` via `Lazy.__call__`).

**Other hosts may need the same treatment** if they emit eager evaluation
for let bindings. Check by writing a deeply-nested `let-in-maybes` term and
confirming it doesn't blow up in inference.

### 5. Inline `cases _Enum` is a different code path from top-level `match`

The Python coder has two emit paths for `cases _Enum`:

- Top-level (statement context) → emits Python `match-case` statement.
  Works correctly.
- Inline (expression context, e.g., inside a let binding's RHS) → emits a
  conditional chain `result1 if check1 else result2 if check2 else ...` via
  `encodeUnionEliminationInline` → which builds a Hydra Python AST
  `Comparison(lhs, [(op, rhs)])` node → which goes through
  `comparisonToExpr` to render as Python source.

This means the **two paths can diverge silently**. We had a TODO in
`comparisonToExpr` that dropped the RHS list (`-- comparison operators are
rarely used in generated code`). It was harmless until Lazy emission
started forcing more `cases _Enum` dispatches into expression context, at
which point every inline enum dispatch silently picked the first branch.

Lesson: when adding emit paths for the same DSL construct, run the same
*test fixture* through both, not different fixtures.

### 6. CPython vs PyPy: not a single answer

The conventional wisdom was "use PyPy for Hydra-Python." After this
investigation:

- **Bootstrap (one-shot codegen)**: CPython slightly faster than PyPy
  (3m 54s vs 4m 5s). PyPy's JIT doesn't have time to warm up.
- **Long-running inference / term walks**: PyPy is **4–5× faster**.
  Per-call dispatch dominates and JIT amortizes it.
- **Microbenchmarks**: CPython's C `dict`/`frozenset` operations beat PyPy's
  pure-Python equivalents by 2–3×.

Both interpreters are first-class. The bootstrap demo prefers PyPy when
available because real workloads (development, deeper analyses) will run
inference repeatedly.

### 7. Be skeptical of cross-worktree perf comparisons

Some of the early perf data in this investigation crossed worktrees and
days. The "5m 43s pre-#362" baseline came from staging at one commit on
one day; "10m 34s post-#362" from feature_344 on another day with a
different test surface. The numbers were directionally right but not
rigorously comparable — extra modules in #344 inflated the post-#362
side, and a then-undiagnosed coder bug was producing partly-broken output
in the intermediate configs.

Re-measuring everything on the same day, same hardware, same worktree
(after creating scratch worktrees with `git worktree add`) was the only
way to get trustworthy numbers. Total wallclock to redo the comparison was
~2 hours; total wallclock spent debating shaky numbers was much more.
**For perf comparisons, always check whether your "before" and "after"
were measured the same way.**

### 8. Identity-based tests can lock you out of impl swaps

Two `test_cons_list.py` tests asserted `result is original` to verify
"structural sharing." Tuple-backed `ConsList` doesn't share — slicing a
tuple creates a new object. Once the impl swap happened, those tests
failed.

If a test checks `is` rather than `==`, ask whether the test cares about the
impl detail. In our case it didn't — the public contract is "drop produces
the right elements," not "drop reuses memory."
We changed the assertions to `==`. If a test exists primarily to police
implementation-side invariants, prefer to put it in a separate file marked
"impl details" so future swaps know what to update.

## Lessons for other Hydra hosts

The Python work did three things that other hosts can adopt:

### A. Use named collection-class facades, not raw native types

Generated code that uses `tuple(...)` or `dict(...)` directly is hard to
swap later. Generated code that uses `MyList.of(...)`/`MyMap.from_pairs(...)`
gives you a one-line indirection point.

In Python this lives in `hydra.python.util/`. In Java it's
`hydra.util.PersistentMap`/`hydra.util.ConsList`. Scala and Lisp should
follow the same pattern when they extend their kernels.

### B. Emit lazy let bindings or accept exponential blowup

If your host language has eager binding semantics (Python walrus, Scala
`val`, Java effectively-final), and Hydra's DSL has lazy let semantics,
you have a mismatch. The mismatch is invisible until a term is deeply
nested under `maybes.maybe` or `cases _Either`, at which point inference
stops terminating.

The fix is small (a one-shot memoizer class plus a coder-side wrapping
pass) and the resulting overhead is negligible compared to what it
prevents. If your host doesn't already have `Lazy<T>`, add it.

### C. Profile when the host hits a wall

Every Hydra host targeting non-trivial terms will eventually hit a perf
wall during bootstrap or inference. The right first move is **always**
to profile, not to optimize the obvious-looking part of the code. The
obvious part is rarely the cost; the cost is some library function being
called 100M times because of an O(n × k) algorithm hiding inside what
looks like an O(n) loop.

In Python: `cProfile` + `pstats`.
In Java: VisualVM or async-profiler.
In Haskell: `+RTS -p -hd`.
In Scala: VisualVM.
In Lisp: dialect-specific (`time` macro is the floor).

## Open follow-ups

Things this investigation noticed but didn't pursue:

- **Selective Lazy emission.** Site 3 v3 emits `Lazy(...)` for *all* inline
  let bindings, including those whose RHS is a literal or a simple
  variable reference. Skipping Lazy for trivially-cheap RHS would shave
  some allocator pressure. ROI estimate: small (Lazy machinery is ~12% of
  inference profile, half of which is the actual lazy semantics we want).

- **Native-tuple lib functions.** `hydra.lib.lists.map` etc. currently
  return `ConsList.from_iterable(...)`. Returning bare `tuple(...)` would
  skip an allocation per call. But ~1440 call sites in the generated code
  use `ConsList`-specific methods like `.head`/`.tail`/`.is_empty`, so
  this would require a coordinated codegen change.

- **PyPy bench parity.** PyPy is +18% slower on the microbench than
  pre-#362. Probably acceptable as the price of native dict/frozenset on
  CPython, but if PyPy bench matters, an interpreter-aware
  `PersistentMap` impl (HAMT on PyPy, dict on CPython) is possible.
  The complexity probably isn't worth it.

## Pointers to the actual code

- Lazy class: `heads/python/src/main/python/hydra/python/util/lazy.py`
- Persistent collection facades: `heads/python/src/main/python/hydra/python/util/{cons_list,persistent_map,persistent_set}.py`
- Lazy emission in coder: `packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Coder.hs::encodeTermAssignment`
- comparisonToExpr serde fix: `packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Serde.hs::comparisonToExpr`
- `encodeUnionEliminationInline` enum-attr fix: `packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Coder.hs:1969+`
