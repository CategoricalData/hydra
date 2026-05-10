# Lazy fix design: Coder.hs edits

This document captures the line-level changes I plan to make to
`packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Coder.hs`
to replace the eager-walrus + `@lru_cache(1)` thunk patterns with a
single `hydra.python.util.Lazy`-based approach. To be reviewed before
editing.

## Java reference (correct shape we're matching)

From `dist/java/hydra-kernel/.../Variables.java:212` and surrounding:

```java
// Definition site:
hydra.util.Lazy<Map<Name, Name>> subst =
    new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(sb.get()));

// Use site:
subst.get()
```

Same `Lazy<>` mechanism for **both** let-bindings and zero-arg memoized
helpers. Python should follow the same pattern.

## Edit sites in Coder.hs

### Site 1: `lruCacheDecorator` (lines 2631-2638) — REMOVE

This emits `lru_cache(1)` as a decorator attribute. In the Lazy world
there's no decorator at all — the function body becomes `Lazy(...)`
construction. Definition is unused after the other edits land.

**Action**: delete (or leave as dead code initially; remove after
verification).

### Site 2: `makeThunk` (lines 2681-2691) — REWRITE

Currently emits: `lru_cache(1)(lambda: pbody)()` — i.e., calls the
0-arg lru_cache-wrapped lambda. Used by the walrus path at line 885.

**New emission**: `Lazy(lambda: pbody)` — note: the **call** (`()`)
goes away here; the result is the Lazy object itself. The `.get()`
happens at the use site (Site 4).

```haskell
makeThunk :: TTermDefinition (Py.Expression -> Py.Expression)
makeThunk = def "makeThunk" $
  doc "Create a Lazy(lambda: ...) wrapping the given expression for memoization" $
  "pbody" ~>
    PyUtils.functionCall @@
      (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "Lazy") @@
      list [wrapInNullaryLambda @@ var "pbody"]
```

### Site 3: `functionDefinitionToExpr` (lines 1178-1182) — REWRITE

Currently, when the function being defined is zero-arg (`isThunk`),
adds `@lru_cache(1)` decorator. The function body remains as-is, and
use sites emit `name()` to call it.

**Tricky**: the `def`-level form is at *statement* level, not
expression level. `Lazy(lambda: ...)` is an expression. So this isn't
a like-for-like swap — we'd need to convert the zero-arg `def name(): ...`
into an assignment statement `name = Lazy(lambda: ...)`. That's a
larger structural change in this function.

**Two options**:

**(a) Conservative**: leave `def`-form thunks alone for now (keep the
`@lru_cache(1)` decorator on `def`s). Only convert the walrus path
(Site 2 above). The cliff is in the walrus path (`encodeBindingAsAssignment`
at line 885), so this fixes the cliff and leaves the lru_cache-on-def
bloat for a follow-up.

**(b) Full**: convert zero-arg `def`s to `name = Lazy(lambda: <body>)`
assignments. Body of the original `def` becomes the lambda body. This
is the structurally-correct match to Java but requires the body to be
an *expression*. If the original body is multi-statement (which it
usually is for a generated `def`), we'd need to either inline-flatten
to an expression (hard) or wrap in a helper function and Lazy-wrap the
helper.

**Recommendation: start with (a).** The cliff is the prize; lru_cache
bloat is ~6-7%. (a) is a small, well-scoped change. (b) is its own
project.

If we go with (a), this edit site is *unchanged* — we just leave the
`@lru_cache(1)` decorator path intact for `def`s.

### Site 4: `encodeVariable` use-site rewrite (lines 2126 and 2173) — REWRITE

These are the two cases where a graph-element variable reference is
emitted as `asFunctionCall` (= `name()`). They correspond to the
post-thunking "this binding was thunked" detection logic. With Lazy,
the use site needs to emit `name.get()` instead of `name()`.

**Currently** (line 2126 path, simplified):
```haskell
Logic.ifElse (Logic.and (...) (Logic.not (var "elTrivial1")))
  (right $ var "asFunctionCall")    -- emits name()
  (...)
```

**New**:
```haskell
Logic.ifElse (Logic.and (...) (Logic.not (var "elTrivial1")))
  (right $ lazyDotGet @@ var "asVariable")    -- emits name.get()
  (...)
```

Where `lazyDotGet` is a small helper:
```haskell
-- | Wrap a Py.Expression in a `.get()` call (for Lazy use sites)
lazyDotGet :: TTermDefinition (Py.Expression -> Py.Expression)
lazyDotGet = def "lazyDotGet" $
  doc "Wrap an expression in a .get() method call (for Lazy thunks)" $
  "expr" ~>
    PyUtils.functionCall @@
      (PyUtils.attributeAccess @@ (PyUtils.pyExpressionToPyPrimary @@ var "expr") @@ string "get") @@
      list ([] :: [TTerm Py.Expression])
```

Apply at line 2126 and line 2173.

(I'll need to find/use the right `attributeAccess` helper — TBD on
exact API; this might already exist as `Py.AttributeRef` or similar.)

### Site 5: Imports — FOLLOW-UP

Wherever `from functools import lru_cache` is emitted in generated
Python files, replace with `from hydra.python.util import Lazy` (and
keep `lru_cache` only when option (b) is taken later, since `def`-form
thunks still use it under option (a)).

Under option (a), `lru_cache` is still emitted by Site 3, so the
import stays. New import for `Lazy` is needed wherever Site 2 fires
(walrus thunks via `makeThunk`).

Where does the `lru_cache` import get added? Line 2757:
```haskell
condImportSymbol @@ string "lru_cache" @@ ...
```

Need to add a parallel `condImportSymbol @@ string "Lazy" @@ ...` for
`Lazy`, gated on a new `usesLazy` metadata flag.

This means new metadata plumbing similar to `usesLruCache` — a
`usesLazy` field on `PythonModuleMetadata`, set whenever `makeThunk` is
emitted, propagated through all the metadata builders that already
plumb `usesLruCache` (lines 2880-3443 — many `>>: project ... usesLruCache`
instances).

**Note**: this metadata plumbing is the most tedious part. ~20
parallel additions, all mechanical.

## Sequence I propose

1. Land `Lazy.py` + tests (DONE — 5 tests pass).
2. Site 4 use-site rewrite + Site 2 `makeThunk` rewrite + new
   `lazyDotGet` helper. **Site 1 (lruCacheDecorator) and Site 3
   (functionDefinitionToExpr) untouched** under option (a).
3. Add `usesLazy` metadata + import emission for `Lazy`.
4. Sync. Verify utils n=7 byte-eq still passes (or if not, debug the
   semantic mismatch). If sync fails, the issue is likely in step 3
   (missing import).
5. Re-measure n=7 / n=16 / n=67 plain timing.

## Scope risk flags (the "ping if larger than expected")

- **Step 2's `lazyDotGet` helper**: the exact `attributeAccess` API in
  `Hydra.Dsl.Python.Helpers` / `Hydra.Dsl.Python.Syntax` needs to be
  located. If it doesn't exist, I'll need to construct an `attribute`
  access expression manually — small but unfamiliar code.
- **Step 3 metadata plumbing**: 20 mechanical additions. Tedious but
  straightforward; just want to flag that it's bulky.
- **Use-site at line 2126 vs 2173**: I assume both correspond to
  thunked-binding references. If line 2173 actually corresponds to
  something else (e.g. a distinct typed-binding case that ISN'T thunked),
  changing it to `.get()` would break correctness. Need to verify by
  reading the surrounding code more carefully or by experiment.
- **Walrus assignment to a Lazy**: `pyName := Lazy(lambda: pbody)` is
  syntactically fine but I want to confirm pyright doesn't complain
  about the resulting `Sequence[T]` vs `Lazy[T]` type mismatch
  somewhere downstream.

## Decision points for Josh before editing

1. **Option (a) vs (b)** for `def`-form thunks: keep `@lru_cache(1)`
   on `def`s (cliff fix only) or convert all to `Lazy = ...`
   assignments (full fix)?
2. **Helper naming**: `lazyDotGet` — better name?
3. **OK to leave `lruCacheDecorator` definition in place as dead
   code**, removing in a follow-up cleanup commit, vs deleting now?

---

## Update after sampling generated `rewriting.py`

All `@lru_cache(1) def name(): return <expr>` bodies in
`dist/python/.../rewriting.py` are structurally **single-return**
(even multi-line ones are just `return <expr-formatted-across-lines>`).

This means option (b) — fully replace `@lru_cache(1)` `def`s with
`name = Lazy(lambda: <expr>)` assignments — is *trivially* supportable;
no statement-flattening required.

Updated unified emission:

| Form | Before | After |
|---|---|---|
| Walrus thunk | `(name := lru_cache(1)(lambda: e)())` | `(name := Lazy(lambda: e))` |
| Walrus use | `name()` | `name.get()` |
| Def thunk | `@lru_cache(1)\ndef name(): return e` | `name = Lazy(lambda: e)` |
| Def use | `name()` | `name.get()` |
| Import | `from functools import lru_cache` | `from hydra.python.util import Lazy` |

Both forms collapse to the same `Lazy(lambda: e)` / `name.get()`
pattern. This matches Java's emission exactly.

## Revised plan (option b, full Lazy fix)

1. ✅ `Lazy.py` + tests (DONE).
2. Rewrite `makeThunk` (Site 2) to emit `Lazy(lambda: pbody)` —
   this handles the walrus path.
3. Rewrite `functionDefinitionToExpr` thunk path (Site 3) to emit
   a `name = Lazy(lambda: <body-expr>)` assignment statement instead
   of a decorated `def`. Requires extracting the single return
   expression from the function body. Two sub-cases:
   - **(3a)** If the body is `return <expr>`, lift `<expr>` and emit
     the assignment. This is the common case (verified for `rewriting.py`).
   - **(3b)** If the body has prefix statements before the return
     (e.g. `let`-prelude), keep them with a small helper `def`. Need
     to check whether this case occurs. If yes, emit:
     ```python
     def _name_compute(): <prefix-stmts>; return <expr>
     name = Lazy(_name_compute)
     ```
4. Rewrite `encodeVariable` use sites (Site 4: lines 2126 and 2173)
   to emit `name.get()` via a new `lazyDotGet` helper.
5. Replace `lru_cache` import emission (Site 5) with `Lazy`. Add
   `usesLazy` metadata if needed; or drop the conditional and always
   import `Lazy` when the module has any thunked binding.
6. Delete `lruCacheDecorator` (Site 1) — no longer used.
7. Sync. Diagnose any breakage.
8. Re-measure n=7/n=16/n=67 plain timing.

## Key risk surface for option (b)

The structural rewrite at Site 3 — converting a `def` to an
assignment — touches the **statement** representation, not just
expressions. Need to check what `functionDefinitionToExpr` returns
at the type level (`Py.Statement`?), and how its callers handle the
result. If the surrounding context expects exactly a `def`-statement
(e.g., for naming purposes), it might need additional adjustments.

I'll investigate this carefully before editing.

---

## Critical insight after deeper read of `makeThunk` (line 2681-2691)

The current `makeThunk` emits:
```
lru_cache(1)(lambda: pbody)()
```
**With a trailing `()` — i.e., it forces evaluation immediately.** The
cache only helps for *subsequent* calls (none of which happen since
the caller assigns the result to a walrus once). So the current
"thunk" isn't a thunk at all — it's eager evaluation with a useless
LRU cache wrapper.

This explains the cProfile finding mechanically:
- `no_type := lru_cache(1)(lambda: <pbody>)()` evaluates `<pbody>` eagerly.
- `<pbody>` contains `step(...)` recursive call.
- `step(...)` recurses into the next binding.
- That binding's `no_type :=` again evaluates eagerly.
- ... → 2^n explosion.

**Java's `new Lazy<>(() -> pbody)`** does NOT call `.get()` at
construction. The lambda is stored. Only forced when `.get()` is
called at a use site.

The Python coder needs the same shape: emit `Lazy(lambda: pbody)`
WITHOUT the trailing call. Then use-sites need `.get()`.

## Simplified plan (cliff fix, sites 2 and 4 only)

The cliff is fixed entirely by:

1. ✅ `Lazy.py` + tests.
2. Rewrite `makeThunk` to emit `Lazy(lambda: pbody)` — **drop the
   trailing call**. This is the entire cliff fix at the emission
   side.
3. Rewrite `encodeVariable` use sites to emit `name.get()` instead
   of `name()` for thunked-binding references (lines 2126, 2173).
4. Add `Lazy` import to emitted modules. May need `usesLazy`
   metadata, but easier: just always import alongside any other
   `hydra.python.util` import.
5. Sync. Verify utils n=7 byte-eq, then re-measure.

The `def`-form thunks (Site 3) are a SEPARATE issue from the cliff
— they cause constant-factor `update_wrapper` overhead but no
exponential explosion. **Defer Site 3 entirely.** The cliff fix is
~100 LOC of Coder.hs change, all in `makeThunk`/`encodeVariable`
detection logic + import emission. Half-day estimate stands.

After cliff fix lands and we measure, we can decide whether the
def-form lru_cache→Lazy migration is worth the additional structural
work.

---

## Final plan (after deeper investigation)

I dug deeper and discovered that `makeThunk` is **not** what produces the
cliff-causing walrus assignments. `encodeBindingAsAssignment` is called
with `allowThunking=false` from both call sites (lines 1508, 1540), so
`needsThunk` is always False for inline let-bindings. The walrus path
emits the binding's value RAW, not wrapped in any thunk. Java, in
contrast, **wraps every let-binding in `Lazy<>`**.

So the fix is structurally larger than I thought, but conceptually
clean:

### Definition site change

In `encodeBindingAsAssignment` (line 855-886), unconditionally wrap the
walrus value in `Lazy(lambda: ...)`. Replace line 885:

```haskell
"pterm" <~ (Logic.ifElse (var "needsThunk") (makeThunk @@ var "pbody") (var "pbody")) $
```

with:

```haskell
"pterm" <~ (makeLazy @@ var "pbody") $
```

where `makeLazy` is a new helper that emits `Lazy(lambda: pbody)`. The
`needsThunk` analysis becomes irrelevant — every binding is lazy.
(Trivial bindings like literals could skip the wrap as a small
optimization, but Java doesn't bother, and it's not load-bearing.)

### Use site change

In `encodeVariable` (line 2065+), the inline-vars branches at lines
2105 and 2152 (which emit `asVariable` for inline walrus references)
should emit `name.get()` instead. Since under the new scheme **every
inline let is Lazy**, this rewrite is unconditional in those branches.

Also lines 2126 and 2173: top-level thunked-graph-element references
that currently emit `asFunctionCall` (= `name()`) should emit
`name.get()` only if the binding is now wrapped in Lazy. **But Site 3
(the def-form `@lru_cache(1) def name(): return e`) is staying for
now**, so these top-level thunks still emit as call-form `name()`.
Lines 2126/2173 stay unchanged.

### Helper functions to add

```haskell
-- | Wrap an expression in `Lazy(lambda: ...)` for one-shot memoization.
makeLazy :: TTermDefinition (Py.Expression -> Py.Expression)
makeLazy = def "makeLazy" $
  doc "Wrap an expression in Lazy(lambda: ...) for one-shot lazy memoization" $
  "pbody" ~>
    PyUtils.functionCall @@
      (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "Lazy") @@
      list [wrapInNullaryLambda @@ var "pbody"]

-- | Wrap an expression in a `.get()` method call (for Lazy use sites).
lazyDotGet :: TTermDefinition (Py.Expression -> Py.Expression)
lazyDotGet = def "lazyDotGet" $
  doc "Wrap an expression in a .get() method call (for Lazy unwrap)" $
  "expr" ~>
    PyUtils.functionCall @@
      (PyUtils.attributeAccess @@ (PyUtils.pyExpressionToPyPrimary @@ var "expr") @@ string "get") @@
      list ([] :: [TTerm Py.Expression])
```

(`PyUtils.attributeAccess` may need to be located/created; if not, use
`Py.AttributeRef` constructor directly.)

### Imports

Add `from hydra.python.util import Lazy` whenever a module uses inline
let-bindings, which is **most modules**. Probably easiest: always
import `Lazy` alongside any other `hydra.python.util` import (since
many modules already use ConsList/PersistentMap/PersistentSet from
that package). Add to the `condImportSymbol` machinery near line 2757.

`makeThunk` and `lruCacheDecorator`: leave them alone for this change.
They're used by Site 3 (the def-form) which we're keeping.

### Edit summary

| File | Change |
|---|---|
| `heads/python/.../hydra/python/util/lazy.py` | NEW (DONE) |
| `heads/python/.../hydra/python/util/__init__.py` | Export Lazy (DONE) |
| `heads/python/.../test/python/test_lazy.py` | NEW (DONE) |
| `Environment.hs` (Sources/Python) | Add `usesLazy` boolean to `PythonModuleMetadata` |
| `Coder.hs` line 885 | `pterm <~ makeLazy @@ pbody` |
| `Coder.hs` ~line 2630 | NEW `makeLazy` and `lazyDotGet` definitions |
| `Coder.hs` line 2105 | wrap `right $ var "asVariable"` in `lazyDotGet` |
| `Coder.hs` line 2152 | same as 2105 |
| `Coder.hs` line ~191 (toDefinition list) | add `makeLazy`, `lazyDotGet` |
| `Coder.hs` import emission (line 2774-2780) | add `Lazy` symbol gated on `usesLazy` |
| `Coder.hs` ~26 places | mirror `usesLruCache` plumbing for `usesLazy` |
| `Coder.hs` `emptyMetadata` (line 502) | add `usesLazy >>: false` |
| `Coder.hs` add `setMetaUsesLazy` helper | analogous to `setMetaUsesLruCache` |
| `Coder.hs` `gatherMetadata` | set `usesLazy = true` when emitting walrus binding |

Estimated ~100 lines of additions + edits. Half-day to land + sync +
verify. Most risk is in the import emission machinery — I haven't read
that fully yet.

### What stays unchanged

- `makeThunk` / `lruCacheDecorator` / Site 3 def-form thunks: untouched.
  Constant-factor `update_wrapper` overhead remains. Defer that fix.
- `encodeVariable` lines 2126/2173 (graph-element call paths): unchanged.
- `inlineVars` mechanism: unchanged structurally; semantics shift to
  "names introduced as Lazy inline lets."

### Verification

1. After edits + sync: `pytest src/test/python/test_lazy.py -x` (already passes).
2. Quick smoke: import `dist/python/hydra-kernel/.../hydra/variables.py` and
   verify it parses (Python syntax).
3. Eyeball `dist/python/.../variables.py` `step` function — expect to see:
   ```python
   no_type := Lazy(lambda: (
     new_val := Lazy(lambda: rewrite_with_subst(...).get(),
     ...
   )),
   ...
   maybes.maybe((lambda: no_type.get()), (lambda ts: with_type(ts).get()), b.type_scheme)
   ```
4. Re-run `verify_module.py` for utils to check byte-eq against canonical
   JSON. Likely fails initially; debug.
5. Re-measure n=7 / n=16 / n=67 plain timing. Expected: n=7 ≈ 1-3s,
   n=67 ≈ 10-50s. (Pre-fix: 103s, doesn't terminate.)
