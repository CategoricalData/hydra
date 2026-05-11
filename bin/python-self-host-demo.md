# Python self-hosting demo (issue #344)

This directory contains a standalone driver that generates
`dist/json/hydra-python` from the **Python** DSL sources rather than the
Haskell ones, demonstrating that the Python head can self-host its own
coder.

## Components

| Script | Purpose |
|---|---|
| `bin/python-self-host-demo.py` | Driver: load Python sources → infer → emit JSON |
| `bin/compare-self-host.py`      | Byte-compare two `hydra-python` JSON trees |

The driver mirrors what `stack exec update-json-main` does for the Haskell
side, but starting from the parallel Python DSL sources at
`packages/hydra-python/src/main/python/hydra/sources/python/*.py` instead of
`packages/hydra-python/src/main/haskell/Hydra/Sources/Python/*.hs`.

## Quick start

```bash
# Run the Python self-host pipeline, write output to /tmp/hp-from-python:
uv --directory heads/python run python ../../bin/python-self-host-demo.py \
    --out-root /tmp/hp-from-python

# Byte-compare against the Haskell-generated canonical:
bin/compare-self-host.py
```

## What the driver does

```
1. Load kernel universe from dist/json/hydra-kernel/.
2. Import hydra.sources.python.{coder, environment, language, names,
   serde, syntax, testing, utils} and pull their module_.
3. infer_modules_given(ctx, bootstrap_graph(),
                       universe ∪ python_sources, python_sources).
4. Build a graph + schema_map.
5. For each inferred module, codegen.module_to_json(schema, m) →
   hydra/python/<m>.json.
```

## Current state (2026-05-11)

All 8 modules byte-identical (132/132 definitions):

```
      module  status            ours       canon     delta  diff lines
----------------------------------------------------------------------
       coder  BYTE-EQ        2632293     2632293        +0           0
 environment  BYTE-EQ           7975        7975        +0           0
    language  BYTE-EQ          18343       18343        +0           0
       names  BYTE-EQ          86744       86744        +0           0
       serde  BYTE-EQ         320747      320747        +0           0
      syntax  BYTE-EQ          74573       74573        +0           0
     testing  BYTE-EQ          48517       48517        +0           0
       utils  BYTE-EQ         329758      329758        +0           0
----------------------------------------------------------------------
Summary: 8 / 8 byte-identical
```

### Fixes applied to reach byte-equivalence

1. **DSL helpers** (`heads/python/.../meta/`): added missing literal
   helpers (`char`, `decimal`, `int_`, `uint8..uint64`) and
   `binary_to_bytes`.

2. **Python sources** — bigfloat removal (#330):
   dropped `Core.float_type_bigfloat`, `T.bigfloat()`, `showBigfloat`,
   `float32_to_bigfloat`, `float64_to_bigfloat`, the `_FloatValue_bigfloat`
   case, the `_FloatType_bigfloat` case, and the
   `_Literal_float`/`_FloatValue` sub-cases under `extendMetaForTerm`/
   `extendMetaForType`. Used `Float64`/`showFloat64`/`float32_to_float64`
   instead.

3. **Python sources** — module dependencies: added missing
   `hydra.validation` namespace to every source module's
   `KERNEL_TYPES_NAMESPACES`.

4. **_let_chain helpers**: kept `coder.py`'s `_let_chain` as cascading
   `let1` (matches Haskell's `<~`); added a separate `_lets_flat` for
   the rare flat-let case. In `serde.py`, kept `_let_chain` flat (all
   serde call sites map to Haskell `lets [...]`).

5. **Python coder updates from recent optimizations**:
   - `extendMetaForType._Type_function`: applied the exponential-recursion
     fix from commit `b7d29d9b4` — drop the redundant cod/dom recursion.
   - `encodeBindingAsAssignment`: unconditional `makeLazy` wrap instead
     of the old `if needsThunk then makeThunk else identity`.
   - `encodeApplicationInner`: added `inlineVars` binding and lazy-aware
     `lazyDotGet` path for inline-let references.
   - `encodeVariable`: added `asLazyCall` binding; routed inline-var
     references through `.get()`; in `has_typ_branch` for inline vars,
     bind `unwrapped = lazyDotGet asVariable`; in the empty-args branch
     for inline vars, return `lazyDotGet(asVariable)` (not bare
     `asVariable`).
   - `encodeTermAssignment`: added `topLevel` parameter (now 7 params)
     with `topLevel=false` for inner bindings and `topLevel=true` for
     definition emission; added the inner-zero-arg-thunk case
     (`name = Lazy(lambda: <body>)`).
   - `encodeTermInline._Term_list`: encode as `ConsList.of(items...)`.
   - `encodeTermInline._Term_map`: encode as
     `PersistentMap.of_entries((k, v), ...)`.
   - `encodeTermInline._Term_set`: encode as `PersistentSet.of(...)`.
   - `encodeType`: use `Sequence`/`Mapping`/`Set` for list/map/set names
     (instead of `frozenlist`/`FrozenDict`/`frozenset`).
   - `extendMetaForTerm`: added missing `_Term_list` and `_Term_set` arms.
   - `extendMetaForType`: added missing `_Type_set` arm.
   - `moduleStandardImports`: rewrote for the current import set —
     `collections.abc` now includes Mapping/Sequence/Set; added a new
     `hydra.python.util` block (ConsList, Lazy unconditionally,
     PersistentMap, PersistentSet); removed obsolete `FrozenDict` and
     `frozenlist` from `hydra.dsl.python`.

6. **Definition order**: reordered `_encode_float_value_*`,
   `_function_definition_to_expr`, and `_enum_variant_pattern` to match
   the Haskell `definitions` list (which preserves the registration
   order verbatim, not alphabetical).

## Timing

Numbers from runs on the 'feature_344_self_hosting_coders' branch,
branched off staging-tidy (= origin/main). Both paths re-infer the
same 8 hydra.python.* modules; the Haskell side runs in incremental
mode (277 kernel modules are cached) while the Python side does a
full pass since there is no per-module cache yet.

| Pipeline | Total | Inference | Universe load | JSON write |
|---|---:|---:|---:|---:|
| Haskell `update-json-main`, 8 dirty / 277 clean | **183s** | ~110s | (in-memory) | <5s |
| CPython `python-self-host-demo.py`, 8 modules full | **494s** | 429s | 35s | 30s |
| PyPy `python-self-host-demo.py`, 8 modules full    | **113s** | 85s  | 14s | 12s |

PyPy is about **~4.4× faster** than CPython and about **1.6× faster** than
Haskell incremental on this workload. CPython is about ~2.7× slower than
Haskell.

The Haskell pipeline timing is for "incremental inference of 8 dirty
modules against a pre-typed universe of 277 clean ones". A from-scratch
Haskell run (with no cache hit, full 285-module inference) would take
substantially longer — on the order of 10–15 minutes — but is not the
relevant comparison since the digest cache makes that path uncommon in
day-to-day work.

The Haskell pipeline benefits from:
- A digest cache that short-circuits when nothing has changed.
- Incremental inference that only re-types dirty modules against a
  pre-typed universe.
- A native-code Hindley-Milner inferencer.

The Python pipeline currently:
- Re-loads the universe from JSON on every invocation (35s).
- Always runs full inference over 8 modules; no caching.
- Uses the Python-self-hosted inferencer (PyPy would help here).

## Required fixes propagated to the heads

The Python head needed several small fixes for the sources to even
load. These also benefit any other consumer of the Python DSL helpers:

- `heads/python/src/main/python/hydra/dsl/meta/phantom_literals.py`:
  added `char`, `decimal`, `int_`, `uint8..uint64` helpers.
- `heads/python/src/main/python/hydra/dsl/meta/lib/literals.py`:
  added `binary_to_bytes`.
- `heads/python/bin/copy-kernel-runtime.sh`: must be re-run after any
  edit to `heads/python/src/main/python/hydra/dsl/` to propagate the
  fix into `dist/python/hydra-kernel/src/main/python/`.

## Files outside the heads

- `packages/hydra-python/src/main/python/hydra/sources/python/language.py`:
  dropped `Core.float_type_bigfloat`; added `hydra.validation` namespace.
- `packages/hydra-python/src/main/python/hydra/sources/python/syntax.py`:
  changed `Number.float` arm from `T.bigfloat()` to `T.float64()`.
- `packages/hydra-python/src/main/python/hydra/sources/python/serde.py`:
  used `showFloat64` instead of `showBigfloat`; flattened `_let_chain`;
  added `hydra.validation`.
- `packages/hydra-python/src/main/python/hydra/sources/python/coder.py`:
  dropped multiple bigfloat sub-cases; flattened `_let_chain`; added
  `hydra.validation`.
- `packages/hydra-python/src/main/python/hydra/sources/python/names.py`,
  `utils.py`, `testing.py`: added `hydra.validation`.
