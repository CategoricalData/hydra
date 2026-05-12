# Hydra-Common-Lisp

> **Note:** In the 0.15 layout, Hydra-Common-Lisp's hand-written runtime lives at
> [`heads/lisp/common-lisp/`](https://github.com/CategoricalData/hydra/tree/main/heads/lisp/common-lisp),
> and generated code lives at
> [`dist/common-lisp/hydra-kernel/`](https://github.com/CategoricalData/hydra/tree/main/dist/common-lisp/hydra-kernel).
> This directory is a stub kept for historical path compatibility.

Common Lisp is one of four Lisp dialects supported by
[Hydra-Lisp](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-lisp).

## Prerequisites

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp).
  On Apple Silicon (arm64) macOS, install the native arm64 build via Homebrew
  (`brew install sbcl` from `/opt/homebrew`); the Intel-Homebrew (`/usr/local`)
  build runs under Rosetta translation and is 1.4× slower for inference and
  bootstrap workloads.

## Running tests

From the repo root:

```bash
packages/hydra-lisp/bin/run-tests.sh common-lisp
```

Set `HYDRA_BENCHMARK_OUTPUT` to a file path to produce benchmark JSON output.

## Collections

Hydra-Common-Lisp uses persistent maps and sets backed by Okasaki red-black
trees, mirroring the API/implementation split in
[Hydra-Java](../../hydra-java/README.md#collection-classes) and
[Hydra-Python](../../hydra-python/README.md#collections):

- **At the API level**, `lib/maps.lisp` and `lib/sets.lisp` expose the same
  `hydra_lib_maps_*` and `hydra_lib_sets_*` functions every other host
  implements. Callers do not see the underlying representation; they get
  and return values that behave like persistent maps and sets.
- **At the implementation level**, both libraries are backed by an Okasaki
  red-black tree (`rbnode` defstruct, `rb-insert`/`rb-lookup`/`rb-delete` in
  `maps.lisp`). `insert`, `lookup`, and `delete` are O(log n), and an
  `insert` allocates O(log n) new nodes along the root-to-leaf path rather
  than copying the whole collection. This is the Common-Lisp analogue of
  the Java collection-classes fix from #359 and the Python persistent-dict
  fix from #344.

Key comparison is structural and dispatches through `rb-cmp`: Hydra `Name`
records take a fast path on their interned string field, raw strings
compare lexicographically, otherwise `equal` and finally a `generic-compare`
fallback. This keeps the kernel's typical key shape (interned names) fast
while still handling the long tail of structural keys correctly. Where
ordered iteration matters (`hydra_lib_maps_keys`/`elems`/`to_list`, set
iteration), the tree's in-order traversal yields elements in sorted order
by construction.

Hydra-Common-Lisp has **no third-party runtime dependencies** beyond SBCL
itself.

## Lazy let bindings

The Hydra-Lisp coder emits `let` and `let*` natively, which Common Lisp
evaluates eagerly. Kernel functions commonly bind a value, then use it in
only one branch of a conditional or pass it to a lambda that may never run
it; eager evaluation makes the cost of unused branches compound, and on
inference workloads this manifests as exponential blowup in chain length.

`loader.lisp` rewrites generated `let` / `let*` forms into thunk-backed
`symbol-macrolet` bindings (see `lib/lazy.lisp` and the
`hydra-wrap-let-lazy` transform). Each binding's RHS becomes a `make-lazy`
thunk; references expand into a `lazy-force` call that evaluates the RHS
at most once on first use, caches the result by mutating the cell's tag,
and returns the cached value on subsequent reads. Wrapping is gated on the
body containing a conditional or a lambda, which catches the exponential
patterns without imposing thunk overhead on trivial helper functions.

This is the Common-Lisp analogue of Hydra-Python's
[`Lazy<>`](../../hydra-python/README.md) wrapper from #344, implemented at
the loader rather than the coder so that the same generated `.lisp` files
ship across all four Lisp dialects.

## Floating-point traps

`bootstrap.lisp` and `run-tests.lisp` call
`(sb-int:set-floating-point-modes :traps nil)`. Hydra's IEEE-754 semantics
require that operations like `(sqrt -1)` and `(log 0)` return quiet
NaN/Inf; native SBCL on Linux and on Apple Silicon raises
`FLOATING-POINT-INVALID-OPERATION` by default, and the kernel evaluates
such expressions during test data generation.

## Performance

After the collections and lazy-let work in #360, Common Lisp matches or
beats CPython on the kernel-test suite and on type inference. Indicative
numbers on a 10-core Apple Silicon machine running native arm64 SBCL
2.6.4 (collected 2026-05-12, branch `feature_360_common_lisp_collections`):

| Benchmark | Haskell | CPython | Common-Lisp |
|---|---:|---:|---:|
| Kernel-test suite, mean wall (5 reps) | 14.0s | 8.5s | **7.1s** |
| `linearChain` inference, n = 100 | 16.3s | 149s | 124s |
| `linearChain` scaling exponent k | (flat) | 1.111 | **1.062** |
| Bootstrap CL→Python (full path) | 68s | 305s (pypy3) | **45s** |

Inference workloads scale near-linearly: the lazy-let rewrite eliminated
the pre-#360 exponential cliff (k ≈ 2.06 → 1.06), and the red-black
collections eliminated the per-insert O(n) copy that pushed `Common-Lisp`
into a 56× total slowdown at n = 25 against the previous alist
representation. See `feature_360_common_lisp_collections-plan.md` (branch
only) for the full pre/post comparison.

## See also

- [Hydra-Lisp README](../README.md) — overview of all four Lisp dialects
- [Adding primitives](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/adding-primitives.md) —
  how to add new primitives across all implementations
