# Hydra-Common-Lisp

> **Note:** In the 0.15 layout, Hydra-Common-Lisp's hand-written runtime lives at
> [`heads/lisp/common-lisp/`](https://github.com/CategoricalData/hydra/tree/main/heads/lisp/common-lisp),
> and generated code lives at
> [`dist/common-lisp/hydra-kernel/`](https://github.com/CategoricalData/hydra/tree/main/dist/common-lisp/hydra-kernel).
> This directory is a stub kept for historical path compatibility.

Common Lisp is one of four Lisp dialects supported by
[Hydra-Lisp](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-lisp).

## Prerequisites

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)

## Running tests

From the repo root:

```bash
packages/hydra-lisp/bin/run-tests.sh common-lisp
```

Set `HYDRA_BENCHMARK_OUTPUT` to a file path to produce benchmark JSON output.

## Collections — known performance limitation

Hydra-Common-Lisp's `lib/maps.lisp` represents maps as **alists** and
`lib/sets.lisp` as **sorted lists**. Both give O(n) `insert`/`lookup`/
`delete`, which makes inference-style workloads (lots of incremental
inserts into the same map) quadratic in the map size. SBCL has native
`hash-table` (and `make-hash-table :test 'equalp` works for the structural
keys Hydra uses), so a CL persistent or amortized-O(1) map/set is
straightforward — it has just not been done yet. The
[Hydra-Java collection classes](../../hydra-java/README.md#collection-classes)
section describes the analogous fix landed for Java in #359; the same
principle applies here. This will be tracked in a follow-up issue, and
this section will be updated when the work lands.

## See also

- [Hydra-Lisp README](../README.md) — overview of all four Lisp dialects
- [Adding primitives](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/adding-primitives.md) —
  how to add new primitives across all implementations
