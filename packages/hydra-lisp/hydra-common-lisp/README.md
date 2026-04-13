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

## See also

- [Hydra-Lisp README](../README.md) — overview of all four Lisp dialects
- [Adding primitives](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/adding-primitives.md) —
  how to add new primitives across all implementations
