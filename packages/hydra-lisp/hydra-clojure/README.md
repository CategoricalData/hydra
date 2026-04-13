# Hydra-Clojure

> **Note:** In the 0.15 layout, Hydra-Clojure's hand-written runtime lives at
> [`heads/lisp/clojure/`](https://github.com/CategoricalData/hydra/tree/main/heads/lisp/clojure),
> and generated code lives at
> [`dist/clojure/hydra-kernel/`](https://github.com/CategoricalData/hydra/tree/main/dist/clojure/hydra-kernel).
> This directory is a stub kept for historical path compatibility.

Clojure is one of four Lisp dialects supported by
[Hydra-Lisp](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-lisp).

## Prerequisites

- [Clojure CLI](https://clojure.org/guides/install) (`clojure` command)

## Running tests

From the repo root:

```bash
packages/hydra-lisp/bin/run-tests.sh clojure
```

Set `HYDRA_BENCHMARK_OUTPUT` to a file path to produce benchmark JSON output.

## See also

- [Hydra-Lisp README](../README.md) — overview of all four Lisp dialects
- [Adding primitives](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/adding-primitives.md) —
  how to add new primitives across all implementations
