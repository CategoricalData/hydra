# Hydra-Lisp

Hydra-Lisp is the Lisp implementation of [Hydra](https://github.com/CategoricalData/hydra),
covering four dialects: Clojure, Scheme (R7RS), Common Lisp, and Emacs Lisp.

All four dialects share a single code generator (the Lisp coder) and serializer,
but each has its own runtime primitives and test runner.

In the 0.15 layout:

- **Lisp coder DSL sources** (this package):
  [`packages/hydra-lisp/src/main/haskell/Hydra/Sources/Lisp/`](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-lisp/src/main/haskell/Hydra/Sources/Lisp).
- **Hand-written primitives and test infrastructure** for each dialect:
  [`heads/lisp/clojure/`](https://github.com/CategoricalData/hydra/tree/main/heads/lisp/clojure),
  [`heads/lisp/scheme/`](https://github.com/CategoricalData/hydra/tree/main/heads/lisp/scheme),
  [`heads/lisp/common-lisp/`](https://github.com/CategoricalData/hydra/tree/main/heads/lisp/common-lisp),
  [`heads/lisp/emacs-lisp/`](https://github.com/CategoricalData/hydra/tree/main/heads/lisp/emacs-lisp).
- **Generated kernel and tests** per dialect:
  `dist/clojure/hydra-kernel/`, `dist/scheme/hydra-kernel/`,
  `dist/common-lisp/hydra-kernel/`, `dist/emacs-lisp/hydra-kernel/`.
- **Shared test runner script** (used by all four dialects):
  [`packages/hydra-lisp/bin/run-tests.sh`](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-lisp/bin/run-tests.sh).

## Dialects

| Dialect     | Head dir                         | Runtime                                  | Status   |
|-------------|----------------------------------|------------------------------------------|----------|
| Clojure     | `heads/lisp/clojure/`            | Clojure CLI (`clojure`)                  | Complete |
| Scheme      | `heads/lisp/scheme/`             | Guile 3.x (recommended), chibi-scheme    | Complete |
| Common Lisp | `heads/lisp/common-lisp/`        | SBCL                                     | Complete |
| Emacs Lisp  | `heads/lisp/emacs-lisp/`         | Emacs 29+ (batch mode)                   | Complete |

## Running tests

The shared runner dispatches to each dialect. Run from the repo root:

```bash
packages/hydra-lisp/bin/run-tests.sh clojure
packages/hydra-lisp/bin/run-tests.sh scheme
packages/hydra-lisp/bin/run-tests.sh common-lisp
packages/hydra-lisp/bin/run-tests.sh emacs-lisp
```

Each dialect also has a thin `run-tests.sh` wrapper under `heads/lisp/<dialect>/` that
delegates to the shared script.

Set `HYDRA_BENCHMARK_OUTPUT` to a file path to produce benchmark JSON output.

## Code generation

Lisp code is generated from the Haskell head. To regenerate all four dialects:

```bash
heads/haskell/bin/sync-lisp.sh
```

To regenerate only some dialects:

```bash
heads/haskell/bin/sync-lisp.sh --dialects clojure,scheme
```

## Architecture

The Lisp coder generates code for all four dialects from a shared representation.
Dialect-specific differences (Lisp-1 vs Lisp-2 semantics, module systems,
naming conventions) are handled by dialect-specific backends within the coder.
