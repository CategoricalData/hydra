# Hydra-Lisp

Hydra-Lisp is the Lisp implementation of [Hydra](https://github.com/CategoricalData/hydra),
covering four dialects: Clojure, Scheme (R7RS), Common Lisp, and Emacs Lisp.

All four dialects share a single code generator (the Lisp coder) and serializer,
but each has its own bootstrapping head and runtime primitives.
Generated code lives in `src/gen-main/` and `src/gen-test/`;
hand-written primitives and test infrastructure live in `src/main/` and `src/test/`.

## Dialects

| Dialect | Directory | Runtime | Status |
|---------|-----------|---------|--------|
| Clojure | [hydra-clojure](hydra-clojure/) | Clojure CLI (`clojure`) | Complete |
| Scheme | [hydra-scheme](hydra-scheme/) | Guile 3.x (recommended), chibi-scheme | Complete |
| Common Lisp | [hydra-common-lisp](hydra-common-lisp/) | SBCL | Complete |
| Emacs Lisp | [hydra-emacs-lisp](hydra-emacs-lisp/) | Emacs 29+ (batch mode) | Complete |

## Running tests

Each dialect has a `run-tests.sh` script.
Run from the dialect's directory:

```bash
cd hydra-clojure && ./run-tests.sh
cd hydra-scheme && ./run-tests.sh
cd hydra-common-lisp && ./run-tests.sh
cd hydra-emacs-lisp && ./run-tests.sh
```

Set `HYDRA_BENCHMARK_OUTPUT` to a file path to produce benchmark JSON output.

## Code generation

Lisp code is generated from Hydra-Haskell.
To regenerate:

```bash
cd ../../heads/haskell
./bin/sync-lisp.sh
```

## Architecture

The Lisp coder generates code for all four dialects from a shared representation.
Dialect-specific differences (Lisp-1 vs Lisp-2 semantics, module systems,
naming conventions) are handled by dialect-specific backends within the coder.

Each dialect follows the standard Hydra code organization:

```
hydra-<dialect>/
  src/
    gen-main/   # Generated kernel code (never edit)
    gen-test/   # Generated test code (never edit)
    main/       # Hand-written primitives and runtime
    test/       # Hand-written test infrastructure
```

See the dialect-specific READMEs for details on test results, performance,
and implementation notes:
- [Hydra-Clojure README](hydra-clojure/README.md)
- [Hydra-Scheme README](hydra-scheme/README.md)
- [Hydra-Common-Lisp README](hydra-common-lisp/README.md)
- [Hydra-Emacs-Lisp README](hydra-emacs-lisp/README.md)
