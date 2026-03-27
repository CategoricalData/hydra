# Hydra Clojure

Hydra implementation for Clojure, generated from the Hydra kernel via the Lisp coder.
This is one of four Lisp dialects supported by
[Hydra-Lisp](https://github.com/CategoricalData/hydra/tree/main/hydra-lisp).

## Prerequisites

- [Clojure CLI](https://clojure.org/guides/install) (`clojure` command)

## Quick start

```bash
cd hydra-clojure
./run-tests.sh
```

Or run directly:

```bash
cd hydra-clojure
clojure -M -m run-tests
```

Set `HYDRA_BENCHMARK_OUTPUT` to a file path to produce benchmark JSON output.

## Project structure

```
hydra-clojure/
  src/
    gen-main/clojure/hydra/   # Generated kernel modules (never edit)
    gen-test/clojure/hydra/   # Generated test modules (never edit)
    main/clojure/hydra/lib/   # Hand-written primitive implementations
    test/clojure/              # Test runner
  deps.edn                    # Clojure dependencies
  run-tests.sh                # Test runner script
```

### Hand-written primitives

15 native library implementations in `src/main/clojure/hydra/lib/`:
chars, eithers, equality, libraries, lists, literals, logic, maps, math,
maybes, pairs, preload, regex, sets, strings.

## See also

- [Hydra-Lisp README](../README.md) -- overview of all four Lisp dialects
- [Adding primitives](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/adding-primitives.md) --
  how to add new primitives across all implementations
