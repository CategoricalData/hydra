# Hydra Common Lisp

Hydra implementation for Common Lisp, generated from the Hydra kernel via the Lisp coder.
This is one of four Lisp dialects supported by
[Hydra-Lisp](https://github.com/CategoricalData/hydra/tree/main/hydra-lisp).

## Prerequisites

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)

## Quick start

```bash
cd hydra-common-lisp
./run-tests.sh
```

Or run directly:

```bash
cd hydra-common-lisp
sbcl --noinform --non-interactive --no-userinit \
  --load src/test/common-lisp/run-tests.lisp
```

Set `HYDRA_BENCHMARK_OUTPUT` to a file path to produce benchmark JSON output.

## Project structure

```
hydra-common-lisp/
  src/
    gen-main/common-lisp/hydra/   # Generated kernel modules (never edit)
    gen-test/common-lisp/hydra/   # Generated test modules (never edit)
    main/common-lisp/hydra/lib/   # Hand-written primitive implementations
    test/common-lisp/              # Test runner
  run-tests.sh                    # Test runner script
```

### Hand-written primitives

14 native library implementations in `src/main/common-lisp/hydra/lib/`:
chars, eithers, equality, libraries, lists, literals, logic, maps, math,
maybes, pairs, regex, sets, strings.

## See also

- [Hydra-Lisp README](../README.md) -- overview of all four Lisp dialects
- [Adding primitives](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/adding-primitives.md) --
  how to add new primitives across all implementations
