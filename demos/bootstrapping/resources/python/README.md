# Hydra (bootstrapped)

This directory contains a bootstrapped copy of Hydra, generated from a
language-independent JSON representation by a non-Haskell host. The generated
Python code is combined with static resources (hand-written libraries and test
infrastructure) to form a self-contained, buildable project.

## Structure

- `src/main/python/` -- Generated main modules (from JSON)
- `src/test/python/` -- Generated test modules (from JSON)
- `src/main/python/`     -- Hand-written source files (primitives, DSL, etc.)
- `src/test/python/`     -- Test harness

## Prerequisites

- Python 3.12+ and [pytest](https://docs.pytest.org/)

## Running the tests

```
pytest src/test/ src/test/
```

This runs the full test suite. All tests should pass, confirming that the
bootstrapped code is functionally equivalent to the Haskell-hosted original.
