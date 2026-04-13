# Hydra (bootstrapped)

This directory contains a bootstrapped copy of Hydra, generated from a
language-independent JSON representation by a non-Haskell host. The generated
Java code is combined with static resources (hand-written libraries and test
infrastructure) to form a self-contained, buildable project.

## Structure

- `src/main/java/` -- Generated main modules (from JSON)
- `src/test/java/` -- Generated test modules (from JSON)
- `src/main/java/`     -- Hand-written source files (primitives, DSL, etc.)
- `src/test/java/`     -- Test harness

## Prerequisites

- JDK 11+ and [Gradle](https://gradle.org/) (via the included wrapper)

## Running the tests

```
./gradlew test
```

This builds the library and runs the full test suite. All tests should pass,
confirming that the bootstrapped code is functionally equivalent to the
Haskell-hosted original.
