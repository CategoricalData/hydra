# Hydra-Scheme

> **Note:** In the 0.15 layout, Hydra-Scheme's hand-written runtime lives at
> [`heads/lisp/scheme/`](https://github.com/CategoricalData/hydra/tree/main/heads/lisp/scheme),
> and generated code lives at
> [`dist/scheme/hydra-kernel/`](https://github.com/CategoricalData/hydra/tree/main/dist/scheme/hydra-kernel).
> This directory is a stub kept for historical path compatibility.

Scheme is one of four Lisp dialects supported by
[Hydra-Lisp](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-lisp).
The implementation targets R7RS Scheme and is primarily tested under Guile 3.x.

## Running tests

From the repo root:

```bash
packages/hydra-lisp/bin/run-tests.sh scheme
```

The runner uses Guile by default and falls back to chibi-scheme if Guile is not
installed.

## Performance: Scheme implementation choice

The generated Hydra code is standard R7RS and runs on any compliant
implementation. However, performance varies dramatically because Hydra's
functional style produces deeply curried closures — every multi-argument
function is `(lambda (a) (lambda (b) ...))`, creating intermediate closures
on every call.

Three R7RS implementations were evaluated:

| Implementation | Test time | Compilation | Availability |
|----------------|-----------|-------------|--------------|
| **Guile 3.x** | **~8s** | Bytecode + JIT | Linux (default), macOS (Homebrew) |
| Chez Scheme 10.x | Not tested* | Native code | macOS (Homebrew), Linux |
| chibi-scheme | ~5m | None (interpreter) | Everywhere (C only) |

**Guile was selected** because:

1. It gives ~40x speedup over chibi-scheme via its bytecode compiler and JIT
   (added in Guile 3.0).
2. It has good R7RS support (`--r7rs` flag) with only two small compatibility
   shims needed: `(scheme bytevector)` and `(srfi 151)`.
3. It is widely available — the default extension language on GNU/Linux systems,
   and installable via Homebrew on macOS.
4. It produces identical test results to chibi-scheme on the same R7RS code.
5. It auto-compiles and caches modules (`.go` files), so subsequent runs are fast.

**Why not Chez Scheme?** Chez compiles to native code and would likely be even
faster. However, Chez 10.3.0's R7RS `define-library` support has issues with
`(import ...)` inside library bodies. Worth revisiting as Chez's R7RS support
matures.

**Why not stay with chibi-scheme?** chibi-scheme is the R7RS reference
implementation with the strictest compliance, and all tests pass on it. However,
it is a pure interpreter with no compilation step, making it ~40x slower than
Guile for Hydra's curried-call-heavy workload. chibi-scheme remains a useful
conformance check.

## Architecture

### Hand-written files (in `heads/lisp/scheme/`)

- `src/main/scheme/hydra/lib/` — native library implementations
  (chars, eithers, equality, lists, literals, logic, maps, math, maybes,
  pairs, sets, strings, libraries)
- `src/main/scheme/hydra/prims.scm` — TermCoder constructors and primitive builders
- `src/test/scheme/hydra/annotation_bindings.scm` — hand-written annotation bindings
  used by the test graph
- `run-tests.scm` — entry point wrapper
- `run-tests.sh` — delegates to `packages/hydra-lisp/bin/run-tests.sh scheme`

### Generated files (in `dist/scheme/hydra-kernel/`)

- `src/main/scheme/hydra/` — kernel modules as R7RS libraries (`.sld` + `.scm`)
- `src/test/scheme/hydra/test/` — generated test modules

### Compatibility shims

Two small shim files in `heads/lisp/scheme/src/main/scheme/` provide libraries
that Guile lacks but chibi-scheme has natively:

- `scheme/bytevector.sld` — re-exports from Guile's `(rnrs bytevectors)`
- `srfi/srfi-151.scm` — re-exports bitwise operations from Guile's `(srfi srfi-60)`

These shims are harmless on chibi-scheme (which has its own `(scheme bytevector)`
and `(srfi 151)` and ignores the shim files).

## See also

- [Hydra-Lisp README](../README.md) — overview of all four Lisp dialects
