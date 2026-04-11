# Hydra Scheme

Hydra implementation for R7RS Scheme, generated from the Hydra kernel via the Lisp coder.
This is one of four Lisp dialects supported by
[Hydra-Lisp](https://github.com/CategoricalData/hydra/tree/main/hydra-lisp).

## Quick start

### Guile (recommended)

```bash
cd hydra-scheme
guile --r7rs --no-auto-compile \
  -L src/gen-main/scheme -L src/main/scheme \
  -L src/gen-test/scheme -L src/test/scheme \
  -s run-tests.scm
```

### chibi-scheme

```bash
cd hydra-scheme
chibi-scheme \
  -I src/gen-main/scheme -I src/main/scheme \
  -I src/gen-test/scheme -I src/test/scheme \
  run-tests.scm
```

## Test results

2194 passed, 0 failed, 24 skipped.

## Performance: Scheme implementation choice

The generated Hydra code is standard R7RS and runs on any compliant
implementation. However, performance varies dramatically because Hydra's
functional style produces deeply curried closures — every multi-argument
function is `(lambda (a) (lambda (b) ...))`, creating intermediate closures
on every call.

Three R7RS implementations were evaluated:

| Implementation | Test time | Compilation | Availability |
|----------------|-----------|-------------|--------------|
| **Guile 3.x** | **8s** | Bytecode + JIT | Linux (default), macOS (Homebrew) |
| Chez Scheme 10.x | Not tested* | Native code | macOS (Homebrew), Linux |
| chibi-scheme | 5m 34s | None (interpreter) | Everywhere (C only) |

**Guile was selected** because:

1. It gives 40x speedup over chibi-scheme (8s vs 5m34s) via its bytecode
   compiler and JIT (added in Guile 3.0)
2. It has good R7RS support (`--r7rs` flag) with only two small compatibility
   shims needed: `(scheme bytevector)` and `(srfi 151)`
3. It is widely available — the default extension language on GNU/Linux systems,
   and installable via Homebrew on macOS
4. It produces identical test results to chibi-scheme on the same R7RS code
5. It auto-compiles and caches modules (`.go` files), so subsequent runs are fast

**Why not Chez Scheme?** Chez compiles to native code and would likely be even
faster (estimated 1-3s). However, Chez 10.3.0's R7RS `define-library` support
has issues with `(import ...)` inside library bodies — the generated `.sld`
files trigger "invalid context for definition" errors. Chez is where the Scheme
ecosystem is heading (it backs Racket, its author co-designed R6RS, and it was
open-sourced by Cisco in 2016), so this is worth revisiting as Chez's R7RS
support matures.

**Why not stay with chibi-scheme?** chibi-scheme is the R7RS reference
implementation with the strictest compliance, and all tests pass on it. However,
it is a pure interpreter with no compilation step, making it 40x slower than
Guile for Hydra's curried-call-heavy workload. For development and CI, 5+ minutes
per test run is impractical. chibi-scheme remains a useful conformance check —
if the code runs on chibi, it will run on any R7RS implementation.

## Bootstrapping

Hydra-Scheme can serve as a bootstrapping host, generating code for any target language
from the JSON kernel modules.
The bootstrap script is at `src/main/scheme/hydra/bootstrap.scm`.
Generating the full Python kernel takes approximately 6 minutes.

The bootstrap uses Guile-specific optimizations not available in portable R7RS:
- **vhash persistent data structures** (`(ice-9 vlist)`) for O(1) map and set operations,
  replacing the O(n) sorted alists used in the portable gen-main code
- **IEEE 754 float32 bytevector round-trip** (`(rnrs bytevectors)`) for exact float32 precision
- **Loader transformations** (`fix-letrec`, `fix-if-else`, `and`/`or` short-circuiting)
  to adapt generated code for eager evaluation

The `src/main/` libraries use vhash for performance.
The `src/gen-main/` libraries use portable alists so that standalone generated targets
work without Guile-specific modules.
The `sync-lisp.sh` script restores the alist gen-main versions after generation.

### Compatibility shims

Two small shim files in `src/main/scheme/` provide libraries that Guile lacks
but chibi-scheme has natively:

- `scheme/bytevector.sld` — re-exports from Guile's `(rnrs bytevectors)`
- `srfi/srfi-151.scm` — re-exports bitwise operations from Guile's `(srfi srfi-60)`

These shims are harmless on chibi-scheme (which has its own `(scheme bytevector)`
and `(srfi 151)` and ignores the shim files).

## Architecture

### Generated files

- `src/gen-main/scheme/hydra/` — 113 kernel modules as R7RS libraries (`.sld` + `.scm`)
- `src/gen-test/scheme/hydra/test/` — 47 test modules

### Hand-written files

- `src/main/scheme/hydra/lib/` — 13 native library implementations
  (chars, eithers, equality, lists, literals, logic, maps, math, maybes,
  pairs, sets, strings, libraries)
- `src/main/scheme/hydra/prims.scm` — TermCoder constructors and primitive builders
- `src/test/scheme/hydra/test_runner_body.scm` — Test suite runner
- `run-tests.scm` — Entry point
