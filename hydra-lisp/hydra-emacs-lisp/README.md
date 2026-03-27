# Hydra Emacs Lisp

Hydra implementation for Emacs Lisp, generated from the Hydra kernel via the Lisp coder.
This is one of four Lisp dialects supported by
[Hydra-Lisp](https://github.com/CategoricalData/hydra/tree/main/hydra-lisp).

## Quick start

```bash
cd hydra-emacs-lisp
emacs --batch --eval '(setq max-lisp-eval-depth 10000 max-specpdl-size 10000)' \
  -l src/main/emacs-lisp/hydra/loader.el \
  --eval '(hydra-load-gen-main)' \
  --eval '(hydra-set-function-bindings)' \
  --eval '(hydra-load-prims-and-libraries)' \
  --eval '(hydra-load-gen-test)' \
  --eval '(hydra-set-function-bindings)' \
  -l src/test/emacs-lisp/hydra/test_runner.el \
  --eval '(hydra-ensure-test-graph)' \
  --eval '(hydra-run-tests)'
```

## Test results

**2170 passed, 8 failed, 3 skipped** (99.6% of non-skipped tests)

| Category | Pass | Fail | Skip |
|----------|------|------|------|
| Evaluation (all primitives + reduction) | 919 | 0 | 0 |
| Inference | 347 | 0 | 3 |
| Type checking | 332 | 2 | 0 |
| Rewriting (incl. hoisting, fold, rewrite) | 264 | 0 | 0 |
| Eta expansion | 49 | 0 | 0 |
| JSON (parsing, writing, coder, roundtrip, decode) | 127 | 0 | 0 |
| Sorting, substitution, unification | 61 | 0 | 0 |
| Annotations | 34 | 3 | 0 |
| Formatting, serialization | 28 | 0 | 0 |
| String/unicode edge cases | 0 | 2 | 0 |
| Float/math precision | 0 | 1 | 0 |
| Carriage return (coder issue) | 7 | 0 | 0 |
| **Total** | **2170** | **8** | **3** |

Remaining 8 failures:
- 3 `getTermDescription` — annotation cache cannot intercept byte-compiled
  closure references to `deannotate_term`
- 2 type checking — alpha-equivalence mismatch in reconstructed types
  (same 2 tests that fail in Common Lisp)
- 2 carriage return — coder generates `\n` instead of `\r` in EL test data
- 1 `asinh` — last-digit rounding difference

**Timing**: ~57 seconds with byte-compilation, estimated 5-10 seconds with
native ahead-of-time compilation (see below).

## Architecture

### Loader (`loader.el`)

The loader handles the impedance mismatch between the generated Lisp-1 code
(Scheme-style, single namespace) and Emacs Lisp's Lisp-2 semantics (separate
function and value namespaces):

- **`funcall` insertion**: Detects lambda-bound and let-bound variables used
  in function position and wraps with `funcall`
- **`letrec` transform**: Converts Scheme's `letrec` to either `cl-labels`
  (when the bound name is only called, not passed as a value) or mutable
  cells with `setcar` (when the name is passed as a first-class value)
- **`hydra-defstruct`**: Replaces `cl-defstruct` with alist-based records
  using keyword keys, matching the coder's representation
- **Quoted alist unquoting**: Detects quoted alists containing unevaluated
  forms and converts to `list`/`cons` expressions

### Performance: byte-compilation

The generated Hydra code is heavily functional — every multi-argument function
is curried, producing intermediate closures on each call. In interpreted Emacs
Lisp, this curried-call pattern is ~20x slower than a direct function call.

Three performance strategies were evaluated:

| Strategy | Speedup | Portability | Effort |
|----------|---------|-------------|--------|
| **Byte-compile loaded values** | ~20-90x | All Emacs | 10 lines |
| Native compilation | ~50-100x | Emacs 28+ with libgccjit | 10 lines |
| Different code generation (defun) | ~2x | All Emacs | Major coder refactor |

**Byte-compilation was selected** because:

1. It gives 20-90x speedup on curried calls, which dominate runtime
2. It works on every Emacs version (byte-compilation has existed since Emacs 18)
3. It requires no changes to the code generator — just one call after loading
4. Without it, the test suite would take 60+ minutes (pure interpretation)

The loader calls `hydra-byte-compile-all` after loading gen-main, which
byte-compiles all `hydra_*` function values.

**Current performance**: The full test suite runs in ~57 seconds with
byte-compilation. This is ~8x slower than SBCL (Common Lisp, 7s) and Guile
(Scheme, 8s), both of which compile to native code or use JIT. The remaining
gap is inherent to Emacs's bytecode interpreter — the heavy tests (inference,
type checking) involve deep recursive computations that benefit greatly from
native compilation.

**Native compilation** (Emacs 28+ with `libgccjit`) would close this gap.
However, `native-compile` compiles each function individually to a shared
library, taking ~0.8s per function × 1369 functions = ~17 minutes. This is
impractical at load time. The proper approach is to pre-compile the `.el`
source files using Emacs's ahead-of-time native compilation, which produces
cached `.eln` files that load instantly on subsequent runs.

To install Emacs with native compilation on macOS:

```bash
brew install d12frosted/emacs-plus/emacs-plus@30
```

On Linux, most recent distribution packages include native compilation by
default (e.g., `emacs-nativecomp` on Arch, `emacs-pgtk` on Ubuntu 23.04+).

With native-comp Emacs, `load` auto-compiles `.el` files to `.eln` on first
use and caches them. Set `native-comp-deferred-compilation` to `t` (default)
and the first run will be slow but subsequent runs will benefit from native
code. Alternatively, batch-compile ahead of time:

```bash
EMACS=/usr/local/Cellar/emacs-plus@30/30.2/Emacs.app/Contents/MacOS/Emacs
find src/gen-main -name '*.el' | xargs -P4 -I{} $EMACS --batch -f batch-native-compile {}
```

### Hand-written files

- `src/main/emacs-lisp/hydra/lib/` — 13 native library implementations
  (chars, eithers, equality, lists, literals, logic, maps, math, maybes,
  pairs, sets, strings, libraries)
- `src/main/emacs-lisp/hydra/prims.el` — TermCoder constructors and
  primitive builders with proper type schemes
- `src/main/emacs-lisp/hydra/loader.el` — Loading and transformation
- `src/test/emacs-lisp/hydra/test_runner.el` — Test suite runner

### Generated files

- `src/gen-main/emacs-lisp/hydra/` — 113 kernel modules + extras
- `src/gen-test/emacs-lisp/hydra/test/` — 47 test modules
