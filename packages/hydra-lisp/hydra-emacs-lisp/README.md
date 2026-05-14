# Hydra-Emacs-Lisp

> **Note:** In the 0.15 layout, Hydra-Emacs-Lisp's hand-written runtime lives at
> [`heads/lisp/emacs-lisp/`](https://github.com/CategoricalData/hydra/tree/main/heads/lisp/emacs-lisp),
> and generated code lives at
> [`dist/emacs-lisp/hydra-kernel/`](https://github.com/CategoricalData/hydra/tree/main/dist/emacs-lisp/hydra-kernel).
> This directory is a stub kept for historical path compatibility.

Emacs Lisp is one of four Lisp dialects supported by
[Hydra-Lisp](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-lisp).

## Running tests

From the repo root:

```bash
packages/hydra-lisp/bin/run-tests.sh emacs-lisp
```

Set `HYDRA_BENCHMARK_OUTPUT` to a file path to produce benchmark JSON output.

## Architecture

### Loader

The loader (`heads/lisp/emacs-lisp/src/main/emacs-lisp/hydra/loader.el`) handles
the impedance mismatch between the generated Lisp-1 code (Scheme-style, single
namespace) and Emacs Lisp's Lisp-2 semantics (separate function and value
namespaces):

- **`funcall` insertion**: detects lambda-bound and let-bound variables used in
  function position and wraps with `funcall`
- **`letrec` transform**: converts Scheme's `letrec` to either `cl-labels`
  (when the bound name is only called, not passed as a value) or mutable cells
  with `setcar` (when the name is passed as a first-class value)
- **`hydra-defstruct`**: replaces `cl-defstruct` with alist-based records using
  keyword keys, matching the coder's representation
- **Quoted-alist unquoting**: detects quoted alists containing unevaluated forms
  and converts to `list`/`cons` expressions

### Performance: byte-compilation

The generated Hydra code is heavily functional — every multi-argument function
is curried, producing intermediate closures on each call. In interpreted Emacs
Lisp, this curried-call pattern is much slower than a direct function call.

The loader calls `hydra-byte-compile-all` after loading the generated modules,
which byte-compiles all `hydra_*` function values. This typically gives a
~20–90x speedup on curried calls and is the reason the test suite completes in
under a minute rather than an hour.

**Native compilation** (Emacs 28+ with `libgccjit`) closes most of the
remaining gap with SBCL and Guile. Use `batch-native-compile` to pre-compile
the generated `.el` files to cached `.eln` files:

```bash
EMACS=/path/to/emacs  # any Emacs with native-comp support
find dist/emacs-lisp/hydra-kernel/src/main/emacs-lisp -name '*.el' \
  | xargs -P4 -I{} $EMACS --batch -f batch-native-compile {}
```

### Performance: collections and lazy let

`lib/maps.el` and `lib/sets.el` are facades over Emacs' native
`make-hash-table :test 'equal`, with copy-on-write semantics on
mutating operations (insert/delete/union/alter/…) to preserve the
immutable contract Hydra expects. Iteration helpers (`to_list`,
`keys`, `elems`) sort entries via `generic-compare` so downstream
serialization stays deterministic. This replaces the prior
sorted-alist / sorted-list representation, which was O(n) per op and
made inference-style workloads quadratic in collection size.

`loader.el` also rewrites every non-trivial `(let ((x EXPR)) BODY)`
into a lazy thunk + `cl-symbol-macrolet` use-site form, so that the
RHS is evaluated at most once on first reference. Without this, the
Emacs Lisp coder's eager native lets cause deeply-nested kernel
inference code to re-evaluate sub-expressions exponentially — the
same blowup Python had pre-#344 and Common Lisp had pre-#360. The
transformation is gated on the body containing a conditional or
lambda; straight-line let chains stay as plain native lets to avoid
unnecessary overhead. Runtime helpers (`make-lazy`, `lazy-force`)
live in `heads/lisp/emacs-lisp/src/main/emacs-lisp/hydra/lazy.el`.

These two fixes together (#361, mirroring #359 for Java and #360 for
Common Lisp) make Hydra-Emacs-Lisp inference scale near-linearly with
workload size. See
[docs/history/emacs-lisp-collections-perf.md](../../../docs/history/emacs-lisp-collections-perf.md)
for the side-by-side numbers and investigation notes.

### Hand-written files (in `heads/lisp/emacs-lisp/`)

- `src/main/emacs-lisp/hydra/lib/` — native library implementations
  (chars, eithers, equality, lists, literals, logic, maps, math, maybes,
  pairs, sets, strings, libraries)
- `src/main/emacs-lisp/hydra/prims.el` — TermCoder constructors and primitive
  builders with proper type schemes
- `src/main/emacs-lisp/hydra/loader.el` — loading and transformation
- `src/test/emacs-lisp/hydra/test_runner.el` — test suite runner

## See also

- [Hydra-Lisp README](../README.md) — overview of all four Lisp dialects
