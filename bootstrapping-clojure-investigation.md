# Bootstrapping investigation: Lisp targets and Clojure host

Date: 2026-03-19
Branch: `integration`

## Summary

This document describes the work to add all four Lisp dialects (Clojure, Scheme, Common Lisp, Emacs Lisp)
as bootstrapping targets, and Clojure as a bootstrapping host. It covers what was implemented, what issues
were encountered, and what remains to be investigated.

## What was implemented

### Lisp dialect targets (all four)

All four Lisp dialects can now be generated as targets from any host language:

- **bootstrap-from-json (Haskell)**: Added `clojure`, `scheme`, `common-lisp`, `emacs-lisp` as valid
  `--target` values. Each dispatches to `moduleToLispDialect` with the appropriate dialect and file extension.

- **Python host**: Added `write_lisp_dialect()` to `generation.py` and Lisp target dispatch to `bootstrap.py`.
  The function wraps `module_to_lisp` with serialization (`program_to_expr` + `print_expr`) and file path
  generation (`namespace_to_file_path` with dialect-appropriate case convention).

- **Java host**: Added `writeLispDialect()` to `Generation.java` and Lisp target dispatch to `Bootstrap.java`.
  Same wrapping pattern as Python.

- **Clojure host**: The Clojure target uses a custom coder wrapper in `resolve-coder` that calls
  `module_to_lisp` with the `(:clojure nil)` dialect tag, serializes via `program_to_expr`, and computes
  file paths.

- **Bootstrapping demo scripts**: Created `setup-{clojure,scheme,common-lisp,emacs-lisp}-target.sh` and
  `test-{clojure,scheme,common-lisp,emacs-lisp}-target.sh`. Updated `bootstrap-all.sh` with environment
  checks, baseline comparison, and test dispatch for all four dialects.

- **Ext manifest**: Added Lisp coder modules (`hydra.ext.lisp.coder`, `hydra.ext.lisp.language`,
  `hydra.ext.lisp.serde`, `hydra.ext.lisp.syntax`) to `hydraBootstrapCoderModules` in `All.hs`.

- **Generated coder modules**: Synced Lisp coder modules to Java (`hydra-java/src/gen-main/`), Python
  (`hydra-python/src/gen-main/`), and Clojure (`hydra-lisp/hydra-clojure/src/gen-main/`).

### Clojure as a bootstrapping host

New hand-written files in `hydra-lisp/hydra-clojure/src/main/clojure/hydra/`:

- **`generation.clj`**: I/O wrapper for code generation. Provides `load-modules-from-json`,
  `read-manifest-field`, `generate-sources`, `bootstrap-graph`, `bootstrap-schema-map`, `parse-json-file`,
  `decode-module`. Uses `clojure.data.json` for JSON parsing and runtime symbol resolution via
  `ns-resolve 'clojure.core` (because generated symbols are globalized into `clojure.core` by the preload
  system after the `hydra.generation` namespace is compiled).

- **`bootstrap.clj`**: CLI entry point (`-main`). Parses args, loads the kernel via
  `preload/load-gen-main!`, loads coder modules via `load-coder-modules!`, then calls `generate-sources`.
  Supports `--target`, `--json-dir`, `--output`, `--kernel-only`, `--include-tests`, `--types-only`.

- **`invoke-clojure-host.sh`**: Bootstrapping demo script. If coder modules for the target language
  are not already present in `hydra-clojure/src/gen-main/`, generates them using the Haskell
  `bootstrap-from-json --include-coders`. Then runs `clojure -M -m hydra.bootstrap`.

## Verified bootstrapping paths

| Host | Target | Status | Notes |
|------|--------|--------|-------|
| Haskell | Clojure | Pass | 118 files via bootstrap-from-json |
| Python | Clojure | Pass | 118 files, 2194 tests pass, 0 fail, 3 skip |
| Clojure | Clojure | Pass | 118 files in ~33s, 2194 tests pass, 0 fail, 3 skip |
| Java | Clojure | Compiles | Code added, Java compiles and tests pass; not yet run in demo |
| Clojure | Python | Functional but slow | ~28s per term module; see performance section below |
| Clojure | Haskell/Java | Not tested | Same coder performance concern applies |

## Issues encountered and resolved

### 1. Haskell target: missing generated DSL modules

The `setup-haskell-target.sh` script copies hand-written source files from `hydra-haskell/src/main/`
and selected directories from `hydra-haskell/src/gen-main/` (Ext, Sources/Decode, Sources/Encode).
But the hand-written `Hydra.Dsl.Meta.*` modules import generated DSL modules (`Hydra.Dsl.Core`,
`Hydra.Dsl.Graph`, `Hydra.Dsl.Context`) from `gen-main/haskell/Hydra/Dsl/`, which was not being
copied.

**Fix**: Added copy of `Hydra/Dsl/` directory from baseline `gen-main` to the setup script.

### 2. Haskell target: missing `Hydra.Dsls` module

`Hydra.Dsls` is generated separately from `mainModules` (excluded from the manifest due to stack
overflow during generation). The hand-written `Generation.hs` imports it, and `Generation.hs` is
needed by test infrastructure (`TestSuiteSpec.hs`, `TestUtils.hs`).

**Fix**: Added copy of `Hydra/Dsls.hs` from baseline `gen-main` to the setup script.

### 3. Clojure: compile-time vs runtime symbol resolution

Generated Hydra symbols (e.g., `hydra_ext_python_language_python_language`) are globalized into
`clojure.core` by the preload system at runtime, but are not available at compile time when
`bootstrap.clj` or `generation.clj` is loaded by the Clojure compiler.

Using bare symbols like `hydra_ext_python_language_python_language` in source code causes
"Unable to resolve symbol" errors at compile time.

**Fix**: All generated symbol references use `@(ns-resolve 'clojure.core 'symbol-name)` for
runtime resolution. A helper `(defn- rc [sym] ...)` in `bootstrap.clj` and `(defn- r [sym] ...)`
in `generation.clj` encapsulate this pattern.

Note: plain `(resolve 'sym)` does NOT work because it looks up the symbol in the current
namespace's mappings. Namespaces created before the symbol was interned into `clojure.core`
don't see it via `resolve`. Using `ns-resolve 'clojure.core` always works.

### 4. Clojure: coder module loading with forward references

Generated Clojure coder modules (e.g., `hydra.ext.python.names`) depend on each other. Loading
them one at a time with `require-and-globalize!` fails because symbols from later modules are
not yet defined when earlier modules reference them.

**Fix**: `load-coder-modules!` pre-declares all symbols across ALL coder modules first
(creating namespaces and interning unbound vars), then loads each module in dependency order.
This two-phase approach ensures cross-module references resolve.

### 5. Clojure: mutually recursive `let` bindings in generated code

Several generated Clojure modules (notably `hydra.adapt`) contain `let` blocks where bindings
reference each other (forward references). For example, `hydra_adapt_adapt_type` has:

```clojure
(let [for_supported (fn [typ] ...)
      for_unsupported (fn [typ] ... (try_type ...) ...)  ;; references try_type
      try_type (fn [typ] ... (for_supported ...) (for_unsupported ...) ...)]
  ...)
```

Clojure's `let` is sequential — each binding can only see bindings before it. So
`for_unsupported` cannot reference `try_type`.

**Fix**: Added `fix-forward-ref-let` to the preload system. It detects `let` forms where all
bindings are functions and at least one binding references a later binding. It transforms them
using atoms:

```clojure
(let [for_supported__ref (atom nil)
      for_unsupported__ref (atom nil)
      try_type__ref (atom nil)
      for_supported (fn [typ] (@for_supported__ref typ))
      for_unsupported (fn [typ] (@for_unsupported__ref typ))
      try_type (fn [typ] (@try_type__ref typ))]
  (reset! for_supported__ref (fn [typ] ...))
  (reset! for_unsupported__ref (fn [typ] ... (try_type ...) ...))
  (reset! try_type__ref (fn [typ] ... (for_supported ...) (for_unsupported ...) ...))
  ...)
```

The proxy functions `(fn [typ] (@ref typ))` delegate to the atoms, which are set to the real
implementations after all proxies exist. This handles both forward references and mutual recursion.

**Gotcha**: Some generated functions are named fns (`(fn name [params] body)`) where `(second val)`
returns the name symbol, not the param vector. The proxy extraction must check `(vector? (second val))`
to distinguish `(fn [params] body)` from `(fn name [params] body)`.

**Gotcha**: The `all-fns` guard (only transform when all bindings are functions) is essential.
Without it, non-function bindings like `(let [match_target stripped, match_value (second match_target)] ...)`
get wrapped in atom proxies, breaking pattern matching where `(first match_target)` expects a
sequence, not a function.

### 6. Clojure: incomplete `gen-main-load-order` in preload

The preload system's `gen-main-load-order` list was missing many modules needed for bootstrapping:
all encode/decode modules, JSON decode/encode, `hydra.adapt`, `hydra.templates`, `hydra.grammars`,
`hydra.parsers`, YAML modules, and JSON org modules. These are needed by `hydra.codeGeneration`
(which depends on `hydra.adapt`, `hydra.json.decode`, etc.) and by `generation.clj` (which calls
`hydra_json_decode_from_json` and `hydra_decode_module_module`).

**Fix**: Extended `gen-main-load-order` with all missing modules in dependency order.

### 7. Clojure: `uint64` parsing overflow

Test module `hydra.test.checking.fundamentals` contains the value `18446744073709551615` (uint64 max)
stored as a JSON string. The Clojure `hydra_lib_literals_read_uint64` function used `Long/parseLong`
which throws on values exceeding `Long.MAX_VALUE`.

**Fix**: Changed to `BigInteger` parsing with range validation against `2^64 - 1`. The parsed
value is converted to `long` via `.longValue` (which wraps to negative for values > `Long.MAX_VALUE`,
matching the Haskell/Java behavior for unsigned integers in a signed representation).

### 8. Java: generated Lisp coder PartialVisitor type errors

The generated Java Lisp coder (`Coder.java`) had two `PartialVisitor` anonymous classes with
incorrect type parameters. The diamond operator `<>` inferred the wrong type, causing the
`otherwise()` and `visit()` methods to have mismatched return types.

For example, the coder method returns `Either<T2, Expression>` but the generated visitor had:
```java
new Term.PartialVisitor<>() {
    public Either<Expression, Expression> otherwise(Term instance) { ... }
```

**Fix**: Replaced the diamond operator with the explicit full return type:
```java
new Term.PartialVisitor<Either<T2, Expression>>() {
    public Either<T2, Expression> otherwise(Term instance) { ... }
```

This is a code generator bug — the Java coder's generated code for `PartialVisitor` with
polymorphic error types doesn't correctly propagate the type parameter through the diamond
operator. The fix is a manual patch to `gen-main`; a proper fix would be in the Haskell
Java code generator.

### 9. Clojure: namespace representation mismatch

Modules decoded from JSON have their `namespace` field as a plain string (e.g., `"hydra.accessors"`),
not as a `hydra_module_namespace` record with a `:value` field. Code that accessed `(:value (:namespace mod))`
returned `nil`.

**Fix**: Added `ns-str-of` helpers that handle both representations:
`(if (string? ns) ns (:value ns))`.

### 10. Clojure: language values are records, not functions

Generated language values like `hydra_ext_lisp_language_lisp_language` are `def` bindings (values),
not zero-argument functions. The initial code wrapped the resolved value in `(...)` to "call" it,
causing `ClassCastException: hydra_coders_language cannot be cast to IFn`.

**Fix**: Removed the extra parentheses — use `@(rc 'sym)` not `(@(rc 'sym))`.

### 11. Clojure: Lisp dialect represented as tagged union

The Lisp coder's `module_to_lisp` function expects the dialect parameter as a tagged union
`(list :clojure nil)`, not a bare keyword `:clojure`. Passing the keyword caused
"Don't know how to create ISeq from: clojure.lang.Keyword" in the serde.

**Fix**: Pass `(list :clojure nil)` as the dialect value.

## Open issue: Clojure host performance for non-Lisp targets

### Observation

| Path | Time for 5 term modules | Per module |
|------|------------------------|------------|
| Clojure → Clojure | 6.7s | 1.3s |
| Clojure → Python | 142s | 28s |

Both paths use the same `generate_source_files` function and the same `adapt` pipeline. The
difference is entirely in the coder: `module_to_python` in Clojure is ~21x slower than
`module_to_lisp`.

For comparison, Haskell generates all 118 Python modules in 6.9s total.

### What was investigated

1. **Confirmed the coders run as native Clojure code**, not through the Hydra term-level evaluator.
   The coder functions are loaded from `.clj` files via the preload system and called directly by
   `generate_source_files`.

2. **Confirmed `adapt_data_graph_to_definitions` is called for both targets**. It is not skipped
   for Lisp targets. The adapt step takes roughly the same time for both.

3. **Timed individual modules**: Type modules are fast (~250ms each). Term modules are the
   bottleneck. `hydra.adapt` (a complex term module) takes ~4s for the Clojure target but ~38s
   for the Python target.

4. **The `fix-forward-ref-let` atom proxy pattern was investigated as a potential cause**.
   Optimizing from `(fn [& args__] (apply (deref ref) args__))` to `(fn [x] (@ref x))` with
   matching param counts made no measurable difference. The overhead is not in the proxy dispatch.

5. **The `do_infer` flag was tested**. Setting all flags to `[false false false false]` for the
   Python target did not significantly change the timing. The slowness is in the coder itself,
   not in the inference/adaptation pipeline.

### Hypothesis

The generated Clojure implementation of `module_to_python` is inherently slower than
`module_to_lisp` due to the Python coder's greater complexity:

- More pattern matching branches (Python has more syntax forms than Lisp)
- More helper function calls (`encode_name`, `serde` operations, Python-specific transforms)
- Deeper closure nesting in the generated curried code

Each of these adds overhead in Clojure's generated code style (deeply curried functions,
pattern matching via nested `cond` on `(first tagged-union)`, no type hints). The JVM's
JIT compiler may not optimize these patterns as effectively as Haskell's native compilation
or Java's generated visitor pattern.

### Practical impact

- **Clojure → Clojure**: ~33s for 118 modules. Fast enough for the demo.
- **Clojure → Python**: ~50 minutes estimated for 118 modules. Too slow for the demo
  but functionally correct.
- **Haskell/Java/Python → Clojure**: All work well and are fast (use native coders).

The Clojure host is most useful for generating Lisp targets. For generating non-Lisp targets,
the Haskell, Java, or Python hosts are much faster.

### Potential future improvements

- Profile the generated Clojure Python coder to identify specific hot spots
- Add type hints to generated Clojure code to reduce reflection
- Investigate whether the Clojure code generator can produce more JVM-friendly patterns
  (e.g., `defprotocol`/`defrecord` dispatch instead of `cond` on keywords)
- Consider compiling the coder modules AOT to improve JIT warmup
