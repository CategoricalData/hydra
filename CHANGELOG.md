# Hydra Changelog

All notable changes to the Hydra project are documented in this file.

This changelog tracks changes across all Hydra implementations
(Haskell, Java, Python, Scala, Clojure, Common Lisp, Emacs Lisp, Scheme)
and supporting infrastructure.

The format is inspired by [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and [Apache TinkerPop](https://github.com/apache/tinkerpop/blob/master/CHANGELOG.asciidoc).

---

## [0.15.0] - in progress

A maintenance and structural-cleanup release. The dominant theme is a
top-to-bottom repository reorganization (#290) into a three-tree
`packages/` (DSL sources), `heads/` (per-host runtime), `dist/`
(generated output) layout, accompanied by a series of kernel
simplifications: `Function`/`Elimination` are eliminated in favor of
direct term constructors, `FunctionPrimitive` is replaced by
`TermVariable`-based resolution, the `Hydra.Ext.*` module prefix is
gone, the `hydra.module` namespace is now `hydra.packaging`, and
unsafe partial primitives have been removed in favor of `Maybe`-returning
alternatives. A new `decimal` literal type joins the kernel, and the
JSON coder is type-directed with idiomatic optional encoding.
Generation gains incremental, content-hash-based caches across the
sync stack. Three new generation-only targets are in progress as
Claude collaborations: Coq, WebAssembly, and a `grad` differentiation
demo.

### Highlights

- **Three-tree repository layout** (`packages/`, `heads/`, `dist/`) — every path,
  build command, and CI workflow now operates on the new structure (#290).
- **Kernel simplification**: removed `Function`/`Elimination` (#332) and
  `FunctionPrimitive` (#251); promoted `lambda`/`cases`/`project`/`unwrap`
  directly into `Term`. All named references resolve through `TermVariable`.
- **Decimal literal type** (#338) and a type-directed JSON coder with
  idiomatic optional encoding (#314); JSON `Value.number` migrated from
  `bigfloat` to `decimal` (#340).
- **Incremental, content-hash-based caches** across the sync pipeline (#247),
  with three new generation-only targets in progress: Coq (#326),
  WebAssembly (#325), and an automatic-differentiation demo (#324).

### Breaking Changes

- **Repository layout** (#290): old top-level `hydra-haskell/`, `hydra-java/`,
  `hydra-python/`, `hydra-scala/`, `hydra-lisp/`, `hydra-ext/` directories
  (and their `src/main` / `src/gen-main` / `src/gen-test` subdirectories) are
  gone. Sources live under `packages/<pkg>/`, hand-written runtimes under
  `heads/<lang>/`, and generated output under `dist/<lang>/<pkg>/`. Build
  commands now run from the heads (`cd heads/haskell && stack test`,
  `cd heads/python && pytest`, `./gradlew :hydra-java:test`, etc.). Many
  scripts, CI configs, and docs need updating.
- **`hydra-ext` split into per-domain packages** (#290): `hydra-coq`
  (Coq coder), `hydra-pg` (property graph model + GraphSON, Cypher,
  Tinkerpop, Graphviz), `hydra-rdf` (RDF, SHACL, OWL, ShEx, XML schema),
  and `hydra-ext` (long-tail: Avro, Protobuf, GraphQL, Cpp, Csharp, Go,
  Rust, TypeScript, Yaml, ...). The `hydra-ext` name is retained for
  Maven Central continuity.
- **`Hydra.Ext.*` module prefix removed; domain hierarchies flattened**
  (#331): generated Haskell modules formerly under `Hydra.Ext.Java.*`,
  `Hydra.Ext.Python.*`, etc. now live under `Hydra.Java.*`,
  `Hydra.Python.*`, etc. DSL sources follow the same pattern:
  `Hydra.Sources.Foo.*` rather than `Hydra.Ext.Sources.Foo.*`. Over-deep
  domain hierarchies like `hydra.ext.org.w3.shacl.model` are flattened to
  `hydra.shacl.model`. Every import of `Hydra.Ext.*` must be updated.
- **`hydra.module` renamed to `hydra.packaging`**: `Module`, `Namespace`,
  `QualifiedName`, `Definition`, and related types live in `hydra.packaging`;
  the `hydra.module` namespace is gone.
- **`Function` and `Elimination` types eliminated from `hydra.core`** (#332):
  the `lambda` constructor and the elimination forms (`project`, `cases`,
  `unwrap`) are now direct `Term` constructors. Code that pattern-matched on
  `Function`/`Elimination` must be updated.
- **`FunctionPrimitive` removed** (#251): the `primitive` variant of the
  `Function` union is gone. All named references (module definitions,
  primitives, constants) now use `TermVariable`. At runtime, variables
  resolve through `graphBoundTerms`, then `graphPrimitives`, then
  lambda-bound scope. A new `graphWithPrimitives` API in `hydra.lexical`
  assembles primitives from built-in and user-provided lists. Code that
  pattern-matches on `Function.Primitive` (Java), `FunctionPrimitive`
  (Python/Haskell), or `Function.primitive` (Scala) must be updated.
- **`Term.union` renamed to `Term.inject`** (#334) for consistency with
  the type-side terminology.
- **JSON `Value.number` is now `decimal`** (#340), not `bigfloat`. Consumers
  constructing or pattern-matching on `Json.ValueNumber` must use the
  decimal type of their host (e.g. `realToFrac` to convert
  `Double -> Scientific` in Haskell).
- **13 unsafe (partial) primitives removed** (#201) in favor of
  `Maybe`-returning alternatives:
  - `hydra.lib.lists.at` → `hydra.lib.lists.maybeAt`
  - `hydra.lib.lists.head` → `hydra.lib.lists.maybeHead`
  - `hydra.lib.lists.init` → `hydra.lib.lists.maybeInit`
  - `hydra.lib.lists.last` → `hydra.lib.lists.maybeLast`
  - `hydra.lib.lists.tail` → `hydra.lib.lists.maybeTail`
  - `hydra.lib.lists.safeHead` → `hydra.lib.lists.maybeHead` (rename)
  - `hydra.lib.math.div` → `hydra.lib.math.maybeDiv` (Nothing on divisor 0)
  - `hydra.lib.math.mod` → `hydra.lib.math.maybeMod` (Nothing on divisor 0)
  - `hydra.lib.math.rem` → `hydra.lib.math.maybeRem` (Nothing on divisor 0)
  - `hydra.lib.math.pred` → `hydra.lib.math.maybePred` (Nothing at minBound)
  - `hydra.lib.math.succ` → `hydra.lib.math.maybeSucc` (Nothing at maxBound)
  - `hydra.lib.strings.charAt` → `hydra.lib.strings.maybeCharAt`
  - `hydra.lib.maybes.fromJust` → `hydra.lib.maybes.fromMaybe`,
    `hydra.lib.maybes.maybe`, or `hydra.lib.maybes.cases`
- **Error context type removed** (#292): the `(error) Context` type is gone;
  errors propagate without an in-band context wrapper. Hand-written
  Java/Python/Scala/Lisp code referring to `InContext` must be updated.
- **Java generated code uses standard collection interfaces** (#313):
  `java.util.List`, `java.util.Map`, `java.util.Set` in field types and
  constructor parameters, instead of the internal `ConsList`,
  `PersistentMap`, `PersistentSet`. The internal classes remain available
  in `hydra.util` for primitive implementations. Generated `compareTo`
  methods use `hydra.util.Comparing.compare()` for safe collection
  comparison.

### New Features

- **`decimal` literal type** in the kernel (#338): arbitrary-precision
  decimal values across all hosts (Haskell `Scientific`, Java
  `BigDecimal`, Python `Decimal`, Clojure `BigDecimal`, etc.). The YAML
  scalar union gained a matching `decimal` variant.
- **Type-directed JSON encoder** with idiomatic optional encoding (#314):
  `Maybe a` fields elide when `Nothing` instead of round-tripping as
  `null`; type-directed dispatch produces clean JSON aligned with
  conventional usage.
- **Incremental type inference and content-hash-based caches** (#247):
  per-namespace digests under `dist/json/<pkg>/digest.json`, per-target
  per-package digests under `dist/<lang>/<pkg>/digest.json`, and per-target
  test caches under `dist/<lang>/test-cache.json` short-circuit clean
  reruns. ~70x speedup on a no-op resync.
- **Coq generation target** (#326, in progress): `hydra-coq` package
  with full Coq syntax model, coder, and a test runner that walks 31
  test modules. 131/131 generated `.v` files pass `coqc`. Coq is
  generation-only and not part of the bootstrapping matrix.
- **WebAssembly target** (#325, in progress): `hydra-wasm` package with
  a Wasm coder, M3 milestone (19/19 surveyed kernel functions run
  correctly under Wasm), M4 closure mechanism via `call_indirect`, and
  a Node-based test harness.
- **Automatic-differentiation demo** (#324, in progress): symbolic
  source-to-source differentiation as a `Term -> Term` transformation,
  evaluated via `reduceTerm`. Covers chain/product/power rules and
  transcendental derivatives; gradient-checked against finite differences.
- **`hydra.show.error` module** (#265): consolidated error-message
  builders for the kernel.
- **`hydra.lib.maybes.toList`** primitive (#257): bridges `Maybe a` to
  `[a]` for use cases that previously reached for the legacy
  `hydra.monads.maybeToList`.
- **Typeclass inference test group** (#274) added to the common test suite.
- **Float64 arithmetic primitive test cases** (#324): coverage for
  hyperbolic and inverse-trig functions added.
- **NaN / Inf round-tripping** in math primitives, the JSON coder, and
  per-language serdes (#312, #330): IEEE special values that `decimal`
  cannot hold (`NaN`, `Infinity`, `-Infinity`, `-0.0`) round-trip through
  JSON as string sentinels; `showFloat64` preserves the sign of zero in
  Python and all four Lisp dialects.

### Improvements

- **Sync infrastructure**: `bin/sync.sh --hosts/--targets` matrix tool,
  `bin/sync-packages.sh` per-package orchestrator, `bin/sync-all.sh`
  exhaustive runner with phase reporting; per-language `sync-<lang>.sh`
  wrappers (#290).
- **Detect stale generated sources** (#228): warm-cache short-circuits
  added to all sync entrypoints; sync-haskell skips when DSL+runtime
  inputs are byte-identical to the last green run.
- **Eliminated TestGraph post-generation patches** (#25): the DSL now
  emits `TestEnv` references directly; the `sed` patches that previously
  rewrote `Hydra.Lexical.emptyGraph` / `emptyContext` calls are retired.
- **Minimize standard imports in generated Haskell** (#161): bottom-up
  metadata gathering keeps generated import lists tight.
- **Improved auto-generated import aliases** (#322) for Haskell targets.
- **Removed kernel term dependencies from the Hydra interpreter** (#257):
  the interpreter no longer pulls in `hydra.monads`-style helpers; what's
  needed is provided as primitives.
- **Removed `Graph` and `Context` arguments from primitive
  implementations** (#266): primitives are pure functions of their term
  arguments; the previous threading was unused.
- **Refactored kernel term modules** (#221): split `rewriting`/`schemas`,
  renamed `show.meta` → `codeGeneration` → `codegen`, moved `Coder` to
  `coders`, merged `extract.helpers`. All implementations regenerated.
- **Removed Aeson and HsYAML dependencies** from `hydra-haskell` and
  `hydra-ext` (#261): YAML coder moved to `hydra-ext`; native JSON
  parser/writer (introduced in 0.13) replaces Aeson throughout.
- **Lisp recursive let bindings** (#341): emit `letfn` for Clojure cyclic
  let bindings via SCC topo sort; universal SCC handling across Lisp
  dialects.
- **Lisp parenthesis cleanup** (#84): generated Lisp output drops
  redundant parentheses where operator precedence allows.
- **Everything-to-everything bootstrapping demo** (#254): the
  bootstrapping matrix supports any (host, target) combination, with
  environment checks before running.
- **Re-enabled skipped Python test cases** (#263): `disabledForPython`
  testing tag removed; all common-suite tests run on Python.
- **Promoted remaining host-specific code in `packages/`** (#337) into
  per-head locations under `heads/<lang>/`. Coq syntax model moved from
  `hydra-ext` to `hydra-coq`.
- **Migrated `hydra.common` Java module** (#10): the last
  hand-maintained common Haskell module is gone; its contents are now
  generated DSL output or merged into other modules.
- **Test infrastructure unification** (#246): legacy test case types
  removed from `hydra.testing`; all test runners use the unified
  `UniversalTestCase` format.
- **Default property graph → RDF mapping** (#296) demonstrated end-to-end
  with regenerated Java/Python PG modules.
- **`hydra-ext.haskell.operators`** dependency removed from tests (#288).
- **Inconsistencies in domain DSLs checked and removed** (#219): no-longer-needed
  hand-written DSL code deleted.

### Bug Fixes

- **`substInTypeScheme` capture-unsafe substitution** (#290): under some
  forall-binder shapes, substitution could capture a variable bound by an
  inner forall. Added quantifier-shadowing tests; surfaced and fixed via
  the incremental-inference unification regression in #247.
- **`test-distribution.sh` exited 1 after passing tests**: when invoked
  with a relative `${BASH_SOURCE[0]}` and the script `cd`'d before
  recording the cache, `xargs shasum` failed silently on a no-longer-
  resolvable path; pipefail propagated the non-zero exit. Fix in
  `bin/lib/test-cache.sh` resolves runner_script absolutely and emits
  shasum errors instead of swallowing them. Affected all eight
  `heads/<lang>/bin/test-distribution.sh` callers.
- **Java `Float64` arithmetic primitives** corrected to match the current
  `PrimitiveFunction` API (#324).
- **Clojure decimal handling** (#340): equality compares `BigDecimal` by
  value rather than scale-sensitive `.equals`; `decimal_to_bigint`
  returns `BigInteger`, not `long`.
- **Scheme runtime** R7RS imports: `sets.scm` and `maps.scm` correctly
  use `vlist`, Guile `make-hash-table`/`sort`; `eithers.map_set` routes
  through the public sets API.
- **Python coder defaults** corrected from `Py.Name` to `Py.Expression`
  (followup to #201).

### Documentation

- **Documentation refresh for the 0.15 packaging restructure** (#290, #331):
  CLAUDE.md, README.md, every `packages/<pkg>/README.md`,
  `docs/implementation.md`, `docs/recipes/*.md`, `docs/tco-implementation.adoc`,
  every demo README, and seven wiki pages (`Code-Organization`, `Concepts`,
  `Home`, `Property-graphs`, `RDF`, `Release-process`, `Testing`)
  updated for the new layout.
- **Code-generation recipe rewritten** (#282) for the per-package dist
  layout and three-layer (orchestrator → assembler → transformer) sync.
- **`maintenance.md` recipe** consolidates non-source-file scans, stale
  generated file detection, design-violation checks, definition-ordering
  audit, primitive-consistency checks, JSON kernel freshness, and
  `__init__.py` freshness; surfaces a `/maintenance()` shorthand for
  recurring runs.
- **Documentation style guide** added at `docs/documentation-style-guide.md`.
- **Cross-worktree messaging protocol** documented in CLAUDE.md for
  parallel-Claude workflows.
- **Wiki "Updating the changelog"** section formalized.
- **Decimal type** documented in the lexicon and per-language READMEs (#338).
- **JSON encoding format docs** updated for idiomatic optional encoding (#314).

---

## [0.14.0] - 2026-03-29

Major release adding four new complete Hydra implementations — Scala (#273),
Clojure (#278), Common Lisp, and Scheme — bringing the total to seven bootstrapping hosts
(plus Emacs Lisp, which passes the test suite but does not yet participate in bootstrapping).
Also includes kernel simplifications, an Avro bidirectional coder (#301), eval primitives (#281),
new demos, and significant performance work on the Scheme host.

### Highlights

- **Four new complete implementations**: Hydra-Scala, Hydra-Clojure, Hydra-Common Lisp,
  and Hydra-Scheme pass the common test suite and can serve as bootstrapping hosts.
  The four Lisp dialects share a single coder and serializer.
- **Kernel simplifications**: Removed `RowType`/`WrappedType` (#82),
  unified `Graph`/`TypeContext`/`InferenceContext` (#192),
  replaced `Module.elements` with `Module.definitions` (#214),
  consolidated error types (#268), and promoted all staging modules to DSL sources (#267).
- **Avro bidirectional coder** (#301) with property graph pipeline demo.
- **Scheme bootstrap performance**: 58 minutes → 5.7 minutes via O(1) vhash data structures
  and IEEE 754 float precision fixes.

### New Features

- **Hydra-Scala** (#273): complete implementation, sbt/Scala 3, 3043 tests
- **Hydra-Lisp**: Clojure (#278), Common Lisp, Emacs Lisp, Scheme — four dialects,
  one coder. CI workflows for Clojure, Common Lisp, and Scheme.
- **Avro bidirectional coder** (#301) with demo
- **Eval primitives** (#281): Logic, Equality, Math, collections, and Groups
- **Generated DSL modules** (#180): `hydra.dsls` generates DSL helpers from type modules
- **New demos**: GraphQL+JSON (#279), PG validation (#284), SHACL (#294), Avro+PG
- **Packaging module** (#290): `hydra.packaging` with `Package` type and validation
- **New primitives**: `lists.foldr` (#280), `maybes.toList`, float rounding (#264, #285)

### Improvements

- Promoted all staging modules to DSL sources (#267)
- Removed legacy adapters, JSON/YAML coders, grammar framework (#236, #295)
- Consolidated `hydra.compute` into `hydra.util` (#269)
- Merged `hydra.tarjan` into `hydra.sorting` (#220)
- Merged `hydra.constraints` into `hydra.query` (#272)
- Renamed `hydra.accessors` to `hydra.paths` (#271)
- Moved `hydra.workflow` to hydra-ext (#270)
- Replaced legacy Flow monad with Either (#276)
- Java: `Pair` replaces `Tuple2` (#245), flattened term naming (#223)

### Bug Fixes

- Fixed `Maybe TypeScheme` handling in ext coders (#295)
- Fixed `buildGraph` shared subexpression issue in Adapt.hs and Inference.hs

### Documentation

- Thorough cleanup of user documentation (#302, #303)
- Coding style guide added to wiki
- Updated 'new implementation' recipe with learnings from Clojure head (#278)

---

## [0.13.0] - 2026-02-27

Major release completing Hydra-Python and Hydra-Java as self-hosting Hydra implementations, and demonstrating mutual self-hosting across all three languages. Significant improvements to type inference, type checking, rewriting, and adaptation. New language features include first-class Either types, typeclass inference, binary data support, and a native JSON parser/writer. Comprehensive tooling for cross-implementation bootstrapping and provisional support for Go, Rust, and JavaScript targets.

### Highlights

- **Mutual self-hosting across three implementations**: Hydra is now fully self-hosting in Haskell, Java, and Python, and can cross-generate between every combination of host and target language (9 paths total). This is the project's most significant self-hosting milestone to date.
- **Hydra-Java is now complete** (#166): All kernel and generation tests pass. Hydra now has three full implementations that pass the common test suite.
- **Hydra-Python is now complete** (#66): 100% test parity with Haskell. All kernel and generation tests pass.
- **Language coders promoted into the Hydra kernel** (#176): The Haskell, Java, and Python coders are now defined as Hydra DSL modules rather than hand-written staging code, making them self-hosting.
- **Either type support** (#210): First-class Either types in the core type system with full inference, checking, and library support.
- **Native JSON parser and writer** (#188, #242): Hydra's own JSON parser and writer replace the Aeson dependency, enabling language-independent JSON processing.

### Breaking Changes

- Renamed `Optional` to `Maybe` throughout the codebase (#204)
  - `hydra.lib.optionals` → `hydra.lib.maybes`
  - All `optional` variants renamed to `maybe`
- Removed unlabeled product and sum types; replaced with pair types (#212)
- Renamed `hydra.core.TypedTerm` to `hydra.core.TypeApplicationTerm`
- Renamed `hydra.json` to `hydra.json.model`
- Renamed `hydra.lib.maps.remove` to `hydra.lib.maps.delete`
- Removed deprecated `hydra.decoding` module
- Removed old `hydra.mantle.Either` type (replaced by core `Either`)
- Removed `hydra.lib.tuples` library
- Removed C# coder stub (C# syntax model remains)
- Minimum Java version lowered to 11 (#249)
- DSL syntax migration: removed OverloadedStrings for term expressions (#238)

### New Features

- **Self-Hosting and Bootstrapping**
  - Hydra is now fully self-hosting in all three implementations (Haskell, Java, Python)
  - Mutual cross-generation between every combination of host and target language (Haskell→Java, Haskell→Python, Java→Haskell, Java→Python, Python→Haskell, Python→Java, plus self-generation for each)
  - New bootstrapping demo validating all 9 paths across 249 modules
  - Promoted Haskell, Java, and Python coders from staging code into Hydra DSL sources (#176)
  - JSON-as-source-of-truth bootstrapping architecture (#243, #253): new executables and scripts enabling `bootstrap-from-json` for cross-implementation code generation
  - Generated term encoders and decoders replace legacy hand-coded modules (#47)

- **Hydra-Java Completion** (#166, #131)
  - All kernel tests pass
  - All generation tests pass
  - Deep changes to Java coder enabling complete kernel generation
  - Added generation test codec for Java
  - Scripts and executables for Java generation (`sync-java.sh`)
  - Support for unit type and term in Java language constraints
  - Added `Lazy` utility class for delayed evaluation
  - Array type support in Java serde
  - Tail-call optimization in the Java coder
  - `Tuple` implements `Comparable`

- **Hydra-Python Completion** (#66)
  - 100% test pass rate and parity with Haskell
  - Restructured project layout with `src/main` and `src/gen-main` pattern (#191)
  - Full library implementations including `hydra.lib.flows`
  - `@lru_cache` optimization for nullary bindings
  - PyPy compatibility: Python coder can produce Python 3.10 code for a 65% performance boost with PyPy
  - Generation and bootstrapping logic in Python

- **Either Type Support** (#210)
  - Added `EitherType` to core type system
  - Added `either` term constructor
  - New `hydra.lib.eithers` library with `either`, `map`, `mapList`, `mapMaybe`, `bimap`, `bind`, `fromLeft`, `fromRight`, `isLeft`, `isRight`, `lefts`, `rights`, `partitionEithers`
  - Full support in type inference, type checking, rewriting, encoding/decoding
  - Either support in all three implementations
  - Comprehensive test coverage

- **Void Type** (#237)
  - Added uninhabited `void` type to the core type system as the dual of `unit`
  - Type-level only: no term constructor (void has no inhabitants)
  - Support in type unification, rewriting, encoding/decoding, adaptation, and all language coders
  - Void type support across all implementations (Haskell, Java, Python)

- **Native JSON Parser and Writer** (#188, #242)
  - New bidirectional JSON encoder/decoder
  - Replaced Aeson dependency with Hydra's own JSON parser and writer
  - Cross-checked against Aeson via a special test runner
  - Output round numbers without `.0`

- **Typeclass Inference** (#164)
  - Type schemes with typeclass constraints via `TypeVariableMetadata`
  - Typeclass metadata on primitive definitions
  - Haskell coder uses typeclass information when available

- **Binary/ByteString Support** (#172)
  - Binary data represented using `ByteString` instead of `String`
  - Haskell coder uses `Data.ByteString` for binary literals
  - New `hydra.lib.literals.binaryToBytes` primitive

- **Parser Combinators**
  - New `hydra.parsing` module with parser combinator types
  - Parser combinator implementations
  - DSL for `hydra.parsing`

- **Higher-Order Primitive Interpreter** (#198)
  - All primitives are now fully interpretable across all three implementations
  - Removed legacy `requiresInterp` tag; every primitive is compatible with the interpreter

- **Let Hoisting and Flattening**
  - Deep changes to support hoisting polymorphic let terms
  - Refined let hoisting for monomorphic bindings inside polymorphic ones
  - More efficient `liftLambdaAboveLet` implementation (#202)
  - Case statement hoisting: multipurpose subterm hoisting functions for moving deeply nested subterms up to the level of let bindings
  - New test cases for let hoisting focusing on polymorphic bindings

- **Provisional Language Targets**
  - Go: syntax model, serde, and generated sources
  - Rust: syntax model, serde, generated sources, and experimental type-level coder
  - JavaScript: syntax model, serde, DSL, and generated sources
  - Java syntax DSL
  - Python syntax DSL

- **New Adapter Framework** (#236)
  - All external coders (Haskell, YAML, Scala, JSON Schema, C++, PDL, Protobuf, GraphQL) refactored to use the new framework

- **New Primitives**
  - `hydra.lib.lists.find` and `hydra.lib.lists.partition` (all implementations)
  - `hydra.lib.flows.withDefault` and `hydra.lib.flows.foldl`
  - `hydra.lib.math.max`, `hydra.lib.math.min`, `hydra.lib.math.abs`, `hydra.lib.math.pred`, `hydra.lib.math.signum`, `hydra.lib.math.succ`, `hydra.lib.math.even`, `hydra.lib.math.odd`
  - `hydra.lib.pairs.bimap`
  - `hydra.lib.eithers.bind`
  - `hydra.lib.literals.binaryToBytes`
  - Complete `hydra.lib.literals.readXxx` and `hydra.lib.literals.showXxx` families
  - `hydra.lexical.chooseUniqueName`
  - Renamed `neg` to `negate` for consistency with Haskell Prelude
  - Completed floating-point math primitives in Python (#208)

- **DeepCore DSL**
  - New DSL layer one level deeper than the Meta DSLs, for programs that construct programs that construct terms

- **Developer Documentation**
  - Added developer recipes: extending Hydra Core, adding primitives, syncing Python, promoting code, refactoring, JSON kernel
  - LLM quickstart guide and recipe for LLM-assisted development
  - Updated all implementation READMEs with documentation sections
  - Comprehensive cross-linking between wiki pages, READMEs, and recipes

### Improvements

- **Type System**
  - Refactored `hydra.inference` to create new module `hydra.checking`
  - Inference checks after unification to prevent schema names from being unified with inferred type variables
  - Type application terms preserved when preparing application terms for Python
  - Sanity-check tuple projection index against arity during inference
  - Enabled inference logic to check for and bind lost type variables in term annotations
  - Made `extendTypeContextForLet` tolerant of untyped bindings

- **Code Generation**
  - Python: support for deeply-nested match statements
  - Python: more complete `let` support
  - Python: Pythonic syntax for polymorphic function definitions
  - Python: refinements for complex case hoisting scenarios
  - Java: method declaration style consistent with calling style
  - Haskell coder updated to use standardized typeclass names
  - Increased maximum trace depth to support more complex sources

- **Kernel Improvements**
  - Common test suite promotion: all tests promoted into a shared, implementation-independent test kernel (#213)
  - Included all Hydra kernel types in test schema (#205)
  - Reorganized Haskell DSL definitions for better clarity
  - Standardized imports across all kernel sources
  - Added TODO comments to all unsafe primitives (#201)
  - New `rewriteAndFoldTerm` utility for simultaneous rewriting and folding
  - Lexical helper for dereferencing schema types through aliases
  - Made JSON parser lazy with new `lazy` parser combinator
  - More efficient substitution helpers for empty substitutions
  - Precomputed type/inference context in kernel test runner for performance
  - Property graph encoding and decoding modules promoted into DSL
  - `hydra.tabular` and `hydra.pg.graphson` modules promoted into DSL
  - `hydra.show.*` modules generated into Java and Python

- **GenPG Demo**
  - Refactored to support Haskell, Python, and Java modes
  - Added Python runner and Java generation for the demo
  - Command-line helper for generating Python
  - Finalized output format

- **Testing**
  - Extended test runners for let hoisting test cases (Haskell, Python, Java)
  - Test cases for floating-point precision in show primitives
  - Tests for `hydra.lib.maps` and `hydra.lib.sets` enforcing ordering in `toList`
  - Inference tests for inferred System F terms
  - Test cases specifically for case statement hoisting
  - New JSON coder tests

- **Infrastructure**
  - GitHub Actions workflow for automatic JavaDoc deployment on tag push
  - Central `VERSION` file and `bin/bump-version.sh` for version synchronization
  - `bin/sync-all.sh` for full cross-implementation regeneration
  - `bin/verify-release.sh` for pre-release verification
  - Pixi build environment for `sync-all.sh`
  - Benchmarking scripts and dashboard
  - Added `LICENSE` file for hydra-python

### Bug Fixes

- Fixed interpreter bugs causing test failures (#235)
- Fixed bug in type checking in connection with dead code
- Fixed bug in `removeTypesFromTerm`
- Fixed bug in `typeOfMap`
- Fixed 32-bit max int value in `hydra.constants`
- Fixed numeric precision in Python
- Added `BigDecimal` support and fixed `uint8` (short) support in Java
- Fixed Haskell coder with respect to imports for binary literals
- Excluded `Prelude.encodeFloat` and `Prelude.decodeFloat` from generated Haskell due to name collisions
- Fixed consistency issues in Python primitives:
  - `hydra.lib.sets.toList`
  - `hydra.lib.maps.union` (precedence)
  - `hydra.lib.strings.lines`
  - `hydra.lib.strings.readString` (dequoting)
  - `hydra.lib.lists.apply`
  - `hydra.lib.maps.toList`
- Fixed Haskell type signature of `hydra.lists.span`
- Fixed issue with eta expansion of typed terms
- Fixed shadowing issue in generated encoding modules
- Added missing alternatives in `hydra.rewriting.rewriteTermM`

### Documentation

- Updated main README with complete implementation status
- Added Documentation sections to all implementation READMEs
- Fixed outdated Code-organization.md (Python now uses src/gen-main pattern)
- Fixed incorrect code paths in Implementation.md
- Added Java section to Testing wiki
- Comprehensive developer recipes in `docs/recipes/`
- Documentation for tail-call optimization implementation
- Added Haddock comments to Haskell primitives

### Community

- Accepted babeloff's `isTrivialTerm` changes

---

## [0.12.0] - 2025-08-28

Major release focused on Python development progress and architectural improvements.

### Breaking Changes

- Removed `hydra.decoding` module (#190)
- Flattened dependency tiers; eliminated tier-1, tier-2, tier-3 organization (#184)
- Removed `typed` term variant (#173)
- Module naming changes: moved tabular and relational modules into kernel (#152)

### New Features

- **Python Implementation**: Significant progress toward Hydra-Python completion
  - Generated all kernel types and terms into Python
  - Added Decimal support for bigfloat values
  - Environment tracking for proper variable scoping
  - Generate `hydra.languages` into Python
- Added designated `unit` term and type variants (#186)
- Added `hydra.lib.flows.mapElems` and `hydra.lib.flows.mapKeys` utilities
- Promoted JSON coder into the DSL (#181)

### Improvements

- Condensed repeated elements in error traces (#165)
- Fixed transitive schema dependencies (#185)
- Improved adapter solution with respect to literals
- Better handling of type variables in Python coder
- Enhanced term-level Python generation with proper ordering to minimize forward references
- Added topo-sort helper function for dependency management
- Renamed `hydra.lib.flows.traverseOptional` to `hydra.lib.flows.mapOptional`
- Updated all Python libraries with latest changes
- Distinguished between deannotation and detyping of terms
- Adapted primitives when applying language constraints to a graph

### Bug Fixes

- Fixed System F terms for records and case statements (#168)
- Fixed issue with transitive type-level module dependencies (#185)
- Corrected handling of variable references in Python application terms
- Fixed nullary functions in Python (added empty parens)
- Added missing alternatives in `hydra.rewriting.rewriteTermM`

### Documentation

- Updated Hydra-Haskell README
- Updated Hydra-Java README with corrected links
- Updated JavaDocs

---

## [0.8.1] - 2024-09-24

Patch release with utility improvements.

### Improvements

- Generalized property graph merging utility to accept type systems other than Hydra Core

---

## [0.8.0] - 2024-09-09

**Breaking Changes**: Version bumped to 0.8 due to breaking changes.

### Breaking Changes

- Made `FlowException` class serializable for Spark support (may affect existing error handling)

### New Features

- **C# Support**: Added C# syntax module based on ANTLR grammar from Microsoft documentation (#139)
- **Graphviz DOT**: Added comprehensive DOT model and coder (#136)
  - Full support for lambdas and recursive `let` statements
  - Compact 'accessor graph' visualization option
  - Customizable label styles
  - Highlighted let-bound terms in yellow
  - Edge labels for clearer diagrams
- Added term accessor type to facilitate lenses and path-aware transformations

### Improvements

- Added `</>` as alternative to `@@` application operator in Haskell DSLs
- Added utilities for working with term accessors
- More specific exception class for JSON decoding
- Additional convenience methods to `JsonDecoding.java`
- Used transitive dependencies in hydra-java and hydra-ext
- Upgraded Antlr to avoid vulnerability
- Moved TinkerPop utilities from `hydra/ext/org/apache/tinkerpop` into `hydra/pg`

---

## [0.7.0] - 2024-08-21

Major refactoring of module organization and namespace management.

### Breaking Changes

- **Namespace reorganization**: Migrated to DNS-based module naming in hydra-ext
  - `hydra/ext/avro` → `hydra/ext/org/apache/avro`
  - `hydra/ext/graphql` → `hydra/ext/org/graphql`
  - `hydra/ext/yaml` → `hydra/ext/org/yaml`
  - `hydra/ext/json/decoding` → `hydra/ext/org/json/decoding`
  - `hydra/ext/rdf` → `hydra/ext/org/w3/rdf`
  - `hydra/ext/shacl` → `hydra/ext/org/w3/shacl`
  - `hydra/ext/tinkerpop` → `hydra/ext/org/apache/tinkerpop`
- **Property graphs**: Promoted property graph modules from `hydra/ext/org/apache/tinkerpop` to `hydra/pg`
- **Project structure**:
  - Renamed `hydra-extensions` to `hydra-ext`
  - Renamed `hydra/langs` to `hydra/ext` (#138)
  - Made hydra-ext into a Gradle subproject alongside hydra-java
  - Added top-level Gradle build

### Improvements

- Moved TinkerPop modules from hydra-haskell into hydra-ext for better separation
- Moved miscellaneous models to `Hydra.Ext.Other` (#138)
- Moved XML Schema, SQL, ShEx, and OWL models to hydra-ext
- Moved KQL, Parquet, and Python modules to hydra-ext
- Separated JavaDocs for hydra-java and hydra-ext
- Updated publishing configuration in build.gradle

---

## [0.6.0] - 2024-08-19

Major release with type system improvements and expanded language support.

### New Features

- **System F Support**: Added System F `typeAbstraction` and `typeApplication` term constructors
- **Python3 Syntax**: Added Python3 syntax model based on official Python BNF (#80)
- **Graphviz**: Added Graphviz DOT model and initial support (#136)
- Added constants for field names in Java code generation (#137)

### Breaking Changes

- Eliminated `InferenceContext` helper type
- Changed typing environment from `Map Name Type` to `Map Name TypeScheme` in graphs
- Updated annotations and flows to use `map<Name, Term>` instead of `map<string, Term>` (#133)

### Improvements

- **Primitive organization**: Organized Hydra primitives into libraries for namespace management
- **Type inference**: Added provisional Algorithm W implementation for reference
- **Code generation**:
  - Escaped field name constants and `with_` methods in Java coder
  - Generated field name constants in Java (#137)
  - Avoided duplicated comments in wrapper classes in generated Java
  - Removed superfluous newtype comments in Haskell coder
- **Delta Parquet**: Updated Delta Parquet model to follow Java API more closely
- **OpenCypher**: Enriched OpenCypher features module with complete list of standard Cypher functions
- Added convenience methods to `JsonEncoding.java`
- Restored `Hydra.Inference` subdirectory

### Bug Fixes

- Fixed handling of type annotations in term adapters
- Fixed decoding of encoded terms annotated with a type

---

## [0.2] - 2024-01-10

Hydra-Java 0.2.0 release with focus on Java implementation improvements.

### New Features

- **Property Graph Validation**: Fine-tuned PG validation and added unit tests in Java (#100)
- **Checkstyle**: Added Checkstyle configuration based on Google style guide (#111)
- **Python3**: Added Python3 syntax model based on official Python BNF (#80)
- **Cypher**: Initial Cypher query language support (#108)

### Improvements

- **Java Utilities**:
  - Added convenience method for consuming Flow results
  - Added `fromFlow` variants including one that throws exceptions
  - Added `Flows.bind3` utility methods
  - Added convenience functions for printing literals and literal types in Java DSL
- **Code Quality**:
  - Updated main and test sources to conform to Checkstyle configuration (#111)
  - Extended line length limit from 100 to 120 characters
  - Disabled overly restrictive indentation and import order rules
  - Disabled `RightCurlyAlone` check for compact inline map definitions
- **Property Graphs**:
  - Streamlined property graph element merging to unify properties with identical keys and types
  - Improved property graph validation with comprehensive unit tests
- Minor addition to JSON decoding

---

## [0.1.1] - 2022-12-04

Patch release with minor fixes and improvements.

---

## [0.1.0] - 2022-12-04

**First packaged release of Hydra.**

This initial release contains the complete foundation of Hydra:

### Core Language

- Hydra's core type and data languages (`hydra.core`)
- Core models for graphs and modules
- Computation model with Flow monad
- BNF grammars support
- Phantom types
- Basic operations on types and terms

### Type System

- **Hindley-Milner style type inference**
- Type schemes with polymorphism
- Type/term validation

### Transformations

- **Adapter system**: Type-to-type rewriting and transformation
- **Coders**:
  - Haskell coder (types and terms)
  - Java coder (types and terms)
  - Partial Scala coder (terms only)
  - Avro coder
  - JSON coder
  - PDL (Pegasus Data Language) coder
  - RDF + SHACL coder
  - YAML coder

### Language Support

- **Models** for:
  - GraphQL
  - OWL (Web Ontology Language)
  - ShEx (Shape Expressions)
  - TinkerPop-style property graphs

### Developer Tools

- **DSLs**:
  - Type construction DSL
  - Term construction DSL
- **Testing**:
  - QuickCheck property-based tests

### Implementations

- **Hydra-Haskell**: Bootstrapping implementation with full kernel
- **Hydra-Java**: Java implementation with mature tooling

---

## Version History

- **0.13.0** (2026-02-27) - Self-hosting in three languages, mutual cross-generation, Java and Python completion, Either type, native JSON, coder promotion
- **0.12.0** (2025-08-28) - Python progress, architectural improvements
- **0.8.1** (2024-09-24) - Property graph utilities
- **0.8.0** (2024-09-09) - C# support, Graphviz DOT, breaking changes
- **0.7.0** (2024-08-21) - Module reorganization
- **0.6.0** (2024-08-19) - System F, Python3 syntax
- **0.2** (2024-01-10) - Java improvements, validation
- **0.1.1** (2022-12-04) - Patch release
- **0.1.0** (2022-12-04) - Initial release

---

## Contributing

We welcome contributions! Please see:
- [Developer Recipes](https://github.com/CategoricalData/hydra/tree/main/docs/recipes)
- [LambdaGraph Discord](https://bit.ly/lg-discord)

## Release Process

See the [Release Process](https://github.com/CategoricalData/hydra/wiki/Release-process) wiki page for information on how Hydra releases are created.
