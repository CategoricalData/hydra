# Hydra changelog

All notable changes to the Hydra project are documented in this file.

This changelog tracks changes across all Hydra implementations
(Haskell, Java, Python, Scala, Clojure, Common Lisp, Emacs Lisp, Scheme)
and supporting infrastructure.

The format is inspired by [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and [Apache TinkerPop](https://github.com/apache/tinkerpop/blob/master/CHANGELOG.asciidoc).

Pre-1.0 versions are listed below.
Untagged versions correspond to in-repository version bumps that were not pushed as Git tags or Hackage releases;
they are documented here for completeness.

---

## [Unreleased] (in development for 0.16.0)

In progress on the `staging` branch.
Major themes: kernel-oriented validation rules, JSON Schema coder promotion,
homogenization of writer conventions, and a unified `Module.dependencies` field.

### Highlights

- **Kernel-oriented style checks** in `hydra.validate.*` (#321, #351, #352).
- **JSON Schema coder promoted** to a fully functional DSL coder (#350).
- **Homogenized writer conventions** across all target languages (#339).
- **Unified `Module.dependencies`** (replaces split term/type dependency fields) (#354).

### New features

- Kernel-oriented style checks in `hydra.validate.*` (#321):
  alphabetical ordering of kernel definitions (#351),
  `doc` annotations on every kernel definition (#352).
- JSON Schema coder promotion (#350): full DSL-based coder; `forall` and type
  application support; schema/language namespaces lifted.

### Improvements

- Homogenized writer conventions across all target languages (#339):
  `*ToExpr` writer renames, adaptive layout for 120-width compaction.
- Merged `Module.termDependencies` and `Module.typeDependencies`
  into a single `Module.dependencies` field (#354).
- Migrated kernel definition names with underscores to camelCase (#348).
- Eliminated `Hydra.Module.Compat` shim (#315).

### Bug fixes

- Satisfied or suppressed warnings in generated Java (#349).
- Restored JSON parser test cases (#336):
  fixed polymorphic case-statement handling in the eta-expander
  (`Reduction.etaExpandTerm`) and the Java coder (`encodeElimination`),
  added a structural fallback to Python's `hydra.lib.equality.compare`
  for term values that lack a native `<` ordering.

### Removed

- Removed the `bigfloat` literal type from `FloatType` and `FloatValue` (#330).
  Arbitrary-precision floating-point values were inconsistently supported
  across host languages (Java/Scala in particular lacked a true IEEE 754
  arbitrary-precision implementation), and nothing in the codebase relied on
  them. `FloatType` and `FloatValue` now have only `float32` and `float64`
  variants. Removed library primitives: `bigfloatToBigint`, `bigfloatToFloat32`,
  `bigfloatToFloat64`, `bigintToBigfloat`, `float32ToBigfloat`,
  `float64ToBigfloat`, `readBigfloat`, `showBigfloat`, `roundBigfloat`. New
  primitives `float32ToFloat64` and `float64ToFloat32` (the latter lossy)
  replace the removed bigfloat-routed conversions.

---

## [0.15.0] - 2026-04-29

A structural-cleanup release.
The repository is reorganized into a three-tree
`packages/` (DSL sources) / `heads/` (per-host runtime) / `dist/` (generated output)
layout (#290), the kernel is simplified (#251, #292, #332), and Java + Python now
ship as per-package publishable artifacts on Maven Central / PyPI / conda-forge
(#305).
Incremental, content-hash-based caches accelerate the sync pipeline (#247).
Three new generation-only targets — Coq (#326), WebAssembly (#325), and an
automatic-differentiation demo (#324) — are in progress as Claude collaborations.

### Highlights

- **Three-tree repository layout**: `packages/` (DSL), `heads/` (host runtime),
  `dist/` (generated) (#290).
- **Per-package distributions** for Java (4 Maven artifacts) and Python (5 wheels):
  `hydra-kernel`, `hydra-pg`, `hydra-rdf`, `hydra-{java,python}`, plus `hydra-ext`
  on PyPI (#305).
- **Kernel simplifications**: `Function`/`Elimination` removed (#332),
  `FunctionPrimitive` removed (#251), `Hydra.Ext.*` prefix retired (#331),
  `Context` error wrapper gone (#292).
- **Incremental caches** across the sync pipeline; ~70x speedup on no-op resync (#247).
- **JSON kernel format v1** with formal wire-format spec and four-field rename (#343).
- **Three new generation-only targets** (in progress): Coq (#326), WebAssembly (#325),
  automatic-differentiation demo (#324).

### New features

- Per-package distributions (#305): standalone Maven / PyPI / conda-forge builds,
  generated `build.gradle` / `pyproject.toml` per package, transitive dep resolution.
- JSON kernel format v1 (#343): formal spec at `docs/json-format.md`,
  four-field rename to `typeScheme`, `Module` field reorder, `formatVersion` stamp.
- `decimal` literal type in the kernel (#338) across all hosts.
- Type-directed JSON encoder with idiomatic optional encoding (#314, #318).
- Incremental type inference plus content-hash caches (#247).
- Coq generation target (#326, in progress): `hydra-coq` package, 131/131 `.v` files pass `coqc`.
- WebAssembly target (#325, in progress): `hydra-wasm` package, M4 closure mechanism.
- Automatic-differentiation demo (#324, in progress): symbolic source-to-source.
- `hydra.show.error` module (#265): consolidated error-message builders.
- `hydra.lib.maybes.toList` primitive (#257).
- NaN / Inf round-tripping through JSON and per-language serdes (#312, #318, #330).
- Typeclass inference test group (#274).
- Term and type linters (#232).
- Validation for modules and graphs (#155).
- User-defined-function (UDF) support and DSL-vs-primitive overrides
  (#62, #63, #102, #158).
- Stale-source detection across sync entrypoints (#228).

### Improvements

- Sync infrastructure: matrix tool, per-package orchestrator, per-language wrappers (#290).
- TestGraph post-generation patches eliminated; DSL emits `TestEnv` refs directly.
- Minimized standard imports in generated Haskell (#161); improved auto-aliases (#322).
- Removed kernel term dependencies from the interpreter (#257).
- Removed `Graph` and `Context` arguments from primitive implementations (#266).
- Refactored kernel term modules: split rewriting/schemas, renamed show.meta → codegen,
  moved `Coder` to `coders`, merged `extract.helpers` (#221).
- Removed Aeson and HsYAML dependencies; YAML coder moved to `hydra-ext` (#261).
- Lisp recursive let bindings via SCC (#341); parenthesis cleanup (#84).
- Everything-to-everything bootstrapping demo (#254).
- Re-enabled skipped Python tests; `disabledForPython` retired (#263).
- Promoted host-specific code in `packages/` into per-head locations (#337).
- Migrated `hydra.common` Java module out (#10).
- Test infrastructure unification under `UniversalTestCase` (#246).
- Default property graph → RDF mapping (#296).
- 13 unsafe partial primitives replaced with `Maybe`-returning alternatives (#201).
- Java generated code uses standard `java.util` collection interfaces (#313).
- `Term.union` renamed to `Term.inject` (#334).
- Inconsistencies in domain DSLs cleaned up (#219).
- Coder packages moved out of hydra-haskell (#182).
- Java package-naming scheme revised (#223).
- Decimal used (instead of bigfloat) for JSON numbers (#340).
- Hydra kernel moved out of hydra-haskell (#282).

### Bug fixes

- `substInTypeScheme` capture-unsafe substitution under nested forall binders.
- Java `Float64` arithmetic primitives corrected for current `PrimitiveFunction` API.
- Clojure decimal handling: value-based equality, `BigInteger` return from `decimal_to_bigint`.
- Scheme runtime R7RS imports for `sets.scm` / `maps.scm` / `eithers.map_set`.
- Python coder defaults corrected from `Py.Name` to `Py.Expression`.
- Scala host JSON-decode failure on modules containing term definitions (#342).
- Lisp coder mutually-recursive let bindings for Clojure (#341).

### Documentation

- Documentation refresh for the 0.15 packaging restructure (#290, #331); CLAUDE.md,
  READMEs, recipes, demos, and seven wiki pages updated.
- Code-generation recipe rewritten for the per-package dist layout (#282).
- New `maintenance.md` recipe consolidates non-source-file scans, stale generated
  detection, design-violation checks, and freshness checks.
- Documentation style guide added at `docs/documentation-style-guide.md`.
- Cross-worktree messaging protocol documented in CLAUDE.md.
- Decimal type documented in the lexicon and per-language READMEs (#338).
- JSON encoding format docs updated for idiomatic optional encoding (#314).

---

## [0.14.1] - 2026-03-30

Patch release.
Regenerates all implementations after a fix to the group eval primitive (#281),
and relocates the property-graph and RDF/SHACL "ext" modules into hydra-java and hydra-python.

### Improvements

- Moved PG, RDF, and SHACL ext modules from hydra-ext into hydra-java and hydra-python.

### Bug fixes

- Group eval primitive fix; all targets regenerated (#281).
- JavaDoc HTML-entity escaping in generated `DestructuringPattern.java`.

---

## [0.14.0] - 2026-03-29

Major release adding four new complete Hydra implementations — Scala (#273),
Clojure (#278), Common Lisp, and Scheme — bringing the total to seven bootstrapping hosts
(plus Emacs Lisp, which passes the test suite but does not yet participate in bootstrapping).
Also includes kernel simplifications, an Avro bidirectional coder (#301),
eval primitives (#281), new demos, and significant performance work on the Scheme host.

### Highlights

- **Four new complete implementations**: Hydra-Scala (#273), Hydra-Clojure (#278),
  Hydra-Common Lisp, and Hydra-Scheme pass the common test suite and can serve as
  bootstrapping hosts.
  The four Lisp dialects share a single coder and serializer.
- **Kernel simplifications**: removed `RowType`/`WrappedType` (#82),
  unified `Graph`/`TypeContext`/`InferenceContext` (#192),
  replaced `Module.elements` with `Module.definitions` (#214),
  consolidated error types (#268), and promoted all staging modules to DSL sources (#267).
- **Avro bidirectional coder** (#301) with property graph pipeline demo.
- **Scheme bootstrap performance**: 58 minutes → 5.7 minutes via O(1) vhash data structures
  and IEEE 754 float precision fixes.

### Breaking changes

- Removed legacy adapter framework; replaced by the staging-promoted variant (#236, #295).
- Replaced legacy Flow monad with Either (#245).
- Removed `Module.elements`; use `Module.definitions` (#214).
- Removed `RowType` and `WrappedType` (#82).
- Renamed `hydra.accessors` to `hydra.paths` (#271).
- Renamed `hydra.tarjan` and merged into `hydra.sorting` (#220).
- Merged `hydra.constraints` into `hydra.query` (#272).
- Moved `hydra.workflow` to hydra-ext (#270).
- Consolidated `hydra.compute` into `hydra.util` (#269).
- Removed `hydra.describe` (#216) and types from `hydra.mantle` (#217).
- Replaced `OtherError` with task-specific error types (#268).
- Java: lowered minimum Java version to 11 (#249).
- Java: `Pair` replaces `Tuple2` (#252); `Comparable` implemented for generated classes (#131).

### New features

- Hydra-Scala (#273): complete implementation, sbt/Scala 3, 3043 tests.
- Hydra-Lisp: Clojure (#278), Common Lisp, Emacs Lisp, Scheme — four dialects,
  one coder. CI workflows for Clojure, Common Lisp, and Scheme.
- Avro bidirectional coder (#301) with demo.
- Eval primitives (#281): Logic, Equality, Math, collections, and Groups.
- Generated DSL modules (#180): `hydra.dsls` generates DSL helpers from type modules.
- New demos: GraphQL+JSON (#279), PG validation (#284), SHACL (#294), Avro+PG.
- Packaging module (#290): `hydra.packaging` with `Package` type and validation.
- New primitives: `lists.foldr` (#280), `maybes.toList`, float rounding (#264, #285).
- Cross-language benchmarking suite (#234).
- Limited typeclass inference (#164).
- Standardized validation patterns across modules (#291).

### Improvements

- Promoted all staging modules to DSL sources (#267).
- Removed legacy adapters, JSON/YAML coders, grammar framework (#236, #295).
- Java: efficient immutable data structures (#193); flattened term naming.
- Java: hidden specialized collection classes in type-level files (#313).
- Inference error messages improved (#231).
- Combined nested let bindings in the Haskell coder (#248).
- Simplified `hydra.adapt.simple` (#255).
- Removed Haskell operators dependency from tests (#288).
- Regression tests for non-Haskell/Java coders (#85).
- Regex support added (#293).
- `hydra.annotations.isNativeType` revisited (#214).
- Migrated grammar-generated models (#295).
- `<<= -shouldBe-> <<~` operator change (#276).
- Minimize Generation.hs (#225).
- Implemented missing Flows primitives (#183).
- Removed JavaDocs from the repository (#262).
- Floating-point test portability fixes for Linux CI (#264, #285).
- Moved YAML coder to hydra-ext (#224).
- Minimized native Python and Java primitives in favor of DSL implementations (#244).
- Removed the `disabledForPython` test tag (#250).

### Release tooling

- 0.14 release tracking (#304).

### Documentation

- Thorough cleanup of user documentation (#302, #303).
- Coding style guide added to wiki.
- Updated 'new implementation' recipe with learnings from Clojure head (#278).

---

## [0.13.0] - 2026-02-27

Major release completing Hydra-Python (#66) and Hydra-Java (#166) as self-hosting Hydra implementations
and demonstrating mutual self-hosting across all three languages.
Significant improvements to type inference, type checking, rewriting, and adaptation.
New language features include first-class Either types, typeclass inference, binary data support,
and a native JSON parser/writer.
Comprehensive tooling for cross-implementation bootstrapping
and provisional support for Go, Rust, and JavaScript targets.

### Highlights

- **Mutual self-hosting across three implementations**: Hydra is now fully self-hosting
  in Haskell (#179), Java, and Python, and can cross-generate between every combination
  of host and target language (9 paths total).
- **Hydra-Java is complete** (#166): all kernel and generation tests pass.
- **Hydra-Python is complete** (#66): 100% test parity with Haskell.
- **Language coders promoted into the Hydra kernel** (#176): Haskell, Java, and Python
  coders are now defined as Hydra DSL modules, making them self-hosting.
- **Either type support** (#210) with full inference, checking, and library support.
- **Native JSON parser and writer** (#188, #243, #253): Hydra's own JSON parser and writer
  replace the Aeson dependency; bootstrap-from-JSON architecture for cross-implementation
  code generation.

### Breaking changes

- Renamed `Optional` to `Maybe` throughout the codebase (#204):
  `hydra.lib.optionals` → `hydra.lib.maybes`; all `optional` variants renamed to `maybe`.
- Removed unlabeled product and sum types; replaced with pair types (#212).
- Renamed `hydra.core.TypedTerm` to `hydra.core.TypeApplicationTerm`.
- Renamed `hydra.json` to `hydra.json.model`.
- Renamed `hydra.lib.maps.remove` to `hydra.lib.maps.delete`.
- Removed deprecated `hydra.decoding` module.
- Removed old `hydra.mantle.Either` type (replaced by core `Either`).
- Removed `hydra.lib.tuples` library.
- Removed C# coder stub (C# syntax model remains).
- DSL syntax migration: removed `OverloadedStrings` for term expressions (#238).
- Removed `Hydra.Dsl.ShorthandTypes` (#222).

### New features

- Self-hosting in Haskell (#179) and full mutual cross-generation (Haskell ↔ Java ↔ Python).
- New bootstrapping demo validating all 9 paths across 249 modules.
- Promoted Haskell, Java, and Python coders from staging code into Hydra DSL sources (#176).
- JSON-as-source-of-truth bootstrapping architecture (#243, #253):
  new executables and scripts enabling `bootstrap-from-json` for cross-implementation
  code generation.
- Generated term encoders and decoders replace legacy hand-coded modules (#47).
- Hydra-Python completion (#66): restructured project layout with `src/main` and
  `src/gen-main` (#191); full library implementations including `hydra.lib.flows`;
  `@lru_cache` optimization for nullary bindings; PyPy compatibility.
- Hydra-Java completion (#166): all kernel and generation tests pass; deep changes
  to Java coder; `Lazy` utility class for delayed evaluation; array-type support
  in Java serde; tail-call optimization in the Java coder; `Tuple` implements `Comparable`.
- Either type (#210): `EitherType`, `either` term constructor, `hydra.lib.eithers` library
  with `either`, `map`, `mapList`, `mapMaybe`, `bimap`, `bind`, `fromLeft`, `fromRight`,
  `isLeft`, `isRight`, `lefts`, `rights`, `partitionEithers`.
- Pair type as a core construct (#211).
- Void type (#237): uninhabited `void` type as the dual of `unit`; type-level only.
- Native JSON parser and writer (#188, #242):
  bidirectional JSON encoder/decoder; replaces the Aeson dependency;
  cross-checked via a special test runner.
- Typeclass inference (#164):
  type schemes with typeclass constraints via `TypeVariableMetadata`;
  typeclass metadata on primitive definitions; Haskell coder uses typeclass info.
- Binary/`ByteString` support (#172): binary data via `ByteString` instead of `String`;
  Haskell coder uses `Data.ByteString`; new `binaryToBytes` primitive.
- Parser combinators: new `hydra.parsing` module with combinator types and DSL.
- Higher-order primitive interpreter (#198): all primitives are fully interpretable
  across all three implementations; legacy `requiresInterp` tag removed.
- Let hoisting and flattening: hoisting polymorphic let terms;
  refined hoisting for monomorphic bindings inside polymorphic ones;
  more efficient `liftLambdaAboveLet` (#202);
  case-statement hoisting (#241); subterm hoisting helpers.
- Provisional language targets: Go (#65), Rust (with experimental type-level coder),
  JavaScript (syntax model, serde, DSL, generated sources), and Java/Python syntax DSLs.
- New adapter framework (#236): all external coders refactored
  (Haskell, YAML, Scala, JSON Schema, C++, PDL, Protobuf, GraphQL).
- Compact labeled records and variants (#122).
- New primitives:
  `hydra.lib.lists.find`, `hydra.lib.lists.partition`,
  `hydra.lib.flows.withDefault`, `hydra.lib.flows.foldl`,
  `hydra.lib.math.{max,min,abs,pred,signum,succ,even,odd}`,
  `hydra.lib.pairs.bimap`, `hydra.lib.eithers.bind`,
  `hydra.lib.literals.binaryToBytes`, complete `readXxx`/`showXxx` families,
  `hydra.lexical.chooseUniqueName`, `neg` renamed to `negate`,
  full floating-point math primitives in Python (#208).
- DeepCore DSL: a layer one level deeper than the Meta DSLs,
  for programs that construct programs that construct terms.
- `typeOf` implementation (#168).
- Implement `Comparable` for generated Java classes (#131); update of Hydra-Java (#166).
- Compact labeled records exploration (#122).
- Comparison primitives without typeclasses (#187).
- Investigated core support for Either and Flow monads (#200).

### Improvements

- Type system:
  - Refactored `hydra.inference` to create new module `hydra.checking`.
  - Inference checks after unification to prevent schema names from being unified
    with inferred type variables.
  - Type application terms preserved when preparing application terms for Python.
  - Sanity-check tuple projection index against arity during inference.
  - Bind lost type variables in term annotations.
  - Made `extendTypeContextForLet` tolerant of untyped bindings.
- Code generation:
  - Python: support for deeply-nested match statements; more complete `let` support;
    Pythonic syntax for polymorphic function definitions; refinements for complex
    case-hoisting scenarios; performance improvements (#209, #239, #240); inline
    let/case rewriting for Python targets (#203).
  - Java: method declaration style consistent with calling style.
  - Haskell: standardized typeclass names.
  - Increased maximum trace depth to support more complex sources.
- Common test suite promotion: all tests promoted into a shared, implementation-independent
  test kernel (#213).
- Statically-compiled test suite in Haskell (#207).
- Included all Hydra kernel types in test schema (#205).
- Reorganized Haskell DSL definitions; standardized imports.
- Added TODO comments to all unsafe primitives (#201).
- New `rewriteAndFoldTerm` utility for simultaneous rewriting and folding.
- Lexical helper for dereferencing schema types through aliases.
- Made JSON parser lazy with new `lazy` parser combinator.
- More efficient substitution helpers for empty substitutions.
- Precomputed type/inference context in kernel test runner for performance.
- Property graph encoding and decoding modules promoted into DSL.
- `hydra.tabular` and `hydra.pg.graphson` modules promoted into DSL.
- `hydra.show.*` modules generated into Java and Python.
- Aligned epsilon encoding with the LambdaGraph spec (#89).
- Fixed asymmetry of introductions and eliminations (#86, #134).
- Minimized inferred type annotations (#114).
- Investigated combining products with records, sums with unions (#196, #212).
- Refined type reduction in adapters (#144).
- Investigation of unification issues (#163).
- Refactored RowType (#82).
- LLM-assisted PG schemas and mappings (#171).
- Simplified unit-valued variants in Python (#206).
- Checked for inconsistencies between Term/TTerm and Type/TType DSLs (#218).
- GenPG demo: refactored to support Haskell, Python, and Java; Python runner;
  Java generation; finalized output format.
- Testing: extended test runners for let hoisting (Haskell, Python, Java);
  floating-point precision tests; ordering tests for `hydra.lib.maps`/`hydra.lib.sets`;
  inference tests for inferred System F terms; case-hoisting test cases; new JSON coder tests;
  thorough test cases for all primitives (#199).

### Bug fixes

- Fixed interpreter bugs causing test failures (#235).
- Fixed bug in type checking in connection with dead code.
- Fixed bug in `removeTypesFromTerm`.
- Fixed bug in `typeOfMap`.
- Fixed 32-bit max int value in `hydra.constants`.
- Fixed numeric precision in Python.
- Added `BigDecimal` support and fixed `uint8` (short) support in Java.
- Fixed Haskell coder with respect to imports for binary literals.
- Excluded `Prelude.encodeFloat` and `Prelude.decodeFloat` from generated Haskell
  due to name collisions.
- Fixed consistency issues in Python primitives:
  `hydra.lib.sets.toList`, `hydra.lib.maps.union` (precedence),
  `hydra.lib.strings.lines`, `hydra.lib.strings.readString` (dequoting),
  `hydra.lib.lists.apply`, `hydra.lib.maps.toList`.
- Fixed Haskell type signature of `hydra.lists.span`.
- Fixed issue with eta expansion of typed terms.
- Fixed shadowing issue in generated encoding modules.
- Added missing alternatives in `hydra.rewriting.rewriteTermM`.

### Documentation

- Updated main README with complete implementation status.
- Added Documentation sections to all implementation READMEs.
- Fixed outdated Code-organization.md (Python now uses `src/gen-main`).
- Fixed incorrect code paths in Implementation.md.
- Added Java section to Testing wiki.
- Comprehensive developer recipes in `docs/recipes/`.
- Documentation for tail-call optimization implementation.
- Added Haddock comments to Haskell primitives.
- Created wiki page on the release process (#194).

### Community

- Accepted babeloff's `isTrivialTerm` changes.

---

## [0.12.0] - 2025-08-28

A consolidation release: significant Python progress, kernel-level cleanup,
and a wave of bookkeeping closures for Hydra-Java module ports.

### Breaking changes

- Removed `hydra.decoding` module (#190).
- Flattened dependency tiers; eliminated tier-1, tier-2, tier-3 organization (#184).
- Removed `typed` term variant (#173, #162).
- Module naming changes: moved tabular and relational modules into kernel (#152).
- Standardized on uncurried helper functions in term-level DSLs (#174).
- Renamed `hydra.lib.flows.traverseOptional` to `hydra.lib.flows.mapOptional`.
- Eliminated camel-cased namespace parts from the kernel (#152).
- Replaced list and optional eliminations with primitive functions (#150).
- Moved `Hydra.Ext` to hydra-ext (#178); moved 'ext' sources into hydra-ext (#189).

### New features

- Significant progress toward Hydra-Python completion:
  generated all kernel types and terms into Python;
  added `Decimal` support for bigfloat values;
  environment tracking for variable scoping;
  generated `hydra.languages` into Python;
  `__hash__` and `__eq__` on serializable generated classes (#160).
- Designated `unit` term and type variants (#186).
- `hydra.lib.flows.mapElems` and `hydra.lib.flows.mapKeys` utilities.
- Promoted JSON coder into the DSL (#181).
- Promoted JSON utilities into the DSL (#181).
- C# coder (#139).
- GQL model and parser (#140).
- Tabular adapters (#142).
- Type-level C++ coder (#170).
- Variable-types-to-Haskell-typedefs transformation (#70).
- A newline at the end of each generated file (#154).
- Refactor kernel modules and primitives by connectivity (#177).

### Improvements

- Condensed repeated elements in error traces (#165).
- Fixed transitive schema dependencies (#185).
- Improved adapter solution with respect to literals.
- Better handling of type variables in Python coder.
- Enhanced term-level Python generation with proper ordering to minimize forward references.
- Topo-sort helper function for dependency management.
- Updated all Python libraries with latest changes.
- Distinguished between deannotation and detyping of terms.
- Adapted primitives when applying language constraints to a graph.
- Use unqualified names where possible in generated code (#153).
- Eliminated `Data.Graph` (containers) dependency (#167).
- "Close the loop" demonstration (#175).
- Implemented `typeOf` (#168).

### Bug fixes

- Fixed System F terms for records and case statements (#168).
- Fixed issue with transitive type-level module dependencies (#185).
- Corrected handling of variable references in Python application terms.
- Fixed nullary functions in Python (added empty parens).
- Added missing alternatives in `hydra.rewriting.rewriteTermM`.

### Bookkeeping

- Closed long-standing Hydra-Java module-port issues
  (#9, #11, #15, #16, #18, #19, #20, #21, #22, #23, #25, #44, #45);
  most of the underlying work shipped in earlier releases.
- Closed Hydra-Java DSL-port issues (#24, #26, #27, #28, #29) and
  the Scala/Avro tracking issues (#77, #81); these had been resolved
  by earlier releases (Scala in 0.14.0, Avro bidi in 0.14.0).

### Documentation

- Updated Hydra-Haskell README.
- Updated Hydra-Java README with corrected links.
- Updated JavaDocs.

---

## [0.11.0] - 2025-03-16

Untagged in-repository version.
A type-inference rebase release: major restructuring of inference internals
to align with Algorithm W, plus a sweeping removal of explicit type annotations
across the kernel.

### Highlights

- **Inference rebased on Algorithm W** (#118): unify-early strategy (#146),
  infinite-type checks restored (#116), recursive-element inference (#90),
  unified handling of elements and let bindings (#112), minimized manual
  type annotations (#119).
- **`hydra.inference` module** introduced for inference type definitions;
  substitution and unification moved up a level.

### Breaking changes

- Argument order of `Logic.ifElse` changed (#147).

### New features

- Inference test cases promoted into the common test suite (#148);
  added "kernel examples" inference tests; more recursion / mutual recursion cases.

### Improvements

- Removed type annotations across all kernel sources (#119).
- Migrated to fully-applied primitive library DSLs (#157).
- Aligned `Graph` and `Element` with `LetBinding` (#159).
- `hydra.core.Unit` recognized by the Haskell coder.
- Refactored inference tests.

### Bug fixes

- Fix for underdetermined type in `hydra.decode`.

---

## [0.10.0] - 2025-02-19

Untagged in-repository version.
Adds Hydra-Python (initial completion), a GraphSON coder,
provisional Hydra-Go support, and dot-separated namespace conventions.

### Highlights

- **Hydra-Python initial completion** (#80, #66): Python coder, generated kernel
  modules in Python, and full library implementations across kernel namespaces.
- **GraphSON coder** (#79).
- **Provisional Hydra-Go** (#65).
- **Namespace formatting** standardized to dot-separated form (#151).

### Breaking changes

- All namespaces use dot-separated form (#151).

### New features

- Hydra-Python: Python coder; generated `hydra.coreEncoding`, `hydra.strip`,
  `hydra.literals` and other kernel modules in Python; tuple-expression support;
  updated `hydra.constants` in Python.
- GraphSON coder (#79).
- Provisional Hydra-Go (#65).
- `string-to-char-list` and `char-list-to-string` primitives (#149).
- Restored normalization for generated term variables (#145).

### Improvements

- Re-leveled dependency tiers (#135).
- Updated all Java, Haskell, hydra-ext, and Python primitives to current sources.

---

## [0.9.0] - 2025-01-06

Untagged in-repository version.
Bootstraps Hydra-Python and the JSON Schema coder; introduces a fluent Java DSL for flows.

### Highlights

- **JSON Schema coder** (#141).
- **Python coder scaffold**: serde, language constraints, and generated Haskell
  sources for Python.
- **Fluent-style Java DSL for flows** (#143).

### New features

- JSON Schema coder (#141).
- Python coder scaffold and serde.
- Provisional Python language constraints module.
- Fluent-style Java DSL for flows (#143); moved Flows DSL into `hydra.dsl`.
- New `hydra.coders` DSL.
- New DSL functions for core variants.

### Improvements

- Generalized `Namespaces` so it can be used for languages other than Haskell.
- Local-name-only Python type aliases (the directory tree provides the namespace).
- Coders DSL used to simplify Java language-constraint sources.
- Removed redundant productions in the Python grammar.
- Added unit tests for fluent flows.

### Documentation

- Updated JavaDocs for 0.9.0.

---

## [0.8.1] - 2024-09-24

Patch release.

### Improvements

- Generalized the property graph merging utility to accept type systems other than Hydra Core.

---

## [0.8.0] - 2024-09-09

C# support (#139), Graphviz DOT coder, term accessors, and a small breaking change in Java
to support Spark.

### Breaking changes

- Made `FlowException` serializable for Spark support (may affect existing error handling).

### New features

- C# syntax module based on the Microsoft ANTLR grammar (#139).
- Graphviz DOT coder (#136):
  full support for lambdas and recursive `let` statements;
  compact 'accessor graph' visualization option;
  customizable label styles;
  highlighted let-bound terms; edge labels.
- Term accessor type to facilitate lenses and path-aware transformations.
- OpenGQL grammar and generated Haskell for the OpenGQL model (#140).

### Improvements

- Added `</>` as alternative to `@@` application operator in Haskell DSLs.
- Utilities for working with term accessors.
- More specific exception class for JSON decoding.
- Additional convenience methods on `JsonDecoding.java`.
- Used transitive dependencies in hydra-java and hydra-ext.
- Upgraded ANTLR to address a vulnerability.
- Moved TinkerPop utilities from `hydra/ext/org/apache/tinkerpop` into `hydra/pg`.

---

## [0.7.0] - 2024-08-21

Major refactoring of module organization and namespace management.
Establishes the DNS-style namespace convention in hydra-ext
and lifts property-graph models into a top-level `hydra/pg` namespace.

### Breaking changes

- Namespace reorganization: migrated to DNS-based module naming in hydra-ext (#138):
  `hydra/ext/avro` → `hydra/ext/org/apache/avro`,
  `hydra/ext/graphql` → `hydra/ext/org/graphql`,
  `hydra/ext/yaml` → `hydra/ext/org/yaml`,
  `hydra/ext/json/decoding` → `hydra/ext/org/json/decoding`,
  `hydra/ext/rdf` → `hydra/ext/org/w3/rdf`,
  `hydra/ext/shacl` → `hydra/ext/org/w3/shacl`,
  `hydra/ext/tinkerpop` → `hydra/ext/org/apache/tinkerpop`.
- Property graphs: promoted property graph modules from
  `hydra/ext/org/apache/tinkerpop` to `hydra/pg`.
- Project structure: renamed `hydra-extensions` to `hydra-ext`;
  renamed `hydra/langs` to `hydra/ext` (#138);
  made hydra-ext into a Gradle subproject alongside hydra-java;
  added top-level Gradle build.

### Improvements

- Moved TinkerPop modules from hydra-haskell into hydra-ext for better separation.
- Moved miscellaneous models to `Hydra.Ext.Other` (#138).
- Moved XML Schema, SQL, ShEx, and OWL models to hydra-ext.
- Moved KQL, Parquet, and Python modules to hydra-ext.
- Separated JavaDocs for hydra-java and hydra-ext.
- Updated publishing configuration in `build.gradle`.

---

## [0.6.0] - 2024-08-19

Type-system improvements (System F type abstraction/application; eliminated polymorphic
types from Hydra Core), expanded language support (Graphviz DOT model, generated Java
field-name constants), and revamped annotation handling.

### Breaking changes

- Eliminated `InferenceContext` helper type (#103).
- Changed typing environment from `Map Name Type` to `Map Name TypeScheme` in graphs (#76).
- Updated annotations and flows to use `map<Name, Term>` instead of `map<string, Term>` (#133).
- Unified `Name` and `FieldName`; aliased to `string` (#121).
- Renamed `hydra/core.UnitType` to `hydra/core.Unit` (#123).
- Eliminated annotation classes; Hydra has a single built-in notion of annotations (#113).
- Unified the `Nominal` type with `RowType` (#115).
- Removed the `extends` parameter for row types (#132).
- Replaced `hydra/Kv` with `hydra/Annotations` (rename revisited).

### New features

- System F support: `typeAbstraction` and `typeApplication` term constructors.
- Python3 syntax model based on the official Python BNF (#80).
- Graphviz DOT model and initial support (#136).
- Field-name constants generated in Java code generation (#137).
- Provisional Algorithm W implementation in Haskell (originally from @wisnesky), kept for reference.

### Improvements

- Primitive organization: Hydra primitives organized into libraries for namespace management.
- Code generation:
  - Escaped field name constants and `with_` methods in Java coder.
  - Generated field name constants in Java (#137).
  - Avoided duplicated comments in wrapper classes in generated Java.
  - Removed superfluous newtype comments in Haskell coder.
- Updated Delta Parquet model to follow the Java API more closely.
- Enriched OpenCypher features module with the complete list of standard Cypher functions.
- Added convenience methods to `JsonEncoding.java`.
- Restored `Hydra.Inference` subdirectory.
- Allowed arbitrary case for element names (#7).
- Corrected `uint8` representation in Java to `Short` (#120).

### Bug fixes

- Fixed handling of type annotations in term adapters.
- Fixed decoding of encoded terms annotated with a type.
- Corrected encoding of universal types in `showTerm` (#117).

---

## [0.5.3] - 2024-08-06

Patch release.
Preserves order of map key/value pairs and set elements during JSON serialization.

### Bug fixes

- Preserved ordering of key/value pairs and set elements in JSON output.

### New features

- Polymorphic `equal` primitive in Haskell and Java.
- Generated Hydra type definitions together with native instantiations of Hydra types
  in Haskell (a stepping stone toward generated coders, #47).

---

## [0.5.1] - 2024-08-01

Patch release.
Dependency updates and minor improvements following the 0.5.0 Hydra Core overhaul.

### Improvements

- Updated dependencies.

---

## [0.5.0] - 2024-07-26

Untagged in-repository version.
Major Hydra Core overhaul: eliminates polymorphic types in favor of explicit
TypeScheme; consolidates the annotation system; revamps `Let` term encoding.

### Breaking changes

- Eliminated polymorphic types from Hydra Core in Haskell (#125):
  removed type parameters from `hydra/core` types in Hydra-Java; replaced type schemes
  with forall types (#76).
- Revamped core encoding and `Let`: bindings are now a list of
  `(name, term, optional type scheme)` triplets instead of a map.
- Unified typed-term types and moved `TypeScheme` into Hydra Core.
- Eliminated `AnnotationClass`; Hydra has a single built-in notion of annotations now (#113).
- Renamed and refactored phantom-type wrappers (#128); removed deprecated `Reference`
  type from `hydra/phantoms`.
- Removed incomplete support for stream types and stream terms (#88).

### New features

- `LiteralTypes` DSL module so literals can be built independently of terms.
- `Hydra.Minimal` module to facilitate collaboration on Hydra application prototypes.
- `TypeConstraint` type added to the kernel.
- `foldl` primitive in Haskell and Java; DSL uses `foldl` instead of the built-in
  list elimination term by default.

### Improvements

- Improvements to Java and JSON coders; improved untyped JSON coder.
- Pre-order and post-order term-traversal test cases added.
- Refactored type inference tests.
- Restored generated Java (Core through Tier 3) with only a few manual tweaks.
- Element names allow arbitrary case (#7).

---

## [0.4.0] - 2024-07-10

Untagged in-repository version.
Adds null checks to generated Java (breaking, but justified — Hydra-Java disallows nulls),
the Gremlin model, and Protobuf annotation refinements.

### Breaking changes

- Null checks added to constructors and `withXXX()` methods in generated Java;
  null values are no longer permitted in Hydra-Java.
- Removed Java 8 restriction (raised minimum Java version).

### New features

- Gremlin model (#127); generated Haskell for Gremlin.
- `hydra/lib/lists.safeHead` primitive in Haskell (necessary in Java due to eager evaluation).
- Support for Protobuf `deprecated=true` annotation; non-string Protobuf options.
- Java options in generated Protobuf.

### Improvements

- Used `java.util.Objects.requireNonNull` instead of a custom null check in generated Java.
- Added 'automatically generated file' disclaimer to generated Java files.
- Added autogen comments and null checks to (most) generated Java.
- Added necessary schema-level dependencies for built-in modules.
- Added null checks to the Flows DSL in Java.

---

## [0.3.0] - 2024-06-05

Untagged in-repository version.
Adds an OpenCypher parser based on an ANTLR grammar (#124),
a KQL (Kusto Query Language) model and serializer,
a Delta Parquet model,
and an early property graph queries model in the style of Cypher.

### New features

- OpenCypher parser based on the OpenCypher M23 ANTLR grammar (#124);
  ANTLR added as a build dependency;
  large Cypher test suite drawn from the Cypher Manual.
- Cypher-to-PG-query transformer (Java; partially implemented).
- Property graph queries DSL and generated property graph query classes in Java.
- Property graph queries model (Cypher / future GQL style).
- KQL (Kusto Query Language) initial model and basic serializer.
- Delta Parquet model with generated sources.

### Improvements

- Java options and `deprecated=true` support in generated Protobuf.
- Convenience methods in Java.

---

## [0.2.0] - 2024-01-10

Hydra-Java 0.2.0 release.
Significant Java tooling and DSL build-out, Protobuf bidirectional coder,
Python3 syntax model (#80), property graph validation (#100),
JSON bidirectional support in Java (#104),
OpenCypher initial model (#108),
Cypher unification, and the start of the typed-DSL story in Haskell.

### New features

- Property graph validation: validator type, optionality on property types,
  customizable id/property handling in the merging adapter, fine-tuned validation,
  comprehensive unit tests in Java (#100).
- Bidirectional JSON serialization/deserialization in Java (#104).
- Checkstyle configuration based on Google style guide (#111).
- Python3 syntax model based on the official Python BNF (#80).
- Initial OpenCypher model and parser scaffold (#108).
- Protobuf coder (#99): type encoder, field numbering, formatting, deprecated annotation,
  Java options, non-string options.
- Adapter framework tolerance of recursive types (#58); default branch for case
  statements (#60).
- Topological sort in type inference (#67).
- `hydra/query` model (#73).
- Records-to-TinkerPop-elements coder (#64).
- Equality primitives for all literal types (#92); polymorphic logic primitives (#95).
- `mapKeys` primitive (#93).
- Tuple support in Java (#94).
- Java 8 as an optional target (#96); optionally generate Java classes as `Serializable` (#97).
- Bidirectional "merged vertex" and "merged edge" coders (#106).
- Nullability and parameterization in the tabular model (#107).
- Term-level / type-level dependency distinction for modules (#109).
- Flatten nested `let` terms for transformation into Java (#110).
- Various `Flow` utilities in Java: `bind3`; convenience methods for consuming results;
  `fromFlow` variants including one that throws.

### Improvements

- Hydra-Java module ports (#10, #12, #13, #14, #17, #30, #31, #32, #33, #34, #35,
  #38, #39, #41, #43, #48, #71, #72, #75, #83):
  `Common`, `CoreEncoding`, `CoreLanguage`, `Kernel`, `Flows`, `Lists`, `Literals`,
  `Math`, `Sets`, `Strings`, `Optionals`, `Maps`, `Flows` primitives, `Sets` primitives.
- Common DSLs in Haskell: Flow support (#68), Let support (#69).
- Renamed annotation type parameter `m` to `a` (#41).
- Updated wrapper, element, and variable types (#75).
- Removed `schema` field from `Element` (#83).
- GraphQL coder (#78).
- Code quality:
  - Updated main and test sources to conform to Checkstyle configuration (#111).
  - Extended line-length limit from 100 to 120 characters.
  - Disabled overly restrictive indentation and import-order rules.
  - Disabled `RightCurlyAlone` for compact inline map definitions.
- Streamlined property-graph element merging to unify properties with identical
  keys and types (#106).
- Convenience functions for printing literals and literal types in the Java DSL.
- Minor addition to JSON decoding.
- DSL convenience: alphabetic-case flexibility for element names (#7).

### Bug fixes

- Fixed type annotations for the Sets Java port (#48).
- Various minor fixes during the Java module ports.

---

## [0.1.1] - 2022-12-04

Patch release the same day as 0.1.0.
README polish only.

---

## [0.1.0] - 2022-12-04

First packaged release of Hydra (Hackage).
This release contains the complete foundation of Hydra.

### Core language

- Hydra's core type and data languages (`hydra.core`).
- Core models for graphs and modules.
- Computation model with the `Flow` monad.
- BNF grammars support.
- Phantom types.
- Basic operations on types and terms.

### Type system

- Hindley–Milner-style type inference.
- Type schemes with polymorphism.
- Type/term validation.

### Transformations

- Adapter system: type-to-type rewriting and transformation.
- Coders:
  Haskell coder (types and terms),
  Java coder (types and terms),
  partial Scala coder (terms only),
  Avro coder,
  JSON coder,
  PDL (Pegasus Data Language) coder,
  RDF + SHACL coder,
  YAML coder.

### Language support

- Models for GraphQL, OWL (Web Ontology Language), ShEx (Shape Expressions),
  and TinkerPop-style property graphs.

### Developer tools

- Type-construction DSL and term-construction DSL.
- QuickCheck property-based tests.

### Implementations

- Hydra-Haskell: bootstrapping implementation with full kernel.
- Hydra-Java: Java implementation with mature tooling.

### Community

- Set up the LambdaGraph Discord server (#2).

---

## Contributing

We welcome contributions! Please see:

- [Developer recipes](https://github.com/CategoricalData/hydra/tree/main/docs/recipes)
- [LambdaGraph Discord](https://bit.ly/lg-discord)

## Release process

See the [Release process](https://github.com/CategoricalData/hydra/wiki/Release-process)
wiki page for information on how Hydra releases are created.
