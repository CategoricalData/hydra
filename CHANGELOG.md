# Hydra Changelog

All notable changes to the Hydra project are documented in this file.

This changelog tracks changes across all Hydra implementations (Haskell, Java, Python) and supporting infrastructure.

The format is inspired by [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and [Apache TinkerPop](https://github.com/apache/tinkerpop/blob/master/CHANGELOG.asciidoc).

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
- Minimum Java version bumped to 18 (#249)
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
- Comprehensive developer recipes in `docs/src/recipes/`
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
- [Developer Recipes](https://github.com/CategoricalData/hydra/tree/main/docs/src/recipes)
- [LambdaGraph Discord](https://bit.ly/lg-discord)

## Release Process

See the [Release Process](https://github.com/CategoricalData/hydra/wiki/Release-process) wiki page for information on how Hydra releases are created.
