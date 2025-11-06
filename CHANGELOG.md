# Hydra Changelog

All notable changes to the Hydra project are documented in this file.

This changelog tracks changes across all Hydra implementations (Haskell, Java, Python, Scala) and supporting infrastructure.

The format is inspired by [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and [Apache TinkerPop](https://github.com/apache/tinkerpop/blob/master/CHANGELOG.asciidoc).

---

## [0.13.0] - Upcoming

Next release with Either type support and library improvements.

### Breaking Changes

- Renamed `Optional` to `Maybe` throughout the codebase (#204)
  - `hydra.lib.optionals` → `hydra.lib.maybes`
  - All `optional` variants renamed to `maybe`
- Removed deprecated `hydra.decoding` module
- Removed old `hydra.mantle.Either` type (replaced by core `Either`)

### New Features

- **Either Type Support** (#210)
  - Added `EitherType` to core type system
  - Added `either` term constructor
  - New `hydra.lib.eithers` library
  - Full support in type inference, type checking, rewriting, encoding/decoding
  - Added Either support in Python implementation
  - Comprehensive test coverage (inference and type checking)
- **Developer Documentation**
  - Added developer recipes to in-repo documentation
  - Created guides for extending Hydra Core and creating new implementations
- **Library Additions**
  - Added `hydra.lib.tuples` for tuple operations
  - Added library DSLs for each primitive library
  - New math functions: `abs`, `pred`, `signum`, `succ`, `even`, `odd`
  - Renamed `neg` to `negate` for consistency with Prelude
  - Completed floating-point math primitives in Python (#208)

### Improvements

- Included all Hydra kernel types in test schema (#205)
- More efficient implementation of `liftLambdaAboveLet` (#202)
- Reorganized Haskell DSL definitions for better clarity
- Added TODO comments to all unsafe primitives (#201)
- Sanity-check tuple projection index against arity during inference
- Propagated rewriting fixes into Python

### Bug Fixes

- Fixed bug in type checking in connection with dead code
- Fixed bug in `removeTypesFromTerm`

---

## [0.12.0] - 2025-08-28

Major release focused on completing Hydra-Python and architectural improvements.

### Breaking Changes

- Removed `hydra.decoding` module (#190)
- Flattened dependency tiers; eliminated tier-1, tier-2, tier-3 organization (#184)
- Removed `typed` term variant (#173)
- Module naming changes: moved tabular and relational modules into kernel (#152)

### New Features

- **Python Implementation**: Hydra-Python is now complete and being tested for production readiness
  - Restructured Python project layout (#191)
  - Full library implementations including `hydra.lib.flows`, `hydra.lib.equality`, `hydra.lib.lists`, `hydra.lib.strings`, `hydra.lib.chars`
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

- **0.12.0** (2025-08-28) - Python completion, architectural improvements
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
- [Contributing Guidelines](https://github.com/CategoricalData/hydra/blob/main/CONTRIBUTING.md)

## Release Process

See the [Release Process](https://github.com/CategoricalData/hydra/wiki/Hydra-release-process) wiki page for information on how Hydra releases are created.
