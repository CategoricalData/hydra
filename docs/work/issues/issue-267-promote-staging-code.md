# Issue #267: Promote All Staging Code

> **GitHub Issue**: [#267 - Promote all staging code](https://github.com/CategoricalData/hydra/issues/267)
>
> **Status**: Open
>
> **Created**: March 7, 2026
>
> **Category**: Code Quality / DSL Migration

## Executive Summary

Promote all "staging" code in hydra-ext from raw Haskell implementations to Hydra Sources DSL modules. This enables the code to participate in Hydra's self-hosting pipeline: generating equivalent implementations in Java, Python, and other target languages.

## Problem

The `hydra-ext/src/main/haskell/Hydra/Ext/Staging/` directory contains ~37 files (~7,600 lines) of raw Haskell code that defines language constraints, serialization logic, and coders. Because this code is written in plain Haskell rather than the Hydra DSL, it:

1. Cannot be automatically generated for other target languages (Java, Python, etc.)
2. Does not participate in the self-hosting bootstrap pipeline
3. Creates an inconsistency with the `Sources/` modules that do use the DSL

## Solution

Incrementally promote staging modules to Sources DSL following the recipe in `docs/src/recipes/promoting-code.md`. Priority order:

1. **Language modules** (6 files) - define language constraints and reserved words
2. **Serde modules** - serialization/deserialization logic
3. **Coder modules** - language-specific code generation

## Implementation Plan

### Phase 1: Language Modules

Promote 5 of 6 staging Language modules to Sources DSL:
- `Staging/Avro/Language.hs` → `Sources/Avro/Language.hs`
- `Staging/Graphql/Language.hs` → `Sources/Graphql/Language.hs`
- `Staging/Pegasus/Language.hs` → `Sources/Pegasus/Language.hs`
- `Staging/Scala/Language.hs` → `Sources/Scala/Language.hs`
- `Staging/Shacl/Language.hs` → `Sources/Shacl/Language.hs`

Deferred: `Staging/Tinkerpop/Language.hs` (polymorphic, runtime-parameterized).

### Phase 2: Serde Modules (future)

### Phase 3: Coder Modules (future)
