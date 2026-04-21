# Hydra-TypeScript (DSL sources)

Haskell DSL sources for the TypeScript coder.
This package follows the standard `packages/` convention: it contains only the
Haskell modules that *describe* TypeScript as a Hydra target language.

The hand-written TypeScript runtime (primitives, DSL helpers, tests) lives in
[`heads/typescript/`](../../heads/typescript/).
Generated TypeScript output lives in [`dist/typescript/`](../../dist/typescript/).

## Contents

| Path | Purpose |
|------|---------|
| `src/main/haskell/Hydra/Sources/TypeScript/Syntax.hs` | TypeScript AST model |
| `src/main/haskell/Hydra/Sources/TypeScript/Language.hs` | Language constraints |
| `src/main/haskell/Hydra/Sources/TypeScript/Serde.hs` | AST serializer |
| `src/main/haskell/Hydra/Sources/TypeScript/Operators.hs` | Operator definitions |
| `src/main/haskell/Hydra/Sources/TypeScript/Manifest.hs` | Package manifest |
| `src/main/haskell/Hydra/Dsl/TypeScript/Helpers.hs` | DSL helper functions |

See [`heads/typescript/README.md`](../../heads/typescript/README.md) for build
instructions, design notes, and the full TypeScript runtime documentation.
