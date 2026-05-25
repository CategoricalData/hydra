# Hydra-Ext

**Extension coders** for Hydra: schema languages and data formats outside the
core `hydra-pg` / `hydra-rdf` set. This package holds DSL sources only — generated
output lands under `dist/<lang>/hydra-ext/` for each target language that the
package supports.

Hydra is a functional programming language based on the
[LambdaGraph](https://bit.ly/lg-kgc2024) data model. See the main Hydra
[README](https://github.com/CategoricalData/hydra) for project overview and use cases.

## What's in this package

Coders, syntax models, and language metadata for:

- **Schema languages and IDLs** — Avro, Protobuf, GraphQL, Pegasus/PDL,
  JSON Schema, YAML, SQL (ANSI), Datalog, Kusto KQL.
- **Generation targets** — C++, C# (Csharp), Rust.
- **Domain models** — Workflow, Atlas, Azure DTDL, GeoJSON, IANA Relations,
  OSV (Open Source Vulnerability), STAC Items, Parquet/Delta Parquet.

The full list is maintained in
[`src/main/haskell/Hydra/Sources/Ext/Manifest.hs`](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-ext/src/main/haskell/Hydra/Sources/Ext/Manifest.hs).

### Not in this package

A few originally-Ext members have moved out into their own packages as they
matured into proper coders:

- **Go** → [`packages/hydra-go/`](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-go) (issue #289).
- **TypeScript** → [`packages/hydra-typescript/`](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-typescript) (issue #126).

## Sync scope

`hydra-ext` is **not** part of the default `bin/sync.sh` host × target matrix.
Most modules here are experimental schema and syntax models rather than full
cross-language coders. The package's `package.json` declares:

```json
"targetLanguages": ["python"]
```

which scopes regeneration to Python-side artifacts under `dist/python/hydra-ext/`.
To regenerate, run from a worktree root:

```bash
bin/sync.sh --hosts haskell --targets python    # via the kernel manifest
```

The Haskell-side DSL sources are processed in Phase 1 of every `bin/sync.sh`
invocation; the result lands in `dist/json/hydra-ext/`. Target-language emission
beyond Python is currently opt-in and limited.

## Demos using `hydra-ext`

Several demos exercise extension coders:

- **[Avro demo](https://github.com/CategoricalData/hydra/tree/main/demos/avro)**
  — bidirectional Avro schema reading and writing.
- **[GenPG demo](https://github.com/CategoricalData/hydra/tree/main/demos/genpg)**
  — CSV → property graph (uses Avro for schemas).
- **[GraphQL JSON demo](https://github.com/CategoricalData/hydra/tree/main/demos/graphql-json)**
  — GraphQL schema inference and JSON validation.
- **[SHACL demo](https://github.com/CategoricalData/hydra/tree/main/demos/shacl)**
  — uses several ext models.

## Adding a coder

For a new schema language or generation target:

1. Add the DSL source modules under `src/main/haskell/Hydra/Sources/<Lang>/`
   (Coder.hs, Language.hs, Serde.hs, Syntax.hs at minimum).
2. Register them in `src/main/haskell/Hydra/Sources/Ext/Manifest.hs`'s
   `mainModules` list.
3. Run `bin/sync-haskell.sh` to regenerate the JSON kernel and validate that
   the new sources type-check against the rest of `hydra-ext`.

For the broader process, see
[Creating a new Hydra implementation](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/new-implementation.md)
(for full host languages) or
[Adding new primitives](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/adding-primitives.md)
(for new functions in `hydra.lib.*`).

## See also

- **[Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts)** — the
  LambdaGraph data model.
- **[`hydra-kernel` README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-kernel/README.md)**
  — the core types, terms, and primitives every extension coder builds on.
- **[Implementation overview](https://github.com/CategoricalData/hydra/blob/main/docs/implementation.md)**
  — the coder framework and how it's organized.
