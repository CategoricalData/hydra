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
3. Run `heads/haskell/bin/sync-haskell.sh` to regenerate the JSON kernel and validate that
   the new sources type-check against the rest of `hydra-ext`.

For the broader process, see
[Creating a new Hydra implementation](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/new-implementation.md)
(for full host languages) or
[Adding new primitives](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/adding-primitives.md)
(for new functions in `hydra.core.lib.*`).

## Testing

Coder tests cannot be written in Hydra's cross-host DSL test framework
(`Hydra.Sources.Test`): every DSL test case compiles to a pure `unit -> string` thunk compared
by string equality, but a coder needs a `Graph`/`InferenceContext` and builds `Adapter`/`Coder`
values that aren't expressible that way. Coder tests are therefore **host-native** — written as
HSpec specs under
[`heads/haskell/src/test/haskell/Hydra/`](https://github.com/CategoricalData/hydra/tree/main/heads/haskell/src/test/haskell/Hydra),
one `<Coder>CoderSpec.hs` per coder, run via `stack test`.

[`Avro/CoderSpec.hs`](https://github.com/CategoricalData/hydra/blob/main/heads/haskell/src/test/haskell/Hydra/Avro/CoderSpec.hs)
is the fullest example and the pattern to follow for a new or expanded coder test suite: type-level
forward/reverse/round-trip, term-level forward/reverse/round-trip (JSON↔Term), union encoding,
name mapping, lossiness (`adapterIsLossy`), schema-JSON serialization, and golden `.avsc` fixture
round-trips. `ProtobufCoderSpec.hs` and `GraphqlCoderSpec.hs` add golden-output coverage
(generated `.proto`/GraphQL SDL text compared against expected strings, rendered through the same
`Serde.definitionToExpr` → `printExpr` pipeline the real `moduleToProtobuf`/`moduleToGraphql`
entry points use) alongside their existing bug-regression cases. `YamlCoderSpec.hs` covers the
YAML coder's `Term` ↔ `Hydra.Core.Yaml.Model.Node` mapping (golden `Node` output, plus round-trip)
— there is no YAML *text* serde in this repo yet, so YAML coverage stops at the `Node` AST level.

**Coverage status** (#699): Avro, Protobuf, GraphQL, and YAML have golden-output and/or
round-trip tests. The remaining coders (SQL, Parquet, Delta, Kusto, Pegasus/PDL, C++, C#, Rust,
and the "Other" domain modules) have no tests yet; see #816.

## See also

- **[Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts)** — the
  LambdaGraph data model.
- **[`hydra-kernel` README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-kernel/README.md)**
  — the core types, terms, and primitives every extension coder builds on.
- **[Implementation overview](https://github.com/CategoricalData/hydra/blob/main/docs/implementation.md)**
  — the coder framework and how it's organized.
