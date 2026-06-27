# hydra-tinkerpop

Java binding between Hydra and Apache TinkerPop (Gremlin).

Provides a thin bidirectional mapping between Hydra's `hydra.tinkerpop.gremlin` model — a
host-independent model of Gremlin traversals, generated from `packages/hydra-pg` and kept current with
the Apache TinkerPop ANTLR grammar (3.8.1) — and TinkerPop's native **`Bytecode`** representation.

Contents:
- `hydra.tinkerpop.HydraToBytecode` — forward mapping: a Hydra `RootTraversal` → TinkerPop `Bytecode`.
  Source/`withX` instructions, the spawn step, then each chained step. Compose with
  `JavaTranslator.of(g).translate(bytecode)` to obtain a runnable `GraphTraversal`.
- `hydra.tinkerpop.BytecodeToHydra` — reverse mapping: `Bytecode` → Hydra `RootTraversal`, dispatching on
  each instruction's operator name. Mirrors the forward step subset, so implemented steps round-trip.
- `hydra.tinkerpop.GremlinText` — text entry points: `parse(String)` (Gremlin text → Hydra, via
  TinkerPop's own ANTLR `GremlinQueryParser` → `Bytecode` → `BytecodeToHydra`) and `toGremlin(RootTraversal)`
  (Hydra → Gremlin-Groovy text, via `HydraToBytecode` → `GroovyTranslator`). No grammar is vendored here.

## Why Bytecode

`Bytecode` is TinkerPop's language-agnostic traversal representation: an ordered list of
`(operator, arguments)` instructions, split into source instructions and step instructions. It is what
TinkerPop's own `Translator` instances consume, it serializes via GraphSON, and it is structurally
isomorphic to Hydra's streamlined Gremlin model — so the mapping is shallow and stable, and execution
is available for free via `JavaTranslator`.

The full mapping design — structural correspondence table (Hydra `RootTraversal` ↔ Bytecode
source/step instructions), the 135-step coverage plan, and the round-trip/risk analysis — is tracked in
the branch's design notes. Briefly: source/`withX` → `addSource`; spawn + chained steps → `addStep`;
`GenericLiteral` → boxed Java values; `TraversalPredicate` → `P.*`; enums → `T`/`Direction`/`Column`/…;
nested traversals → nested Bytecode (via `__`).

## Maven coordinates

```
net.fortytwo.hydra:hydra-tinkerpop:0.16.0
```

## Dependencies

- `net.fortytwo.hydra:hydra-pg` — provides the `hydra.tinkerpop.gremlin` model (transitively
  hydra-java + hydra-kernel)
- `org.apache.tinkerpop:gremlin-core:3.8.0` — the `Bytecode` / `Translator` API

## Building locally

```sh
(cd heads/java && ./gradlew :hydra-tinkerpop:compileJava)
```

## Status

**Full bidirectional mapping — all 135 Gremlin steps, both directions.** Hydra↔Bytecode
(`HydraToBytecode` / `BytecodeToHydra`) and Gremlin-text↔Hydra (`GremlinText.parse` / `toGremlin`) are
implemented end-to-end across the entire step set, the predicate algebra (`Predicates` ↔ `P`/`TextP`,
including `and`/`or`/`not`), and the enum families (`Gtypes`, `Directions`, `Dts`, `Operators`, plus
order/pop/column/cardinality/token). Implemented steps round-trip.

A small number of *rare sub-cases* throw a clear `UnsupportedOperationException` rather than guess, and
are documented inline: `from`/`to` with a literal `StructureVertex`; `barrier` with a sack-consumer;
`option` merge-forms; `property(map)` / `call(map)`; `with` per-algorithm option keys and IO options;
map-literal arguments; the unsigned (`…U`) / `uuidL` `GType` tokens (no TinkerPop `GType` counterpart);
and all `…Variable` (bound-parameter) argument forms. These are uncommon in generated traversals and are
each a small isolated addition.

## Note on location (#442 / #511)

This binding currently lives under `bindings/java/` so we can make progress before
[#511](https://github.com/CategoricalData/hydra/issues/511) (fold `bindings/` → `overlay/`) is resolved.
The intent is to migrate the mapper into `overlay/java/hydra-pg` (so it ships with the Java build of
hydra-pg) once #511 settles how a *generated* `dist/<lang>/<pkg>` build config absorbs an
overlay-declared third-party dependency — here, `gremlin-core`. That dependency-injection mechanism is
the open problem #511 calls out; until it exists, the binding's own Gradle build is the working home.
The mapper code is written to move verbatim.

## See also

- The Hydra `bindings/` philosophy: handwritten host-language adapters wiring Hydra packages to external
  libraries, kept separate from `heads/` runtimes and `packages/` DSL definitions.
- [`docs/implementation.md`](../../../docs/implementation.md) principle 7.
