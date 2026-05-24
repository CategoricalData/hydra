# Getting started with Hydra (for library users)

This guide is for developers who want to **use Hydra as a library** from their own project.
It is not for contributors extending the Hydra kernel — for that, see
[Adding new type and term constructors to Hydra Core](recipes/extending-hydra-core.md)
and the [DSL guides](dsl-guide.md).

Hydra is a translingual data and computation framework based on the LambdaGraph data model.
For library users the common use cases are:

- **Typed property-graph construction and validation** (TinkerPop, Neo4j, Neptune)
- **Schema-driven coders** between Avro, Protobuf, JSON Schema, GraphQL, PDL, RDF, etc.
- **Code generation** of typed data structures from Hydra modules into Java / Python / Scala / Lisp dialects

The library shape is the same on every host: depend on the package, import what you need, call functions.
There is no CLI to invoke; you are integrating Hydra into your own application.

> **Reference project.** A real downstream consumer lives outside this repo and is a good place to learn from:
> [HydraPop](https://github.com/CategoricalData/HydraPop) (Java + Python, TinkerPop validation).

---

## Java

Hydra 0.16 publishes one Maven artifact per package. Pick the ones you need; coordinates are under group `net.fortytwo.hydra`, all at the same version.

| ArtifactId | Contains |
|---|---|
| `hydra-kernel` | Core types (`Literal`, `Type`, `Term`), `hydra.show.*`, `hydra.validate.core`, `hydra.error.core`, library stdlib. The minimum dependency. |
| `hydra-pg` | Property-graph model (`hydra.pg.model.*`), validation (`hydra.validate.pg`), errors (`hydra.error.pg`). |
| `hydra-pg-dsl` | Hand-written Java fluent builders for constructing schemas and graphs (`hydra.pg.dsl.Graphs`, `hydra.pg.dsl.Queries`). |
| `hydra-rdf` | RDF model + serdes. |
| `hydra-rdf4j` | Binding to the rdf4j library. |
| `hydra-neo4j` | Cypher and openGQL parsers via ANTLR. |
| `hydra-java`, `hydra-python`, `hydra-scala`, `hydra-haskell`, `hydra-lisp`, `hydra-go` | Per-language coder packages. Depend on these if your code needs to generate code in that target. |

### Gradle

```gradle
dependencies {
    implementation 'net.fortytwo.hydra:hydra-kernel:0.16.0'
    implementation 'net.fortytwo.hydra:hydra-pg:0.16.0'
    implementation 'net.fortytwo.hydra:hydra-pg-dsl:0.16.0'
}
```

### Maven

```xml
<dependencies>
  <dependency>
    <groupId>net.fortytwo.hydra</groupId>
    <artifactId>hydra-kernel</artifactId>
    <version>0.16.0</version>
  </dependency>
  <dependency>
    <groupId>net.fortytwo.hydra</groupId>
    <artifactId>hydra-pg</artifactId>
    <version>0.16.0</version>
  </dependency>
  <dependency>
    <groupId>net.fortytwo.hydra</groupId>
    <artifactId>hydra-pg-dsl</artifactId>
    <version>0.16.0</version>
  </dependency>
</dependencies>
```

> Earlier releases (0.13, 0.14) shipped a single rolled-up `hydra-ext` artifact. HydraPop still depends on it
> at `0.14.1`; that bundle is no longer published in 0.15+. Downstream projects on 0.14 will need to switch to
> the per-package artifacts when they upgrade.

### Construct a schema and validate a graph

The Java DSL helpers in `hydra.pg.dsl.Graphs` provide a fluent builder for schemas and values:

```java
import hydra.core.LiteralType;
import hydra.core.Literal;
import hydra.dsl.LiteralTypes;
import hydra.dsl.Literals;
import hydra.pg.dsl.Graphs;
import hydra.pg.model.GraphSchema;
import hydra.pg.model.Graph;

GraphSchema<LiteralType> schema = Graphs.graphSchema(
    java.util.List.of(
        Graphs.vertexType("Person", LiteralTypes.string())
            .property("name", LiteralTypes.string())
            .property("age", LiteralTypes.int32())
            .build()),
    java.util.List.of(
        Graphs.edgeType("knows", LiteralTypes.string(), "Person", "Person")
            .build()));

Graph<Literal> graph = Graphs.graph(
    java.util.List.of(
        Graphs.<Literal>vertex("Person", Literals.string("p1"))
            .property("name", Literals.string("Alice"))
            .property("age", Literals.int32(30))
            .build(),
        Graphs.<Literal>vertex("Person", Literals.string("p2"))
            .property("name", Literals.string("Bob"))
            .property("age", Literals.int32(28))
            .build()),
    java.util.List.of(
        Graphs.<Literal>edge("knows", Literals.string("e1"), Literals.string("p1"), Literals.string("p2"))
            .build()));
```

Validation uses `hydra.validate.pg.Pg.validateGraph`. The `checkValue` callback is supplied by the caller —
a small adapter that bridges the kernel's typed `InvalidLiteralError` to the stringified `InvalidValueError`
that `validateGraph` returns in its accumulator. See
[`demos/src/main/java/hydra/demos/validatepg/ValidateDemo.java`](../demos/src/main/java/hydra/demos/validatepg/ValidateDemo.java)
for a complete worked example.

For a project that wraps this further into a one-call `Validate.validate(schema, gremlinGraph)` API,
read [HydraPop's `Validate.java`](https://github.com/CategoricalData/HydraPop/blob/main/src/main/java/net/fortytwo/hydra/hydrapop/Validate.java).

---

## Python

Hydra 0.15+ publishes one conda-forge package per Hydra package: `hydra-kernel`, `hydra-pg`, `hydra-pg-dsl`, etc.
PyPI publication is tracked by [#290](https://github.com/CategoricalData/hydra/issues/290).

### conda

```bash
conda install -c conda-forge hydra-kernel hydra-pg
```

After install, the packages expose `hydra.core`, `hydra.pg.model`, `hydra.validate.core`, `hydra.validate.pg`,
`hydra.show.core`, `hydra.error.core`, `hydra.error.pg`, etc.

### Local install from the repo

For active development against an unreleased version, work directly in the repo:

```bash
git clone https://github.com/CategoricalData/hydra
cd hydra/heads/python
uv venv --python 3.12
source .venv/bin/activate
uv sync
```

The `pyproject.toml` declares `dist/python/<pkg>/src/main/python` paths under `pytest.ini_options.pythonpath`,
so once `uv sync` finishes you can import all the kernel modules.

### Construct a schema and validate a graph

[HydraPop](https://github.com/CategoricalData/HydraPop) provides a one-call validation API on top of Hydra-PG.
It exposes `hydrapop.dsl.pg` for schema construction and `hydrapop.validate.validate(schema, g)` for validation:

```python
from hydrapop.dsl.pg import edge_type, graph_schema, int32, string, vertex_type
from hydrapop.validate import validate

schema = graph_schema(
    vertex_types=[
        vertex_type("Person", string())
            .property("name", string())
            .property("age", int32()),
    ],
    edge_types=[
        edge_type("knows", string(), out_label="Person", in_label="Person"),
    ],
)

# `g` is a gremlinpython GraphTraversalSource connected to a Gremlin Server.
result = validate(schema, g)
if result.is_valid:
    print("VALID")
else:
    print(result.error)
```

For low-level access without HydraPop, import the kernel directly. The DSL helpers in
`hydra.dsl.literal_types` and `hydra.dsl.literals` provide convenience constructors:

```python
from hydra.dsl import literal_types, literals
from hydra.validate.core import check_literal
from hydra.lib.maybes import is_just

# Type mismatch returns Just(InvalidLiteralError(...)) -- typed error, not a string
result = check_literal(literal_types.string(), literals.int32(42))
assert is_just(result)
```

---

## Haskell

Hydra-Haskell is on Hackage as `hydra`. From a stack project:

```yaml
# stack.yaml
extra-deps:
  - hydra-0.16.0
```

```yaml
# package.yaml
dependencies:
  - hydra
```

### Check a literal type

```haskell
import qualified Hydra.Core as Core
import qualified Hydra.Validate.Core as Validate

main :: IO ()
main = case Validate.checkLiteral Core.LiteralTypeString (Core.LiteralString "hello") of
  Nothing  -> putStrLn "match"
  Just err -> print err  -- typed InvalidLiteralError
```

---

## What's in `hydra.*`?

A handful of namespaces you'll touch most often as a library user:

| Namespace | Contents |
|---|---|
| `hydra.core` | The kernel types: `Literal`, `LiteralType`, `Term`, `Type`, `Name`, etc. |
| `hydra.show.core` | Pretty-printers for literals, types, terms. |
| `hydra.validate.core` | Validators that return typed errors (e.g. `checkLiteral`, `checkTerm`). |
| `hydra.error.core` | The `InvalidTermError` / `InvalidTypeError` / `InvalidLiteralError` union types. |
| `hydra.pg.model` | Property-graph schema and value types (`GraphSchema`, `Graph`, `Vertex`, `Edge`). |
| `hydra.pg.dsl` | Fluent builders for constructing schemas and graphs (Java only at present; Python users typically use HydraPop's `hydrapop.dsl.pg`). |
| `hydra.validate.pg` | Property-graph validation against a schema. |
| `hydra.error.pg` | Property-graph validation errors. |
| `hydra.lib.maps`, `hydra.lib.lists`, etc. | Stdlib-style helpers, generic over host stdlib. |
| `hydra.util.Maybe`, `hydra.util.Either` | Functional-style result wrappers. Java/Python; Haskell uses native `Maybe` and `Either`. |
| `hydra.reflect` | Runtime reflection on the kernel types (`literalType`, `literalVariant`, etc.). |

The full primitive lexicon is in
[`docs/hydra-lexicon.txt`](https://github.com/CategoricalData/hydra/blob/main/docs/hydra-lexicon.txt)
(~180 primitives with their type signatures).

---

## Next steps

- **Construct a typed schema**: see the per-language DSL guides ([Java](dsl-guide-java.md), [Python](dsl-guide-python.md), [Haskell](dsl-guide.md)).
- **Convert schemas between formats**: see the coder documentation in `hydra-ext` (Avro ↔ Protobuf ↔ JSON Schema ↔ GraphQL, etc.).
- **Generate code from a schema**: see [Generating code with Hydra](recipes/code-generation.md).
- **Reference project**: [HydraPop](https://github.com/CategoricalData/HydraPop) — TinkerPop validation, Java + Python.
