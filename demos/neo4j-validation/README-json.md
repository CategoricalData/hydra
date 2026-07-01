# Neo4j validation demo — one schema, many graphs, every language

This demo shows Hydra giving Neo4j something its own ecosystem does not: **client-side
schema validation that is provably identical across client languages.** You bring a graph as
**plain Neo4j Cypher** (the same `CREATE` script you'd run in `cypher-shell`); it is validated in
Java, Python, Haskell, and TypeScript — each running a validator generated from a single Hydra
source. Every host produces the same verdict. (Java, Python, JavaScript/TypeScript, and Go have
official Neo4j drivers and Haskell a community one, so this is exactly the set of client languages
a Neo4j team would care about.)

You never have to author anything in Hydra: you write Neo4j Cypher, the demo translates it into
Hydra's model under the hood (you can peek at the JSON if you like), and you read the verdicts.
For convenience the demo can also generate its own sample graphs directly — but the headline path
is *your Cypher in, identical validation out.*

There is also a [live-server variant](README.md) that reads a running Neo4j over Bolt;
this document covers the **offline, JSON-artifact** variant, which needs no database.

## The idea

```
                   authored once, in the Java DSL
                                │
           hydra.neo4j.model values (schema + 11 graphs)
                                │  Hydra JSON encoding
                                ▼
              schema.json + valid graphs + invalid graphs
                                │
     ┌──────────────┬──────────┼───────────┬──────────────┐
     ▼              ▼          ▼           ▼              ▼
  Java          Python     Haskell    TypeScript     (more hosts)
  validator     validator  validator  validator
     │              │          │           │
     └──────────────┴──────────┴───────────┘
              identical output, by construction
```

The schema and graphs are just data — a portable JSON artifact. The validator
(`hydra.validate.neo4j.validateGraph`) is written once in Hydra and code-generated into
each host. So the **data** under test is identical (same files) and the **logic** is
identical (same source), which is exactly the guarantee a Neo4j client team cannot get
today: their Java app and their Python app validate with separate, hand-maintained code
that can silently drift.

## What it validates

The schema is a small movie domain (a Neo4j `GraphType`):

- `(:Person { name :: STRING required, born :: INTEGER })`
- `(:Movie { title :: STRING required, released :: INTEGER required })`
- `(:Person)-[:ACTED_IN]->(:Movie)`
- `(:Person)-[:LIKES]->(:Movie)` and `(:Person)-[:LIKES]->(:Person)` (an overloaded type)

Eleven graphs are checked against it — several valid, several with planted violations. Each shows a
different facet of the validator; pick the ones that tell the story you want.

| Graph file | Expected result | What it shows |
|------------|-----------------|---------------|
| `valid.json` | VALID | the happy path |
| `valid_richer.json` | VALID | a bigger graph touching every type, including both `LIKES` patterns |
| `valid_likes_person.json` | VALID | the *other* overloaded `LIKES` pattern (`Person`→`Person`) resolves |
| `missing_required_property.json` | node `p3`: missing required property `name` | property existence |
| `wrong_property_type.json` | node `m2`: property `released` has the wrong type (expected INTEGER) | property type |
| `missing_required_released.json` | node `m3`: missing required property `released` | existence on a second node type |
| `optional_wrong_type.json` | node `o1`: property `born` has the wrong type (expected INTEGER) | type checks apply to *optional* properties too |
| `endpoint_mismatch.json` | relationship `r3`: endpoints (`Movie`→`Person`) match no declared `LIKES` pattern | overloaded-relationship endpoint matching |
| `acted_in_wrong_direction.json` | relationship `dr1`: endpoints match no declared pattern | a reversed `ACTED_IN` (`Movie`→`Person`) |
| `multiple_violations.json` | **4 violations at once** (two missing properties, one wrong type, one bad endpoint) | reports *all* violations — the differentiator versus Neo4j's write-time, fail-fast enforcement |
| `undeclared_label_open_world.json` | VALID | open-world: a node with an undeclared label (`Robot`) is not an error under the default profile (a strict profile would flag it) |

## Prerequisites

- A **JDK 17+** (the generated Java kernel targets a modern JDK). On macOS the script
  finds one via `/usr/libexec/java_home -v 17+`.
- **Python 3** on `PATH` for the Python host.
- For the **Haskell** host: a working `stack` in `heads/haskell` (the same toolchain the
  rest of the project uses).
- For the **TypeScript** host: **Node 20+** on `PATH`. The script generates the TypeScript
  `dist/` and installs the TypeScript head's dev dependencies (`tsx`) automatically on
  first run.

No Neo4j server and no third-party drivers are needed — this variant is fully offline.
The script generates the per-language Hydra kernels (and, for TypeScript, the full
`hydra-kernel` + `hydra-rdf` + `hydra-pg` dist) into `dist/` automatically if absent.

## Step by step

### 1. Run everything with the orchestrator

From the repository root:

```bash
./demos/neo4j-validation/bin/run-json.sh
```

This:

1. compiles the Java data generator and validator against the generated `dist/java` kernel;
2. generates `schema.json` plus the graph files into a temporary directory;
3. runs the Java, Python, Haskell, and TypeScript validators over those files;
4. diffs the outputs and reports whether they match.

Scope to a subset of hosts with `--hosts`:

```bash
./demos/neo4j-validation/bin/run-json.sh --hosts java,python
```

Keep the generated JSON to inspect it:

```bash
./demos/neo4j-validation/bin/run-json.sh --keep
```

Expected tail of the output:

```
=== Cross-host comparison ===
  java == python
  java == haskell
  java == typescript
  python == haskell
  python == typescript
  haskell == typescript

All host outputs match: the same validation logic, run identically in every language.
```

### 2. Validate your own Neo4j Cypher (you never touch Hydra)

You don't have to author anything in Hydra. Write plain Neo4j Cypher — the same `CREATE`
statements you'd type into `cypher-shell` — and the demo ingests it, converts it to the Hydra
model, and validates it in every language against the reference schema:

```bash
./demos/neo4j-validation/bin/run-json.sh \
  --cypher demos/neo4j-validation/examples/valid.cypher \
  --cypher demos/neo4j-validation/examples/with_violations.cypher
```

Two example inputs ship with the demo:

- `examples/valid.cypher` — a conforming movie graph → all hosts report VALID.
- `examples/with_violations.cypher` (a copy of `fixture.cypher`) — the same Cypher you'd use to
  seed a live Neo4j, with four planted problems → all hosts report the same four violations:

  ```
  Graph "cypher_with_violations": INVALID (4 violation(s))
    - node nameless: missing required property 'name'
    - node reloaded: property 'released' has the wrong type (expected INTEGER)
    - node untitled: missing required property 'released'
    - relationship _r4: endpoints match no declared pattern for this relationship type
  ```

Point `--cypher` at your own `.cypher` file to validate your own data. The user-facing input is
Neo4j Cypher; Hydra is just the engine that makes the four languages agree. (The ingester handles
the `CREATE`-statement subset used to seed data — node and relationship patterns with literal
property maps — which is exactly what a seed script contains.)

### 3. (Optional) Generate the data, then run a single host by hand

Generate the shared JSON once and reuse it:

```bash
./demos/neo4j-validation/bin/run-json.sh --hosts java --keep    # prints the data dir
DATA=/path/printed/above

# Python, against the same files:
PYTHONPATH="dist/python/hydra-pg/src/main/python:dist/python/hydra-kernel/src/main/python:overlay/python/hydra-kernel/src/main/python:demos/src/main/python" \
  python3 demos/src/main/python/hydra/demos/neo4jvalidation/json_demo.py "$DATA"
```

The Java, Python, Haskell, and TypeScript hosts can each be run alone with
`--hosts <lang>`; the orchestrator generates the dist that host needs on first use.

### 4. (Optional) Inspect a JSON artifact

```bash
python3 -m json.tool "$DATA/schema.json"
python3 -m json.tool "$DATA/valid.json"
```

These are Hydra's canonical term-JSON: records become objects, unions become single-key
objects (`{"string": {}}`), wraps (`ElementId`, `Key`, `NodeLabel`) become their inner
value, a node's `properties` map becomes an array of `{"key": ..., "value": ...}` pairs,
and 64-bit integers are encoded as strings.

## How it is wired

| Piece | File |
|-------|------|
| Authoring schema + graphs, encoding to JSON | `demos/src/main/java/hydra/demos/neo4jvalidation/GenerateData.java` |
| Java JSON decoder | `demos/src/main/java/hydra/demos/neo4jvalidation/JsonNeo4jDecoder.java` |
| Java validator host | `demos/src/main/java/hydra/demos/neo4jvalidation/Neo4jJsonValidateDemo.java` |
| Python decoder + validator host | `demos/src/main/python/hydra/demos/neo4jvalidation/json_demo.py` |
| Haskell validator host | `demos/src/main/haskell/Hydra/Demos/Neo4jValidation/JsonDemo.hs` |
| TypeScript decoder + validator host | `demos/src/main/typescript/hydra/demos/neo4jvalidation/jsonDemo.ts` |
| Cypher `CREATE` → Hydra model ingester | `demos/src/main/java/hydra/demos/neo4jvalidation/CypherIngest.java` |
| Example Cypher inputs | `demos/neo4j-validation/examples/*.cypher` |
| Orchestrator | `demos/neo4j-validation/bin/run-json.sh` |

The validator and the Neo4j model are translingual Hydra definitions in
[`packages/hydra-pg`](../../packages/hydra-pg/README.md) (`hydra.neo4j.model`,
`hydra.validate.neo4j`). Only the small JSON decode glue is written per host, and only
because reading a generic JSON file into typed model values is inherently host-shaped;
the validation itself is shared.

## See also

- [Live-server variant](README.md) — read a running Neo4j over Bolt and validate the live graph.
- [hydra-pg README](../../packages/hydra-pg/README.md) — the Neo4j model, validator, and PG↔Neo4j mapping.
