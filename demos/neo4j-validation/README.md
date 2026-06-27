# Neo4j validation demo

A translingual demo that validates a **live Neo4j graph** against a Hydra Neo4j
graph type, running the same Hydra validation logic in **Java** and **Python**,
using Neo4j's own client driver APIs to read the data.

It demonstrates the capability Neo4j itself lacks: **retroactive, client-side,
whole-graph validation that reports all violations.** Neo4j enforces constraints
and graph types only at write time, one element at a time, failing fast; this
demo reads an existing graph and reports every violation at once.

## What it does

For each host (Java, Python), the demo:

1. Connects to a running Neo4j over Bolt using the official Neo4j driver
   (`org.neo4j.driver` for Java, the `neo4j` package for Python).
2. Reads all nodes and relationships.
3. Maps the driver's `Node` / `Relationship` / value types onto Hydra's
   `hydra.neo4j.model` types.
4. Builds a graph type (the schema) in host code — a small movie domain
   (`Person`, `Movie`, `ACTED_IN`, and an overloaded `LIKES`).
5. Runs `hydra.validate.neo4j.validateGraph` and prints the violations.

The same Hydra validation runs identically in both hosts; only the
client-interfacing layer (the driver calls and the driver-to-Hydra mapping)
differs, and that layer lives here in the demo.

> The client-interfacing logic is in the demo **for now**. Once #511 lands
> (folding `bindings/` into `overlay/`), it will move to a reusable
> `overlay/{java,python}/hydra-neo4j` so applications get it without copying.

## Prerequisites

This demo, unlike the offline Hydra demos, needs a **running Neo4j**:

- A Neo4j instance reachable over Bolt (default `bolt://localhost:7687`, user
  `neo4j`). For example, install Neo4j 5.x and start it, or run it in a container.
- **Java host**: a JDK 17+ (the Neo4j Java driver requires it). `run.sh` selects
  one automatically via `/usr/libexec/java_home -v 17+`. The Neo4j Java driver
  and its runtime dependencies are downloaded automatically into
  `demos/neo4j-validation/.cache/` on first run.
- **Python host**: `pip install neo4j` (use a virtualenv if you like; `run.sh`
  uses whatever `python3` is on `PATH`).

`run.sh` also generates the Hydra kernel and `hydra-pg` into `dist/{java,python}`
if they are not already present (the published `hydra-java` jar is coder-only and
does not include the runtime kernel, so the demo uses the generated kernel).

Both host programs **skip gracefully** (print a notice, exit 0) when no Neo4j is
reachable or the driver is missing, so the demo does not break offline runs.

## Quick start

```bash
# Start Neo4j (5.x; JDK 17 compatible), set a password, then:
cd demos/neo4j-validation

# Seed the fixture (deliberate violations) and run both hosts in one step:
./bin/run.sh --password <your-password> --seed --neo4j-home /path/to/neo4j

# Or seed yourself and just run:
cypher-shell -u neo4j -p <your-password> --file fixture.cypher
./bin/run.sh --password <your-password>
```

Expected output (Java and Python both): `Read 6 nodes and 5 relationships from
Neo4j.` followed by `4 violation(s)` — the planted violations below.

## The fixture and expected violations

`fixture.cypher` seeds a movie graph with deliberate violations. Validated
against the graph type defined in the demo, the validator reports:

| Element | Violation |
|---------|-----------|
| `Person {born: 1970}` (no name) | missing required property `name` |
| `Movie {released: '2003'}` | property `released` has the wrong type (STRING, not INTEGER) |
| `Movie {title: 'Untitled'}` | missing required property `released` |
| `(:Movie)-[:LIKES]->(:Person)` | endpoints match no declared pattern for `LIKES` |

## Validation conditions covered

- **Property existence** — a required property is absent.
- **Property type** — a property's value has the wrong type.
- **Endpoint patterns** — for an overloaded relationship type (`LIKES` connects
  `Person→Movie` and `Person→Person`), a relationship whose endpoints match no
  declared pattern is flagged.
- **Open-world by default** — nodes/relationships whose labels/type match no
  element type are not errors (matching Neo4j's open GRAPH TYPE). A strict
  profile (`strictNeo4jProfile`) makes them errors.

## See also

- The Neo4j model and validator: [`packages/hydra-pg` README](../../packages/hydra-pg/README.md), "Neo4j model".
- The validator source: `hydra.validate.neo4j` (`packages/hydra-pg/.../Sources/Validate/Neo4j.hs`).
