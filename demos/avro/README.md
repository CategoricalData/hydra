# Avro bidirectional coder demo

Demonstrates Hydra's bidirectional Avro coder, which converts between Avro schemas/data
and Hydra types/terms in both directions, including transformation to property graphs.

## Quick start

From the repo root:

```bash
demos/avro/bin/run.sh
```

Or run a specific demo:

```bash
demos/avro/bin/run.sh --demo 1   # Forward only
demos/avro/bin/run.sh --demo 2   # Reverse only
demos/avro/bin/run.sh --demo 3   # Round-trip
demos/avro/bin/run.sh --demo 4   # Schema codec
demos/avro/bin/run.sh --demo 5   # Property graph
```

## What it demonstrates

### Demo 1: Forward pipeline (Avro -> Hydra)

Reads the `Review.avsc` schema and example JSON data, converts through the forward adapter
(Avro schema -> Hydra type, JSON value -> Hydra term), and round-trips the terms back to JSON.

This exercises `avroHydraAdapter` and the forward direction of the Avro coder.

### Demo 2: Reverse pipeline (Hydra -> Avro)

Defines Hydra types programmatically (Person with nested Address, optional email, and tags list),
then encodes them as Avro schemas and serializes a sample term to JSON.

This exercises the `Encoder.encodeType` and term-level `coderEncode`.

### Demo 3: Round-trip (Avro -> Hydra -> Avro)

Loads the `AirplaneInfo.avsc` schema (a complex real-world schema with nested records,
enums, optional fields, and annotations), converts to Hydra types via the forward adapter,
then converts back to an Avro schema via the reverse encoder.
Compares the original and round-tripped schemas structurally.

This exercises the full bidirectional pipeline end-to-end.

### Demo 4: Schema string codec

Demonstrates `avroSchemaStringCoder`: constructs an Avro schema in memory, serializes it
to a JSON string, parses it back, and verifies the round-trip.

This exercises `SchemaJson.encodeSchema` and `SchemaJson.decodeSchema`.

### Demo 5: Avro to property graph (GraphSON)

Reads the `Review.avsc` schema and example JSON data, converts through the forward adapter,
then transforms to a GraphSON property graph using the Tinkerpop annotation schema.
The Avro schema uses Hydra-specific annotations (`@vertexLabel`, `@edgeLabel`, `@vertexId`,
`@outVertex`, `@inVertex`) to guide the mapping from Hydra types/terms to property graph
vertices, edges, and properties.

Example output (Review schema):
- Vertices: `Movie` (with title), `Review` (with rating, text, date), `User` (with name)
- Edges: `hasReview` (Movie -> Review), `reviewer` (Review -> User)

This exercises `propertyGraphGraphsonLastMile`, `typeApplicationTermToPropertyGraph`,
and the full Avro-to-PG pipeline.

## Output

Demo output is written to `/tmp/hydra-avro-demo/`:

```
/tmp/hydra-avro-demo/
  forward/         JSON round-trip output
  reverse/         Generated .avsc schema and JSON data
  roundtrip/       Original vs round-tripped schema JSON
  graphson/        GraphSON property graph output (JSONL)
```

## GHCi usage

You can also run the demos interactively:

```haskell
import Hydra.Ext.Demos.AvroBicoder
runAllDemos          -- run all five demos
runForwardDemo       -- just the forward pipeline
runReverseDemo       -- just the reverse pipeline
runRoundTripDemo     -- just the round-trip comparison
runSchemaCodecDemo   -- just the schema codec
runPropertyGraphDemo -- just the property graph output
```

## Prerequisites

- Stack (Haskell build tool)
- Hydra-Haskell must be buildable (`stack build` in `heads/haskell`)
