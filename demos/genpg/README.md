# GenPG demo - property graph generation from CSV tables

This demo demonstrates end-to-end transformation of relational CSV data into
a property graph. Two output formats are supported:

- **GraphSON** (JSON Lines) for import into TinkerPop-compatible graph databases
- **RDF/SHACL** (N-Triples) for Semantic Web tooling and SPARQL queries

Both output paths are translingual: they run in Haskell, Java, and Python.

Demo videos:
- [Part 1: Introducing Hydra](https://www.linkedin.com/posts/joshuashinavier_in-case-you-were-wondering-what-i-have-been-activity-7358601538463830017-U5YE)
  (10 min, August 2025)
- [Part 2: Property graph generation](https://www.linkedin.com/posts/joshuashinavier_here-is-part-2-of-the-hydra-property-graph-activity-7358601988755910657-HnCh)
  (10 min, August 2025)
- [Full walkthrough](https://drive.google.com/file/d/10HCElcG7n0tprOTdtX4bSa5yWYs08nV-/view?usp=sharing) (Google Drive)

## Overview

The GenPG demo:
1. Defines database schemas (table structures), graph schemas (vertex/edge types),
   mappings from tables to graph elements in a Haskell-based DSL
2. Generates executable Haskell and Python code from these definitions
3. Executes the transformation pipeline in either Haskell or Python

## Directory structure

```
demos/
├── genpg/
│   ├── README.md                 # This file
│   ├── bin/run.sh                   # Run GraphSON demo (all hosts)
│   ├── bin/run-rdf.sh               # Run RDF/SHACL demo (all hosts)
│   ├── bin/generate-python.ghci      # Script to generate Python modules
│   ├── bin/generate-java.ghci        # Script to generate Java modules
│   ├── data/
│   │   ├── sources/sales/        # CSV input files (sales example)
│   │   └── sources/health/       # CSV input files (health example)
│   └── output/                   # GraphSON output (shared by all modes)
├── src/                          # Demo sources (demos/src/)
│   ├── main/
│   │   ├── haskell/Hydra/Ext/Demos/GenPG/
│   │   │   ├── Demo.hs           # Haskell GraphSON driver
│   │   │   ├── Rdf.hs            # Haskell RDF/SHACL driver
│   │   │   ├── Modules.hs        # Shared module definitions (sales/health)
│   │   │   ├── GeneratePython.hs # Python code generation
│   │   │   ├── GenerateJava.hs   # Java code generation
│   │   │   └── Examples/
│   │   │       ├── Sales/        # Sales dataset definitions
│   │   │       │   ├── DatabaseSchema.hs
│   │   │       │   ├── GraphSchema.hs
│   │   │       │   └── Mapping.hs
│   │   │       └── Health/       # Health dataset definitions
│   │   │           ├── DatabaseSchema.hs
│   │   │           ├── GraphSchema.hs
│   │   │           └── Mapping.hs
│   │   ├── python/hydra/demos/genpg/
│   │   │   ├── demo.py           # Python GraphSON driver
│   │   │   ├── rdf.py            # Python RDF/SHACL driver
│   │   │   └── generate_prompt.py # LLM prompt generator
│   │   └── java/hydra/demos/genpg/
│   │       ├── Demo.java         # Java GraphSON driver
│   │       └── RdfDemo.java      # Java RDF/SHACL driver
│   └── gen-main/
│       ├── haskell/Hydra/Pg/
│       │   ├── Model.hs          # Generated: property graph model
│       │   ├── Mapping.hs        # Generated: mapping definitions
│       │   └── Graphson/         # Generated: GraphSON coder, syntax, utils
│       ├── python/hydra/
│       │   ├── pg/               # Generated: property graph models
│       │   ├── demos/genpg/      # Generated: transform.py, sales.py, health.py
│       │   ├── encode/pg/        # Generated: encoders
│       │   └── decode/pg/        # Generated: decoders
│       └── java/hydra/
│           ├── pg/               # Generated: PG mapping, GraphSON modules
│           ├── demos/genpg/      # Generated: Transform, Sales, Health
│           ├── encode/pg/        # Generated: encoders
│           └── decode/pg/        # Generated: decoders
```

## Prerequisites

- GHC and Stack (for Haskell mode and code generation)
- Python 3.10+ (for Python mode)
- Java 11+ and Gradle (for Java mode)

## Running the demo

### Haskell mode

```bash
cd heads/haskell
stack ghci
```

In GHCI:
```haskell
generateSalesGraphSON    -- processes sales data
generateHealthGraphSON   -- processes health data
```

### Python mode

Python 3.10+ is required (for `match` statement support).
Set up a virtual environment from the `hydra-python` directory:

```bash
cd hydra-python
uv venv --python 3.12
source .venv/bin/activate
```

Then run the demo from the repo root:

```bash
cd ../..
.venv/bin/python demos/src/main/python/hydra/demos/genpg/demo.py sales   # processes sales data
.venv/bin/python demos/src/main/python/hydra/demos/genpg/demo.py health  # processes health data
```

The `sales` argument is the default, so it can be omitted.

### Java mode

Java 11+ is required. From the repository root:

```bash
./gradlew compileJava
```

Then run the demo:

```bash
java -cp $(./gradlew printClasspath -q 2>/dev/null || echo "build/classes/java/main") \
  hydra.demos.genpg.Demo sales    # processes sales data
  hydra.demos.genpg.Demo health   # processes health data
```

Or run directly via Gradle if an `application` plugin or JavaExec task is configured.

All three modes read from `demos/genpg/data/sources/` and write to `demos/genpg/output/`.

### Translingual run script (GraphSON)

Run all three hosts and compare outputs:

```bash
demos/genpg/bin/run.sh sales
```

## RDF/SHACL output

The RDF output path converts the same property graph to:
- **SHACL shapes** (`*-shapes.nt`) derived from the graph schema,
  with property constraints (datatypes, cardinality) and edge constraints (target vertex classes)
- **RDF instance data** (`*-data.nt`) encoding vertices as typed resources with property triples,
  and edges as relationship triples
- **Invalid test data** (`*-invalid.nt`) with intentional violations for negative validation

### Running the RDF demo

#### All hosts via run script

```bash
demos/genpg/bin/run-rdf.sh sales
```

This runs Haskell, Java, and Python, compares outputs across hosts, and validates
with [pyshacl](https://github.com/RDFLib/pySHACL) if installed.

#### Individual hosts

Haskell (GHCi):
```haskell
:l Hydra.Ext.Demos.GenPG.Rdf
generateSalesRdf
```

Python:
```bash
python3 src/main/python/hydra/demos/genpg/rdf.py sales
```

Java (after `./gradlew compileJava`):
```bash
java -cp <classpath> hydra.demos.genpg.RdfDemo sales
```

### SHACL validation

Install pyshacl (`pip install pyshacl` or into `.venv`), then validate:

```bash
# Conforming data should pass
pyshacl -s demos/genpg/output/sales-shapes.nt -sf nt -df nt demos/genpg/output/sales-data.nt

# Invalid data should fail with 3 violations
pyshacl -s demos/genpg/output/sales-shapes.nt -sf nt -df nt demos/genpg/output/sales-invalid.nt
```

### RDF output format

The shapes graph uses the [SHACL](https://www.w3.org/TR/shacl/) vocabulary to constrain RDF instance data.
Each vertex type becomes a `sh:NodeShape` with:
- `sh:targetClass` matching the vertex label
- `sh:property` shapes for each property (with `sh:datatype` and optional `sh:minCount`)
- `sh:property` shapes for each outgoing edge type (with `sh:node` referencing the target vertex class)

The instance data uses a `urn:hydra:genpg:` namespace. Vertices have `rdf:type` triples and
property triples with XSD-typed literals. Edges become direct triples from source to target vertex.

## LLM-assisted schema generation

This demo also provides an example of a hands-off graph generation workflow
using an LLM for schema and transform design.
See the demo video linked above to understand how the demo works.

To run it yourself, you will need a small tabular dataset which illustrates the structure of your data.
You can find example datasets in `data/sources/sales` and `data/sources/health`;
the former is a reference dataset which is built into the workflow,
and the latter is a stand-in for your own domain-specific dataset.
Feel free to start with the provided `health` dataset, or insert your own from the beginning.

### Step 1: Generate the LLM prompt

From the repo root, generate a prompt based on the reference and target datasets:

```bash
python3 demos/src/main/python/hydra/demos/genpg/generate_prompt.py demos/genpg/data/sources/health > demos/genpg/data/prompt.txt
```

### Step 2: Use the LLM to generate schemas

Take the generated prompt and:
1. Copy it into your favorite LLM chat interface
   ([ChatGPT](https://chatgpt.com), [Claude](https://claude.ai), etc.; Claude was used in the video)
2. Copy the LLM-generated files to their expected locations under
   `demos/src/main/haskell/Hydra/Ext/Demos/GenPG/Examples/Health`

Just overwrite the files which are already checked in at that location.
Feel free to use any file system integration available in your tool to avoid manual copy-and-paste.

### Step 3: Run the transform

Enter GHCi using:
```bash
stack ghci
```

You will need to have installed [Haskell Tool Stack](https://docs.haskellstack.org/en/stable) first.
Once in the REPL, there are two built-in demo routines you can use,
both of which run a Hydra transform to generate graph data in
[GraphSON](https://github.com/apache/tinkerpop/blob/master/docs/src/dev/io/graphson.asciidoc):

```haskell
generateSalesGraphSON    -- generates graph from the built-in sales dataset
generateHealthGraphSON   -- generates graph from the health dataset (or your custom dataset)
```

Results are written to `demos/genpg/output/`.

### What you've accomplished

1. Taught the LLM how to write schemas and transforms in Hydra, using a specific reference dataset as an example.
2. Shown the LLM a new dataset, and asked it to generate a schema and transform.
   This is out of Hydra's control, so hopefully the LLM did a good job!
3. Used your shiny new Hydra schema and transform to generate a property graph.
   This phase of the demo is completely deterministic, fast, and scalable.

## Code generation

### Step 1: Generate Haskell encoder sources (one-time setup)

If modifying the PG model definitions, regenerate the Haskell encoder sources:

```bash
cd heads/haskell
stack ghci
```

In GHCI:
```haskell
:set +m
writeEncoderSourceHaskell "src/gen-main/haskell" (kernelModules <> hydraExtModules) [
  Hydra.Ext.Sources.Pg.Mapping.module_,
  Hydra.Ext.Sources.Pg.Model.module_]
```

### Step 2: Sync Python kernel (if kernel code changed)

If Hydra kernel code has changed (e.g., `hydra.json.writer`, `hydra.lib.*`), regenerate the Python kernel modules:

```bash
cd heads/haskell
bin/sync-python.sh
```

### Step 3: Generate Python modules

Generate all Python modules for the GenPG demo:

```bash
cd heads/haskell
stack ghci < ../../demos/genpg/bin/generate-python.ghci
```

Or interactively in GHCI:
```haskell
import Hydra.Ext.Demos.GenPG.GeneratePython
generatePythonModules
```

This generates to `dist/python/hydra-pg/src/main/python` (and related directories):
- `hydra.pg.model` - Property graph data model
- `hydra.pg.mapping` - Mapping definitions
- `hydra.pg.graphson.*` - GraphSON coder, syntax, utilities
- `hydra.encode.pg.*`, `hydra.decode.pg.*` - Encoders/decoders
- `hydra.demos.genpg.transform` - Table-to-graph transformation logic
- `hydra.demos.genpg.sales` - Sales demo schemas and mapping
- `hydra.demos.genpg.health` - Health demo schemas and mapping

### Step 4: Generate Java modules

Generate all Java modules for the GenPG demo:

```bash
cd heads/haskell
stack ghci --ghci-options='+RTS -K256M -A32M -RTS' < ../../demos/genpg/bin/generate-java.ghci
```

Or interactively in GHCI:
```haskell
import Hydra.Ext.Demos.GenPG.GenerateJava
generateJavaModules
```

This generates to `dist/java/hydra-pg/src/main/java` (and related directories):
- `hydra.pg.mapping` - Mapping definitions
- `hydra.pg.graphson.*` - GraphSON coder, syntax, utilities
- `hydra.encode.pg.*`, `hydra.decode.pg.*` - Encoders/decoders
- `hydra.demos.genpg.transform` - Table-to-graph transformation logic
- `hydra.demos.genpg.sales` - Sales demo schemas and mapping
- `hydra.demos.genpg.health` - Health demo schemas and mapping

Note: `hydra.pg.model` types are already generated in `hydra-java/src/gen-main/java/`.

## How it works

### Data flow

1. **Load schemas**: Import database and graph schema definitions
2. **Read CSV files**: Parse CSV tables from `data/sources/`
3. **Decode values**: Convert string values to typed terms based on column types
4. **Transform rows**: Apply mapping specifications to generate vertices and edges
5. **Encode output**: Convert property graph elements to GraphSON or RDF format
6. **Write output**: Produce JSONL (GraphSON) or N-Triples (RDF/SHACL) files

Steps 1-4 are shared between GraphSON and RDF output. Only the encoding and serialization differ.

### Python path resolution

The Python demo combines modules from two locations:
1. **dist/python/hydra-pg** (`src/main/python`): PG models, transform logic, example schemas
2. **packages/hydra-python** (`src/main/python`), **dist/python/hydra-kernel** (`src/main/python`): Hydra kernel types and DSL

Both use namespace packages (`pkgutil.extend_path`) to allow `hydra.*` to span directories.

## Sample data

### Sales dataset (`data/sources/sales/`)
- `employees.csv` - Employee records
- `departments.csv` - Department hierarchy
- `customers.csv` - Customer records
- `products.csv` - Product catalog
- `sales.csv` - Sales transactions
- `sale_items.csv` - Individual sale items
- `calls.csv`, `emails.csv`, `meetings.csv` - Customer interactions

### Health dataset (`data/sources/health/`)
- Alternative dataset with medical domain (doctors, patients, appointments, etc.)

## Output formats

### GraphSON

The GraphSON output is GraphSON 3.0 format (JSON Lines), suitable for import into:
- Apache TinkerPop / Gremlin Server
- JanusGraph
- Amazon Neptune
- Other TinkerPop-compatible graph databases

### Visualizing in G.V()

If you want to visualize your generated graphs in [G.V()](https://gdotv.com),
open the desktop interface and load the GraphSON files when you create a new graph playground.
You can re-load them with Gremlin commands like:

```gremlin
g.io("/path/to/hydra/demos/genpg/output/sales.jsonl").read().iterate()
g.io("/path/to/hydra/demos/genpg/output/health.jsonl").read().iterate()
```

Run a Gremlin query like `g.E()`, and off you go.

### RDF/SHACL

The RDF output consists of N-Triples files suitable for use with:
- SPARQL endpoints (Apache Jena, Blazegraph, GraphDB, etc.)
- RDF validation tools (pyshacl, TopBraid, etc.)
- Linked Data platforms
- Any tool that consumes RDF 1.1
