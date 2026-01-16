# GenPG Demo - Property Graph Generation from CSV Tables

This demo demonstrates end-to-end transformation of relational CSV data into
a property graph in GraphSON format. It can be run in either Haskell or Python.

There is a demo video [here](https://drive.google.com/file/d/10HCElcG7n0tprOTdtX4bSa5yWYs08nV-/view?usp=sharing).

## Overview

The GenPG demo:
1. Defines database schemas (table structures) in Haskell
2. Defines graph schemas (vertex/edge types) in Haskell
3. Defines mappings from tables to graph elements in Haskell
4. Generates Python modules from these definitions
5. Executes the transformation pipeline in either Haskell or Python

## Directory Structure

```
hydra-ext/
├── demos/genpg/
│   ├── README.md                 # This file
│   ├── data/
│   │   ├── sources/sales/        # CSV input files (sales example)
│   │   └── sources/health/       # CSV input files (health example)
│   └── output/                   # GraphSON output (shared by both modes)
├── src/
│   ├── main/
│   │   ├── haskell/Hydra/Ext/Demos/GenPG/
│   │   │   ├── Demo.hs           # Haskell demo driver
│   │   │   ├── ExampleDatabaseSchema.hs
│   │   │   ├── ExampleGraphSchema.hs
│   │   │   └── ExampleMapping.hs
│   │   └── python/hydra/demos/genpg/
│   │       └── demo.py           # Python demo driver
│   └── gen-main/python/
│       └── hydra/
│           ├── pg/               # Generated: property graph models
│           ├── demos/genpg/      # Generated: transform.py, example.py
│           ├── encode/pg/        # Generated: encoders
│           └── decode/pg/        # Generated: decoders
```

## Prerequisites

- GHC and Stack (for Haskell mode and code generation)
- Python 3.10+ (for Python mode)

## Running the Demo

### Haskell Mode

```bash
cd hydra-ext
stack ghci
```

In GHCI:
```haskell
generateExampleGraphSON   -- processes sales data
generateCopilotGraphSON   -- processes health data
```

### Python Mode

```bash
cd hydra-ext
python3 src/main/python/hydra/demos/genpg/demo.py
```

Both modes read from `demos/genpg/data/sources/` and write to `demos/genpg/output/`.

## LLM-Assisted Schema Generation

This demo also provides an example of a hands-off graph generation workflow using an LLM for schema and transform design.
See the demo video linked above to understand how the demo works.

To run it yourself, you will need a small tabular dataset which illustrates the structure of your data.
You can find example datasets in `data/sources/sales` and `data/sources/health`;
the former is a reference dataset which is built into the workflow,
and the latter is a stand-in for your own domain-specific dataset.
Feel free to start with the provided `health` dataset, or insert your own from the beginning.

### Step 1: Generate the LLM Prompt

While in the hydra-ext directory, generate a prompt based on the reference and target datasets:

```bash
python3 src/main/python/hydra/demos/genpg/generate_prompt.py demos/genpg/data/sources/health > demos/genpg/data/prompt.txt
```

### Step 2: Use the LLM to Generate Schemas

Take the generated prompt and:
1. Copy it into your favorite LLM chat interface
   ([ChatGPT](https://chatgpt.com), [Claude](https://claude.ai), etc.; Claude was used in the video)
2. Copy the LLM-generated files to their expected locations under
   `src/gen-main/haskell/Hydra/Ext/Demos/GenPG/Generated`

Just overwrite the files which are already checked in at that location.
Feel free to use any file system integration available in your tool to avoid manual copy-and-paste.

### Step 3: Run the Transform

Enter GHCi using:
```bash
stack ghci
```

You will need to have installed [Haskell Tool Stack](https://docs.haskellstack.org/en/stable) first.
Once in the REPL, there are two built-in demo routines you can use,
both of which run a Hydra transform to generate graph data in [GraphSON](https://github.com/apache/tinkerpop/blob/master/docs/src/dev/io/graphson.asciidoc):

```haskell
generateExampleGraphSON   -- generates graph from the built-in sales dataset
generateCopilotGraphSON   -- generates graph from health dataset (or your custom dataset)
```

Results are written to `demos/genpg/output/`.

### What You've Accomplished

1. Taught the LLM how to write schemas and transforms in Hydra, using a specific reference dataset as an example.
2. Shown the LLM a new dataset, and asked it to generate a schema and transform.
   This is out of Hydra's control, so hopefully the LLM did a good job!
3. Used your shiny new Hydra schema and transform to generate a property graph.
   This phase of the demo is completely deterministic, fast, and scalable.

## Code Generation

### Step 1: Generate Haskell Encoder Sources (one-time setup)

If modifying the PG model definitions, regenerate the Haskell encoder sources:

```bash
cd hydra-ext
stack ghci
```

In GHCI:
```haskell
:set +m
writeEncoderSourceHaskell "src/gen-main/haskell" (kernelModules <> hydraExtModules) [
  Hydra.Ext.Sources.Pg.Mapping.module_,
  Hydra.Ext.Sources.Pg.Model.module_]
```

### Step 2: Generate Python Modules

Generate all Python modules for the GenPG demo:

```bash
cd hydra-ext
stack ghci < demos/genpg/generate-python.ghci
```

Or interactively in GHCI:
```haskell
import Hydra.Ext.Demos.GenPG.Generate
generatePythonModules
```

This generates to `hydra-ext/src/gen-main/python`:
- `hydra.pg.model` - Property graph data model
- `hydra.pg.mapping` - Mapping definitions
- `hydra.pg.graphson.*` - GraphSON coder, syntax, utilities
- `hydra.encode.pg.*`, `hydra.decode.pg.*` - Encoders/decoders
- `hydra.demos.genpg.transform` - Table-to-graph transformation logic
- `hydra.demos.genpg.example` - Sales demo schemas and mapping

## How It Works

### Data Flow

1. **Load schemas**: Import database and graph schema definitions
2. **Read CSV files**: Parse CSV tables from `data/sources/`
3. **Decode values**: Convert string values to typed terms based on column types
4. **Transform rows**: Apply mapping specifications to generate vertices and edges
5. **Encode GraphSON**: Convert property graph elements to GraphSON JSON format
6. **Write output**: Produce JSONL file suitable for graph database import

### Python Path Resolution

The Python demo combines modules from two locations:
1. **hydra-ext** (`src/gen-main/python`): PG models, transform logic, example schemas
2. **hydra-python** (`src/gen-main/python`, `src/main/python`): Hydra kernel types and DSL

Both use namespace packages (`pkgutil.extend_path`) to allow `hydra.*` to span directories.

## Sample Data

### Sales Dataset (`data/sources/sales/`)
- `employees.csv` - Employee records
- `departments.csv` - Department hierarchy
- `customers.csv` - Customer records
- `products.csv` - Product catalog
- `sales.csv` - Sales transactions
- `sale_items.csv` - Individual sale items
- `calls.csv`, `emails.csv`, `meetings.csv` - Customer interactions

### Health Dataset (`data/sources/health/`)
- Alternative dataset with medical domain (doctors, patients, appointments, etc.)

## Output Format

The output is GraphSON 3.0 format (JSON Lines), suitable for import into:
- Apache TinkerPop / Gremlin Server
- JanusGraph
- Amazon Neptune
- Other TinkerPop-compatible graph databases

### Visualizing in G.V()

If you want to visualize your generated graphs in [G.V()](https://gdotv.com),
open the desktop interface and load the GraphSON files when you create a new graph playground.
You can re-load them with Gremlin commands like:

```gremlin
g.io("/path/to/hydra/hydra-ext/demos/genpg/output/sales.jsonl").read().iterate()
g.io("/path/to/hydra/hydra-ext/demos/genpg/output/copilot.jsonl").read().iterate()
```

Run a Gremlin query like `g.E()`, and off you go.
