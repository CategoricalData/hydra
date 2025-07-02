# Hydra-Ext

This subproject contains domain-specific extensions to Hydra, currently
including the coders and models listed below.
Most of these artifacts are written in "raw" Haskell, rather than in the Hydra
DSL.
There are also a number of demos and tools also written in Haskell, as well as
a few artifacts in Java and Python.

JavaDocs for Hydra-Ext can be found [here](https://categoricaldata.github.io/hydra/hydra-ext/javadoc),
and releases can be found on Maven Central [here](https://central.sonatype.com/artifact/net.fortytwo.hydra/hydra-ext).

## Coders

Hydra-Ext contains the following two-level coders:
* Avro
* Java
* Property graphs ("PG")
* Python
* Scala

The following are schema-only coders:
* C++
* C-sharp
* Graphql
* Pegagus (PDL)
* Protobuf
* SHACL

And the following are data-only coders:
* GraphSON
* GraphViz
* RDF

## Models

The following models are included:
* [Apache Atlas](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Models/Atlas.hs)
* [Azure DTLD](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Models/AzureDtld.hs)
* [Coq](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Models/Coq.hs)
* [Datalog](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Models/Datalog.hs)
* [GeoJSON](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Models/GeoJson.hs)
* [IANA Relations](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Models/IanaRelations.hs)
* [OSV](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Models/Osv.hs)
* [STAC Items](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Models/StacItems.hs)

These extensions are listed [here](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Extensions.hs),
and the generated Haskell APIs for all of these models can be found [here](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/src/gen-main/haskell).
For the sake of space, only generated Haskell is checked in to the repository, but Java APIs can be generated from GHCi (use `stack ghci`) as follows:

```haskell
writeJava "src/gen-main/java" hydraExtModules
```

The generated Haskell can be updated using:

```haskell
writeHaskell "src/gen-main/haskell" hydraExtModules
```

## Tools

Miscellaneous tools include:
* **Analysis**: utilities for analyzing the Hydra kernel or other Hydra graphs, for the sake of optimization, visualization, or LLM-based features
* **AvroWorkflows**: transform Avro schemas and matching JSON data to one of multiple targets (RDF with SHACL, property graphs with schemas)
* **Csv**: utilities for working with CSV data
* **OsvToRdf**: transform [OSV](https://osv.dev) dumps to RDF
* **PropertyGraphToRdf**: like the name
* **Tabular**: utilities for working with tabular data

## Demos

### AvroToPropertyGraphs

* **GenPG**: uses an LLM to generate Hydra schemas and mappings based on tabular data sources. There is a demo video [here](https://drive.google.com/file/d/10HCElcG7n0tprOTdtX4bSa5yWYs08nV-/view?usp=sharing).
* **AvroToPropertyGraphs**: transforms a specific Avro schema and matching sample JSON to a property graph representation
* **MeteredEvaluation**: demonstrates term reduction with logging, e.g. for tracking usage or estimating cost

To run the `AvroToPropertyGraphs` demo, first enter `stack ghci`, then:

```haskell
import Hydra.Tools.AvroWorkflows
import Hydra.Demos.AvroToPropertyGraphs

-- Arguments
jsonLastMile = propertyGraphJsonLastMile examplePgSchema () ()
graphsonLastMile = propertyGraphGraphsonLastMile exampleGraphsonContext examplePgSchema () ()
aviationSchema = "src/test/avro/aviationdemo/AirplaneInfo.avsc"
aviationDataDir = "src/test/json/aviationdemo"
movieSchema = "src/test/avro/moviedemo/Review.avsc"
movieDataDir = "src/test/json/moviedemo"
outDir = "/tmp/avro-pg-demo/output"

-- Try a few combinations
transformAvroJsonToPg jsonLastMile aviationSchema aviationDataDir outDir
transformAvroJsonToPg graphsonLastMile aviationSchema aviationDataDir outDir
transformAvroJsonToPg jsonLastMile movieSchema movieDataDir outDir
transformAvroJsonToPg graphsonLastMile movieSchema movieDataDir outDir
```

## Code generation

Java generation is similar to Haskell generation (see the [Hydra-Haskell README](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/README.md)), e.g.

```haskell
writeJava "../hydra-java/src/gen-main/java" mainModules
```

For Java tests, use:

```haskell
writeJava "../hydra-java/src/gen-test/java" testModules
```

Scala generation has known bugs, but you can try it out with:

```haskell
writeScala "../hydra-scala/src/gen-main/scala" kernelModules
```

There is schema-only support for GraphQL:

```haskell
import Hydra.Sources.Langs.Graphql.Syntax
import Hydra.Sources.Langs.Json.Model
writeGraphql "/tmp/graphql" [graphqlSyntaxModule, jsonModelModule]
```

Because GraphQL does not support imports, the GraphQL coder will gather all of the dependencies of a given module together,
and map them to a single `.graphql` file.
Hydra has a similar level of schema-only support for [Protobuf](https://protobuf.dev/):

```haskell
writeProtobuf "/tmp/proto" [jsonModelModule]
```

...and similarly for [PDL](https://linkedin.github.io/rest.li/pdl_schema):

```haskell
writePdl "/tmp/pdl" [jsonModelModule]
```

Note that neither the Protobuf nor PDL coder currently supports polymorphic models.
