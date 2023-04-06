# Hydra Extensions

This project contains additional models and Haskell-based tools which have been used in applications
and/or which may be generally useful. There are also a couple of small demos.

## Models

The following models are included:
* [Apache Atlas](https://github.com/CategoricalData/hydra/blob/main/hydra-extensions/src/main/haskell/Hydra/Models/Atlas.hs)
* [Azure DTLD](https://github.com/CategoricalData/hydra/blob/main/hydra-extensions/src/main/haskell/Hydra/Models/AzureDtld.hs)
* [Coq](https://github.com/CategoricalData/hydra/blob/main/hydra-extensions/src/main/haskell/Hydra/Models/Coq.hs)
* [Datalog](https://github.com/CategoricalData/hydra/blob/main/hydra-extensions/src/main/haskell/Hydra/Models/Datalog.hs)
* [GeoJSON](https://github.com/CategoricalData/hydra/blob/main/hydra-extensions/src/main/haskell/Hydra/Models/GeoJson.hs)
* [IANA Relations](https://github.com/CategoricalData/hydra/blob/main/hydra-extensions/src/main/haskell/Hydra/Models/IanaRelations.hs)
* [OSV](https://github.com/CategoricalData/hydra/blob/main/hydra-extensions/src/main/haskell/Hydra/Models/Osv.hs)
* [STAC Items](https://github.com/CategoricalData/hydra/blob/main/hydra-extensions/src/main/haskell/Hydra/Models/StacItems.hs)

These extensions are listed [here](https://github.com/CategoricalData/hydra/blob/main/hydra-extensions/src/main/haskell/Hydra/Extensions.hs),
and the generated Haskell APIs for all of these models can be found [here](https://github.com/CategoricalData/hydra/tree/main/hydra-extensions/src/gen-main/haskell).
For the sake of space, only generated Haskell is checked in to the repository, but Java APIs can be generated from GHCi (use `stack ghci`) as follows:

```haskell
writeJava hydraExtensionsModules "src/gen-main/java"
```

The generated Haskell can be updated using:

```haskell
writeHaskell hydraExtensionsModules "src/gen-main/haskell"
```

## Tools

Experimental tools include:
* **AvroWorkflows**: transform Avro schemas and matching JSON data to one of multiple targets (RDF with SHACL, property graphs with schemas)
* **OsvToRdf**: transform [OSV](https://osv.dev) dumps to RDF
* **PropertyGraphToRdf**: like the name

## Demos

### AvroToPropertyGraphs

* **AvroToPropertyGraphs**: transforms a specific Avro schema and matching sample JSON to a property graph representation
* **MeteredEvaluation**: demonstrates term reduction with logging, e.g. for tracking usage or estimating cost

To run the `AvroToPropertyGraphs` demo, use:

```haskell
import Hydra.Tools.AvroWorkflows
import Hydra.Demos.AvroToPropertyGraphs

transformAirplaneInfo (propertyGraphLastMile examplePgSchema)
```
