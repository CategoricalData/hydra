# Hydra

Hydra is a transformation toolkit along the lines of [Dragon](https://eng.uber.com/dragon-schema-integration-at-uber-scale),
but open source, and with a more advanced type system and other new features.
Hydra maps data and schemas between languages in a way which maintains type conformance.
It will even map functional programs between selected languages, including parts of its own source code.

For more information, see [hydra-haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell)
as well as the Data Day Texas presentation, "[Transpilers Gone Wild](https://www.slideshare.net/joshsh/transpilers-gone-wild-introducing-hydra)".
You can find an early design document [here](https://bit.ly/hydra-design-doc)
and a Discord server [here](https://discord.gg/3uq8WpFqbG).

The project encompasses:
* **[Hydra-Haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell)**: the reference implementation of Hydra.
This is currently the most complete and mature implementation of the language, and the source-of-truth for all of Hydra's generated code.
Here you will find Hydra coders (type-aware encoders/decoders) for Haskell itself, Java, Scala,
[Avro](https://avro.apache.org), [JSON](https://json.org), [YAML](https://en.wikipedia.org/wiki/YAML),
[RDF](https://www.w3.org/RDF) + [SHACL](https://www.w3.org/TR/shacl), [GraphQL](https://graphql.org/),
and LinkedIn's [PDL Schema](https://linkedin.github.io/rest.li/pdl_schema) language.
Most of the Hydra documentation is also here.
* **[Hydra-Java](https://github.com/CategoricalData/hydra/tree/main/hydra-java)** is a Java implementation of Hydra which is under active development.
It includes a substantial portion of the Hydra kernel, though none of the coders mentioned above is fully ported to Java yet.
JavaDocs are available [here](https://categoricaldata.github.io/hydra/hydra-java/javadoc).
* [Hydra-Scala](https://github.com/CategoricalData/hydra/tree/main/hydra-scala) is an experimental Scala implementation which has been on the back burner for a little while;
Java has been getting most of the attention lately.
* [hydra-extensions](https://github.com/CategoricalData/hydra/tree/main/hydra-extensions) is Haskell project which includes a collection of less-essential, and less thoroughly documented, Hydra models and tools, which nonetheless can be useful for building applications.
For example, there is a GeoJson model, a Coq model, and an AvroWorkflows tool which has been used at LinkedIn for ingestion of Avro-formatted data into RDF triple stores.

Both of the active language variants (Haskell and Java) contain a complete copy of Hydra's built-in generated APIs, including:
* Core data model (types and terms)
* Graphs and modules
* Standard library of primitive functions
* Computation
* Bidirectional transformations
* BNF grammars
* Various utilities for transform workflows, formatting, etc.
* Models for Avro, GraphQL, Haskell, Java, JSON, OWL, Parquet, PDL, Protobuf, Python v3, RDF, Scala, SHACL, ShEx, SQL, [TinkerPop](https://tinkerpop.apache.org)-style property graphs, and YAML

Hydra also features a language-agnostic test suite which guarantees parity of program evaluation and primitive functions across the language variants.
Along with the rest of Hydra's generated code, the test suite will be used as a seed for future implementations in additional languages, such as
[Python](https://github.com/CategoricalData/hydra/issues/66)
and [Go](https://github.com/CategoricalData/hydra/issues/65).
