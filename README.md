# Hydra

Welcome to the Hydra project!
Hydra is a unique functional programming language based on the [LambdaGraph](https://bit.ly/lg-kgc2024) data model.
It explores an isomorphism between labeled [hypergraphs](https://en.wikipedia.org/wiki/Hypergraph) and [typed lambda calculus](https://en.wikipedia.org/wiki/Typed_lambda_calculus):
in Hydra, programs are graphs, and graphs are programs.
Hydra has the ability to translate its own kernel into several other languages, including
Haskell ([Hydra-Haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell)),
Java ([Hydra-Java](https://github.com/CategoricalData/hydra/tree/main/hydra-java)),
and Python ([Hydra-Python](https://github.com/CategoricalData/hydra/tree/main/hydra-python)).
All three implementations are complete and pass the [common test suite](https://github.com/CategoricalData/hydra/wiki/Testing).
Hydra is used for data modeling, validation, and transforms at Microsoft,
while its closed-source predecessor [Dragon](https://www.uber.com/blog/dragon-schema-integration-at-uber-scale/) was used
for data integration and graph construction at Uber.
The language is now being developed for its own sake, with the intention of becoming an [Apache Incubator](https://incubator.apache.org) project,
integrating more directly with [Apache TinkerPop](https://tinkerpop.apache.org),
and branching out into additional concrete programming languages.
Typical use cases include:
* **Graph construction**. Hydra supports TinkerPop-style property graphs 
  as well as [RDF](https://en.wikipedia.org/wiki/Resource_Description_Framework) and [SHACL](https://en.wikipedia.org/wiki/SHACL),
  and has been used in combination with the ISO/IEC [GQL](https://en.wikipedia.org/wiki/Graph_Query_Language) standard.
  Hydra provides [DSLs](https://en.wikipedia.org/wiki/Domain-specific_language) for defining schemas and mappings,
  as well as tools for validating schemas and data, and moving them seamlessly into and out of the graph formats.
* **Data integration**. Hydra includes "coders" (encoders+decoders) for many data and schema languages which you can easily compose together
  to build data transform pipelines. Some of the currently supported languages and formats include 
  [Protobuf](https://en.wikipedia.org/wiki/Protocol_Buffers),
  [Avro](https://avro.apache.org),
  [JSON](https://json.org) and [YAML](https://en.wikipedia.org/wiki/YAML),
  [RDF](https://www.w3.org/RDF) formats including N-Triples,
  [GraphQL](https://graphql.org/),
  LinkedIn's [PDL Schema](https://linkedin.github.io/rest.li/pdl_schema) language, as well as simple tabular data (CSV/TSV).
  Hydra has been used extensively with (Delta) [Parquet](https://en.wikipedia.org/wiki/Apache_Parquet), although this support is not currently open source. 
 There is also limited support for [C Sharp](https://en.wikipedia.org/wiki/C_Sharp_(programming_language)) and [Scala](https://en.wikipedia.org/wiki/Scala_(programming_language)) ([Hydra-Scala](https://github.com/CategoricalData/hydra/tree/main/hydra-scala)).

* **Computational graphs**. Unusually among graph data models and query languages, Hydra has deep support for [polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism),
  as well as embedding computational elements with a graph (sometimes called *computational knowledge graphs*).
  As we mentioned, programs are graphs and vice versa. See the KGC 2024 presentation [Graphs, logics, and lambda calculus](https://bit.ly/lg-kgc2024) for examples.

One of the less exciting, but very important aspects of Hydra is its test suite, which guarantees parity across the supported programming languages.
This ensures that each Hydra implementation behaves the same,
and we think it will be very useful in heterogeneous environments like TinkerPop where we need exactly the same logic
-- validation, query steps, user-defined functions, etc. -- to be manifested identically in more than one programming language.

If any of the above sounds interesting, feel free to ask questions or get involved via the [LambdaGraph Discord](https://bit.ly/lg-discord) server.
We are preparing for the 1.0 release, with all three core implementations (Haskell, Java, Python) now complete.
Near-term goals include integrating with [Apache TinkerPop](https://tinkerpop.apache.org),
expanding language support, and growing the community.
There is a recent [demo video](https://drive.google.com/file/d/1p2R6WWc1cW02eb7O8l-4cV1htDb84Y43/view) you can check out,
as well as a couple of earlier presentations [here](https://www.slideshare.net/joshsh/transpilers-gone-wild-introducing-hydra)
and [here](https://docs.google.com/presentation/d/1PF0K3KtopV0tMVa0sGBW2hDA7nw-cSwQm6h1AED1VSA),
and the original design document [here](https://bit.ly/hydra-design-doc).

## Documentation

- **[Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts)** - Core concepts, type system, and design principles
- **[Implementation](https://github.com/CategoricalData/hydra/blob/main/docs/src/implementation.md)** - Detailed implementation guide
- **[DSL Guide](https://github.com/CategoricalData/hydra/blob/main/docs/src/dsl-guide.md)** - Domain-specific languages for Hydra
- **[Testing](https://github.com/CategoricalData/hydra/wiki/Testing)** - Common test suite across implementations
- **[Developer Recipes](https://github.com/CategoricalData/hydra/blob/main/docs/src/recipes/index.md)** - Step-by-step guides for common tasks
- **[CHANGELOG](CHANGELOG.md)** - Detailed history of changes

## Releases

All Hydra implementations share a single version number.
The current release is **0.13.0**; see the [CHANGELOG](CHANGELOG.md) for details.

| Implementation | Package | Install |
|----------------|---------|---------|
| Haskell | [hydra on Hackage](https://hackage.haskell.org/package/hydra) | `cabal install hydra` or add to `package.yaml` |
| Java | [hydra-java on Maven Central](https://central.sonatype.com/artifact/net.fortytwo.hydra/hydra-java) | `net.fortytwo.hydra:hydra-java:0.13.0` |
| Java (extensions) | [hydra-ext on Maven Central](https://central.sonatype.com/artifact/net.fortytwo.hydra/hydra-ext) | `net.fortytwo.hydra:hydra-ext:0.13.0` |
| Python | [conda-forge](https://github.com/conda-forge/staged-recipes/pull/30887) (in progress) | Coming soon |

See the [release process](https://github.com/CategoricalData/hydra/wiki/Release-process)
for how releases are built and published.
