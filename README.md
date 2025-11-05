# Hydra

Welcome to the Hydra project!
Hydra is a unique functional programming language based on the [LambdaGraph](https://bit.ly/lg-kgc2024) data model.
It explores an isomorphism between labeled [hypergraphs](https://en.wikipedia.org/wiki/Hypergraph) and [typed lambda calculus](https://en.wikipedia.org/wiki/Typed_lambda_calculus):
in Hydra, programs are graphs, and graphs are programs.
Hydra has the ability to translate its own kernel into several other languages, including
Haskell ([Hydra-Haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell)),
Java ([Hydra-Java](https://github.com/CategoricalData/hydra/tree/main/hydra-java)),
and Python ([Hydra-Python](https://github.com/CategoricalData/hydra/tree/main/hydra-python); complete and being tested for production readiness).
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
Near-term goals of the project include what we call "closing the loop" (completely folding Hydra's kernel into the Hydra DSL),
[finishing](https://github.com/CategoricalData/hydra/wiki/New-Hydra-implementations) Hydra-Python and updating Hydra-Java,
and getting Hydra out into the community.
There is a recent [demo video](https://drive.google.com/file/d/1p2R6WWc1cW02eb7O8l-4cV1htDb84Y43/view) you can check out,
as well as a couple of earlier presentations [here](https://www.slideshare.net/joshsh/transpilers-gone-wild-introducing-hydra)
and [here](https://docs.google.com/presentation/d/1PF0K3KtopV0tMVa0sGBW2hDA7nw-cSwQm6h1AED1VSA),
and the original design document [here](https://bit.ly/hydra-design-doc).

Share and enjoy.
