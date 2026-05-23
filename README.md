# Hydra

Hydra is a strongly typed intermediate language for data, schemas, and code.
Programs and domain models written in Hydra map seamlessly to major programming languages
like Java, Scala, and Python, to data exchange formats like Protobuf, Avro, and JSON,
and to graph data models like RDF and labeled property graphs.

Hydra has been used in production at Microsoft for data modeling, validation, and transforms;
its predecessor [Dragon](https://www.uber.com/blog/dragon-schema-integration-at-uber-scale/)
drove data integration and graph construction at Uber.
Expressive enough to define and compile its own kernel,
Hydra is built on the [LambdaGraph](https://bit.ly/lg-kgc2024) data model,
which establishes an isomorphism between labeled [hypergraphs](https://en.wikipedia.org/wiki/Hypergraph)
and [typed lambda calculus](https://en.wikipedia.org/wiki/Typed_lambda_calculus):
in Hydra, programs are graphs, and graphs are programs
(see [The LambdaGraph isomorphism](https://github.com/CategoricalData/hydra/wiki/Concepts#the-lambdagraph-isomorphism)).

## Use cases

* **Translingual programming**. Write a program or domain model once in a Hydra DSL,
  using the host language you are most comfortable with, and the same logic becomes
  available in every other supported language, with test-driven guarantees of  semantic equivalence.
  The Hydra kernel is the most thoroughly exercised example: a working programming language, with its
  tests, ported across seven languages from a single source of truth.
* **Graph construction**. Hydra supports TinkerPop-style property graphs
  as well as [RDF](https://en.wikipedia.org/wiki/Resource_Description_Framework)
  and [SHACL](https://en.wikipedia.org/wiki/SHACL),
  and has been used in combination with the ISO/IEC [GQL](https://en.wikipedia.org/wiki/Graph_Query_Language) standard.
  Hydra provides [DSLs](https://en.wikipedia.org/wiki/Domain-specific_language) for defining schemas and mappings,
  as well as tools for validating schemas and data, and moving them seamlessly into and out of the graph formats.
* **Data integration**. Hydra includes "coders" (encoders+decoders) for many data and schema languages
  which you can easily compose together
  to build data transform pipelines. Some of the currently supported languages and formats include
  [Protobuf](https://en.wikipedia.org/wiki/Protocol_Buffers),
  [Avro](https://avro.apache.org),
  [JSON](https://json.org) and [JSON Schema](https://json-schema.org/),
  [YAML](https://en.wikipedia.org/wiki/YAML),
  [RDF](https://www.w3.org/RDF) formats including N-Triples,
  [GraphQL](https://graphql.org/),
  and simple tabular data (CSV/TSV).
* **Computational graphs**. Unusually among graph data models and query languages,
  Hydra has deep support for [polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism),
  as well as embedding of computational elements within a graph
  (sometimes called *computational knowledge graphs*).
  This follows naturally from the programs-are-graphs framing.
  See the KGC 2024 presentation [Graphs, logics, and lambda calculus](https://bit.ly/lg-kgc2024) for examples.

## Translingual features

Hydra's most distinctive properties come from its graph foundations, and from being a single programming language kernel that
runs natively in multiple host languages. Its unusual ability to translate any valid program -- including its own kernel --
into any supported language distinguishes it from conventional polyglot tooling:

Hydra is *mutually self-hosting*: starting from any one of its current implementations,
the kernel can be regenerated into another host language, and that regenerated
implementation can in turn regenerate the first — without dependency on the original
source language.
Seven implementations have this property today — Haskell (Hydra's original bootstrapping
language), Java, Python, Scala, and three dialects of Lisp (Clojure, Scheme, and Common Lisp) —
and all of them pass the [common test suite](https://github.com/CategoricalData/hydra/wiki/Testing)
under every bootstrapping path.
Additional ports are in active development; see the [Implementations](#implementations) table below
for the full set.

The common test suite is what makes translingual programming load-bearing rather than
aspirational: it ensures every program behaves the same when translated into each supported language,
which is essential in heterogeneous environments where the same logic must be manifested identically
across more than one programming language.
Examples where this is useful are Gremlin language variants in Apache TinkerPop,
where the same queries/programs need to produce identical results against the same data in different runtime environments,
database clients which expose the same API and validation logic in different languages,
and heterogeneous distributed systems.

## Releases

The latest Hydra release is **0.15.0**. Published artifacts:

| Channel | Packages |
|---|---|
| Hackage (Haskell) | [`hydra`](https://hackage.haskell.org/package/hydra) |
| Maven Central (Java) | [`net.fortytwo.hydra:hydra-kernel`](https://central.sonatype.com/artifact/net.fortytwo.hydra/hydra-kernel), [`hydra-java`](https://central.sonatype.com/artifact/net.fortytwo.hydra/hydra-java), [`hydra-pg`](https://central.sonatype.com/artifact/net.fortytwo.hydra/hydra-pg), [`hydra-rdf`](https://central.sonatype.com/artifact/net.fortytwo.hydra/hydra-rdf) |
| PyPI (Python) | [`hydra-kernel`](https://pypi.org/project/hydra-kernel/), [`hydra-python`](https://pypi.org/project/hydra-python/), [`hydra-pg`](https://pypi.org/project/hydra-pg/), [`hydra-rdf`](https://pypi.org/project/hydra-rdf/) |
| conda-forge (Python) | [`hydra-kernel`](https://prefix.dev/channels/conda-forge/packages/hydra-kernel), [`hydra-python`](https://prefix.dev/channels/conda-forge/packages/hydra-python), [`hydra-pg`](https://prefix.dev/channels/conda-forge/packages/hydra-pg), [`hydra-rdf`](https://prefix.dev/channels/conda-forge/packages/hydra-rdf) |

All Hydra packages share a single version number;
see the [CHANGELOG](CHANGELOG.md) for release history and the
[release process](https://github.com/CategoricalData/hydra/wiki/Release-process)
for how releases are built and published.

## Status

Hydra is preparing for its 1.0 release, with the intention of becoming an
[Apache Incubator](https://incubator.apache.org) project and integrating more directly with
[Apache TinkerPop](https://tinkerpop.apache.org) and other projects in the Apache ecosystem.
The last few releases have focused on production-hardening and forward compatibility.

### Implementations

A Hydra **head** is the mapping of the Hydra kernel into a host language, together
with the primitive functions and bootstrapping infrastructure required to make Hydra
a complete programming language on that host. Each head is an independent point of
entry to Hydra: you can pick the head you're most comfortable with and ignore the others.

| Head | Status | Notes |
|---|---|---|
| [Haskell](packages/hydra-haskell/README.md) | Complete | Hydra's original bootstrapping language and reference implementation. ([Haskell](https://www.haskell.org/)) |
| [Java](packages/hydra-java/README.md) | Complete | ([Java](https://www.java.com/)) |
| [Python](packages/hydra-python/README.md) | Complete | ([Python](https://www.python.org/)) |
| [Scala](packages/hydra-scala/README.md) | Complete | ([Scala](https://www.scala-lang.org/)) |
| [Clojure](packages/hydra-lisp/hydra-clojure/README.md) | Complete | A Lisp dialect on the JVM. ([Clojure](https://clojure.org/)) |
| [Scheme](packages/hydra-lisp/hydra-scheme/README.md) | Complete | ([Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))) |
| [Common Lisp](packages/hydra-lisp/hydra-common-lisp/README.md) | Complete | ([Common Lisp](https://common-lisp.net/)) |
| [Emacs Lisp](packages/hydra-lisp/hydra-emacs-lisp/README.md) | In progress | ([Emacs Lisp](https://www.gnu.org/software/emacs/manual/eintr.html)) |
| [TypeScript](packages/hydra-typescript/README.md) | In progress | Most of the common test suite passes; see the package README for current detail. ([TypeScript](https://www.typescriptlang.org/)) |
| [Go](packages/hydra-go/README.md) | In progress | ([Go](https://go.dev/)) |
| [Rust](packages/hydra-ext/src/main/haskell/Hydra/Sources/Rust) | In progress | Coder lives in `hydra-ext`; the Rust head has not yet been split into its own package. ([Rust](https://www.rust-lang.org/)) |
| [Coq](packages/hydra-coq/README.md) | In progress | Generation-only target; there is no Coq-side runtime. ([Coq](https://coq.inria.fr/)) |
| [WebAssembly](packages/hydra-wasm/README.md) | In progress | ([WebAssembly](https://webassembly.org/)) |
| [C++](packages/hydra-ext/src/main/haskell/Hydra/Sources/Cpp) | In progress | Coder lives in `hydra-ext`; the C++ head has not yet been split into its own package. ([C++](https://isocpp.org/)) |

## Resources

### Getting started and using Hydra

- **[Getting started](docs/getting-started.md)** — using Hydra as a library from your own project.
- **[DSL guide](docs/dsl-guide.md)** — writing Hydra programs and domain models using the embedded DSLs.
  See also the [Java](docs/dsl-guide-java.md) and [Python](docs/dsl-guide-python.md) variants.
- **[Demos](docs/demos.md)** — runnable demos illustrating Hydra's capabilities, with input data and expected output.
- **[Troubleshooting](docs/troubleshooting.md)** — common failure modes and how to diagnose them.

### Concepts and design

- **[Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts)** — core type system, terms, modules, and design principles.
- **[Property graphs](https://github.com/CategoricalData/hydra/wiki/Property-graphs)** — Hydra's hypergraph foundation and its relationship to TinkerPop-style property graphs.
- **[RDF support](https://github.com/CategoricalData/hydra/wiki/RDF)** — modeling, validation, and emission for RDF, SHACL, and related semantic-web formats.
- **[JSON format](docs/json-format.md)** — the language-neutral interchange format for kernel modules.

### For contributors

- **[Implementation guide](docs/implementation.md)** — architectural deep dive into kernel modules, DSLs, primitives, and coders.
- **[Code organization](https://github.com/CategoricalData/hydra/wiki/Code-Organization)** — the `packages/`, `heads/`, and `dist/` layout.
- **[Coding style](https://github.com/CategoricalData/hydra/wiki/Coding-style)** — guiding principles, ordering conventions, common mistakes.
- **[Developer recipes](docs/recipes/index.md)** — step-by-step guides for adding primitives, extending core types, refactoring, and similar tasks.
- **[Build system](docs/build-system.md)** — pipeline phases, caching layers, and what triggers regeneration.

### Talks, demos, and community

- **[Hydra Discord](https://bit.ly/lg-discord)** — the LambdaGraph community server.
- **[HydraPop](https://github.com/CategoricalData/HydraPop)** — translingual property-graph and TinkerPop validation extensions for Hydra (Java + Python).
- **[Introductory blog post](https://gdotv.com/blog/introducing-hydra/)** by G.V (Amber Lennox).
- Earlier presentations: [Transpilers Gone Wild](https://www.slideshare.net/joshsh/transpilers-gone-wild-introducing-hydra), and a [longer technical deck](https://docs.google.com/presentation/d/1PF0K3KtopV0tMVa0sGBW2hDA7nw-cSwQm6h1AED1VSA).
- The original [design document](https://bit.ly/hydra-design-doc).
- **LinkedIn demo series** (oldest to newest):
  - **[Hydra property graph demo](https://www.linkedin.com/posts/joshuashinavier_in-case-you-were-wondering-what-i-have-been-activity-7358601538463830017-U5YE)** ([Part 2](https://www.linkedin.com/posts/joshuashinavier_here-is-part-2-of-the-hydra-property-graph-activity-7358601988755910657-HnCh)).
  - **[Translingual programming demo](https://www.linkedin.com/posts/joshuashinavier_hydra-goes-translingual-the-hydra-graph-activity-7418695357901111296-injH)** from Data Day Texas.
  - **[Bootstrapping demo](https://www.linkedin.com/posts/joshuashinavier_graph-programming-in-any-language-hydra-activity-7436798538312851456-kvN0)** — mutual self-hosting across three languages.
  - **[Graph validation in any language](https://www.linkedin.com/posts/joshuashinavier_graph-validation-in-any-language-bringing-activity-7446966175328116736-H3iV)** — applying Hydra's validation logic identically across host languages.

