# Hydra

Hydra is a transformation toolkit along the lines of [Dragon](https://eng.uber.com/dragon-schema-integration-at-uber-scale),
but open source, and with a more advanced type system and other new features.
It is currently in an intermediate "closing the loop" stage.
The main superpower of Hydra is that it is able to map schemas and data consistently between languages
in a way which maintains type conformance.
It is even able to map functional programs between selected languages, including parts of its own source code.
See the recent Data Day Texas presentation, "[Transpilers Gone Wild](https://www.slideshare.net/joshsh/transpilers-gone-wild-introducing-hydra)".

You can find a design document [here](https://bit.ly/hydra-design-doc),
and a Slack channel [here](https://bit.ly/hydra-slack)
(click [here](https://join.slack.com/t/graphcommunity/shared_invite/zt-1a6ohrnn9-rXIBwn3L4NSC4cH0c1DN8A) for an invite to the Graph Community workspace, or send an email to josh at fortytwo net if the link has expired).

## Project structure

This repository currently contains a [hydra-haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell),
a [hydra-java](https://github.com/CategoricalData/hydra/tree/main/hydra-java),
and a [hydra-scala](https://github.com/CategoricalData/hydra/tree/main/hydra-scala) directory with language-specific builds,
as well as some common resources in the root directory.

Additional Hydra "coders" (type-aware encoders/decoders) at this time support
[Avro](https://avro.apache.org),
[JSON](https://json.org),
[YAML](https://en.wikipedia.org/wiki/YAML),
[RDF](https://www.w3.org/RDF)+[SHACL](https://www.w3.org/TR/shacl),
and LinkedIn's [PDL Schema](https://linkedin.github.io/rest.li/pdl_schema) language.

### Haskell build

Haskell is the current source-of-truth language for Hydra, which means that most of the Hydra implementation is written either in "raw" Haskell or in a Haskell-based DSL.
You can find the DSL-based sources [here](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Impl/Haskell/Sources);
anything written in the DSL is also mapped into the generated Scala and Java sources.
You can find the generated Haskell sources [here](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/gen-main/haskell).

To compile the Haskell project, install [Stack](https://docs.haskellstack.org/en/stable/README/) and then run `stack install`.
Enter the GHCi REPL with `stack ghci`. For unit tests, run `stack test`.

### Java build

Build the Java project with `./gradlew build`, or publish the resulting JAR to your local Maven repository with `./gradlew publishToMavenLocal`.

### Scala build

You can compile the Scala code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

## Code generation

One of the main objectives for Hydra is for the framework to generate its own source code into various languages,
producing nearly-complete Hydra implementations in those languages.
At this time, Haskell is fully supported as a target language, while Java is supported for schemas only (i.e. Hydra type definitions map to Java classes),
and Scala is supported for data only (i.e. constants and functions are mapped, but types are not yet).

You can generate Hydra's sources by first entering the GHCi REPL using `stack ghci`, then:

```bash
writeHaskell allModules "/path/to/CategoricalData/hydra/hydra-haskell/src/gen-main/haskell"
```

The first argument to `writeHaskell` is the list of modules you want to generate (in this case, a special list containing all built-in modules),
and the second is the base directory to which the generated files are to be written.
For individual modules, use Haskell list syntax, e.g.

```bash
writeHaskell [rdfSyntaxModule, shaclModelModule] "/path/to/CategoricalData/hydra/hydra-haskell/src/gen-main/haskell"
```

The commands for Scala and Java generation are similar, e.g.

```bash
writeScala allModules "/path/to/CategoricalData/hydra/hydra-scala/src/gen-main/scala"
```

and

```bash
writeJava allModules "/path/to/CategoricalData/hydra/hydra-java/src/gen-main/java"
```

There is also schema-only support for PDL:

```bash
writePdl allModules "/tmp/pdl"
```

For languages other than Haskell and Java, you can expect error messages from Hydra where a given coder encounters language features which are not yet fully implemented.

## Haskell API

### Structures

The most important structural types in Hydra are `Type` and `Term` (provided in the generated [Hydra.Core](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/gen-main/haskell/Hydra/Core.hs) module in Haskell), and `Graph` and `Element` (provided in the generated [Hydra.Graph](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/gen-main/haskell/Hydra/Graph.hs) module).
`Type` provides a datatype, and a `Term` is an instance of a known `Type`.
An `Element` is a named term together with its type, and a `Graph` is a collection of elements.
A `Module` is a collection of elements in the same logical namespace, sometimes called a "model" if most of the elements represent type definitions.
The main purpose of Hydra is to define and carry out transformations between graphs,
where those graphs may be almost anything which fits into Hydra's type system -- data, schemas, source code, transformations themselves, etc.
"Graphs" in the traditional sense are partially supported at this time, including property graphs and RDF graphs.

Types, terms, graphs, elements, and many other things are parameterized by an annotation type, so you will usually see `Type m`, `Term m`, `Context m`, etc.
The most common annotation type is called `Meta` (which is just a map of string-valued keys to terms), so you will also encounter `Type Meta`, etc.

### Transformations

Transformations in Hydra take the form of simple functions or, more commonly, expressions involving the `Flow` monad
(a special case of the [State](https://wiki.haskell.org/State_Monad) monad, which has been implemented in many programming languages)
as well as a bidirectional flow, called `Coder` and a two-level transformation (types and terms) called `Adapter`.
All of these constructs are provided in the generated [Hydra.Evaluation](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/gen-main/haskell/Hydra/Evaluation.hs) module in Haskell,
along with the `Context` type which you will see almost everywhere in Hydra;
a `Context` provides a set of graphs and their elements, a set of primitive functions, an evaluation strategy, and other constructs which are needed for computation.
A context is part of the state which flows through a graph transformation as it is being applied.

In Haskell, you will often see `Flow` and `Context` combined as the `GraphFlow` alias:

```haskell
type GraphFlow m = Flow (Context m)
```

There are two helper types, `FlowWrapper` and `Trace`, which are used together with `Flow`; a `FlowWrapper` is the result of evaluating a `Flow`,
while `Trace` encapsulates a stack trace and error or logger messages.
Since `Flow` is a monad, you can create a `GraphFlow` with `f = pure x`, where `x` is anything you would like to enter into a transformation pipeline.
The transformation is actually applied when you call `unFlow` and pass in a graph context and a trace, i.e.

```haskell
unFlow f cx emptyTrace
```

This gives you a flow wrapper, which you can think of as the exit point of a transformation.
Inside the wrapper is either a concrete value (if the transformation succeeded) or `Nothing` (if the transformation failed), a stack trace, and a list of messages.
You will always find at least one message if the transformation failed; this is analogous to an exception in mainstream programming languages.

A `Coder`, as mentioned above, is a construct which has a `Flow` in either direction between two types.
As a trivial example, consider this coder which serializes integers to strings using Haskell's built-in `show` function, then reads the strings back to integers using `read`:

```haskell
intStringCoder :: Coder () () Int String
intStringCoder = Coder {
  coderEncode = pure . show,
  coderDecode = pure . read}
```

The `()`'s indicates that this coder is stateless in both directions, which makes the use of `Coder` overkill in this case.
For a more realistic, but still simple example, see the [JSON coder](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Ext/Json/Coder.hs), which makes use of state for error propagation.
For a more sophisticated example, see the [Haskell coder](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Ext/Haskell/Coder.hs)
or the [Java coder](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Ext/Java/Coder.hs);
these make use of all of the facilities of a graph flow, including lexical lookups, type decoding, annotations, etc.

### DSLs

Constructing types and terms directly from the `Type` and `Term` APIs mentioned above is perfectly correct, but not very convenient.
For example, the type of all lists of strings may be expressed as `TypeList $ TypeLiteral LiteralTypeString`,
and a specific instance of that type (a term) may be expressed as `TermList [TermLiteral $ LiteralString "foo", TermLiteral $ LiteralString "bar"]`.

Since all of the work of defining transformations in Hydra consists of specifying types and terms, we make the task (much) easier using domain-specific languages (DSLs).
These DSLs are specific to the host language, so we have Haskell DSLs in hydra-haskell, and (similar, but distinct) Java DSLs in hydra-java.
For example, the type of a list of strings is just `list string` if you include the [Types](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Impl/Haskell/Dsl/Types.hs) DSL,
and the specific list of strings we mentioned is just `list [string "foo", string "bar"]`, or (better yet) `list ["foo", "bar"]`.
There is additional syntactic sugar in Haskell which aim to make defining models and transformations as easy as possible;
see the [Sources](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Impl/Haskell/Sources) directory for many examples.

### Phantom types

A minority of Hydra's primary sources, rather than providing models (type definitions), provide collections of functions.
For example, look at [Basics.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Impl/Haskell/Sources/Basics.hs)
or [Utils.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Impl/Haskell/Sources/Adapters/Utils.hs).
There are not many of these files because the syntax for constructing transformations natively in Hydra DSLs is still in flux,
but you will notice that the type signatures in these modules look very different.
For example, you will see signatures like `Definition (Precision -> String)` which appear to use native Haskell types such as `String`,
or generated types like `Precision`, rather than Hydra's low-level constructs (`Type`, `Term`, etc.).
This is a convenience for the programmer which will will be expanded upon as more of Hydra's kernel (indispensable code which is needed in each host language)
is pulled out of raw Haskell and into the DSLs.
If you are curious how these types work, see the [Phantoms](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Impl/Haskell/Sources/Phantoms.hs) model
and [these slides](https://www.slideshare.net/joshsh/transpilers-gone-wild-introducing-hydra/34).
Phantom types are available both in Haskell and Java.
