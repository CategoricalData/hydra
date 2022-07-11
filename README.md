# Hydra

Hydra is a transformation toolkit along the lines of [Dragon](https://eng.uber.com/dragon-schema-integration-at-uber-scale), but open source, and with a more advanced type system and other new features. It is currently in an intermediate "closing the loop" stage. The primary superpower of Hydra is that it is able to map schemas and data consistently between languages. It is even able to map functional programs between selected languages, including parts of its own source code. 

You can find a design document [here](https://bit.ly/hydra-design-doc), and a Slack channel [here](https://bit.ly/hydra-slack) (click [here](https://join.slack.com/t/graphcommunity/shared_invite/zt-1a6ohrnn9-rXIBwn3L4NSC4cH0c1DN8A) for an invite to the Graph Community workspace, or send an email to josh at fortytwo net if the link has expired).

## Project structure

This repository currently contains a [hydra-haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell), a [hydra-scala](https://github.com/CategoricalData/hydra/tree/main/hydra-scala) and a [hydra-java](https://github.com/CategoricalData/hydra/tree/main/hydra-java) directory with language-specific builds, as well as some common resources in the root directory.

Additional Hydra "coders" at this time support [YAML](https://en.wikipedia.org/wiki/YAML) and LinkedIn's [PDL Schema](https://linkedin.github.io/rest.li/pdl_schema) language.

### Haskell build

Haskell is the current source-of-truth language for Hydra, which means that most of the Hydra implementation is written either in "raw" Haskell or in a Haskell-based DSL. You can find the DSL-based sources [here](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Impl/Haskell/Sources); anything written in the DSL is also mapped into the generated Scala and Java sources. You can find the generated Haskell sources [here](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/gen-main/haskell).

To compile the Haskell project, install [Stack](https://docs.haskellstack.org/en/stable/README/) and then run `stack install`. Enter the GHCi REPL with `stack ghci`. For unit tests, run `stack test`.

### Scala build

You can compile the Scala code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

### Java build

Build the Java project with `./gradlew build`.

## Code generation

One of the main objectives for Hydra is for the framework to generate its own source code into various languages. At this time, Haskell is fully supported as a target language, while Java is supported for schemas only (i.e. Hydra type definitions map to Java classes), and Scala is supported for data only (i.e. constants and functions are mapped, but types are not yet).

You can generate Hydra's sources by first entering the GHCi REPL using `stack ghci`, then:

```
writeHaskell coreModules "/path/to/CategoricalData/hydra/hydra-haskell/src/gen-main/haskell"
```

The first argument to `writeHaskell` is the list of modules you want to generate (in this case, a special list containing all built-in modules), and the second is the base directory to which the generated files are to be written. For individual modules, use Haskell list syntax, e.g.

```
writeHaskell [pure rdfSyntaxModule, pure shaclModelModule] "/path/to/CategoricalData/hydra/hydra-haskell/src/gen-main/haskell"
```

The commands for Scala and Java generation are similar, e.g.

```
writeScala coreModules "/path/to/CategoricalData/hydra/hydra-scala/src/gen-main/scala"
```

and

```
writeJava coreModules "/path/to/CategoricalData/hydra/hydra-java/src/gen-main/java"
```

There is also schema-only support for PDL:

```
writePdl coreModules "/tmp/pdl"
```

For languages other than Haskell and Java, you can expect error messages from Hydra where a given coder encounters language features which are not yet fully implemented.
