# Hydra

This is an open-source project along the lines of [Dragon](https://eng.uber.com/dragon-schema-integration-at-uber-scale), but with a more advanced type system and major new features, currently in an early "bootstrapping" stage. The primary superpower of Hydra is that it is able to map schemas and data consistently between languages. It is even able to map functional programs between selected languages, including parts of its own source code.

You can find a design document [here](https://bit.ly/hydra-design-doc), and a Slack channel [here](https://bit.ly/hydra-slack) (requires an invite to the Graph Community workspace; feel free to email josh at fortytwo net).

## Project structure

This repository currently contains a [hydra-haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell), a [hydra-scala](https://github.com/CategoricalData/hydra/tree/main/hydra-scala) and a [hydra-java](https://github.com/CategoricalData/hydra/tree/main/hydra-java) directory with language-specific builds, as well as some common resources in the root directory.

### Haskell build

Haskell is the current source-of-truth language for Hydra, which means that most of the Hydra implementation is written either in "raw" Haskell or in a Haskell-based DSL. You can find the DSL-based sources [here](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Impl/Haskell/Sources); anything written in the DSL is also mapped into the generated Scala and Java sources. You can find the generated Haskell sources [here](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/gen-main/haskell).

To compile the Haskell project, install [Stack](https://docs.haskellstack.org/en/stable/README/) and then run `stack install`. Enter the GHCi REPL with `stack ghci`. For unit tests, run `stack test`.

### Scala build

You can compile the Scala code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

### Java build

Build the Java project with `./gradlew`.
