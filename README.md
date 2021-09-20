# Hydra

This is a new, open-source project along the lines of [Dragon](https://eng.uber.com/dragon-schema-integration-at-uber-scale), currently in an early design and prototyping stage.
You can find a design document [here](https://bit.ly/hydra-design-doc), and a Slack channel [here](https://bit.ly/hydra-slack).

## Project structure

This repository currently contains `hydra-scala` and a `hydra-haskell` directory with language-specific builds, as well as a `common` directory with YAML-based sources.

### Scala build

You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

### Haskell build

Compile the project (and eventually install the executable) with `stack install`. Enter the GHCi REPL with `stack ghci`.

