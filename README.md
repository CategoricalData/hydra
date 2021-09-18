# Hydra

Prepare to be transformed.

## Project structure

This repository currently combines Scala, Haskell, and YAML sources in a single directory. However, the Scala and Haskell projects build separately.

### Scala build

You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

### Haskell build

Compile the project (and eventually install the executable) with `stack install`. Enter the GHCi REPL with `stack ghci`.

