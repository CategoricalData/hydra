# Hydra-Scala

**Status: Implementation stub - currently on hold**

Hydra-Scala is an experimental Scala implementation that is currently on hold while we focus on completing and refining [Hydra-Python](https://github.com/CategoricalData/hydra/tree/main/hydra-python) and [Hydra-Java](https://github.com/CategoricalData/hydra/tree/main/hydra-java).

For production use, please see:
- [Hydra-Haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell) - Most complete implementation
- [Hydra-Java](https://github.com/CategoricalData/hydra/tree/main/hydra-java) - Mature, production-ready
- [Hydra-Python](https://github.com/CategoricalData/hydra/tree/main/hydra-python) - Complete, being tested for production readiness

See the main Hydra [README](https://github.com/CategoricalData/hydra) for more details about the project.

## Build

You can compile the Scala code with `sbt compile` and run it with `sbt run`, while `sbt console` will start a Scala 3 REPL.

## Contributing

Hydra is an open-source project, and we welcome contributors! If you're interested in helping complete the Scala implementation, here are some resources to get started:

- **[Creating a new Hydra implementation](https://github.com/CategoricalData/hydra/blob/main/docs/src/recipes/new-implementation.md)** - Step-by-step guide for implementing Hydra in a new language
- **[LambdaGraph Discord](https://bit.ly/lg-discord)** - Join the community to ask questions and discuss development
- **[Main README](https://github.com/CategoricalData/hydra)** - Project overview and architecture

The Scala syntax model and language coder are already defined in [hydra-ext](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Scala), which provides a foundation to build upon.

