# Project structure (full layout)

For an overview of the bare-repo + worktrees layout, see CLAUDE.md ("Project structure").
This page enumerates the subdirectories inside a worktree.

```
hydra/
  hydra.git/          # Bare repo (the shared object store); never edit manually
  worktrees/          # One working tree per active branch
    staging/          # Main line of development
    feature_NNN_*/    # Active feature branches (one worktree each)
    ...
  wiki/               # Local checkout of the GitHub wiki (separate Git repo)
```

Each worktree under `worktrees/` is a normal git working tree:

```
worktrees/<branch>/
  packages/           # Language-independent package definitions and DSL sources
    hydra-kernel/     # Kernel types, terms, DSL sources, and package manifest
    hydra-haskell/    # Haskell coder DSL sources + generated Haskell coder output
    hydra-java/       # Java coder DSL sources (Haskell-based)
    hydra-python/     # Python coder DSL sources (Haskell-based)
    hydra-scala/      # Scala coder DSL sources
    hydra-lisp/       # Lisp coder DSL sources + per-dialect generated output
    hydra-go/         # Go coder DSL sources (head bud — Coder/Serde still plain Haskell under heads/haskell)
    hydra-pg/         # Property graph model DSL sources
    hydra-rdf/        # RDF/SHACL model DSL sources
    hydra-ext/        # Miscellaneous extension DSL sources (Avro, Protobuf, GraphQL, ...)
    hydra-coq/        # Coq coder DSL sources
    hydra-javascript/ # JavaScript coder DSL sources
  heads/              # Per-host build infrastructure: primitives, DSL runtime, generation
    haskell/          # Stack package ("hydra"), exec binaries, Hydra.Dsl/Lib/Generation
    java/             # Hand-written Java primitives, DSL, utils; gradle source-set crossover
    python/           # Hand-written Python primitives, DSL; pyproject.toml lives here
    scala/            # Hand-written Scala primitives; sbt source crossover
    lisp/             # Per-dialect subdirs: clojure/, common-lisp/, emacs-lisp/, scheme/
    go/               # Head bud: go.mod, bin/{assemble,test}-distribution.sh, mostly placeholder runtime
  dist/               # Generated output per host language
    json/             # Always checked in. Kernel JSON modules.
    haskell/          # Partially checked in (kernel + coders for bootstrap)
    java/             # Generated Java kernel
    python/           # Generated Python kernel
    scala/            # Generated Scala kernel
    clojure/          # Generated Clojure kernel
    common-lisp/      # Generated Common Lisp kernel
    emacs-lisp/       # Generated Emacs Lisp kernel
    scheme/           # Generated Scheme kernel
    go/               # NOT checked in. Regenerate via bin/sync-go.sh (kernel only; head bud)
    coq/              # NOT checked in. Regenerate via generate-coq + generate-coq-tests
  demos/              # Example applications (not published)
  bindings/           # Host-specific third-party integrations
    java/
      hydra-rdf4j/    # Wraps hydra.rdf.syntax.* against eclipse rdf4j (rdf4j-rio-ntriples, etc.)
      hydra-neo4j/    # Cypher (and GQL) parsers via ANTLR; consumes hydra.pg.{model,query}
    # Future bindings: hydra-tinkerpop, hydra-jena, etc., plus
    # bindings/python/ and bindings/scala/ as native bindings appear.
  docs/               # Documentation, recipes, guides
  claude/             # Claude-specific protocol and workflow notes (this directory's parent)
  <branch>-plan.md    # Untracked branch plan (see CLAUDE.md "Session procedures")
```

### About `bindings/`

`bindings/` is the third structural category alongside `packages/` and `heads/`,
introduced after the rollup-everything-into-`hydra-java` design proved unworkable.
The rules:

- Each binding is a **handwritten** Maven/PyPI/etc. artifact (no DSL definition,
  no JSON pipeline, not in `hydra.json`'s package list).
- Each binding **depends on exactly one Hydra package** (e.g., `hydra-rdf4j` depends
  on `hydra-rdf`) and on the third-party library it wraps.
- Bindings are independently versioned and publishable. In a multi-project Gradle
  build they participate as `project(':hydra-rdf4j')` references; downstream
  consumers pull the published artifact.
- Bindings are **not** consumed by the bootstrap demo or by any Hydra package.
  They sit at the leaves of the dependency graph, not in the spine.

If handwritten host-language code wants to depend on a third-party library
(rdf4j, ANTLR, Neo4j, Apache Jena, TinkerPop, etc.), that code belongs in a binding,
not in a `heads/<lang>/` runtime. The runtime stays third-party-free except for
host stdlib + minimal build tooling.

For the corresponding human-facing architecture, see the
[Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization) wiki page
and [docs/implementation.md](../docs/implementation.md).

## Version

Tracked in the `VERSION` file at the worktree root.

## Build artifacts

Build artifacts (`.stack-work/`, `build/`, `.gradle/`) live per worktree
and are not shared across branches.
