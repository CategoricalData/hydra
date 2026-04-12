# Documentation migration checklist for #290

All documentation files that reference the old directory structure and need updating.
Checked items have been updated for the current restructure state.

## Updated
- [x] CLAUDE.md — project structure, one rule, shorthand commands, pitfalls, README links

## Critical — step-by-step instructions that break if paths are wrong

### docs/recipes/
- [x] docs/recipes/adding-primitives.md — 20+ paths to primitives across implementations
- [x] docs/recipes/code-generation.md — writeHaskell/writeJava/writePython commands and output paths
- [x] docs/recipes/extending-hydra-core.md — bootstrap patch instructions with 20+ paths
- [x] docs/recipes/extending-tests.md — test development paths
- [x] docs/recipes/json-kernel.md — JSON export/import paths
- [x] docs/recipes/maintenance.md — 40+ paths across all implementations
- [x] docs/recipes/new-implementation.md — 50+ paths for implementing a new language
- [x] docs/recipes/refactoring.md — 30+ paths for file operations
- [x] docs/recipes/refactoring-namespaces.md — 40+ namespace refactoring paths
- [x] docs/recipes/syncing-python.md — Python sync step-by-step

### docs/
- [x] docs/implementation.md — architecture doc with 40+ embedded paths
- [x] docs/test-suite-architecture.md — test paths and structure
- [x] docs/troubleshooting.md — debugging paths per language

### Package READMEs
- [x] packages/hydra-haskell/README.md — build/test commands, GitHub URLs (30+ refs)
- [x] packages/hydra-ext/README.md — code gen instructions, GitHub URLs (50+ refs)
- [x] packages/hydra-java/README.md — Java-specific docs, GitHub links (15+ refs)
- [x] packages/hydra-python/README.md — Python-specific docs (8 refs)

### Root
- [x] README.md — GitHub links to old paths

## Moderate — mentions in passing, less likely to cause confusion

- [x] docs/dsl-guide.md — a few path examples
- [x] docs/dsl-guide-java.md — table references to Java paths
- [x] docs/dsl-guide-python.md — table references to Python paths
- [x] docs/demos.md — demo path references
- [x] docs/recipes/promoting-code.md — a few examples
- [x] docs/recipes/llm-assisted-development.md — example references
- [x] packages/hydra-scala/README.md — one bootstrapping reference
- [x] packages/hydra-lisp/README.md — general references

## Wiki (separate repo at ../../hydra/wiki/)

- [x] wiki/Code-Organization.md — core page describing src/main vs src/gen-main layout
- [x] wiki/Testing.md — test runner paths
- [x] wiki/Release-process.md — version file locations
- [x] wiki/Benchmarking.md — benchmark runner paths
- [x] wiki/Concepts.md — structural references
- [x] wiki/Property-graphs.md — PG module paths
- [x] wiki/RDF.md — RDF module paths

## Path substitution cheat sheet

| Old path | New path |
|----------|----------|
| `hydra-haskell/` | `packages/hydra-haskell/` (DSL sources + gen coder output) |
| `hydra-haskell/src/main/haskell/Hydra/Dsl/` | `heads/haskell/src/main/haskell/Hydra/Dsl/` |
| `hydra-haskell/src/main/haskell/Hydra/Lib/` | `heads/haskell/src/main/haskell/Hydra/Lib/` |
| `hydra-haskell/src/main/haskell/Hydra/Generation.hs` | `heads/haskell/src/main/haskell/Hydra/Generation.hs` |
| `hydra-haskell/src/main/haskell/Hydra/Kernel.hs` | `heads/haskell/src/main/haskell/Hydra/Kernel.hs` |
| `hydra-haskell/src/gen-main/haskell/` | `dist/haskell/hydra-kernel/src/main/haskell/` |
| `hydra-haskell/src/gen-main/json/` | `dist/json/hydra-kernel/src/main/json/` |
| `hydra-haskell/src/gen-test/haskell/` | `dist/haskell/hydra-kernel/src/test/haskell/` |
| `hydra-haskell/src/gen-test/json/` | `dist/json/hydra-kernel/src/test/json/` |
| `hydra-haskell/src/test/haskell/` | `heads/haskell/src/test/haskell/` |
| `hydra-haskell/src/exec/` | `heads/haskell/src/exec/` |
| `hydra-haskell/bin/` | `heads/haskell/bin/` |
| `hydra-haskell/package.yaml` | `heads/haskell/package.yaml` |
| `hydra-haskell/stack.yaml` | `heads/haskell/stack.yaml` |
| `hydra-ext/` | `packages/hydra-ext/` |
| `hydra-ext/bin/sync-*.sh` | `packages/hydra-ext/bin/sync-*.sh` |
| `hydra-java/` | `packages/hydra-java/` |
| `hydra-java/src/main/java/hydra/lib/` | `heads/java/src/main/java/hydra/lib/` |
| `hydra-java/src/main/java/hydra/dsl/` | `heads/java/src/main/java/hydra/dsl/` |
| `hydra-java/src/main/java/hydra/util/` | `heads/java/src/main/java/hydra/util/` |
| `hydra-java/src/gen-main/java/` | `dist/java/hydra-kernel/src/main/java/` |
| `hydra-java/src/gen-test/java/` | `dist/java/hydra-kernel/src/test/java/` |
| `hydra-java/src/test/java/` | `heads/java/src/test/java/` |
| `hydra-python/` | `packages/hydra-python/` |
| `hydra-python/src/main/python/hydra/` | `heads/python/src/main/python/hydra/` |
| `hydra-python/src/gen-main/python/` | `dist/python/hydra-kernel/src/main/python/` |
| `hydra-python/src/gen-test/python/` | `dist/python/hydra-kernel/src/test/python/` |
| `hydra-python/src/test/python/` | `heads/python/src/test/python/` |
| `hydra-python/pyproject.toml` | `heads/python/pyproject.toml` |
| `hydra-scala/` | `packages/hydra-scala/` |
| `hydra-scala/src/main/scala/hydra/lib/` | `heads/scala/src/main/scala/hydra/lib/` |
| `hydra-scala/src/gen-main/scala/` | `dist/scala/hydra-kernel/src/main/scala/` |
| `hydra-scala/src/gen-test/scala/` | `dist/scala/hydra-kernel/src/test/scala/` |
| `hydra-lisp/` | `packages/hydra-lisp/` |
| `hydra-lisp/hydra-clojure/src/main/clojure/hydra/` | `heads/lisp/clojure/src/main/clojure/hydra/` |
| `hydra-lisp/hydra-clojure/src/gen-main/clojure/` | `dist/clojure/hydra-kernel/src/main/clojure/` |
| `hydra-lisp/hydra-scheme/src/main/scheme/hydra/` | `heads/lisp/scheme/src/main/scheme/hydra/` |
| `hydra-lisp/hydra-scheme/src/gen-main/scheme/` | `dist/scheme/hydra-kernel/src/main/scheme/` |
| (similar patterns for common-lisp and emacs-lisp) | |
| `src/gen-main/<lang>/` (within a package) | `dist/<lang>/hydra-kernel/src/main/<lang>/` |
| `src/gen-test/<lang>/` (within a package) | `dist/<lang>/hydra-kernel/src/test/<lang>/` |
