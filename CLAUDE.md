# LLM quickstart guide for Hydra

This document orients an LLM assistant (or human reader) to the Hydra project.
It provides just enough context to begin working, then links to detailed documentation.
Prefer consulting linked docs over relying on summaries here.

## What is Hydra?

Hydra is a functional programming language based on the LambdaGraph data model.
It explores an isomorphism between typed lambda calculus and labeled hypergraphs:
**programs are graphs, and graphs are programs.**

Hydra is self-hosting: the kernel is defined in Haskell-based DSLs and code-generated
into five complete implementations: Haskell, Java, Python, Scala, and Lisp.
The Lisp implementation covers four dialects: Clojure, Scheme, Common Lisp, and Emacs Lisp.
All five implementations pass the common test suite.
Version is tracked in the `VERSION` file at the repo root.

Key use cases: graph construction (TinkerPop, RDF, SHACL, GQL), data integration
(coders for Protobuf, Avro, JSON, YAML, GraphQL, PDL, CSV/TSV, RDF), and computational
graphs with deep support for polymorphism.

## The one rule

- **`src/main/`** -- Hand-written code. Edit freely.
- **`src/gen-main/`** and **`src/gen-test/`** -- Generated code. **Never manually edit**
  (unless doing a bootstrap patch, which must be overwritten by regeneration afterward).

Generated files have a header: "Note: this is an automatically generated file. Do not edit."

---

## Session procedures

### Startup

At the beginning of every new session, follow these steps **before doing any other work**:

1. **Identify the current branch**: Run `git branch --show-current`.

2. **Load or create the branch plan document**: Look for a Markdown file at the repo root
   named after the current branch (e.g., `main-plan.md`, `feature_249_java_version-plan.md`).
   - If it exists, read it to understand the current state of work.
   - If it does not exist, create it:
     - **Feature branches** (`feature_NNN_*`): Fetch the GitHub issue and draft a plan
       with goal, approach, and task checklist.
     - **Other branches**: Create a minimal plan summarizing purpose and in-progress work.
   - These plan documents are not checked in to Git.

3. **Discuss the plan with the user**: Present it, incorporate feedback, update the file.

4. **Consult task-specific references as needed** (see the [document index](#document-index)
   below and the [task routing](#task-routing) section).

### During the session

- **Save progress periodically**: Update the branch plan at milestones or approach changes.
- **Commit workflow**: Make frequent `WIP:` checkpoint commits when code is stable.
  All commit messages must be short (120 characters or less), single-line, no
  "Co-Authored-By:" line. When ready to finalize, ask the user about squashing:
  soft-reset WIP commits, re-commit as focused topic groups, source changes first,
  generated files last.

### Shutdown

Before ending a session:

1. **Update the branch plan**: Record completed work, current state, and any open questions.
2. **Flag documentation improvements**: If you discovered gaps or inaccuracies in docs,
   recipes, READMEs, or this file during the session, note them in the plan for follow-up.
3. **Notify the user**: Summarize what was accomplished and what remains.

---

## Project structure

```
hydra/
  hydra-haskell/    # Bootstrap implementation (Haskell). Source of truth.
  hydra-java/       # Complete Java implementation
  hydra-python/     # Complete Python implementation
  hydra-lisp/       # Complete Lisp implementation (Clojure, Scheme, Common Lisp, Emacs Lisp)
  hydra-ext/        # Code generators, coders, demos, tools (Haskell)
  hydra-scala/      # Complete Scala implementation
  hydra-rust/       # Early-stage Rust
  hydra-go/         # Early-stage Go
  hydra-javascript/  # Early-stage JavaScript
  docs/             # Documentation, recipes, guides
  wiki/             # Local checkout of the GitHub wiki (separate repo)
```

For detailed code organization, see the
[Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization) wiki page
and [docs/implementation.md](docs/implementation.md).

### Sync workflow

After modifying Haskell sources, regenerate downstream implementations in order:
**Haskell -> Ext -> Java, Python, Scala**. Use `./bin/sync-all.sh` (or `--quick`
to skip tests), or run individual `sync-*.sh` scripts from the appropriate directory.
See [code-generation.md](docs/recipes/code-generation.md) for details.

---

## Document index

### Key references

| Document | Description |
|----------|-------------|
| [docs/hydra-lexicon.txt](docs/hydra-lexicon.txt) | **Most important LLM reference.** All kernel types and ~180+ primitive signatures |
| [docs/implementation.md](docs/implementation.md) | Architecture deep-dive: kernel modules, DSL system, primitives, coders, bootstrap |
| [docs/dsl-guide.md](docs/dsl-guide.md) | Comprehensive Haskell DSL reference: 4 variants, operators, imports, patterns |
| [docs/dsl-guide-java.md](docs/dsl-guide-java.md) | Java DSL: `hydra.dsl.Types`, `hydra.dsl.Terms`, visitor pattern, Flow monad |
| [docs/dsl-guide-python.md](docs/dsl-guide-python.md) | Python DSL: `hydra.dsl.types`, `hydra.dsl.terms`, `FrozenDict`, reserved words |
| [docs/test-suite-architecture.md](docs/test-suite-architecture.md) | Common test suite structure, test case types, TestGraph, module organization |
| [docs/troubleshooting.md](docs/troubleshooting.md) | Debugging strategies, primitive dispatch tracing, common errors across languages |

### Recipes (step-by-step guides)

See [docs/recipes/index.md](docs/recipes/index.md) for the full list. Key recipes:

| Recipe | Description |
|--------|-------------|
| [Adding primitives](docs/recipes/adding-primitives.md) | Add primitives across all 5 implementations (6+ files each) |
| [Promoting code](docs/recipes/promoting-code.md) | Convert raw Haskell to Hydra DSL modules |
| [Extending Hydra Core](docs/recipes/extending-hydra-core.md) | Add type/term constructors (complex; involves the bootstrap problem) |
| [Extending tests](docs/recipes/extending-tests.md) | Add test cases to the common test suite |
| [Code generation](docs/recipes/code-generation.md) | End-to-end guide: DSL/JSON sources, writeXxx functions, sync scripts |
| [Refactoring](docs/recipes/refactoring.md) | Create, rename, move, delete kernel elements and modules |
| [Refactoring namespaces](docs/recipes/refactoring-namespaces.md) | Rename/move a Hydra namespace across all implementations |
| [JSON kernel](docs/recipes/json-kernel.md) | Export Hydra modules to JSON for cross-language access |
| [New implementation](docs/recipes/new-implementation.md) | Implement Hydra in a new language (10 steps) |
| [Syncing Python](docs/recipes/syncing-python.md) | Regenerate Python from Haskell |

### Implementation READMEs

Each has build/test commands and code organization details:

| README | Highlights |
|--------|------------|
| [hydra-haskell/README.md](hydra-haskell/README.md) | Stack build, GHCi REPL, code generation, DSL overview, self-hosting demo |
| [hydra-java/README.md](hydra-java/README.md) | Gradle build, visitor pattern, benchmark runner |
| [hydra-python/README.md](hydra-python/README.md) | uv setup, pytest, ruff, pyright |
| [hydra-ext/README.md](hydra-ext/README.md) | All coders with type mapping tables, sync scripts, demos |
| [hydra-scala/README.md](hydra-scala/README.md) | sbt build, bootstrapping host |
| [hydra-lisp/README.md](hydra-lisp/README.md) | Four Lisp dialects, shared coder, per-dialect test runners |

### Wiki pages

| Page | Description |
|------|-------------|
| [Coding style](https://github.com/CategoricalData/hydra/wiki/Coding-style) | Guiding principles, error handling, definition ordering, import conventions |
| [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) | Core concepts, type system (System F + HM), design principles |
| [Testing](https://github.com/CategoricalData/hydra/wiki/Testing) | Test suite, test runners, test categories |
| [Benchmarking](https://github.com/CategoricalData/hydra/wiki/Benchmarking) | Cross-implementation performance measurement |
| [Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization) | src/main vs src/gen-main per implementation |
| [Developers](https://github.com/CategoricalData/hydra/wiki/Developers) | Source code guide, release processes |
| [Release process](https://github.com/CategoricalData/hydra/wiki/Release-process) | Full release workflow, version file locations |
| [Property graphs](https://github.com/CategoricalData/hydra/wiki/Property-graphs) | Algebraic Property Graphs, mapping annotations |

### Other root-level files

| File | Description |
|------|-------------|
| [README.md](README.md) | Project overview and links |
| [CHANGELOG.md](CHANGELOG.md) | Version history, breaking changes |
| [docs/documentation-style-guide.md](docs/documentation-style-guide.md) | Writing conventions for Hydra docs |
| [docs/demos.md](docs/demos.md) | Overview of the four demos (GenPG, Bootstrapping, Avro-to-PG, Metered) |

---

## Task routing

Use this table to find the right doc for common tasks:

| Task | Start here |
|------|------------|
| Understand the kernel API | [docs/hydra-lexicon.txt](docs/hydra-lexicon.txt) |
| Write or read Haskell DSL code | [docs/dsl-guide.md](docs/dsl-guide.md) |
| Write or read Java DSL code | [docs/dsl-guide-java.md](docs/dsl-guide-java.md) |
| Write or read Python DSL code | [docs/dsl-guide-python.md](docs/dsl-guide-python.md) |
| Add a primitive function | [docs/recipes/adding-primitives.md](docs/recipes/adding-primitives.md) |
| Promote Haskell to DSL | [docs/recipes/promoting-code.md](docs/recipes/promoting-code.md) |
| Extend core types/terms | [docs/recipes/extending-hydra-core.md](docs/recipes/extending-hydra-core.md) |
| Add or modify tests | [docs/recipes/extending-tests.md](docs/recipes/extending-tests.md) |
| Regenerate code | [docs/recipes/code-generation.md](docs/recipes/code-generation.md) |
| Debug test failures | [docs/troubleshooting.md](docs/troubleshooting.md) |
| Understand architecture | [docs/implementation.md](docs/implementation.md) |
| Refactor modules/namespaces | [docs/recipes/refactoring.md](docs/recipes/refactoring.md) |

---

## Shorthand commands

The user may issue these shorthand commands prefixed with `!`:

| Command | Action |
|---------|--------|
| `!SYNC-ALL` | Run `bin/sync-all.sh --targets all` from the repo root, propagating changes into all generated artifacts. If it fails on any step, investigate and fix the issue, then re-run the failing step and all subsequent steps. Do not re-run steps that have already succeeded. |
| `!SYNC-HASKELL` | Run `hydra-haskell/bin/sync-haskell.sh` from the repo root. Same failure handling as `!SYNC-ALL`. |
| `!SYNC-EXT` | Run `hydra-ext/bin/sync-ext.sh` from the repo root. Same failure handling as `!SYNC-ALL`. |
| `!SYNC-JAVA` | Run `hydra-ext/bin/sync-java.sh` from the repo root. Same failure handling as `!SYNC-ALL`. |
| `!SYNC-PYTHON` | Run `hydra-ext/bin/sync-python.sh` from the repo root. Same failure handling as `!SYNC-ALL`. |
| `!SYNC-SCALA` | Run `hydra-ext/bin/sync-scala.sh` from the repo root. Same failure handling as `!SYNC-ALL`. |
| `!SYNC-LISP` | Run `hydra-ext/bin/sync-lisp.sh` from the repo root. Same failure handling as `!SYNC-ALL`. |

## Coding style (read the full guide!)

Before writing any Hydra code, read the
[Coding style](https://github.com/CategoricalData/hydra/wiki/Coding-style) wiki page.
Key rules:

- **Alphabetical ordering**: All definitions in the `elements` list and their
  corresponding function implementations must be in alphabetical order. No exceptions.
- **Fail immediately on errors**: Never silently return defaults or swallow failures.
- **Consistent imports**: Copy the import block from an existing module of the same kind.
  Use the standard aliases (`Lists`, `Maps`, `Core`, `Graph`, etc.).
- **No post-generation patches**: If generated code is wrong, fix the generator.

## Critical pitfalls

These are hard-won lessons. Read the linked docs for full context.

1. **Never edit generated files** (`src/gen-main/`, `src/gen-test/`) except for bootstrap
   patches that will be overwritten by regeneration.

2. **The bootstrap problem**: Extending core types creates a circular dependency.
   You must manually patch generated files, rebuild, then regenerate to overwrite patches.
   See [extending-hydra-core.md](docs/recipes/extending-hydra-core.md).

3. **Three DSL levels**: Term-level, meta-level (phantom-typed), and generated DSL.
   Mixing levels is a common source of errors. See [docs/dsl-guide.md](docs/dsl-guide.md).

4. **Haskell must pass first**: Always ensure `stack test` passes in `hydra-haskell`
   before syncing downstream implementations.

5. **Primitive registration**: A primitive class can exist but be invisible at runtime
   if it isn't registered in `Libraries.java` / `Libraries.hs` / `libraries.py` /
   `Libraries.scala` / `libraries.clj`. Always check registration when debugging
   "unknown primitive" errors.

6. **Primitive `implementation()` must not throw** (Java): Even higher-order (`prim2Eval`)
   primitives need a working `implementation()` that constructs term-level results.
   See [adding-primitives.md](docs/recipes/adding-primitives.md).

7. **Floating-point test portability**: Use `roundedPrimCase1` / `roundedPrimCase2` for
   transcendental math tests. See [extending-tests.md](docs/recipes/extending-tests.md).

8. **Memory for generation**: Use `stack ghci --ghci-options='+RTS -K256M -A32M -RTS'`
   or let the sync scripts handle it.

---

## Scope of user documentation

"User documentation" includes all of the following:
- READMEs: top-level, demo, and per-package/implementation READMEs
- Everything in the `docs/` directory that is checked in (ignore unstaged/temporary files)
- Everything in the wiki (separate Git repo; see [project structure](#project-structure))
- Code comments, to the extent they document public APIs or non-obvious behavior
  (you are not expected to scan all source files on every documentation review)

## Writing style for documentation

When writing or editing any user documentation, follow these conventions.
See [docs/documentation-style-guide.md](docs/documentation-style-guide.md) for the full guide.

- **Line length**: Keep lines under 120 characters.
  Split at sentence boundaries -- put each sentence on its own line.
  Break long sentences at natural clauses.
- **Headings**: Use sentence case (capitalize only the first word and proper nouns).
- **Links**: Use relative links for internal docs.
  Use absolute GitHub URLs for wiki pages.
- **Code formatting**: Use backticks for function names, type names, file names,
  module names, and language keywords.
  Use fenced code blocks with language specification.
- **Voice**: Active voice preferred.
  Second person ("you") for recipes and tutorials.
  Third person for reference documentation.

## Maintaining this file

This file is loaded into every Claude Code conversation, so its size directly impacts
context usage. Keep it lean by following these principles:

- **Link, don't duplicate.** If information exists in a README, recipe, wiki page, or guide,
  link to it with a one-line summary. Do not copy the content here.
- **Prioritize what's unique to Claude.** Session procedures, the task routing table, and
  critical pitfalls belong here. Build commands, DSL syntax, and architecture details belong
  in their respective docs.
- **Add to other docs first.** When new guidance is needed, put it in the most specific
  applicable document (a recipe, a README, the troubleshooting guide, etc.) and add a link
  here if the topic is common enough to warrant one.
- **Keep the document index flat.** One line per document with a brief description. If a
  description needs more than ~15 words, the detail belongs in the linked document.
- **Review before expanding.** Before adding content, check whether it's already reachable
  via an existing link. Two hops (CLAUDE.md -> doc -> section) is acceptable.
