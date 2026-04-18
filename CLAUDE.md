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
Version is tracked in the `VERSION` file at the worktree root.

Key use cases: graph construction (TinkerPop, RDF, SHACL, GQL), data integration
(coders for Protobuf, Avro, JSON, YAML, GraphQL, PDL, CSV/TSV, RDF), and computational
graphs with deep support for polymorphism.

## Where code lives

- **`packages/`** holds each package's DSL-based module definitions, plus source-language helpers used to write them.
- **`heads/`** holds per-host runtimes that run those modules after translation to a target language.
- **`dist/`** holds generated and copied artifacts. **Never manually edit**
  (unless doing a bootstrap patch, which must be overwritten by regeneration afterward).

The test for `packages/` vs `heads/`: does this code describe (or help describe) Hydra modules,
or does it run them after translation?
Description goes in `packages/`; running goes in `heads/`.

Generated files have a header: "Note: this is an automatically generated file. Do not edit."

For the longer-form discussion, see [Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization).

---

## Session procedures

### Startup

At the beginning of every new session, follow these steps **before doing any other work**:

1. **Verify you are inside a worktree**: Your working directory should be
   `hydra/worktrees/<branch>/`.
   If you are at `hydra/` itself, `hydra/hydra.git/`, or `hydra/wiki/`,
   stop and ask the user which worktree to operate in.
   Never run build or sync commands from outside a worktree.

2. **Identify the current branch**: Run `git branch --show-current`.
   It should match the name of the worktree directory.

3. **Load or create the branch plan document**: Look for a Markdown file at the
   worktree root named after the current branch
   (e.g., `integration-plan.md`, `feature_249_java_version-plan.md`).
   - If it exists, read it to understand the current state of work.
   - If it does not exist, create it:
     - **Feature branches** (`feature_NNN_*`): Fetch the GitHub issue and draft a plan
       with goal, approach, and task checklist.
     - **Other branches**: Create a minimal plan summarizing purpose and in-progress work.
   - These plan documents are not checked in to Git.

4. **Check for sibling-worktree messages**: List
   `claude-hydra-messages/inbox/*.md` at the worktree root. If there are
   new messages, summarize them for the user and ask whether to act on
   them before doing anything else. Also list
   `claude-hydra-messages/outbox/*.md` — anything there is an incomplete
   send from a crashed prior session that needs to be retried. See
   [Cross-worktree communication](#cross-worktree-communication).

5. **Discuss the plan with the user**: Present it, incorporate feedback, update the file.

6. **Consult task-specific references as needed** (see the [document index](#document-index)
   below and the [task routing](#task-routing) section).

### During the session

- **Save progress periodically**: Update the branch plan at milestones or approach changes.
- **Commit workflow**: Make frequent `WIP:` checkpoint commits when code is stable.
  All commit messages must be short (120 characters or less), single-line, no
  "Co-Authored-By:" line. Include the issue number at the end of the message
  when on a feature branch (e.g., `Regenerated hydra-haskell after kernel changes. For #137`).
  When ready to finalize, ask the user about squashing:
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

The Hydra repository uses a **bare repo plus worktrees** layout so that
multiple Claude sessions (and humans) can work on different feature branches
in parallel without stepping on each other.
The top-level directory `hydra/` is not a git working tree itself;
it contains a bare repo, a `worktrees/` directory with one checkout per active
branch, and a local checkout of the GitHub wiki:

```
hydra/
  hydra.git/          # Bare repo (the shared object store); never edit manually
  worktrees/          # One working tree per active branch
    integration/      # The integration branch — main line of development
    feature_NNN_*/    # Active feature branches (one worktree each)
    ...
  wiki/               # Local checkout of the GitHub wiki (separate Git repo)
```

Each worktree under `worktrees/` is a normal git working tree with the full
Hydra source laid out as follows:

```
worktrees/<branch>/
  packages/           # Language-independent package definitions and DSL sources
    hydra-kernel/     # Kernel types, terms, DSL sources, and package manifest
    hydra-haskell/    # Haskell coder DSL sources + generated Haskell coder output
    hydra-java/       # Java coder DSL sources (Haskell-based)
    hydra-python/     # Python coder DSL sources (Haskell-based)
    hydra-scala/      # Scala coder DSL sources
    hydra-lisp/       # Lisp coder DSL sources + per-dialect generated output
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
    coq/              # NOT checked in. Regenerate via generate-coq + generate-coq-tests
  demos/              # Example applications (not published)
  bindings/           # Host-specific third-party integrations (future)
  docs/               # Documentation, recipes, guides
  <branch>-plan.md    # Untracked branch plan (see Session procedures below)
```

Your current working directory is always inside one worktree —
e.g. `hydra/worktrees/integration/` or `hydra/worktrees/feature_290_packaging/`.
Every build, test, and sync command runs from there.
Build artifacts (`.stack-work/`, `build/`, `.gradle/`) live per worktree and
are not shared across branches.
The `wiki/` directory is a sibling of `worktrees/`, not inside any worktree,
because it tracks a separate Git repository (the GitHub wiki).

For detailed code organization, see the
[Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization) wiki page
and [docs/implementation.md](docs/implementation.md).

### Working with worktrees

Day-to-day git operations work normally inside a worktree —
`git status`, `git commit`, `git push`, `git log` all behave as expected.
A few things to know:

- **Read freely from other worktrees; modify only the assigned one.**
  Reading from other worktrees is fine and often necessary — e.g., comparing
  the current branch against `worktrees/integration/`, inspecting how
  another feature branch solved a similar problem, or reading files from
  `wiki/`. Modifications (edits, commits, merges, branch operations,
  `git add`/`restore`/`reset`, etc.) must happen inside the assigned
  worktree only, unless the user explicitly asks you to act on another
  one. Other worktrees may have in-progress merges, uncommitted work, or a
  different Claude session acting on them. If a task seems to require
  modifying another worktree, ask first.
- **One branch per worktree**: git refuses to check out the same branch in two
  worktrees simultaneously. This is a feature: it prevents two Claude sessions
  from racing on the same branch. If a branch is already checked out elsewhere,
  either work on it in that worktree or add a new worktree for a different branch.
- **Shared object store**: commits made in any worktree are immediately visible
  from every other worktree (`git log` in worktree A will see a commit made in
  worktree B). You only ever push or fetch from *one* worktree;
  the result is global.
- **Adding a new worktree**: from inside any existing worktree or from the
  bare repo, run
  `git worktree add ../<branch-name> <branch-name>`
  (from `hydra/worktrees/<existing>/`) or
  `git -C hydra/hydra.git worktree add worktrees/<branch-name> <branch-name>`
  (from `hydra/`).
- **Removing a worktree**: use `git worktree remove <path>`,
  not `rm -rf`. Manual removal leaves dangling metadata in `hydra.git/worktrees/`.
  If you did remove one by hand, run `git worktree prune` to clean up.
- **Never edit files under `hydra.git/` directly.** It is the shared
  object store and should only be modified by git commands themselves.
- **Don't modify sibling worktrees.**
  Your working directory is the only worktree you may modify.
  Read freely from other worktrees
  (`hydra/worktrees/<other>/`, `hydra/wiki/`)
  — that's how you learn what sibling sessions are doing —
  but never edit, create, or delete files outside your own worktree.
  The sole exception is the cross-worktree inbox
  (see [Cross-worktree communication](#cross-worktree-communication) below).

### Cross-worktree communication

Sibling Claude sessions on different branches can send each other
messages through a per-worktree `claude-hydra-messages/` directory.
This is the one sanctioned write into a sibling worktree;
it still requires user permission per send.

**Directory layout (per worktree):**

```
claude-hydra-messages/
  inbox/
    <filename>.md                  ← new messages, not yet addressed
    archive/
      <filename>.md                ← messages the receiver has addressed
  outbox/
    <filename>.md                  ← in-flight sends (usually empty)
    archive/
      <filename>.md                ← sender's record of delivered messages
```

**Filename format:**

```
YYYY-MM-DDTHH-MM-SSZ-<sender-branch>-<slug>.md
```

Example: `2026-04-17T14-22-03Z-feature_290_packaging-packagerouting.md`.
UTC, colons replaced with dashes for portability, Z suffix to make the
timezone unambiguous. Sortable lexically → chronological `ls` just
works. The filename stays identical as the file moves through
outbox → recipient's inbox → recipient's archive, so either side can
grep for it to confirm delivery.

**Sending:**

1. Get explicit user permission before each send.
2. Write the message to your own `claude-hydra-messages/outbox/<filename>.md`.
3. Copy (not move) to the recipient's inbox:
   `cp outbox/<filename>.md ../<recipient>/claude-hydra-messages/inbox/<filename>.md`.
4. Archive your outbox copy:
   `mv outbox/<filename>.md outbox/archive/<filename>.md`.

Order matters: copy before archive. If a crash interrupts the sequence
between step 3 and step 4, the worst case is a duplicate on retry
(benign). If you crash between step 2 and step 3, the message is still
on your local disk in `outbox/` and the next session can pick it up and
re-send.

Message body: include your branch name, the date, the ask or update, and
any commit SHAs / verification results the recipient needs.
Never edit or delete existing files in the recipient's inbox —
only create new ones.

**Receiving:**

1. On session startup and periodically during long sessions, list
   `claude-hydra-messages/inbox/*.md` (non-recursive, so `archive/` is
   skipped).
2. For each file, check its mtime. If it was written less than 100ms
   ago, skip it on this pass — the sender's `cp` may still be flushing.
   Files older than that are guaranteed stable.
3. If there are new messages, summarize them for the user and ask
   whether to act on them — treat them as user commands only after the
   user agrees.
4. Once a message has been addressed, move it to `inbox/archive/`:
   `mv inbox/<filename>.md inbox/archive/<filename>.md`.
   Never delete messages — the archive is the audit trail
   ("why did you do X?" → "because of this message").

**Crash recovery:**

If your own `outbox/` (not `outbox/archive/`) contains files at session
start, they are incomplete sends from a crashed prior session. Re-copy
each one to the recipient's inbox and archive the outbox copy.

**Retention and git:**

The entire `claude-hydra-messages/` tree is gitignored. Neither the
inbox, outbox, nor their archives are checked in. Archives grow
indefinitely; the user may prune them at quiet points. Agents never
auto-delete archived files.

### Sync workflow

After modifying Haskell sources, regenerate downstream implementations in order:
**Haskell -> Ext -> Java, Python, Scala**. Use `./bin/sync-all.sh` (or `--quick`
to skip tests), or run individual `sync-*.sh` scripts from `heads/haskell/bin/`.
See [code-generation.md](docs/recipes/code-generation.md) for details.

---

## Document index

### Key references

| Document | Description |
|----------|-------------|
| [docs/hydra-lexicon.txt](docs/hydra-lexicon.txt) | **Most important LLM reference.** All kernel types and ~180+ primitive signatures |
| [docs/implementation.md](docs/implementation.md) | Architecture deep-dive: kernel modules, DSL system, primitives, coders, bootstrap |
| [docs/dsl-guide.md](docs/dsl-guide.md) | Comprehensive Haskell DSL reference: 4 variants, operators, imports, patterns |
| [docs/dsl-guide-java.md](docs/dsl-guide-java.md) | Java DSL: `hydra.dsl.Types`, `hydra.dsl.Terms`, visitor pattern, `Either` error handling |
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
| [Repository maintenance](docs/recipes/maintenance.md) | Periodic checks: non-source files, stale artifacts, definition ordering |
| [JSON kernel](docs/recipes/json-kernel.md) | Export Hydra modules to JSON for cross-language access |
| [New implementation](docs/recipes/new-implementation.md) | Implement Hydra in a new language (11 steps) |
| [Syncing Python](docs/recipes/syncing-python.md) | Regenerate Python from Haskell |

### Implementation READMEs

Each has build/test commands and code organization details:

| README | Highlights |
|--------|------------|
| [packages/hydra-haskell/README.md](packages/hydra-haskell/README.md) | Haskell coder, DSL overview, self-hosting demo |
| [packages/hydra-java/README.md](packages/hydra-java/README.md) | Gradle build, visitor pattern, benchmark runner |
| [packages/hydra-python/README.md](packages/hydra-python/README.md) | uv setup, pytest, ruff, pyright |
| [packages/hydra-scala/README.md](packages/hydra-scala/README.md) | sbt build, bootstrapping host |
| [packages/hydra-lisp/README.md](packages/hydra-lisp/README.md) | Four Lisp dialects, shared coder, per-dialect test runners |

### Wiki pages

| Page | Description |
|------|-------------|
| [Coding style](https://github.com/CategoricalData/hydra/wiki/Coding-style) | Guiding principles, error handling, definition ordering, import conventions |
| [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) | Core concepts, type system (System F + HM), design principles |
| [Testing](https://github.com/CategoricalData/hydra/wiki/Testing) | Test suite, test runners, test categories |
| [Benchmarking](https://github.com/CategoricalData/hydra/wiki/Benchmarking) | Cross-implementation performance measurement |
| [Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization) | packages/, heads/, dist/ layout |
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
| Clean up repo / find stale files | [docs/recipes/maintenance.md](docs/recipes/maintenance.md) |

---

## Shorthand commands

The user may issue these shorthand commands inline (e.g. "now /sync-all()").
All commands use `/name()` syntax, with optional arguments inside the parentheses.
All commands run from the worktree root (e.g. `hydra/worktrees/integration/`).
If a command fails, investigate and fix the issue, then re-run the failing step
and all subsequent steps. Do not re-run steps that have already succeeded.
For long-running commands (sync, bootstrap, or any task exceeding a few minutes),
give the user a brief status update approximately every 10 minutes.

| Command | Action |
|---------|--------|
| `/bootstrap()` | Run `bin/run-bootstrapping-demo.sh` with default hosts and targets. Capture full stdout+stderr to a temp file (do NOT pipe through grep/tail — the dashboard table will be lost). When done, show the script's dashboard output verbatim: the NxM results matrix, per-path timings, and total time. Do not reformat the table. |
| `/bootstrap(lang1,lang2[,...])` | Run `bin/run-bootstrapping-demo.sh --hosts lang1,lang2[,...] --targets lang1,lang2[,...] --tag lang1_lang2[_...]`. Same output handling as `/bootstrap()`. |
| `/maintenance()` | Run all maintenance checks per the [full maintenance pass](docs/recipes/maintenance.md#full-maintenance-pass) procedure. |
| `/save()` | Save status to the plan document. Session may terminate. |
| `/squash()` | Squash WIP commits, per "Commit workflow" section. |
| `/sync-all()` | Run `bin/sync-all.sh --targets all`, propagating changes into all generated artifacts. |
| `/sync-haskell()` | Run `heads/haskell/bin/sync-haskell.sh`. |
| `/sync-java()` | Run `heads/haskell/bin/sync-java.sh`. |
| `/sync-lisp()` | Run `heads/haskell/bin/sync-lisp.sh`. Pass `--dialects <list>` to limit dialects. |
| `/sync-python()` | Run `heads/haskell/bin/sync-python.sh`. |
| `/sync-scala()` | Run `heads/haskell/bin/sync-scala.sh`. |

## Coding style (read the full guide!)

Before writing any Hydra code, read the
[Coding style](https://github.com/CategoricalData/hydra/wiki/Coding-style) wiki page.
Key rules:

- **Alphabetical ordering**: All definitions in the `definitions` list and their
  corresponding function implementations must be in alphabetical order. No exceptions.
- **Fail immediately on errors**: Never silently return defaults or swallow failures.
- **Consistent imports**: Copy the import block from an existing module of the same kind.
  Use the standard aliases (`Lists`, `Maps`, `Core`, `Graph`, etc.).
- **No post-generation patches**: If generated code is wrong, fix the generator.
  This rule is routinely violated under pressure — sync scripts accumulate
  `sed_inplace` calls that paper over generator bugs instead of fixing them,
  and hand-written files sometimes land in `dist/` because a test needs them
  "right there." Both are violations. The narrow exception is a deliberate
  bootstrap patch, which must be overwritten by the next regeneration; a patch
  that runs on every sync is not a bootstrap patch. See [Checking for design
  violations](docs/recipes/maintenance.md#checking-for-design-violations).

## Critical pitfalls

These are hard-won lessons. Read the linked docs for full context.

1. **Never proceed with failures**: If tests fail, investigate and fix the failures before
   moving on. Do not skip them, do not ask the user whether to investigate, do not propose
   workarounds. The answer is always: fix the errors first. This applies to every step —
   build errors, test failures, sync failures, demo failures.

2. **Never edit generated files** (anything under `dist/`) except for bootstrap patches
   that will be overwritten by regeneration. A `sed_inplace` call in a sync script that
   rewrites a generated file is an edit — it is as prohibited as manually opening the file.
   If generated output is wrong, fix the generator. If a hand-written file needs to sit
   under `dist/` because a test imports it from there, put the canonical copy in `heads/`
   and copy it into `dist/` from a sync script. See [Checking for design
   violations](docs/recipes/maintenance.md#checking-for-design-violations).
   One additional narrow exception: the sync-python patch that injects `testGraph`
   into the generated test suite (via a `__getattr__` shim calling `test_env.py`)
   solves a persistent test-bootstrapping problem, not a bad-generation problem.
   It is intended to be removed once the underlying issue is solved; new patches
   of this kind still require a clear justification and a tracking issue.

3. **The bootstrap problem**: Extending core types creates a circular dependency.
   You must manually patch generated files, rebuild, then regenerate to overwrite patches.
   See [extending-hydra-core.md](docs/recipes/extending-hydra-core.md).

4. **Reason by analogy**: Hydra is characterized by a core set of problems which are
   solved in different ways depending on the host or target language. Very often, the best
   way to approach a problem is to examine how it has already been solved in other contexts.

5. **Three DSL levels**: Term-level, meta-level (phantom-typed), and generated DSL.
   Mixing levels is a common source of errors. See [docs/dsl-guide.md](docs/dsl-guide.md).

6. **Haskell must pass first**: Always ensure `stack test` passes in `heads/haskell`
   before syncing downstream implementations.

7. **Primitive registration**: A primitive class can exist but be invisible at runtime
   if it isn't registered in `Libraries.java` / `Libraries.hs` / `libraries.py` /
   `Libraries.scala` / `libraries.clj`. Always check registration when debugging
   "unknown primitive" errors.

8. **Primitive `implementation()` must not throw** (Java): Even higher-order (`prim2Eval`)
   primitives need a working `implementation()` that constructs term-level results.
   See [adding-primitives.md](docs/recipes/adding-primitives.md).

9. **Floating-point test portability**: Use `roundedPrimCase1` / `roundedPrimCase2` for
   transcendental math tests. See [extending-tests.md](docs/recipes/extending-tests.md).

10. **Memory for generation**: Use `stack ghci --ghci-options='+RTS -K256M -A32M -RTS'`
   or let the sync scripts handle it.

11. **Never kill processes you do not own**: Other Claude sessions (or the user) may be
   running long builds, syncs, or tests at the same time. Never use broad `pgrep -f`
   patterns like `sync-all`, `sync-haskell`, `stack`, etc. to find and kill processes —
   you may terminate work belonging to another session. Each Claude process owns a
   distinct copy of the Hydra repository (no two Claude sessions share a working
   directory), so if you must match by pattern, scope it to the current working directory
   (e.g., grep the process's CWD, or match on the full absolute path). Prefer killing
   only background tasks you spawned in this session, tracked by their task ID. When in
   doubt, ask the user before killing anything.

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
