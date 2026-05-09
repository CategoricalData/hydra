# LLM quickstart guide for Hydra

This document orients an LLM assistant (or human reader) to the Hydra project.
It provides just enough context to begin working, then links to detailed documentation.
Prefer consulting linked docs over relying on summaries here.

## What is Hydra?

Hydra is a functional programming language based on the LambdaGraph data model.
It explores an isomorphism between typed lambda calculus and labeled hypergraphs:
**programs are graphs, and graphs are programs.**

Hydra is self-hosting: the kernel is defined in Haskell-based DSLs and code-generated
into eight host languages spanning five implementation families:
Haskell, Java, Python, Scala, and Lisp (Clojure, Common Lisp, Emacs Lisp, Scheme — sharing one coder).
All eight pass the common test suite.

A ninth target, Go (`hydra-go`, `heads/go/`), is a "head bud" — the kernel can be generated
to Go via `bin/sync-go.sh`, but the Go coder still has emission bugs and the head's
hand-written runtime is mostly placeholder. Go is not yet a complete implementation
and does not (yet) host the test suite.

Key use cases: graph construction (TinkerPop, RDF, SHACL, GQL), data integration
(coders for Protobuf, Avro, JSON, YAML, GraphQL, PDL, CSV/TSV, RDF), and computational
graphs with deep support for polymorphism.

## Where code lives

- **`packages/`** holds each package's DSL-based module definitions, plus source-language helpers used to write them.
- **`heads/`** holds per-host runtimes that run those modules after translation to a target language.
- **`dist/`** holds generated and copied artifacts. **Never manually edit**
  (unless doing a bootstrap patch, which must be overwritten by regeneration afterward).
- **`bindings/`** holds host-specific third-party integrations — adapters that wire Hydra
  packages to external libraries (e.g., `bindings/java/hydra-rdf4j/` connects `hydra-rdf` to
  rdf4j; `bindings/java/hydra-neo4j/` provides Cypher and GQL parsers via ANTLR).
  Each binding is a separately publishable artifact in its host language and is **not**
  part of the DSL-driven sync pipeline.

The test for `packages/` vs `heads/`: does this code describe (or help describe) Hydra modules,
or does it run them after translation?
Description goes in `packages/`; running goes in `heads/`.
Bindings sit outside both: hand-written adapters between Hydra and an external system.

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
   (e.g., `staging-plan.md`, `feature_249_java_version-plan.md`).
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

6. **Consult task-specific references as needed** (see [Where to look up X](#where-to-look-up-x) below).

### During the session

- **Save progress periodically**: Update the branch plan at milestones or approach changes.
- **Commit workflow**: Make frequent `WIP:` checkpoint commits when code is stable.
  All commit messages must be short (120 characters or less), single-line, no
  "Co-Authored-By:" line. Include the issue number at the end of the message
  when on a feature branch (e.g., `Regenerated hydra-haskell after kernel changes. For #137`).
  When ready to finalize, ask the user about squashing:
  soft-reset WIP commits, re-commit as focused topic groups, source changes first,
  generated files last.

### When waiting on long-running work

While a sync, bootstrap, or other long task is running, default to *waiting* —
schedule periodic check-ins (`/loop` or self-paced wakeups) rather than
spinning up unrelated work that may interfere with the running task.
Don't proactively run more sync/bootstrap commands; don't kick off other commits.
The user will redirect you if they want something else done in the meantime.

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
Top level: `hydra/hydra.git/` (bare repo), `hydra/worktrees/<branch>/` (one per active branch),
`hydra/wiki/` (separate Git repo for the GitHub wiki).
Your working directory is always one worktree.
Build artifacts (`.stack-work/`, `build/`, `.gradle/`) live per worktree and are not shared.

For the full subdirectory layout, see [claude/project-structure.md](claude/project-structure.md).
For human-facing architecture, see the
[Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization) wiki page
and [docs/implementation.md](docs/implementation.md).

### Working with worktrees

Two hard rules:

- **Read freely from other worktrees; modify only your assigned one.**
  Reading sibling worktrees and `wiki/` is fine.
  Edits, commits, branch operations, and `git add`/`restore`/`reset` happen
  only inside your assigned worktree. The sole exception is the
  cross-worktree inbox (see [claude/cross-worktree-messages.md](claude/cross-worktree-messages.md)).
- **Never edit files under `hydra.git/` directly.** It is the shared object
  store; only git commands modify it.

For git-worktree mechanics (adding, removing, the shared object store, cherry-picks
between worktrees), see [claude/worktree-workflow.md](claude/worktree-workflow.md).

### Cross-worktree communication

Sibling Claude sessions can message each other through a per-worktree
`claude-hydra-messages/` directory (gitignored).
At session startup, list `claude-hydra-messages/inbox/*.md` and
`claude-hydra-messages/outbox/*.md` — anything in outbox is an incomplete
send from a crashed prior session.
Get explicit user permission before each send.

For the full protocol (filename format, send/receive sequence, crash recovery),
see [claude/cross-worktree-messages.md](claude/cross-worktree-messages.md).

### Sync workflow

After modifying Haskell sources, regenerate downstream implementations.
Use `./bin/sync.sh --hosts <H1,H2,...> --targets <T1,T2,...>` (or
`--no-tests` to skip target-language tests).
For the haskell/java/python bootstrapping triad, `./bin/sync-default.sh` is shorthand.
Per-language wrappers (`bin/sync-java.sh`, `bin/sync-python.sh`, etc.) cover host == target.
See [docs/recipes/code-generation.md](docs/recipes/code-generation.md)
and [docs/troubleshooting.md](docs/troubleshooting.md) (for stale-dist and cache-hit issues).

---

## Where to look up X

Primary entry point — the doc most likely to answer the question by task:

| Task | Start here |
|------|------------|
| Understand the kernel API | [docs/hydra-lexicon.txt](docs/hydra-lexicon.txt) — **most important LLM reference**, all kernel types + ~180 primitive signatures |
| Understand architecture | [docs/implementation.md](docs/implementation.md) |
| Write Haskell / Java / Python DSL code | [docs/dsl-guide.md](docs/dsl-guide.md) / [-java.md](docs/dsl-guide-java.md) / [-python.md](docs/dsl-guide-python.md) |
| Add a primitive | [docs/recipes/adding-primitives.md](docs/recipes/adding-primitives.md) |
| Promote Haskell to DSL | [docs/recipes/promoting-code.md](docs/recipes/promoting-code.md) |
| Extend core types/terms | [docs/recipes/extending-hydra-core.md](docs/recipes/extending-hydra-core.md) |
| Add or modify tests | [docs/recipes/extending-tests.md](docs/recipes/extending-tests.md) |
| Regenerate code | [docs/recipes/code-generation.md](docs/recipes/code-generation.md) |
| Debug test failures | [docs/troubleshooting.md](docs/troubleshooting.md) |
| Refactor modules/namespaces | [docs/recipes/refactoring.md](docs/recipes/refactoring.md) / [refactoring-namespaces.md](docs/recipes/refactoring-namespaces.md) |
| Clean up repo / find stale files | [docs/recipes/maintenance.md](docs/recipes/maintenance.md) |
| New target language | [docs/recipes/new-implementation.md](docs/recipes/new-implementation.md) |
| Test suite structure | [docs/test-suite-architecture.md](docs/test-suite-architecture.md) |
| Coding style + import conventions | [Coding style wiki](https://github.com/CategoricalData/hydra/wiki/Coding-style) |
| Build/test commands per language | per-package READMEs under `packages/hydra-<lang>/README.md` |
| Concepts (System F, design) | [Concepts wiki](https://github.com/CategoricalData/hydra/wiki/Concepts) |
| Property graphs | [Property graphs wiki](https://github.com/CategoricalData/hydra/wiki/Property-graphs) / [hydra-pg README](packages/hydra-pg/README.md) |
| RDF / SHACL | [RDF wiki](https://github.com/CategoricalData/hydra/wiki/RDF) / [hydra-rdf README](packages/hydra-rdf/README.md) |
| Release process | [Release process wiki](https://github.com/CategoricalData/hydra/wiki/Release-process) |
| Demos | [docs/demos.md](docs/demos.md) |
| Recipes index (full list) | [docs/recipes/index.md](docs/recipes/index.md) |
| Wiki index | [github.com/CategoricalData/hydra/wiki](https://github.com/CategoricalData/hydra/wiki) |

---

## Shorthand commands

These commands are **user-invoked only**.
Do not run them on your own initiative; wait for the user to ask.
The user may issue them inline (e.g. "now /sync()").
All commands use `/name()` syntax, with optional arguments inside the parentheses.
All commands run from the worktree root (e.g. `hydra/worktrees/staging/`).
If a command fails, investigate and fix the issue, then re-run the failing step
and all subsequent steps. Do not re-run steps that have already succeeded.
For long-running commands (sync, bootstrap, or any task exceeding a few minutes),
give the user a brief status update approximately every 10 minutes.

| Command | Action |
|---------|--------|
| `/bootstrap()` | Run `bin/run-bootstrapping-demo.sh` with default hosts and targets (`haskell,java,python` × `haskell,java,python` — the bootstrapping triad, NOT the full matrix). Capture full stdout+stderr to a temp file (do NOT pipe through grep/tail — the dashboard table will be lost). When done, show the script's dashboard output verbatim: the NxM results matrix, per-path timings, and total time. Do not reformat the table. |
| `/bootstrap(lang1,lang2[,...])` | Run `bin/run-bootstrapping-demo.sh --hosts lang1,lang2[,...] --targets lang1,lang2[,...] --tag lang1_lang2[_...]`. Same output handling as `/bootstrap()`. |
| `/bootstrap(all)` | Run `bin/run-bootstrapping-demo.sh --hosts all --targets all --tag all`. Full all-hosts × all-targets matrix; long-running (an hour or more) and typically reserved for pre-release verification or overnight runs. |
| `/bootstrap(others)` | shorthand for `bin/run-bootstrapping-demo.sh --hosts scala,lisp --targets python`. This is a narrow bootstrapping pass which simply checks whether the "other" hosts can bootstrap Python. |
| `/improve-docs()` | Usually called at the end of a session. If there is anything you have learned during the session which would improve CLAUDE.md, the `claude/` Claude-specific notes, or user documentation, then edit the docs, keeping them concise. Communicate with the user about major changes like splitting one document up into two. |
| `/lexicon()` | Run `bin/regenerate-lexicon.sh` to refresh `docs/hydra-lexicon.txt` from the current Haskell kernel. Run on demand and as part of the pre-release flow; not part of regular sync. |
| `/maintenance()` | Run all maintenance checks per the [full maintenance pass](docs/recipes/maintenance.md#full-maintenance-pass) procedure. |
| `/save()` | Save status to the plan document. Session may terminate. |
| `/squash()` | Squash WIP commits, per "Commit workflow" section. |
| `/sync()` | Run `bin/sync.sh --hosts all --targets all`. Full all-hosts × all-targets matrix (note: opposite of `/bootstrap()` default — `/sync()` defaults to "all", `/bootstrap()` defaults to the triad). |
| `/sync(lang1,lang2[,...])` | Run `bin/sync.sh --hosts <list> --targets <list>` with the same languages on both sides. |
| `/sync-default()` | Run `bin/sync-default.sh` (the haskell,java,python triad — equivalent to `/bootstrap()`'s default scope). |
| `/sync-haskell()` | Run `heads/haskell/bin/sync-haskell.sh` (Phase 1 only: DSL → JSON + Haskell kernel + stack test). The lexicon is no longer regenerated as part of sync; use `/lexicon()` to refresh it. |
| `/sync-java()` | Run `bin/sync-java.sh` (--hosts java --targets java). |
| `/sync-python()` | Run `bin/sync-python.sh`. |
| `/sync-scala()` | Run `bin/sync-scala.sh`. |
| `/sync-go()` | Run `bin/sync-go.sh` — generates kernel into `dist/go/`. Go is a "head bud"; only `hydra-kernel` is targeted (no `hydra-pg`/`hydra-rdf`), and Phase 4 host=go rows are skipped. |
| `/sync-clojure()` etc. | Run `bin/sync-<dialect>.sh` for clojure / common-lisp / emacs-lisp / scheme. |

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

## Hard rules

These are non-negotiable and tend to be violated under pressure.

1. **Never proceed with failures.** If tests fail, investigate and fix before moving on.
   Don't skip, don't ask whether to investigate, don't propose workarounds.
   The answer is always: fix the errors first.
2. **Never edit generated files** (anything under `dist/`) except for deliberate
   bootstrap patches that the next regeneration will overwrite.
   A `sed_inplace` call in a sync script that rewrites a generated file is an edit —
   as prohibited as manually opening the file.
   If generated output is wrong, fix the generator.
   See [Checking for design violations](docs/recipes/maintenance.md#checking-for-design-violations).
3. **No post-generation patches.** If a hand-written file needs to sit under `dist/`
   because a test imports it from there, put the canonical copy in `heads/`
   and copy it into `dist/` from a sync script.
4. **Haskell must pass first.** Always ensure `stack test` passes in `heads/haskell`
   before syncing downstream implementations.
5. **Never kill processes you do not own.** Other Claude sessions (or the user)
   may be running long builds, syncs, or tests in parallel.
   Never use broad `pgrep -f` patterns like `sync-all`, `sync-haskell`, `stack`, etc.
   to find and kill processes —
   you may terminate work belonging to another session.
   Scope by CWD or absolute path; prefer killing only background tasks you spawned
   in this session, tracked by their task ID.
   When in doubt, ask first.

## Mental models

Useful framing when approaching a Hydra problem.

- **Three DSL levels.** Term-level, meta-level (phantom-typed), and generated DSL.
  Mixing levels is a common source of errors.
  See [docs/dsl-guide.md](docs/dsl-guide.md).
- **Reason by analogy.** Hydra is characterized by a core set of problems solved
  differently across host and target languages.
  Often the best way forward is to look at how the same problem was solved elsewhere.
- **The bootstrap problem.** Extending core types creates a circular dependency:
  manually patch generated files, rebuild, then regenerate to overwrite patches.
  See [docs/recipes/extending-hydra-core.md](docs/recipes/extending-hydra-core.md).

## Specific gotchas

For known one-off issues (primitive registration, FP test portability, bash heredoc
hangs, `Too many open files`, stale-dist regen, `struct-compat.lisp` regen, etc.)
see [claude/pitfalls.md](claude/pitfalls.md).

---

## Scope of user documentation

"User documentation" includes all of the following:
- READMEs: top-level, demo, and per-package/implementation READMEs
- Everything in the `docs/` directory that is checked in (ignore unstaged/temporary files)
- Everything in the wiki (separate Git repo; see [project structure](#project-structure))
- Code comments, to the extent they document public APIs or non-obvious behavior
  (you are not expected to scan all source files on every documentation review)

## Writing style for documentation

When writing or editing any user documentation, follow
[docs/documentation-style-guide.md](docs/documentation-style-guide.md).
Quick reminders: lines < 120 chars (one sentence per line), sentence-case headings,
relative links for internal docs, backticks for code, active voice.

## Maintaining this file

This file is loaded into every Claude Code conversation, so its size directly impacts
context usage. Keep it lean by following these principles:

- **Link, don't duplicate.** If information exists in a README, recipe, wiki page,
  or `claude/` doc, link to it with a one-line summary. Don't copy the content here.
- **Prioritize what's unique to orientation.** Session procedures, the
  "Where to look up X" table, hard rules, and mental models belong here.
  Build commands, DSL syntax, architecture details, full protocols (e.g. messaging),
  and specific gotchas belong in their respective docs.
- **Three-tier separation.**
  - **CLAUDE.md** (this file): orientation + entry points, always loaded.
  - **`claude/`**: deeper Claude-specific protocols and gotchas, loaded on demand.
  - **`docs/`** + wiki + READMEs: human-facing documentation. Don't mix Claude-specific
    content into `docs/`.
- **Add to other docs first.** When new guidance is needed, put it in the most specific
  applicable document and add a link here only if the topic is common enough.
- **Review before expanding.** Before adding content, check whether it's already reachable
  via an existing link. Two hops (CLAUDE.md → doc → section) is acceptable.
