# Repository maintenance

This recipe covers periodic maintenance tasks:
identifying accidentally checked-in files, finding stale generated artifacts,
and verifying coding conventions.
Run these checks after major refactoring sessions, branch merges, or whenever the repo feels cluttered.

## Scanning for non-source files

Files that don't belong in version control occasionally get committed —
debug scripts, session artifacts, build outputs, editor files, etc.
The `.gitignore` catches most of these, but gaps appear over time.

### Procedure

1. **List all tracked files** and look for patterns that don't belong:
   ```bash
   git ls-files | sort
   ```

2. **Check for common categories of unwanted files:**

   | Category | Patterns to look for |
   |----------|---------------------|
   | Debug/scratch scripts | `debug_*`, `test_*.py` (root level), `*.tmp` |
   | Session artifacts | `*-plan.md`, `docs/work/`, `.claude/` |
   | Build outputs | `*.class`, `*.pyc`, `__pycache__/`, `*.jar` (outside `gradle/wrapper/`) |
   | IDE/editor files | `.idea/`, `*.iml`, `.vscode/` (beyond shared config) |
   | OS files | `.DS_Store`, `Thumbs.db` |
   | Jupyter artifacts | `.ipynb_checkpoints/` |
   | Backup copies | `*-bak/`, `*.bak`, `*.orig` |
   | Empty/corrupt files | Zero-byte files, files with download-duplicate names like `file (2).pdf` |

3. **Decide how to handle each file:**
   - **Delete from disk and git**: Debug scripts, empty files, corrupt downloads.
     ```bash
     git rm <file>
     ```
   - **Untrack but keep locally**: Working documents, analysis notebooks, session artifacts.
     ```bash
     git rm --cached <file>
     ```
   - **Leave as-is**: Files that look suspicious but serve a purpose (e.g., `gradle-wrapper.jar`).

4. **Update `.gitignore`** to prevent recurrence.
   Add patterns for any new categories you found.

5. **Verify nothing important was removed:**
   ```bash
   git diff --cached --stat
   ```

### Current `.gitignore` coverage

The `.gitignore` already covers these patterns — if files matching them are tracked,
they were committed before the ignore rule was added and need `git rm --cached`:

- `debug_*.py`, `debug_*.hs`, `debug_*.clj` — scratch debugging scripts
- `*-plan.md` — branch plan files (used by LLM sessions)
- `docs/work/` — working documents and issue analysis
- `analysis/` — benchmark analysis notebooks
- `.claude/` — LLM session memory
- `__pycache__/`, `*.pyc` — Python bytecache
- `.stack-work/`, `build/`, `.gradle/` — build artifacts

---

## Finding stale generated files

When a Hydra module is renamed or deleted, or when types within a module are renamed or deleted,
the old generated files remain.
The sync scripts generate new files but **never delete old ones**.

This is especially problematic in Java, where each type becomes its own `.java` file —
renaming a single type leaves an orphan class file on the classpath.

### How stale files accumulate

| Change | Stale files left behind |
|--------|------------------------|
| Module renamed (`hydra.foo` → `hydra.foo.bar`) | Old files in every implementation |
| Module deleted | Old files in every implementation |
| Type renamed (within a module) | Old `.java` file in Java |
| Type deleted (within a module) | Old `.java` file in Java |
| Namespace split (`hydra.error` → `hydra.error.core` + `hydra.error.checking` + ...) | Old unsplit files in every implementation |

### Where to look

All generated output lives under `dist/<lang>/` in 0.15:

| Implementation | Path | Granularity |
|---------------|------|-------------|
| Haskell | `dist/haskell/hydra-kernel/src/main/haskell/` | One `.hs` per module |
| Haskell (decode/encode) | `dist/haskell/hydra-kernel/src/main/haskell/Hydra/Decode/`, `Hydra/Encode/` | One `.hs` per type module |
| Haskell (DSL) | `dist/haskell/hydra-kernel/src/main/haskell/Hydra/Dsl/` | One `.hs` per type module |
| Haskell (ext coders) | `dist/haskell/hydra-ext/src/main/haskell/` | One `.hs` per coder module |
| JSON | `dist/json/hydra-kernel/src/main/json/` | One `.json` per module |
| Java | `dist/java/hydra-kernel/src/main/java/` | **One `.java` per type** |
| Python | `dist/python/hydra-kernel/src/main/python/` | One `.py` per module |
| Scala | `dist/scala/hydra-kernel/src/main/scala/` | One `.scala` per module |
| Clojure | `dist/clojure/hydra-kernel/src/main/clojure/` | One `.clj` per module |
| Scheme | `dist/scheme/hydra-kernel/src/main/scheme/` | One `.scm` per module |
| Common Lisp | `dist/common-lisp/hydra-kernel/src/main/common-lisp/` | One `.lisp` per module |
| Emacs Lisp | `dist/emacs-lisp/hydra-kernel/src/main/emacs-lisp/` | One `.el` per module |

Test files follow the same pattern under `src/test/` within each `dist/<lang>/hydra-kernel/`.

### Procedure

The general approach is to cross-reference generated files against their Source modules.

#### Step 1: identify current Source modules

Source modules define what *should* be generated.
Kernel DSL sources live in `packages/hydra-kernel/src/main/haskell/Hydra/Sources/`.
Per-coder DSL sources live in `packages/hydra-<lang>/src/main/haskell/Hydra/Sources/<Lang>/`
and in `packages/hydra-ext/src/main/haskell/Hydra/Sources/`.

```bash
# List all kernel Source modules
find packages/hydra-kernel/src/main/haskell/Hydra/Sources -name '*.hs' | sort

# List all coder Source modules
find packages/hydra-*/src/main/haskell/Hydra/Sources -name '*.hs' | sort
```

Also check the module registries that control what gets generated:
- `Hydra/Sources/Kernel/Types/All.hs` — `kernelTypesModules` (in `packages/hydra-kernel/`)
- `Hydra/Sources/Kernel/Terms/All.hs` — `kernelPrimaryTermsModules` (in `packages/hydra-kernel/`)
- `Hydra/Sources/All.hs` — `mainModules`, `otherModules` (in `packages/hydra-haskell/`)
- `Hydra/Sources/Ext.hs` — `hydraExtModules` and per-language module lists (in `heads/haskell/`)

#### Step 2: check each implementation for orphans

For each implementation, list the generated files and verify each has a corresponding Source module.

**Haskell** — check for modules not imported anywhere:
```bash
# For each file in dist, check if its module is imported
for f in $(find dist/haskell/hydra-kernel/src/main/haskell/Hydra -name '*.hs'); do
  mod=$(echo "$f" | sed 's|.*/haskell/||;s|/|.|g;s|\.hs$||')
  if ! grep -rq "import.*$mod" packages/ heads/haskell/src/; then
    echo "POSSIBLY STALE: $f ($mod)"
  fi
done
```

Note: many generated modules are legitimately not imported within `packages/`
but serve downstream consumers.
Cross-reference against the module registries (Step 1) before deleting.

**Java** — check for orphaned type files within valid packages:
```bash
# For a specific package (e.g., hydra/testing/), compare Java files
# against the types defined in the corresponding generated Haskell module
diff <(ls dist/java/hydra-kernel/src/main/java/hydra/testing/ | sed 's/.java//' | sort) \
     <(grep '^data ' dist/haskell/hydra-kernel/src/main/haskell/Hydra/Testing.hs | awk '{print $2}' | sort)
```

For Java, also check `Decode/`, `Encode/`, and `Dsl/` subdirectories —
these generate per-type-module files that can go stale when type modules change.

**Python, Scala, Lisp dialects** — check for files without a corresponding Source:
```bash
# Example for Python
for f in $(find dist/python/hydra-kernel/src/main/python/hydra -name '*.py' ! -name '__init__.py'); do
  # Derive the expected Source module path and check it exists
  mod=$(echo "$f" | sed 's|.*/python/hydra/|Hydra/Sources/|;s|\.py$|.hs|;s|/|/|g')
  # ... check against known Source paths
done
```

#### Step 3: verify before deleting

Before deleting a file, confirm it's truly stale:

1. **Check for imports**: `grep -r "import.*ModuleName" <impl>/src/`
2. **Check the Source registry**: is the module listed in any `All.hs` module list?
3. **Check for indirect references**: some files are used by reflection, test discovery (hspec-discover),
   or Lisp preload scripts.

#### Step 4: delete and verify the build

```bash
# Delete stale files
rm <stale-files>

# Verify each implementation still builds
cd heads/haskell && stack build && stack test
./gradlew :hydra-java:compileTestJava
cd heads/python && uv run pytest
# etc.
```

### Known patterns that produce stale files

These refactoring patterns are especially prone to leaving orphans:

- **Namespace splits**: When `hydra.foo` is split into `hydra.foo.bar` and `hydra.foo.baz`,
  the old `foo.hs` / `foo.py` / `foo.java` / `foo.clj` etc. remain.
  The decoder, encoder, and DSL modules also split
  (`Decode/Foo.hs` → `Decode/Foo/Bar.hs` + `Decode/Foo/Baz.hs`).
- **Type consolidation**: When many specific types are replaced by a generic type
  (e.g., `FooTestCase`, `BarTestCase` → `TestCase`), the old per-type Java files remain.
- **Module renames**: Any rename leaves the old file in every implementation
  plus in the JSON kernel, decoders, encoders, and DSL modules.

### Relationship to refactoring recipes

The [refactoring recipe](refactoring.md) and [namespace refactoring recipe](refactoring-namespaces.md)
include "delete orphan files" as a step in their workflows.
This recipe covers the broader audit — finding orphans that were missed during those workflows
or that accumulated across multiple changes.

### Stale generated content from cached Haskell binaries

A particularly insidious class of staleness has nothing to do with orphan files —
the generated content itself is wrong because the executables that produced it were
linked against an out-of-date kernel.

The Haskell sync executables (`update-json-main`, `update-json-test`, `update-json-manifest`,
`bootstrap-from-json`, `verify-json-kernel`, etc.) are compiled by Stack and cached under
`heads/haskell/.stack-work/install/`.
Each binary has constants, type names, namespace strings, and serialized term fragments
**baked in at link time** from whatever the kernel looked like when the binary was built.
If you rename a kernel namespace, regenerate `dist/haskell/hydra-kernel/`, then run a generation
exec without rebuilding it first, the exec emits the **old** namespace string into the JSON
output — even though the kernel sources on disk are correct.

`sync-haskell.sh` does call `stack build` between phases, so in principle Stack should
recompile any exec whose dependencies have changed. In practice this is unreliable:

- Stack tracks dependencies by file mtime within `package.yaml`'s `source-dirs`.
- After a regeneration, the `dist/haskell/hydra-kernel/...` files are rewritten with
  new mtimes — but if the **content** is unchanged, Stack may consider the dependent
  exec still up-to-date and skip recompilation.
- Conversely, if a regeneration changes content but the exec wasn't recompiled because
  Stack misjudged the dependency graph, the next exec invocation runs against the
  old in-memory kernel.

The result: **`sync-all` can run successfully and still leave stale string literals
in `dist/json/`** (or, transitively, in any language target that copies content via
`bootstrap-from-json`).

#### Detecting the problem

Stale binary cache shows up as:

- A specific text pattern (an old namespace, an old type name, a removed function name)
  appearing in `dist/json/` or in language-target outputs **after** sync-all completes.
- The same pattern absent from all hand-written sources (`packages/`, `heads/`, `dist/haskell/`).
- Mtimes on the offending dist files showing they were rewritten by the recent sync,
  even though the content is wrong.

A useful audit:

```bash
# Find string patterns that should no longer exist after a recent rename.
grep -rln "hydra.module" dist/ packages/hydra-ext/src/main/haskell/
```

If the only matches are under `dist/`, the cache is stale.

#### Fixing it

The reliable cure is a clean rebuild:

```bash
# Remove leftover .stack-work directories from before #290 (one-time)
rm -rf packages/hydra-*/.stack-work

# Wipe the active install snapshot to force a fresh build
rm -rf heads/haskell/.stack-work/install

# Re-sync everything
bin/sync.sh --hosts all --targets all
```

The first `rm -rf` only matters if you have leftover per-package `.stack-work` dirs
from before the #290 packaging restructure. The second is the important one: removing
`heads/haskell/.stack-work/install` drops all cached binaries so Stack must recompile
from source. A full rebuild from cold cache takes 30–60 minutes.

#### When to suspect this hazard

After any of:
- A namespace rename across the kernel (e.g., #290's `hydra.module` → `hydra.packaging`).
- An ext-prefix removal (#331).
- A type rename in `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Types/`.
- A move of a primitive between libraries.
- Any structural change to the bootstrap graph.

If in doubt after such a change, do the clean rebuild before trusting `sync-all` output.
A few unnecessary recompilations are cheaper than a silent JSON kernel that propagates
stale strings into every downstream language.

---

## Checking for design violations

Hydra's design principles keep hand-written and generated code strictly separated
and keep host-specific workarounds out of generated code.
These principles drift under pressure: a bug in a generator is easy to paper over
with a `sed` patch, and a one-off utility is easy to drop into `packages/` or `dist/`
rather than fixing the right abstraction.
Periodically scan the repository for violations and fix them at the source.

The core principles (see CLAUDE.md and the
[Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization) wiki page):

1. **No post-generation patches.** Generated code (anything under `dist/`) must
   match what the generator produces. If the output is wrong, fix the generator.
   The only exception is a deliberate bootstrap patch that will be overwritten by
   the next regeneration — document it as such.
2. **No hand-written files under `dist/`.** If a file needs to live alongside
   generated artifacts (because tests import it from that location), write it
   under `heads/` and copy it in from a sync script.
3. **No host-specific code under `packages/`.** Packages hold DSL-based module
   definitions plus source-language helpers for writing them. Host-specific
   runtimes and utilities belong in `heads/`, except for `bindings/` which is
   explicitly for host-specific third-party integrations.
4. **Generated files have the "do not edit" header.** If you see a file under
   `dist/` without the header, it is either hand-written (violation) or the
   generator is missing the header (bug in the generator).

### Procedure

**Check 1: post-generation patches in sync scripts.**
Sync scripts are the most common place violations hide.
Grep for the patterns that indicate patches:

```bash
grep -rn "sed_inplace\|sed -i\|Post-process\|Patch\|patching\|post-process" \
  bin/ heads/haskell/bin/ demos/bootstrapping/bin/ 2>/dev/null \
  | grep -v "test\|grep" \
  | grep -vE ":\s*#"
```

Every match is a potential violation.
For each, ask:
- **Is this fixing the generator, or working around it?**
  A patch that renames `case macro(` to `` case `macro`( `` is working around
  a missing keyword-escape in the Scala code generator.
  The generator should emit the backticks in the first place.
- **Is this copying a hand-written file into `dist/`?**
  That is principle 2; the canonical copy must live in `heads/`.
  Copying *into* `dist/` is acceptable; hand-writing *in* `dist/` is not.
- **Is this a deliberate bootstrap patch?**
  Bootstrap patches are explicitly allowed but must be overwritten by the next
  regeneration.
  A `sed` patch that runs on every sync is not a bootstrap patch — it means
  the generator is broken.

Track the list of accepted post-generation patches.
Each one is tech debt against the corresponding generator.
Record new ones in the relevant issue, not silently.

**Check 2: hand-written files under `dist/`.**
Every file in `dist/` should have the generated-file header.
Scan for files that don't:

```bash
for f in $(find dist -type f \
  \( -name '*.hs' -o -name '*.java' -o -name '*.py' -o -name '*.scala' \
     -o -name '*.clj' -o -name '*.lisp' -o -name '*.el' -o -name '*.scm' \) 2>/dev/null); do
  if ! head -5 "$f" | grep -q 'automatically generated'; then
    echo "$f"
  fi
done
```

Any output is a file that should either be generated (fix the generator to emit
the header) or be moved to `heads/` and copied in by a sync script (principle 2).

**Check 3: host-specific code under `packages/`.**
Every file in `packages/` should either be a Hydra DSL module or a
source-language helper used to write one.
Spot-check by sampling `find packages -name '*.hs'`;
any `.hs` file that doesn't import `Hydra.Kernel` or `Hydra.Sources.*`
and doesn't serve as a DSL helper is suspicious.
Also check non-Haskell files under `packages/` (e.g., `.java`, `.py`, `.scala`),
which are even more likely to be violations.

**Check 4: rule-of-three for inline patches.**
If the same type of patch appears in multiple sync scripts (e.g., "escape the
`macro` keyword" and "escape the `type` keyword"), that is a signal the
generator is missing a whole class of handling, not just one edge case.
Fix it at the generator level.

### Known accepted patches

No post-generation patches are currently applied in the active sync path.
The former `TestGraph.hs` patch (which replaced `emptyGraph` / `emptyContext`
with `TestEnv.testGraph testTypes` / `TestEnv.testContext`) was eliminated
when the DSL was updated to emit the `TestEnv` references directly; see
`heads/haskell/bin/sync-haskell.sh` step 5 for the current no-op note.

`Hydra.Test.TestEnv` remains hand-written and checked in under
`dist/haskell/hydra-kernel/src/test/haskell/`; it is exempted from
regeneration because `bootstrap-from-json` does not target it. This is
tolerated under principle 2 (hand-written file under `dist/`) rather than
moved to `heads/` because the Haskell test harness imports it from that
location and the bridge is small. Treat it as tech debt, not precedent.

Two patches that were previously applied by retired per-language sync
scripts are NOT currently re-applied anywhere:

- Java Lisp `Coder.java`: a `PartialVisitor` type parameter that the Java
  coder infers incorrectly. Surfaces when generating `hydra-lisp` into Java.
- Python `test_graph.py`: empty `test_graph` / `test_context` assignments
  needing a `__getattr__` shim to a hand-written `test_env.py`. Surfaces
  when running Python kernel tests.

These patches need to be re-added (probably to per-target assemblers or a
post-processing step) before the affected combinations work again. The
current bootstrapping-triad sync (haskell/java/python kernel + self-hosting)
does not exercise either combination, so the patches are queued, not blocking.

When adding a new accepted patch, document it here and open an issue against
the generator.
When removing a patch (because the generator was fixed), update this list too.

---

## Checking coding style

The [coding style guide](https://github.com/CategoricalData/hydra/wiki/Coding-style)
defines conventions for all Hydra source code.
These conventions tend to drift over time, especially after additions, moves between modules,
or LLM-assisted edits.
Periodically check primary sources against the style guide and fix any violations.

The style guide covers several areas including strictness, naming conventions,
import organization, and definition ordering.
Of these, definition ordering is the most prone to drift and the easiest to check mechanically.

### Definition ordering

The style guide's
[definition ordering](https://github.com/CategoricalData/hydra/wiki/Coding-style#definition-ordering)
section requires that both the `definitions` list and the corresponding
definition bodies appear in **alphabetical order** within each module.

This applies to:
- Kernel Source modules (`packages/hydra-kernel/src/main/haskell/Hydra/Sources/`)
- Per-language coder Source modules (`packages/hydra-haskell/`, `packages/hydra-java/`, `packages/hydra-python/`, `packages/hydra-scala/`, `packages/hydra-lisp/`, `packages/hydra-ext/`, `packages/hydra-pg/`, `packages/hydra-rdf/`, `packages/hydra-coq/`, `packages/hydra-javascript/`)
- Hand-written runtime modules (`heads/haskell/src/main/haskell/Hydra/`)

Generated files inherit their ordering from Source modules,
so fixing the Source fixes all implementations.

**Check a single module:**
```bash
grep 'toDefinition\|toBinding' packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Lexical.hs \
  | sed 's/.*toDefinition //; s/.*toBinding //; s/[,\]]//g' \
  | awk '{name=$1} NR>1 && name<prev {print prev " before " name " (out of order)"} {prev=name}'
```

**Batch check all Source modules:**
```bash
#!/bin/bash
# check-definition-order.sh — run from the repo root
for f in $(find packages/*/src/main/haskell -name '*.hs' 2>/dev/null); do
  out=$(grep 'toDefinition\|toBinding' "$f" \
    | sed 's/.*toDefinition //; s/.*toBinding //; s/[,\]]//g' \
    | awk '{name=$1} NR>1 && name<prev {print "  " prev " before " name} {prev=name}')
  if [ -n "$out" ]; then
    echo "$f:"
    echo "$out"
  fi
done
```

Modules with no output are correctly ordered.

**Fixing violations:** reorder both the `definitions` list entry *and* the corresponding
definition body together — they must stay in sync.

### Other style checks

The following are harder to automate but worth reviewing manually,
especially after large changes.
See the [full style guide](https://github.com/CategoricalData/hydra/wiki/Coding-style) for details.

- **Import conventions**: Modules should use consistent qualified import aliases
  (`Lists`, `Maps`, `Core`, `Graph`, etc.).
  Copy the import block from an existing module of the same kind.
- **Naming conventions**: Conventional names (`ns`, `define`, `module_`) are reused
  across namespaces with the same roles.
  Functions in type-indexed module families are named after the type they operate on.
- **Error handling**: Fail immediately with informative messages.
  Never silently return defaults or swallow failures.

---

## Verifying primitive consistency

Primitives are defined in Haskell and reimplemented in each target language.
Several kinds of inconsistency can creep in:

- A primitive exists in one language but is missing from another.
- A primitive is implemented but not registered (invisible at runtime).
- Type signatures differ subtly — especially the **order of `forall` type variables**,
  which causes hard-to-diagnose inference and type-checking errors.
- Documentation comments differ across languages.

### Registration files

Each implementation has a registration file that maps primitive names to implementations:

| Implementation | Registration file |
|---------------|-------------------|
| Haskell | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Libraries.hs` |
| Java | `heads/java/src/main/java/hydra/lib/Libraries.java` |
| Python | `heads/python/src/main/python/hydra/sources/libraries.py` |
| Scala | `heads/scala/src/main/scala/hydra/lib/Libraries.scala` |
| Clojure | `heads/lisp/clojure/src/main/clojure/hydra/lib/libraries.clj` |

### Checking primitive coverage

1. **Extract the canonical list** from the Haskell registration
   (`Libraries.hs`), which defines all primitive names and their types
   via `prim1`, `prim2`, `prim2Eval`, `prim3` calls.

2. **Compare against each implementation's registration.**
   Each implementation registers primitives differently,
   but the set of fully qualified primitive names should match.
   ```bash
   # Extract Haskell primitive names
   grep -E 'prim[0-3]' packages/hydra-kernel/src/main/haskell/Hydra/Sources/Libraries.hs \
     | grep -oE '_[a-z]+_[a-zA-Z]+' | sort -u

   # Compare against Java class files
   find heads/java/src/main/java/hydra/lib -name '*.java' \
     ! -name 'Libraries.java' | sort
   ```

3. **Verify registration completeness.**
   A primitive class can exist but be invisible if it's not listed in the registration file.
   For each implementation, check that every primitive implementation file
   has a corresponding entry in the registration.

### Checking type signature consistency

This is the most error-prone area.
The canonical type signatures are in `Libraries.hs`, specified as arguments to `prim1`/`prim2`/etc.
Each Java primitive class has a `type()` method returning a `TypeScheme`;
Python and Clojure registrations specify types inline.

**Critical: `forall` variable ordering must match.**
For example, if `Libraries.hs` defines `foldl` with type variables `[_y, _x]`,
every implementation must use the same order (`y` before `x`).
A mismatch causes the type checker to assign the wrong type to each variable,
leading to inference failures that don't point back to the primitive as the root cause.

To check:
1. Extract the type variable lists from `Libraries.hs` (the `[_x]`, `[_y, _x]`, etc. arguments).
2. Compare against the `TypeScheme` in each Java primitive's `type()` method.
3. Compare against the type variable lists in Python and Clojure registrations.

### Checking documentation consistency

Primitive documentation comments should be the same across all languages.
The canonical descriptions are in `Libraries.hs` (as `doc` strings on the Source definitions)
or in the Haskell implementation files (`heads/haskell/src/main/haskell/Hydra/Lib/*.hs`).
Check that the Javadoc, Python docstrings, and Clojure docstrings match.

### See also

The [adding primitives](adding-primitives.md) recipe documents the full set of files
that must be updated when adding a new primitive.

---

## Verifying test parity

After a sync-all run or a benchmarking run, compare test counts across implementations.
All implementations should have the same number of passing tests
(modulo documented skips for language-specific limitations).

### Procedure

1. **Run tests in each implementation** and capture the summary line:
   ```bash
   # Haskell
   cd heads/haskell && stack test 2>&1 | tail -5

   # Java
   ./gradlew :hydra-java:test 2>&1 | grep -E 'tests.*passed'

   # Python
   cd heads/python && uv run pytest --tb=no -q 2>&1 | tail -3

   # Scala
   cd packages/hydra-scala && sbt test 2>&1 | tail -5

   # Lisp dialects (clojure, common-lisp, emacs-lisp, scheme)
   packages/hydra-lisp/bin/run-tests.sh clojure 2>&1 | tail -5
   ```

2. **Compare pass/skip/fail counts.**
   Expected: all implementations have the same pass count,
   with small documented differences in skip counts.

3. **Investigate any divergence.**
   Common causes:
   - A new test case was added to the common test suite but a runner wasn't updated.
   - A primitive was added/changed in some implementations but not others.
   - A language-specific limitation prevents a test from running
     (should be documented as a skip, not silently absent).

---

## Checking `.cabal` / `package.yaml` source-dirs

`heads/haskell/package.yaml` does not use an explicit `exposed-modules` list;
instead it uses `source-dirs:` to auto-expose everything under the listed directories.
The check is therefore to verify that every listed source directory exists.

### Procedure

```bash
# Extract source-dirs from package.yaml (under the library section) and check each exists
awk '/^library:/{in_lib=1} in_lib && /^  source-dirs:/{in_sd=1; next} in_sd && /^    - /{print $2} in_sd && /^  [a-z]/{in_sd=0}' \
  heads/haskell/package.yaml \
  | while read d; do
      full="heads/haskell/$d"
      [ -d "$full" ] || echo "MISSING: $full"
    done
```

A missing source-dir causes stack to fail at configuration time rather than at
build time, so this is a quick sanity check after restructuring.

---

## Verifying JSON kernel freshness

The JSON kernel files (`dist/json/hydra-kernel/src/main/json/`) are generated from Haskell sources.
They can go stale if someone rebuilds Haskell but forgets to re-export.
The `verify-json-kernel.sh` script checks that JSON files match the current Haskell modules.

```bash
heads/haskell/bin/verify-json-kernel.sh
```

This loads each kernel module from Haskell, decodes the corresponding JSON file,
and compares them element by element.
Run this after any kernel changes, or as a periodic sanity check.

---

## Checking Python `__init__.py` freshness

In 0.15, Python `__init__.py` files under `dist/python/hydra-kernel/` are
**namespace-package markers**: each one just extends `__path__` via
`pkgutil.extend_path` so that `hydra.*` submodules can be discovered across both
`src/main/python` (hand-written) and `dist/python/hydra-kernel/src/main/python`
(generated). They do not contain explicit imports of submodules, so they cannot
go stale in the way earlier versions could.

### Procedure

Spot-check that every `__init__.py` is just the namespace-package stub:

```bash
# Any __init__.py that is NOT the namespace-package stub is suspicious
for f in $(find dist/python/hydra-kernel/src/main/python/hydra -name '__init__.py'); do
  if ! grep -q 'extend_path' "$f"; then
    echo "NON-NAMESPACE-PACKAGE __init__.py: $f"
  fi
done
```

If this prints anything, investigate whether the file is intentional or leftover
from a pre-0.15 version.

---

## Reviewing user documentation

User documentation drifts from the code over time as features are added, renamed, or removed,
and as project structure evolves.
This review catches stale material, broken links, and opportunities for small improvements.

### Scope

"User documentation" includes (see [CLAUDE.md](../../CLAUDE.md) for the full definition):
- Top-level `README.md` and per-implementation / per-package READMEs
- Everything checked into `docs/` (ignore unstaged/temporary files)
- Everything in the `wiki/` checkout (separate Git repo)
- `CHANGELOG.md`
- `CLAUDE.md` itself — the task routing table, document index, and critical pitfalls
  drift as docs and workflows change

Code comments are not in scope for this review — they are maintained alongside code changes.

### Procedure

For each document, perform three passes:

1. **Factual accuracy.** Read the document end-to-end and cross-check claims against
   the current code, recipes, and project structure.
   Flag:
   - Command examples that no longer work (changed flags, renamed scripts, moved files)
   - References to modules, types, functions, or files that have been renamed or deleted
   - Counts and version numbers that have drifted (test counts, module counts, implementation counts)
   - Feature descriptions that no longer match behavior
   - Statements about "current status" that are stale (e.g., "X is in progress" after X shipped)

2. **Link integrity.** Check every link:
   - Relative links to other docs, recipes, and source files resolve to existing paths
   - Absolute links to wiki pages point to existing pages
   - External URLs are still reachable (lightweight check; deep-link rot is acceptable)

3. **Readability and complementarity.** Consider whether the document:
   - Still serves its stated purpose, or has grown beyond it
   - Duplicates content that belongs in another document (consolidate or link)
   - Has gaps that a reader would hit (missing context, unexplained terms)
   - Could be shortened without losing value
   - Has a clear entry point and logical flow

   Make small, targeted improvements — not wholesale rewrites.
   Structural rework of a document is out of scope for this maintenance pass.

### Approach

- Start with the document index in [CLAUDE.md](../../CLAUDE.md) — any doc linked there
  is high-traffic and should be reviewed first.
- Then walk `docs/`, `wiki/`, and top-level READMEs.
- Track findings per document and fix them as you go, not in a separate pass.
- Write to the [style guide](../documentation-style-guide.md):
  line length under 120 characters, sentence-case headings, relative links for internal docs,
  active voice, second person for recipes and tutorials.

### What to avoid

- Don't rewrite documents to match your preferred style.
- Don't add content that belongs in code comments, commit messages, or changelogs.
- Don't expand CLAUDE.md — keep the document index flat (see "Maintaining this file" in CLAUDE.md).
- Don't touch unstaged or temporary files in `docs/work/`.

---

## Logical code review

Code written or extended by an LLM tends to accumulate cruft over time
if not aggressively pruned:
unused features, one-call-site abstractions, duplicated helpers, obsolete flags,
comments justifying workarounds that should have been fixed at the source,
and defensive error handling for scenarios that cannot happen.
This review looks critically at source files and scripts and surfaces candidates for pruning or simplification.

The review is **report-first**: write findings to a dated file under `docs/reviews/`,
then discuss with the user before acting.
Do not edit code as part of the review pass itself.

### Scope and sampling

A full-repo pass is impractical for a single session.
Pick a slice of roughly 40–60 files / ~5,000–7,000 lines per run.

- **When the user specifies a slice, use it.**
  Examples: "review `bin/` scripts," "review the Python coder."
- **When the slice is up to you**, sample evenly across the repo
  (`packages/`, `heads/`, `bin/`, `demos/`), with modest bias toward high-traffic code
  (kernel DSL sources, essential scripts like `bin/sync.sh`, registration files like
  `Libraries.hs` / `Libraries.java` / `libraries.py`).
  Check `docs/reviews/` for what's been covered recently and prefer unseen files.

Exclude from every pass:
- Generated files (anything under `dist/`, and any file whose header is
  "Note: this is an automatically generated file. Do not edit.").
  Review findings against generated files are really generator findings —
  route them to the generator source.
- Files the user has explicitly excluded.

### What to flag

Review against these patterns explicitly.
If you're uncertain whether something is drift, flag it with a note — the user decides.

| Pattern | What to look for |
|---------|------------------|
| Dead CLI flags / options | Parsed and stored but never consulted; aliases whose callers no longer exist |
| Unused top-level definitions | Functions, types, or constants with no call sites (cross-check `packages/` and `heads/haskell/src/main/`; a Haskell function called only from those trees is not dead) |
| One-call-site abstractions | Helpers with a single caller where inlining would be clearer |
| Duplicate helpers | Near-identical functions in different files with cosmetic renames; shared ANSI constants, regexes, sort keys |
| Error swallowing | `|| warn`, `|| true`, `|| echo` in sync scripts (violates "Never proceed with failures"); try/except that logs and continues |
| Post-generation patches | `sed_inplace` or other edits against files under `dist/` (violates "No post-generation patches") |
| Defensive code for impossible scenarios | `case _ of` branches that can't be reached; null checks for internal invariants |
| Stale comments | Comments describing code that no longer exists; obsolete TODOs; "workaround for X" comments where X is fixed |
| Baroque control flow | Nested conditionals that flatten; sentinel values (`""` as "unset") where `Maybe`/`Optional` exists |

**Cross-reference [CLAUDE.md](../../CLAUDE.md) "Critical pitfalls" as review criteria.**
Any violation of a pitfall is a finding regardless of how plausible the surrounding comment sounds.

### Be adversarial toward justifying comments

A comment like "note: this is a workaround for the Java generator" is a finding, not a dismissal.
The workaround may be real, but it's a symptom of drift in either the generator or the review scope.
Flag it with the justification quoted verbatim so the user can decide whether to address the root cause.

### Detect cross-file duplication

Single-file review misses duplication.
For each slice, also:

- Grep for identical identifiers across the slice (regexes, constants, helper names).
- Compare scripts with parallel purposes (e.g., `sync-java.sh` vs `sync-python.sh`;
  `benchmark-dashboard.py` vs `bootstrapping-dashboard.py`).
- Look for copy-pasted blocks that differ only by cosmetic renames.

### Output format

Write findings to `docs/reviews/YYYY-MM-DD-<slice-name>.md`.
The `docs/reviews/` tree is gitignored; reports are a local working record, not a checked-in artifact.
Structure:

```
# Logical code review: <slice>

**Date:** YYYY-MM-DD
**Scope:** <file list or description, line count>

## Summary

<2–4 bullets highlighting the most important findings>

## Safe to remove

<Each finding: file:line(s), one-line description, one-line reason>

## Worth discussing

<Each finding: file:line(s), description, trade-off, why it's worth surfacing>

## False-positive candidates

<Things that look like drift but are load-bearing; document so future passes don't re-flag>

## What this slice says about the codebase

<Optional: patterns that suggest follow-up work beyond the slice>
```

Keep entries terse — one or two lines per finding.
The user will read the file, so don't re-explain context that's obvious from a click-through.

### What to avoid

- Do not edit code during the review pass.
- Do not flag code as drift just because it's complex;
  complexity is fine when it reflects the problem.
- Do not mark findings "intentional" on the basis of a nearby comment alone;
  verify against the rule the comment is justifying.
- Do not try to cover the whole repo in one pass;
  sampling across sessions over time is the point.

### Delegation

Do not delegate the full review to an Explore agent.
In practice, single-pass agents tend to mark too many findings "intentional" and miss
cross-file duplication.
An agent can help read files in parallel if the reviewer reads the reports critically,
but the reviewer (main session) owns the triage.

---

## Full maintenance pass

To run all checks in sequence (invoked via `/maintenance()` in CLAUDE.md):

1. Scan for non-source files; remove or untrack as appropriate; update `.gitignore`.
2. Find stale generated files across all implementations; delete confirmed orphans.
3. Check for design violations (post-generation patches, hand-written files under
   `dist/`, host-specific code under `packages/`); fix at the generator or move
   to `heads/`.
4. Check coding style (definition ordering) across all Source modules; fix violations.
5. Verify primitive consistency (coverage, `forall` variable ordering, documentation).
6. Check `.cabal`/`package.yaml` exposed-modules for stale entries.
7. Verify JSON kernel freshness via `heads/haskell/bin/verify-json-kernel.sh`.
8. Check Python `__init__.py` freshness.
9. Review user documentation for accuracy, broken links, and small improvements.
10. Do a logical code review pass on a sampled slice (report-first, no edits);
    discuss findings with the user before acting.

After all checks, present a summary of findings and changes to the user.
If any changes affect Source modules (e.g., definition reordering),
generated files (e.g., stale file deletion, `.cabal` fixes),
or could affect test outcomes (e.g., primitive fixes),
run `bin/sync.sh --hosts all --targets all` and verify all tests pass.
If no changes affect generated files or tests, skip the sync.

---

## When to run these checks

| Check | When to run |
|-------|------------|
| Non-source file scan | After branch merges, periodically |
| Stale generated files | After refactoring (renames, deletes, splits) |
| Design violations | Periodically, before release, after adding sync-script patches |
| Coding style | After large changes, before release |
| Primitive consistency | After adding/changing primitives, after adding a new implementation |
| Test parity | After sync-all, after benchmarking runs |
| `.cabal` exposed-modules | After deleting generated Haskell modules |
| JSON kernel freshness | After kernel changes |
| Python `__init__.py` | After sync-python |
| User documentation review | Periodically, before releases, after major refactoring |
| Logical code review | Periodically; one slice per session; after extended LLM-driven edits to a region |
