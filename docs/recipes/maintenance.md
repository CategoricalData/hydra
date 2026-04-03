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

Generated files live in `src/gen-main/` and `src/gen-test/` within each implementation:

| Implementation | Path | Granularity |
|---------------|------|-------------|
| Haskell | `hydra-haskell/src/gen-main/haskell/` | One `.hs` per module |
| Haskell (decode/encode) | `hydra-haskell/src/gen-main/haskell/Hydra/Decode/`, `Hydra/Encode/` | One `.hs` per type module |
| Haskell (DSL) | `hydra-haskell/src/gen-main/haskell/Hydra/Dsl/` | One `.hs` per type module |
| JSON | `hydra-haskell/src/gen-main/json/` | One `.json` per module |
| Java | `hydra-java/src/gen-main/java/` | **One `.java` per type** |
| Python | `hydra-python/src/gen-main/python/` | One `.py` per module |
| Scala | `hydra-scala/src/gen-main/scala/` | One `.scala` per module |
| Clojure | `hydra-lisp/hydra-clojure/src/gen-main/clojure/` | One `.clj` per module |
| Scheme | `hydra-lisp/hydra-scheme/src/gen-main/scheme/` | One `.scm` per module |
| Common Lisp | `hydra-lisp/hydra-common-lisp/src/gen-main/common-lisp/` | One `.lisp` per module |
| Emacs Lisp | `hydra-lisp/hydra-emacs-lisp/src/gen-main/emacs-lisp/` | One `.el` per module |

Test files follow the same pattern under `src/gen-test/`.

### Procedure

The general approach is to cross-reference generated files against their Source modules.

#### Step 1: identify current Source modules

Source modules define what *should* be generated.
They live in `hydra-haskell/src/main/haskell/Hydra/Sources/`.

```bash
# List all Source modules (these define the expected generated output)
find hydra-haskell/src/main/haskell/Hydra/Sources -name '*.hs' | sort
```

Also check the module registries that control what gets generated:
- `Hydra/Sources/Kernel/Types/All.hs` — `kernelTypesModules`
- `Hydra/Sources/Kernel/Terms/All.hs` — `kernelPrimaryTermsModules`
- `Hydra/Sources/All.hs` — `mainModules`, `otherModules`

#### Step 2: check each implementation for orphans

For each implementation, list the generated files and verify each has a corresponding Source module.

**Haskell** — check for modules not imported anywhere:
```bash
# For each file in gen-main, check if its module is imported
for f in $(find hydra-haskell/src/gen-main/haskell/Hydra -name '*.hs'); do
  mod=$(echo "$f" | sed 's|.*/haskell/||;s|/|.|g;s|\.hs$||')
  if ! grep -rq "import.*$mod" hydra-haskell/src/ hydra-ext/src/; then
    echo "POSSIBLY STALE: $f ($mod)"
  fi
done
```

Note: many generated modules are legitimately not imported within hydra-haskell
but serve downstream consumers.
Cross-reference against the module registries (Step 1) before deleting.

**Java** — check for orphaned type files within valid packages:
```bash
# For a specific package (e.g., hydra/testing/), compare Java files
# against the types defined in the corresponding Haskell gen-main module
diff <(ls hydra-java/src/gen-main/java/hydra/testing/ | sed 's/.java//' | sort) \
     <(grep '^data ' hydra-haskell/src/gen-main/haskell/Hydra/Testing.hs | awk '{print $2}' | sort)
```

For Java, also check `Decode/`, `Encode/`, and `Dsl/` subdirectories —
these generate per-type-module files that can go stale when type modules change.

**Python, Scala, Lisp dialects** — check for files without a corresponding Source:
```bash
# Example for Python
for f in $(find hydra-python/src/gen-main/python/hydra -name '*.py' ! -name '__init__.py'); do
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
cd hydra-haskell && stack build && stack test
cd hydra-java && ./gradlew compileTestJava
cd hydra-python && uv run pytest
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
section requires that both the `elements` list and the corresponding
definition bodies appear in **alphabetical order** within each module.

This applies to:
- Haskell Source modules (`hydra-haskell/src/main/haskell/Hydra/Sources/`)
- hydra-ext Source modules (`hydra-ext/src/main/haskell/Hydra/Ext/`)
- Hand-written kernel modules (`hydra-haskell/src/main/haskell/Hydra/`)

Generated files inherit their ordering from Source modules,
so fixing the Source fixes all implementations.

**Check a single module:**
```bash
grep 'toDefinition\|toBinding' hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Terms/Lexical.hs \
  | sed 's/.*toDefinition //; s/.*toBinding //; s/[,\]]//g' \
  | awk '{name=$1} NR>1 && name<prev {print prev " before " name " (out of order)"} {prev=name}'
```

**Batch check all Source modules:**
```bash
#!/bin/bash
# check-definition-order.sh — run from the repo root
for f in $(find hydra-haskell/src/main/haskell/Hydra/Sources \
                hydra-ext/src/main/haskell/Hydra/Ext \
                -name '*.hs' 2>/dev/null); do
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

**Fixing violations:** reorder both the `elements` list entry *and* the corresponding
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

## When to run these checks

- After merging feature branches (especially ones that renamed or deleted modules)
- After a major refactoring pass
- Before a release
- Periodically (e.g., monthly) as general hygiene
