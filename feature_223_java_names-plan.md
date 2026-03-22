# Plan: feature_223_java_names — Rethink Java package naming scheme

## Issue

GitHub issue #223: The Java coder maps Hydra elements to Java in a way that creates
excessive single-class packages and repetitive naming. The current scheme:

- **Type elements** like `hydra.core.Term` → Java class `hydra.core.Term`
- **Term elements** like `hydra.formatting.convertCase` → Java interface
  `hydra.formatting.Formatting::convertCase` (note: package = `hydra.formatting`,
  class = `Formatting`, repeating the namespace tail)

This was chosen to avoid collisions between type-based classes and term-based
interfaces, but results in 140 single-file packages and repetitive names.

## Current state

- 985 generated Java files (vs 179 Python, 188 Haskell)
- 140 single-file packages (term-only modules that each contain one interface)
- Biggest contributors: ext/java/syntax (251), ext/python/syntax (197),
  ext/lisp/syntax (67), ext/haskell/ast (65)

## Status

- [x] Analyze strategies for Java namespace mapping
- [x] Present strategies with pros/cons
- [x] Decide on approach (Strategy 1: flatten term interfaces)
- [x] Implement coder changes in Hydra.Ext.Sources.Java.Coder.hs
- [x] Run sync-ext
- [x] Regenerate Java (kernel + ext modules)
- [x] Update hand-written Java imports/references
- [x] Verify compilation (only pre-existing Lisp coder error remains)
- [ ] Run Java tests (blocked by pre-existing Lisp coder compile error)

## Results

- Kernel directories reduced from 136 to 35 (101 single-file packages eliminated)
- Term interfaces now live at parent namespace level: `hydra.formatting.Formatting` → `hydra.Formatting`
- All generated cross-references updated automatically via coder change
- Hand-written Java files updated manually for new import paths

## Pre-existing issues (not caused by this change)

- `hydra/ext/lisp/Coder.java` has visitor pattern errors (was broken on baseline)
- PG model files changed from `java.util.List` to `hydra.util.ConsList` (from ext regeneration)
