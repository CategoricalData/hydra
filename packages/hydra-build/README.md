# Hydra-Build

`hydra-build` is the translingual home of Hydra's build system, promoted into Hydra itself.
It is generated from a common set of DSL sources into every self-hosting dialect
(Haskell, Java, Python, Scala, TypeScript, and the four Lisp dialects) and depends only on
`hydra-kernel`.

The modules were extracted from `hydra-kernel` under #546 (part of the #416 effort to
express Hydra's own build/sync system as Hydra modules rather than host-native scripts).

## What it provides

- **`hydra.build.routing`** — the manifest-derived, fail-loud module-to-package router.
  Package ownership of a module namespace comes from each package's `Manifest.mainModules`
  (never from a hardcoded name prefix); derived DSL/encode/decode names route to their
  source module's package.
- **`hydra.build.reconcile`** — the orphan-reconcile decision: which generated files to delete.
- **`hydra.build.modules`** — module-universe utilities and the lib-pass partition
  (`isLibModule`, `partitionLibModules`, `filterKernelModules`, `stripTermTypes`, ...)
  shared by the Java, Scala, and Haskell generation drivers (#788).
- **`hydra.build.format`** — type-level specifications of the build system's JSON formats
  (`hydra.json`, `package.json`, `digest.json`, `manifest.json`).
- **`hydra.build.manifestWriter`** — per-package `manifest.json` assembly and serialization.
- **`hydra.build.assembly`** and **`hydra.build.assemblyplan`** — the type-level model of a distribution package's
  assembly plan, and its pure derivation (the overlay-copy entries and keep-paths).
- **`hydra.build.libraries`** — the expected-libraries registry: every `hydra.core.lib.<sub>` module that each
  self-hosting host must register, load, and test.
- **`hydra.build.registry`** — the language registry: per-language build identity as `LanguageProfile` records.
- **`hydra.build.langexpansion`** — language and host list expansion shared by the sync, test, and bench drivers.
- **`hydra.build.syncmatrix`** — the pure (package, target) planning core of the sync driver.
- **`hydra.build.packagingProfile`** — per-package packaging-validation policy (strict vs. doc-relaxed profiles).
- **`hydra.build.publishsets`** — the publish-set dependency-closure guard and leaves-first ordering.
- **`hydra.build.versionconsistency`** — version-consistency logic shared by the release and bump drivers.
- **`hydra.build.walk`** — pure directory-traversal decision helpers.
- **`hydra.build.comparereport`**, **`hydra.build.comparereportlogic`**, and **`hydra.build.benchresult`** —
  the JSON snapshot compare-report and benchmark-result models, and the compare decision helpers.

## Status

These modules are pure and behavior-stable, and the sync drivers consume them
(for example, the lib-pass partition in `hydra.build.modules`, #788).
Migrating the remaining driver logic into this package is tracked under #416.
