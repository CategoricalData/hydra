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
- **`hydra.build.reconcile`** — kernel/host reconciliation utilities.
- **`hydra.build.modules`** — pure module-list utilities.

## Status

These modules are pure and behavior-stable; they are consumed today only by
characterization tests. Driver migration — having the sync drivers themselves import and
call these modules — is tracked separately under #416 Step 4. Future `hydra.build.*`
modules (json, manifest, libpass, digest, matrix) are authored directly into this package.
