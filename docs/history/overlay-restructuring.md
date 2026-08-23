# The overlay/ restructuring and the hydra.overlay namespace

Retrospective on how host-native, hand-written source came to live under a single top-level `overlay/`
tree with the governing equation `dist/<lang>/<pkg>/ = transform(packages/<pkg>/) + copy(overlay/<lang>/<pkg>/)`,
and how the `hydra.overlay.<lang>.*` namespace hardened the boundary between translingual generated code
and host-native source. Primary issues:
[#418](https://github.com/CategoricalData/hydra/issues/418),
[#434](https://github.com/CategoricalData/hydra/issues/434),
[#501](https://github.com/CategoricalData/hydra/issues/501),
[#511](https://github.com/CategoricalData/hydra/issues/511).

This is a history of *why* the structure changed and in *what sequence*. The current mechanics — the
governing equation, the two invariants, `build.json`, prune protection — are documented as shipped behavior
in [overlays.md](../overlays.md) and
[build-system.md § what goes in packages vs overlay](../build-system.md#what-goes-in-packages-vs-overlay);
this record links to those rather than restating them.

## Before overlays: runtime scattered across heads/, integrations in bindings/

Host-native static resources — primitive implementations, helper classes, test environments — lived in
idiosyncratic locations under `heads/<lang>/` and were copied case-by-case into `dist/<lang>/<pkg>/` by
each head's own bespoke script. Separately, host-specific third-party integrations lived in a top-level
**`bindings/`** tree as standalone distribution projects (`bindings/java/hydra-{neo4j,rdf4j,pg-dsl,tinkerpop}`,
`bindings/python/hydra-tinkerpop`), each carrying its *own* `build.gradle` / `pyproject.toml`. There was no
single answer to "where does hand-written, shipped, host-specific source live?"

## The trigger: build symmetry and the Hackage split (#418)

`overlay/` was not designed as a standalone concept — it was born *inside* the Hackage-distribution split.
[#418](https://github.com/CategoricalData/hydra/issues/418) ("Split Hackage distribution into per-package
distributions") aimed for build symmetry across hosts: the monolithic Hackage sdist needed a special
`assemble-hackage-sdist.sh` with no Java/Python analog, and achieving parity was a 0.16 goal. Solving that
split forced a clean home for host-native runtime, and the `overlay/` convention fell out of it — commit
`8443b389ca` ("Introduce top-level overlay/ convention; relocate kernel runtime out of `heads/`. For #418")
established `dist = transform(packages) + copy(overlay)` and retired the monolithic sdist script. Reading
the current docs, overlay looks like a first-class design; historically it was a side effect of chasing
build symmetry.

## Generalizing the copy model to all hosts (#434)

[#434](https://github.com/CategoricalData/hydra/issues/434) generalized the uniform copy model from the
initial Haskell/Java/Python trio to *all* hosts (TypeScript, Scala, the four Lisp dialects, and a Go
pre-stage). This is where the two load-bearing invariants were codified: **only the copy step reads
`overlay/`** (nothing else may reference it), and **heads depend on `dist/`**, never on `overlay/` or on
`heads/` for shipped runtime.

## The namespace boundary: three collisions and the provably-safe fix (#501)

[#501](https://github.com/CategoricalData/hydra/issues/501) moved overlay modules to the
`hydra.overlay.<lang>.*` namespace. This was not a cosmetic rename — the substantive part was a
**coder-emission change**: the coders began redirecting `hydra.lib.*` references to
`hydra.overlay.<lang>.lib.*` at emit time. It was motivated by three concrete, latent collisions:

1. `hydra.dsl.*` became a *generated* output namespace (via #467) and collided with hand-written
   `Hydra.Dsl.*` modules — widening generation across all modules literally failed with duplicate-module
   errors.
2. `Hydra.Haskell.*` mixed the generated translingual coder with hand-written implementations, avoiding a
   clash only by the luck of non-overlapping leaf names.
3. Java was the laggard, with hundreds of implementations still at `hydra.lib.*` polluting the translingual
   namespace.

The chosen fix is **provably collision-free by construction**: no kernel module is named `overlay`, so the
generator can never emit into `hydra.overlay.*`. That single reserved root was preferred over continuing
the incremental per-host relocations already underway. The result is the hard boundary named in CLAUDE.md:
`hydra.*` is exclusively translingual; `hydra.overlay.<lang>.*` is exclusively host-native.

## Folding bindings/ in: build config as Hydra types (#511)

[#511](https://github.com/CategoricalData/hydra/issues/511) folded the top-level `bindings/` tree into
overlays — deleting the standalone binding projects and moving the integrations to
`overlay/{java,python}/hydra-{pg,rdf}/`. The wrinkle was that bindings carried their *own* build
configuration; rather than reach for a raw build-tool escape hatch, that configuration was **modeled as
Hydra types** (`hydra.gradle.*`, `hydra.python.pyproject`, a shared `hydra.packaging.VersionSpecifier`) and
read from a per-package `build.json`. Even ANTLR configuration for the Cypher/GQL grammars became a Hydra
type rather than raw Groovy — keeping third-party build config on the translingual side of the boundary.
This is also when [overlays.md](../overlays.md) was created.

## What was deferred

Downstream-package overlays (`hydra-pg`, `hydra-rdf`) exist today *only* for the hosts that have
host-specific integrations — Java and Python. Extending downstream-package overlay coverage to the other
hosts is explicitly deferred under [#434](https://github.com/CategoricalData/hydra/issues/434).

## Invariants that survived

Three invariants have held since the restructure and are worth stating as the durable outcome:

- **Only the copy step reads `overlay/`.** No generator, head, or test may reference it directly.
- **Heads depend on `dist/`,** never on `overlay/` or `heads/` for shipped runtime.
- **Overlays are copied, not generated, so they sit outside the output digest.** That required an explicit
  keep-paths manifest so `--prune-stale` and cache reconciliation never clobber hand-written overlay files
  — a subtle but necessary consequence of the copy model.
