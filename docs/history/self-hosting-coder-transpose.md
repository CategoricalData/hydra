# Self-hosting: transposing the coders to host-native DSLs

Retrospective on the migration that moved Hydra's Java, Python, and Scala coder sources off their
Haskell-DSL copies and into host-native authoring — the change that let Hydra generate those hosts *from
those hosts*. Primary issues: [#182](https://github.com/CategoricalData/hydra/issues/182),
[#344](https://github.com/CategoricalData/hydra/issues/344),
[#346](https://github.com/CategoricalData/hydra/issues/346),
[#370](https://github.com/CategoricalData/hydra/issues/370),
[#509](https://github.com/CategoricalData/hydra/issues/509).

This is a history of *why* the migration happened and *what it surfaced*. The shipped mechanics —
the Phase 5 native-generation pass, the cold-start fallback, the digest model — are documented as
current behavior in [build-system.md](../build-system.md#phases-of-binsyncsh) and
[implementation.md](../implementation.md#multi-language-generation); this record links to those rather
than restating them.

## Background: the two-part split (#182) and the "liberation from Haskell" goal

The migration began as one half of a deliberate two-part split in
[#182](https://github.com/CategoricalData/hydra/issues/182). Part one — *move* each host's coder into
that host's package — shipped in 0.15. Part two — *transpose* each coder from its Haskell-DSL copy into a
host-native DSL — was deferred to 0.16 and became [#344](https://github.com/CategoricalData/hydra/issues/344).
The rationale in #182 was threefold: parallelize development across languages, concentrate each language's
artifacts under the governance of that language's experts, and keep each head slim. The kernel stays
Haskell-authored; the framing in [#346](https://github.com/CategoricalData/hydra/issues/346) was that
"everything else can be distributed" — self-hosting as a liberation of Hydra from Haskell.

## Why host-native authoring

Authoring the Java coder in Java (and Python in Python, Scala in Scala) makes each coder legible and
maintainable to contributors fluent in that language, rather than requiring everyone to read a
Haskell-DSL encoding of Java-emission logic. It also lets each host evolve its coder independently and
removes an entire category of translation drift between the Haskell copy and the host-native intent.

## The migration sequence: Python first, byte-identity as the oracle

The port ran language by language, Python first (129/129 definitions), then Java. The correctness oracle
was **byte-identical output**: the host-native coder was validated by reaching byte-for-byte parity with
the Haskell coder's `dist/json` output *before* any Haskell copy was deleted. This de-risked a very large
deletion — reaching parity meant the new coder provably reproduced the old one's behavior, so removing the
old code changed nothing observable.

## The cold-start bootstrap paradox and its resolution

Self-hosting is circular: a host-native generator needs a running host to execute, but the host is exactly
what the generator produces. The resolution was to wire native generation as a late phase with a
Haskell-DSL **cold-start fallback** — when no host is available to seed generation, the build falls back
to the Haskell path. This is a textbook self-hosting chicken-and-egg, and the fallback was later made
largely obsolete once the build could seed from *published* hosts (see below).

## Bugs the transpose surfaced

Making each coder run *on itself* exposed latent defects that were invisible while Haskell generated
everything:

- **An O(N²) thunking bug.** An older textual `needsThunking` heuristic missed lambda-applied bindings and
  walked whole subtrees per visit. Porting surfaced it as a real algorithmic defect in inference-heavy
  workloads; the fix mirrored Python's `isComplexBinding && !isTrivialTerm` rule
  (commit `34a4fcfa71`, for #344).
- **The digest blind spot.** The self-hosted coder's input digest initially hashed only the
  `hydra.dsl.<lang>.*` modules, not the native `hydra.<lang>.coder` sources — so edits to the native coder
  were *invisible* to change propagation and produced stale downstream output. A self-inflicted
  correctness gap that only self-hosting could create; see
  [build-system.md § what invalidates what](../build-system.md#what-invalidates-what).

The general lesson: self-hosting turns the coder into its own largest test input, which is both a strong
correctness signal (byte-identity) and a way to surface performance and change-tracking bugs that a
Haskell-only pipeline never exercised.

## Deleting the Haskell DSL copies (#346) — and why Scala and Lisp waited

With byte-identity reached, [#346](https://github.com/CategoricalData/hydra/issues/346) deleted the legacy
Haskell DSL copies for Java and Python (~22,000 lines; commit `c962eea484`), making the host-native
sources the **sole source of truth**. Scala and Lisp were *deliberately excluded* from 0.16: no published
Scala/Lisp packages existed yet, and — as #346 put it — liberating a language's sources from Haskell only
makes sense once that language has a published home to be liberated *into*. The migration was gated on
publishing infrastructure, not just on the transpose itself.

## The published-host consume model (#370) as an enabler

[#370](https://github.com/CategoricalData/hydra/issues/370) transitioned the build to consume the
*published* host (from Maven / PyPI) by default rather than building it locally, with a `--local-host` shim
for backward-incompatible kernel changes. This is what let the cold-start fallback recede: a published
host can seed generation directly. The consume model has its own retrospective —
[published-host-consume-model.md](published-host-consume-model.md) — with
[Consuming published hosts](../build-system.md#consuming-published-hosts) for current behavior.

## Downstream ripple: persistent collections

Self-hosting made the coders run real inference workloads on every generation, which exposed that several
hosts' default collection types were O(n²) under that load. Persistent/immutable collection helpers were
retrofitted per host as follow-ups: Java (#359), Python (#362), Common Lisp (#360). The Python arc is
documented in [python-host-perf-investigation.md](python-host-perf-investigation.md); the Emacs-Lisp arc in
[emacs-lisp-collections-perf.md](emacs-lisp-collections-perf.md).

## What changed later

Scala repeated the entire arc post-0.16 under [#509](https://github.com/CategoricalData/hydra/issues/509)
(commit `b01f1d34c6`): ~4,000 Haskell lines deleted, ~5,900 Scala lines added, reaching the same
sole-source-of-truth state. The [#501](https://github.com/CategoricalData/hydra/issues/501) overlay
namespacing (`hydra.overlay.<lang>.*`) later hardened the boundary between translingual generated code and
host-native source; see [overlay-restructuring.md](overlay-restructuring.md).
