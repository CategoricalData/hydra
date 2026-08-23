# Consuming the published host, and how "oil and water" was forged

Retrospective on the decision to build Hydra against the *published* host (from Maven / PyPI / Hackage) by
default rather than a locally-built one, the `--local-host` shim kept as a deliberate escape hatch, and the
two circularity bugs that turned the "don't mix the published host with the working tree" rule into a
named, CI-enforced invariant. Primary issues:
[#369](https://github.com/CategoricalData/hydra/issues/369),
[#370](https://github.com/CategoricalData/hydra/issues/370),
[#500](https://github.com/CategoricalData/hydra/issues/500),
[#608](https://github.com/CategoricalData/hydra/issues/608).

This is a history of *why* the model exists and how its central invariant was discovered. The current
mechanics — the probe gate, `hostOverrides`, `--local-host`, cache keying — are documented as shipped
behavior in [build-system.md § consuming published hosts](../build-system.md#consuming-published-hosts) and
[migration-shims.md](../recipes/migration-shims.md); this record links to those rather than restating them.

## The bootstrap circularity problem

Hydra is self-hosting against a *moving* kernel: the code you edit is also the code that generates and
compiles the build. That raises a non-trivial question — *which host builds the build?* Using a
locally-built host means recompiling the whole host on every kernel edit; using a stale one risks compiling
new generated code against an old kernel. The published-host consume model is the answer to that question,
and its two failure modes (below) are what made the answer precise.

## The forward-compatibility contract (#369)

The precondition was [#369](https://github.com/CategoricalData/hydra/issues/369): freeze `Module` and
`hydra.packaging` so that a version N−1 host can read version N JSON. Without that forward-compatibility
contract, consuming a *prior* release to build the *current* tree would not be safe. Everything downstream
depends on it.

## Deciding to consume the published host (#370)

[#370](https://github.com/CategoricalData/hydra/issues/370) is the thesis: stop using locally-built hosts
for typical operations and retrieve the previously published host instead. The Java, Python, and Haskell
drivers each landed the mode as "consume published by default; probe-gated, `--local-host` shim," wired
through `bin/sync.sh`. Scala never got a published probe and remains local-only to this day.

The payoff was framed in #370 as build speed, but the deeper and subtler win — later rediscovered and
documented — is **decoupling "the compiler you run" from "the code you edit"**: the host is recompiled once
per worktree, and kernel edits re-run only *generation*, not a full host rebuild. The output is identical
either way; what changes is the invalidation scope.

## The escape hatch by design: the --local-host shim

The `--local-host` shim was named in #370 from day one, not bolted on later. Backward-incompatible kernel
changes — where the current tree cannot be built by any previously published host — still require building
a host locally. The design position was that this is an *occasional* shim, downgraded from the normal path
but always present. `hostOverrides[pkg]="local"` forces a single package local while the rest come from the
registry.

## Two circularity bugs that forged "oil and water"

The invariant that the published host must not be mixed with the working tree was not designed up front —
it was retrofitted onto two independent bug post-mortems a month apart, both instances of the same category
error.

### #500: coders linked against a stale kernel

[#500](https://github.com/CategoricalData/hydra/issues/500): in published-Haskell mode, the build dropped
the co-generated kernel source-dirs and linked the generated coder packages (`hydra-pg`, `hydra-rdf`)
against the **stale published kernel** — surfacing as `Not in scope: chooseUniqueLabel` after a term-level
rename. The fix was to **always compile the kernel from the co-generated `dist/haskell/hydra-kernel`**,
never from Hackage — deliberately sacrificing the kernel-recompile speed win for correctness. The interim
`hostOverrides:{haskell:"local"}` was declared "the right interim, not a hack." This is the host-vs-target
split now documented in build-system.md.

### #608: the cold-seeder that linked the published kernel

[#608](https://github.com/CategoricalData/hydra/issues/608): the cold-seeder linked the published
`hydra-kernel` while source-dir'ing HEAD authoring modules, so PRs that add a new kernel type failed with
`Not in scope`-style errors. Same root cause as #500, different tool. The fix was a Terms-free,
single-stage local seeder.

Notably, in both cases the published-linking **read as intentional in a code comment and passed review** —
which is precisely why the resolution was to make the invariant *executable* rather than trust prose.

## Making the invariant executable

The lesson from #500 and #608 — that a prose invariant which looks like design intent gets violated
silently — led to a CI guard (`bin/check-oil-and-water.py`) that enforces the separation mechanically. This
is where "oil and water" crystallized from an ad-hoc description into a named, enforced principle: the
published host and the working-tree kernel do not mix at *runtime*.

## Refinement: runtime vs. build-time, and the avoidable shim

The most easily-misapplied nuance, worked out later, is that oil-and-water is a **runtime** rule, not a
build-time one. DSL authoring sources *are* allowed to depend on the published host as a toolchain. The
anti-pattern that forces an avoidable shim is coupling a kernel rename with switching call sites to the
not-yet-published new name in the same change — which makes the current tree unbuildable by any published
host. Separating those two steps keeps the normal consume path working; see
[migration-shims.md](../recipes/migration-shims.md) for the current guidance.
