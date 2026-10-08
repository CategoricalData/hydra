# Why Java, not Haskell, became the default build host

A retrospective on why Hydra's default build *host* (the program that generates and compiles
code) moved from Haskell to Java, despite Haskell being a strong type-system match for Hydra
and having somewhat faster runtime once compiled. Primary issues:
[#459](https://github.com/CategoricalData/hydra/issues/459),
[#416](https://github.com/CategoricalData/hydra/issues/416),
[#559](https://github.com/CategoricalData/hydra/issues/559),
[#500](https://github.com/CategoricalData/hydra/issues/500),
[#703](https://github.com/CategoricalData/hydra/issues/703),
[#719](https://github.com/CategoricalData/hydra/issues/719),
[#747](https://github.com/CategoricalData/hydra/issues/747),
[#773](https://github.com/CategoricalData/hydra/issues/773),
[#787](https://github.com/CategoricalData/hydra/issues/787),
[#788](https://github.com/CategoricalData/hydra/issues/788),
[#789](https://github.com/CategoricalData/hydra/issues/789).

This is not an argument that Haskell is a poor fit for Hydra. It remains the kernel's most
natural type-system match, and the compiled Haskell host still slightly outperforms the Java
host at runtime for most tasks. This is specifically about the cost of using Haskell as a
**build host** — the program that reads Hydra's own DSL sources and generates code for every
target language — a role distinct from Haskell's merits as a target language or as the kernel's
specification language. The goal is to keep that distinction visible so a future session doesn't
re-couple the build to Haskell without understanding what that costs, and doesn't mistake "Java
is the default host" for "Haskell has been fully retired" — it hasn't.

## The founding argument (#459)

[#459](https://github.com/CategoricalData/hydra/issues/459) is the decision record, and its own
words are worth quoting rather than paraphrasing:

> The Haskell host still slightly outperforms the Java host at most tasks... [but] the latter has
> one major advantage: Maven Central manages Hydra binaries, whereas Hackage only provides Hydra
> sources... anyone who wants to build Hydra has to wait for Cabal to download and build
> Hydra-Haskell from source, which is very time-consuming... Java wins because of bytecode.

That is the thesis in one sentence: Maven Central ships ready-to-run **bytecode**; Hackage ships
**source** that still requires a local GHC compile before it does anything. The same asymmetry
holds for PyPI (importable wheels) versus Hackage. `docs/build-system.md` states it independently
in two places (lines ~306, ~619-623), using the same "bytecode vs. sources" framing.

#459 itself is still open — it was a scoping decision, not a single PR — and the deeper
promotion of the build into Hydra's own `hydra.build.*` is tracked separately by
[#416](https://github.com/CategoricalData/hydra/issues/416) (open) and
[#559](https://github.com/CategoricalData/hydra/issues/559) (closed). #559's own readiness bar is
the clearest statement of how partial this independence is meant to be:

> Switch the default build host from Haskell to Java and the build still works for everything
> except the one intrinsically-Haskell part: the hydra-haskell DSL→JSON path... Haskell may
> remain the actual default and source of truth for most packages for now.

`hydra.json` confirms the result today: `generators.default` is `"java"`, but
`generators.bootstrapSeedHost` is still `"haskell"`. Java generates; Haskell still seeds the
bootstrap. That is the precise shape of "partial independence" — not a completed migration.

## Why this isn't just a missing publishing pipeline

It would be a smaller story if Haskell's binaries simply weren't published to Hackage yet as a
matter of unfinished plumbing. The documentation is explicit that the gap is structural, not
incidental, for the one artifact that matters most: the kernel itself.

`docs/build-system.md` (~687-689, ~764-768) states that the kernel is **always** compiled from
the co-generated `dist/haskell/hydra-kernel`, never consumed from a published Hackage release,
because doing so would link every generated coder package against a potentially *stale* kernel —
the exact failure mode behind [#500](https://github.com/CategoricalData/hydra/issues/500) and
[#608](https://github.com/CategoricalData/hydra/issues/608) (both documented in
[published-host-consume-model.md](published-host-consume-model.md)). The sync script's own
comment (`heads/haskell/bin/sync-haskell.sh:37-44`) puts it most bluntly: Haskell's
`--published-host` default is **"today equivalent to `--local-host`, since no secondary coders
are yet on Hackage."** Java and Python each have a real behavioral fork between published and
local mode. Haskell's fork is a no-op, because the one thing every build needs — the kernel —
can never safely come from Hackage at all.

The documented reason for that wall is a self-consistency hazard ("oil and water": the published
host is a different program from the working tree, and linking published *compiled types*
against working-tree-generated code mixes two revisions that must not mix), not a claim that
Hackage is incapable of distributing binaries. Java and Python's coders avoid this hazard
entirely because they consume the kernel only as *data* (JSON) at a well-defined interchange
boundary — never by linking against compiled kernel types. That data-versus-linked-types
distinction is the real reason Haskell can't simply adopt the same "fetch and go" model Java and
Python already have, independent of whatever one believes about GHC's broader binary
ecosystem (which has never had a cross-version ABI-stability story like the JVM's, and so has
nothing resembling Maven Central for precompiled GHC binaries — a point worth naming for context,
though it is background knowledge, not something Hydra's own docs argue from directly).

## The accumulated operational cost

Each of the following is a separate, concrete incident — not a restatement of the same point —
but they share one root: Haskell-as-host requires a local compile before it can do anything,
and that compile is expensive in ways a fetched artifact never is.

- **Real, measured cost on every cold environment.**
  `docs/contributor-setup.md` (~76-82) gives Haskell's Tier 1 setup a dedicated warning that Java
  and Python's setup sections don't need: `stack setup --install-ghc` downloads ~700MB of GHC,
  and the first `stack build hydra:lib` compiles roughly 80 transitive dependencies plus Hydra's
  own ~800 modules — **~5.7GB on disk, ~20+ minutes**, before any Hydra work can start. There is
  no equivalent paragraph for Java ("install a JDK") or Python ("install Python").

- **A dedicated fleet-wide lock primitive exists only because of this cost.**
  `bin/with-stack-slot.sh` (#730) serializes access to the shared `~/.stack` toolchain across
  every worktree and agent session on a machine, because concurrent GHC/stack builds contend
  heavily enough for CPU and disk to need their own arbitration. Java and Python have no
  equivalent lock, because fetching a jar or a wheel is not a resource-contention risk the way a
  multi-gigabyte parallel GHC compile is.

- **A structural cold-bootstrap circularity that needed a cross-host workaround.**
  A truly empty `dist/haskell` cannot be regenerated by `sync-haskell.sh` alone — generation
  needs *some* already-built Haskell host to run, and no Haskell binary can be fetched to break
  that circularity the way a JVM jar could. [#703](https://github.com/CategoricalData/hydra/issues/703)
  replaced the old Haskell cold-seeder (which linked HEAD Haskell types against a *published*
  Hackage kernel — itself structurally broken for any breaking kernel-shape change, the same
  #500/#608 failure class) with a Java-host, purely data-driven JSON→Haskell generator that is
  immune by construction, since it never links against a host kernel at all.

- **An unconditional full-kernel build on every sync, regardless of relevance.**
  [#773](https://github.com/CategoricalData/hydra/issues/773) found that `bin/sync.sh` ran a full
  930-module Haskell `stack build` before any host/target work — **171 minutes on a cold
  `.stack-work`** — even for a pure `java→python` sync that never touches Haskell. The root cause
  was `sync.sh` using a Haskell executable (`update-json-main`) as its JSON/inference
  orchestration backbone unconditionally. A Haskell-free path already existed and was already
  trusted (the same `transform-json-to-target.sh` that #703 uses) but wasn't wired into the
  general sync pipeline until this fix added a fast path that skips the Haskell build entirely
  when neither host nor target needs it.

- **A forced Haskell dependency that outlived its own justification.**
  [#719](https://github.com/CategoricalData/hydra/issues/719) found a scale-distinct decimal
  test-case filter that lived only in the Haskell driver, so generating into any of the four Lisp
  dialects forced `GENERATOR_HOST=haskell` to avoid emitting unfiltered tests — pulling the full
  Haskell kernel into every Lisp-targeted sync. [#727](https://github.com/CategoricalData/hydra/issues/727)
  gave all four Lisp dialects real scale-preserving decimals, and
  [#735](https://github.com/CategoricalData/hydra/issues/735) moved the filter itself into kernel
  data so both drivers implement it identically — but the `GENERATOR_HOST=haskell` override
  was never removed once its justification disappeared.
  [#787](https://github.com/CategoricalData/hydra/issues/787) tracks removing it, extending
  #773's fast path to Lisp cells.

- **A multi-hour javac hang, reachable only because of the Haskell force above.**
  [#747](https://github.com/CategoricalData/hydra/issues/747) root-caused a deterministic 2+ hour
  `javac` hang (confirmed via `jstack`, a `DeferredAttr` combinatorial blowup under an incomplete
  classpath) in the Java generator's own driver code — a path that was normally dead *because* of
  #719's Haskell force, and only became reachable once something tried to route Lisp generation
  through Java instead. This is a useful caution against treating "stop forcing Haskell" as a
  free lunch: the Java-as-host alternative isn't friction-free either, it just fails differently.
  The real fix was a fail-fast guard, not a reversion to the Haskell force.

- **Orchestration logic duplicated by hand per host, because it was never promoted.**
  [#788](https://github.com/CategoricalData/hydra/issues/788) traces a real `scala→java`
  compile failure (100 "cannot find symbol" errors) to the lib-pass orchestration — which modules
  are `hydra.core.lib.*`, how their defaults are lowered and redirected — being hand-written
  separately in each host's own bootstrap driver, rather than generated once from a single
  translingual source. The coders themselves are generated and guaranteed identical; the
  orchestration *around* them is not, and Java's and Scala's hand-written copies had quietly
  drifted apart. The issue names this explicitly as the same drift class as #719, #773, and #787.

- **A disk-exhaustion incident that produced silent, not loud, corruption — and then took down
  work that had nothing to do with Haskell at all.**
  [#789](https://github.com/CategoricalData/hydra/issues/789), filed the same night as this
  document, found that a `sync-haskell.sh` run interrupted by a full disk left a truncated
  `dist/haskell/hydra-kernel` (48 generated files instead of several hundred, entire core modules
  missing) with no error at the point of failure — the problem only surfaced several steps later
  as a confusing, unrelated-looking `Could not find module` compile error. The immediate trigger
  was a fleet-wide disk-space crisis caused by the accumulated `.stack-work` build-cache footprint
  of dozens of worktrees (5-7GB each, 107GB observed across one machine) — itself a direct
  consequence of Haskell-as-host requiring that large a local build cache in the first place, where
  a prebuilt-artifact host needs none at all.

  What the filed issue doesn't capture is how far the blast radius extended once that cache
  footprint actually ran the machine out of room. Over several hours the same night, root disk
  dropped from single-digit gigabytes to literal kilobytes free — at one point `df` itself failed
  to report, because the session's own temp filesystem had nothing left to write its output to.
  At that depth, the failure stopped being "a Haskell build runs out of disk" and became "no
  process on the machine can reliably write anything": syncs for Java, Python, Scala, and RDF/SHACL
  work queued behind the shared build-slot mutex all failed or stalled, one GHC compile deadlocked
  outright (confirmed by sampling its CPU ticks across a real time window and finding zero progress)
  and sat holding the slot for the rest of the fleet until it was identified and killed, and session
  after session — most of them working on issues that touch Java, Python, RDF, or JSON parsing, not
  Haskell internals — spent the night blocked, not on their own bugs, but on free bytes on disk.
  **None of the queued work was Haskell work.** The queue was Java syncs, Python test runs, a SHACL
  coder fix, a JSON parser fix — ordinary, unrelated engineering, stopped cold by a resource
  footprint that exists only because the build's Haskell side needs to compile itself from source
  on every worktree that has ever touched it. This is the cost compounding at its most literal: not
  a slow build or a forced dependency in one script, but the whole shared machine grinding to a
  halt over a problem that had nothing to do with whatever any individual session was actually
  trying to accomplish. The practical fix that night was manual and immediate — deleting
  `.stack-work` in the largest idle-or-sacrificable worktrees to claw back tens of gigabytes at a
  time — not a code change; the structural fix (a shared cache, or an automated retention policy)
  remains open.

  **It recurred.** The night of 2026-10-07/08 — days later, same machine — root disk again reached
  0 bytes free, with the identical `df`-itself-can't-write symptom and the identical root cause:
  eleven worktrees' `.stack-work/dist` + `.stack-work/install` trees at 5.7-6GB apiece, none of
  it redundant (`~/.stack`'s shared package database is genuine; each worktree's own compiled
  `.o`/`.hi` output reflects that worktree's own checked-out source and cannot be shared). A
  `/bootstrap` run already in flight died mid-write with `No space left on device`; recovery was
  again entirely manual — a human clearing ~35GB by hand, since nothing in the pipeline detects
  low disk before committing to a multi-gigabyte build. The open structural fix named above is
  still open. The absence of even a pre-flight check (fail fast with a clear message before a
  build starts, rather than crash mid-write) is itself notable: the first incident's finding was
  available in this very document, and the second one still ran into the identical failure with
  no guard in between.

## The lesson

No single incident above is damning on its own — a slow cold build, one test filter forced into
the wrong driver, one orchestration function not yet promoted. The pattern across all of them is
what matters: **a host that must be compiled from source before it can do anything accumulates
operational cost — disk, time, contention, fragility — in a way a host that can simply be fetched
as a finished artifact does not**, and that cost compounds every time something new is
(sometimes implicitly) coupled back to Haskell specifically. Each incident above was fixed on its
own terms, but the fixes share a direction: reduce how much of the build pipeline requires
Haskell to be the thing doing the work, without pretending Haskell has been — or needs to be —
removed from the project. It remains the kernel's home language and, per #459 itself, still the
faster host once it's already built. The cost this document tracks is specifically the cost of
getting it built in the first place, and of carrying that cost on every worktree, every cold
checkout, and every sync that didn't need it.

The sharpest version of that last point, and the one worth remembering past any single incident:
this cost does not stay contained to the work that caused it. A shared machine has one disk, and
a resource footprint that scales with "how many worktrees have ever compiled Haskell locally"
rather than "how much Haskell work is happening right now" will eventually come due for everyone
on that machine at once — including sessions whose own work has nothing to do with Haskell.

The current, shipped mechanics of host selection and publishing — the probe gate, `hostOverrides`,
`--local-host`, cache keying — are documented as the system actually behaves today in
[build-system.md § Consuming published hosts](../build-system.md#consuming-published-hosts) and
[migration-shims.md](../recipes/migration-shims.md); this record exists to explain *why* that
system looks the way it does, not to restate its mechanics.
