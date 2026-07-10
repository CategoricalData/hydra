# Release workflow

This document is for Hydra developers and release engineers.
It is the canonical procedure for preparing, tagging, publishing, and documenting a Hydra release.

All official Hydra implementations are maintained in a single repository,
and their releases are synchronized,
so that compatibility of different Hydra artifacts can be understood
on the basis of their version numbers alone, regardless of the implementation language.
However, this makes it very important to maintain a unified and consistent release process.

For how this process measures up against Apache Software Foundation source-release expectations
(signing, checksums, `NOTICE`, dependency-license attestation) and the gaps that remain, see
[release-audit.md](release-audit.md).

As of May 2026, eight complete Hydra hosts pass the common test suite: Hydra-Haskell, Hydra-Java,
Hydra-Python, Hydra-Scala, Hydra-Clojure, Hydra-Common Lisp, Hydra-Scheme, and Hydra-Emacs Lisp.
The four Lisp dialects share a single coder and serializer. Hydra-TypeScript graduated to a full
head under [#126](https://github.com/CategoricalData/hydra/issues/126); Hydra-Go remains a head bud
(generator works, runtime partial). The bootstrapping-demo matrix currently exercises four hosts
(Haskell, Java, Python, Scala) against all eight test-suite targets.

## Overview

Releases are currently performed from the `main` branch and involve the following steps:

1. Finalize any changes to the Hydra kernel
   (whose source of truth is the `Hydra/Sources/Kernel` directory in Hydra-Haskell).
1. Bump the version number using `bin/bump-version.sh` (see "Version synchronization" below).
   This must happen before syncing
   so that the new version appears in all generated code, documentation, and packages.
1. Regenerate all implementations using `bin/sync-all.sh` (see "Synchronizing all implementations" below).
1. Run `./bin/prepare-release.sh` from the repository root.
   This verifies that all implementations are consistent and passing,
   and produces upload-ready release artifacts in `release-artifacts/`
   (see "Release preparation" below).
1. Update `CHANGELOG.md` (see "Updating the changelog" below).
1. Commit all changes and tag the release
   (e.g. `git tag 0.13.0 -m '0.13.0 release' HEAD`, then `git push && git push --tags`).
1. Publish implementation-specific code artifacts (see the Haskell, Java, and Python sections below).
1. After the published artifacts are visible in their registries, update
   [Releases](https://github.com/CategoricalData/hydra/wiki/Releases)
   with the new release, tag, changelog heading, and package links
   (see "Updating the release index" below).

```bash
# Recommended release commands
bin/bump-version.sh 0.13.0         # Bump version everywhere
bin/sync-all.sh                    # Regenerate all implementations
bin/prepare-release.sh             # Verify + build upload-ready artifacts
# Update CHANGELOG.md, commit, tag, push, publish
# After publication, update the wiki Releases page
```

## Access prerequisites

Publishing requires accounts, roles, and credentials on each external registry.
These are **not** stored in the repo; confirm them *before* a release, since a
missing or wrongly-scoped credential surfaces only at upload time (often as a
`403`). Per-registry:

| Registry | What you need | Where |
|----------|---------------|-------|
| **Hackage** | A Hackage account that is a **maintainer** of `hydra`, `hydra-kernel`, `hydra-haskell`; `cabal` configured to upload (interactive login or an API token). | <https://hackage.haskell.org/> |
| **Maven Central** | A **Sonatype Central Portal** account with publish rights for the `net.fortytwo.hydra` namespace, plus a GPG signing key. Credentials in `~/.gradle/gradle.properties` (`sonatypeUsername`/`sonatypePassword` + `signing.*`). **JDK 17+** to run Gradle (see Java releases). | <https://central.sonatype.com/> |
| **PyPI** | An account that is **Owner or Maintainer** of every published project (`hydra-kernel`, `hydra-rdf`, `hydra-pg`, `hydra-python`), and an API token scoped to **all** of them (account-wide is simplest). Token goes in `~/.pypirc` or is pasted at the `twine`/`uv` prompt. | <https://pypi.org/manage/projects/> (confirm you own the projects) |
| **Maven Central (Scala/sbt)** | Same Sonatype Central Portal account as Java (publish rights for `net.fortytwo.hydra`), plus a GPG signing key. Credentials in `~/.sbt/1.0/sonatype.sbt` (`sonatypeUsername`/`sonatypePassword`) or env vars `SONATYPE_USERNAME`/`SONATYPE_PASSWORD`; GPG via sbt-pgp (reads `~/.gnupg` by default). **JDK 11+** required. | <https://central.sonatype.com/> |
| **npm** | An npm account that is **Owner** of `hydra-kernel`, `hydra-rdf`, `hydra-pg`, and `hydra-typescript` on npmjs.com; an **Automation** token with Read and Publish access (set as `NPM_TOKEN` env var) or an active `npm login` session. | <https://www.npmjs.com/> |
| **conda-forge** | Listed in the `recipe/` maintainers of [`hydra-python-feedstock`](https://github.com/conda-forge/hydra-python-feedstock); updates go via PR to that repo. | the feedstock repo |

**Ownership note.** PyPI (and conda-forge) ownership is per-project and may rest
with a maintainer other than the release engineer (e.g. @phreed has historically
owned the PyPI projects and the conda-forge feedstock). The release engineer must
be added as **Owner** (PyPI) / recipe maintainer (conda-forge) ahead of time — you
cannot self-add, and a `403 Forbidden` *after* a token is accepted means the token's
account lacks upload rights for that project, not a bad token.

## Version synchronization

All implementations share a single version number.
The canonical version lives in `hydra.json` at the repository root as the
`currentVersion` field (the standalone `VERSION` file was retired in #347), and
`bin/bump-version.sh` propagates it to all config files.

`hydra.json` carries two distinct version fields:

- `currentVersion` — the package *release* version this repository builds and
  publishes. Bumped by `bin/bump-version.sh` and fanned out to every host's build
  files (below).
- `hostVersion` — the version of the *published hosts the build/sync depends on*
  (the Merkle basis for the per-target generator cache; see
  [build-system.md](build-system.md)). Bumped separately by
  `bin/bump-host-version.sh`. Per-host exceptions live in the
  `hostOverrides` map and are hand-edited. This field is independent of the
  release flow and is normally left alone during a release.

  One release-time interaction: when a release introduces new public generated modules
  (e.g. 0.17.1's `hydra.dsl.<term-module>` reference DSLs, #467), they become consumable
  from published artifacts only once that release publishes.
  Any `hostOverrides` local pins taken out during the development window (e.g. the
  #524/#467 java/python pins) should be reverted as part of the release, after the
  published artifacts are confirmed visible in their registries.

### Bumping the version

```bash
# Set the new release version and propagate to all config files
bin/bump-version.sh 0.13.0

# Or, if you've already edited hydra.json currentVersion manually:
bin/bump-version.sh
```

The script validates the version format (X.Y.Z), writes it to
`hydra.json:currentVersion` (if a version argument is given), and patches all of
the following files:

| File | Format |
|------|--------|
| `hydra.json` | `"currentVersion": "0.15.0"` |
| `heads/haskell/package.yaml` | `version: 0.15.0` |
| `demos/bootstrapping/resources/haskell/package.yaml` | `version: 0.15.0` |
| `heads/java/build.gradle` | `version = '0.15.0'` |
| `demos/bootstrapping/resources/java/build.gradle` | `version = '0.15.0'` |
| `packages/hydra-scala/build.sbt` | `version := "0.15.0"` |
| `demos/bootstrapping/resources/scala/build.sbt` | `version := "0.15.0"` |
| `heads/python/pyproject.toml` | `version = "0.15.0"` |
| `demos/bootstrapping/resources/python/pyproject.toml` | `version = "0.15.0"` |
| `heads/typescript/package.json` | `"version": "0.15.0"` |
| `demos/bootstrapping/resources/typescript/package.json` | `"version": "0.15.0"` |

The `./bin/prepare-release.sh` script checks that all version strings match.

## Synchronizing all implementations

The `bin/sync-all.sh` script regenerates every generated artifact in the Hydra repository.
This is a long-running operation that runs six phases in order, stopping at the first error.
Use `--no-tests` to skip target-language tests during iterative development.

**Phase 1: DSL → JSON + Haskell kernel** (`heads/haskell/bin/sync-haskell.sh`)
1. Build the required Haskell executables (`update-json-main`, `update-json-test`,
   `update-json-manifest`, `verify-json-kernel`, `bootstrap-from-json`,
   `digest-check`, ...)
2. Export every kernel and test module to JSON under `dist/json/<pkg>/`
   (via `update-json-main` + `update-json-test`).
3. Verify the JSON kernel against the in-memory kernel and write the
   per-package JSON manifest.
4. Regenerate the Haskell kernel and test modules from JSON
   (`bootstrap-from-json --target haskell`).
5. Post-process generated files (today this is a no-op; see
   `docs/recipes/maintenance.md` "Known accepted patches").
6. Run `stack test` (skipped with `--no-tests`).

**Phase 2: Synchronize all target languages**
- From the worktree root, `./bin/sync-all.sh` regenerates every
  (package, target) combination and runs every target's tests.
- Internally this runs `bin/sync-packages.sh`, which performs:
  - **Phase 1** (DSL → JSON): `transform-haskell-dsl-to-json --all` in
    one Haskell universe load, routing per-package via
    `namespaceToPackage`.
  - **Phase 2** (assemble): per-(package, target) call into
    `heads/<lang>/bin/assemble-distribution.sh`, skipping any
    (pkg, target) combinations outside the package's declared
    `targetLanguages`.
  - **Phase 3** (tests): per-target invocation of
    `heads/<lang>/bin/test-distribution.sh`. Fails fast on the first
    failing target.
- Individual targets can be driven with `bin/sync.sh --hosts <H,...>
  --targets <T,...>` (matrix tool), or with per-language wrappers
  (`bin/sync-java.sh`, `bin/sync-python.sh`, etc.) which set host ==
  target for a single language.

**Phase 6: Cross-implementation verification**
- `bin/run-bootstrapping-demo.sh` generates every (host, target)
  combination and compares outputs, confirming each pair bootstraps.

All generation steps use `-K256M -A32M` RTS flags to avoid stack overflow.

The individual sync scripts can also be run separately during development. See the script reference below.

## Release preparation

Before tagging a release, run the preparation script from the repository root:

```bash
./bin/prepare-release.sh
```

This script verifies that all implementations are consistent and passing,
and produces the upload-ready artifacts in `release-artifacts/`:

1. **Version synchronization** — all version files match (`hydra.json:currentVersion`, Haskell, Java, Python, Scala, and bootstrapping resources).
2. **Haskell tests** — `stack test` in `heads/haskell`.
3. **Java tests** — `./gradlew test` from the root (requires Java 11+).
4. **Python tests** — `pytest` plus `ruff check` and `ruff format --check`.
5. **TypeScript tests** — the TypeScript head's own test suite.
6. **Scala tests** — `sbt test` in `packages/hydra-scala`.
7. **Lisp tests** — runs `run-tests.sh` for each dialect (Clojure, Common Lisp, Emacs Lisp, Scheme).
8. **JSON kernel** — verifies the exported JSON kernel round-trips correctly.
9. **Lexicon freshness** — `docs/hydra-lexicon.txt` matches the current Haskell kernel.
10. **Per-package Hackage sdists on case-sensitive filesystem** —
   the Hackage build infrastructure runs on Linux (case-sensitive ext4),
   while macOS HFS+/APFS hides duplicate-with-different-case directory clashes.
   Hydra publishes a per-package Hackage distribution for **every** Hydra Haskell
   package (`hydra-kernel`, `hydra-haskell`, each generated coder — `hydra-coq`,
   `hydra-scala`, `hydra-pg`, `hydra-rdf`, … — and the `hydra` umbrella), rather
   than one monolithic `hydra` sdist. The set is derived from the `hydra.json`
   registry (not hardcoded), so new packages publish automatically (#376). This
   step assembles them all (leaves first) on a
   case-sensitive disk image (macOS) or plain temp dir (Linux) via
   `heads/haskell/bin/publish-hackage.sh`, extracts them into one
   `cabal.project`, and runs `cabal v2-build --dry-run` to catch GHC-28623
   ("file name does not match module name") errors without a full compile.
   The publish script also asserts the publish set is **dependency-closed**
   (no published package may depend on an unpublished Hydra package).
11. **Per-package Haddock-for-Hackage docs** —
   builds a Haddock tarball per package from the validated sdists.
   Hackage's auto-doc-builder is best-effort and frequently fails on packages
   with many transitive deps, so we pre-build locally and upload via
   `cabal upload --documentation --publish` after release
   (see "Haskell releases" below).
12. **Canonical source archive + checksum + signature** —
   a single `git archive` tarball of the tracked source at `HEAD`
   (`hydra-<version>-src.tar.gz`), plus a SHA-512 checksum and a detached GPG
   signature. This is the **release of record**: the per-registry artifacts
   (Hackage sdists, Maven jars, PyPI wheels) are convenience binaries downstream
   of it. The step asserts `LICENSE` and `NOTICE` are present and tracked (so the
   archive contains both, as a source release must). Signing uses the key named by
   `HYDRA_RELEASE_SIGNING_KEY` (else gpg's default); a missing key degrades to a
   warning so the script stays runnable outside a real release, but a real release
   must be signed. See "Verifying a release" below.
13. **Per-host published-package self-containment** —
   runs each host's `verify-distribution.sh` gate, which builds that host's
   publish-set packages from the `dist/` tree alone and proves they are
   self-contained when consumed as published artifacts, in isolation from the
   worktree (see the `verify-distribution.sh` row in the script reference below).

On success the script writes, for each of `hydra-kernel`, `hydra-haskell`, `hydra`:

- `release-artifacts/<pkg>-<version>.tar.gz`      — the Hackage sdist
- `release-artifacts/<pkg>-<version>-docs.tar.gz` — the Haddock-for-Hackage docs

and the canonical source release:

- `release-artifacts/hydra-<version>-src.tar.gz`        — the source archive (release of record)
- `release-artifacts/hydra-<version>-src.tar.gz.sha512` — its SHA-512 checksum
- `release-artifacts/hydra-<version>-src.tar.gz.asc`    — its detached GPG signature (if signed)

Per-step logs land in `verify-logs/`. All checks must pass before proceeding with the release.

## Verifying a release

The canonical source archive is signed and checksummed so that anyone can confirm a
download is authentic and untampered. The public signing keys live in the repo-root
[`KEYS`](https://github.com/CategoricalData/hydra/blob/main/KEYS) file.

```bash
# Import the project signing keys (once)
gpg --import KEYS

# Verify the detached signature and the checksum
gpg --verify hydra-<version>-src.tar.gz.asc hydra-<version>-src.tar.gz
shasum -a 512 -c hydra-<version>-src.tar.gz.sha512   # or sha512sum -c on Linux
```

A good signature from a key listed in `KEYS`, plus a matching checksum, confirms the
release. Release managers register their signing key by appending it to `KEYS` (never
replacing existing keys) — see the procedure inside that file.

## Updating the changelog

Update `CHANGELOG.md` before committing and tagging the release.
The format is inspired by [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

### Structure

Each release entry should include:

- A **version header** with the release date: `## [0.14.0] - 2026-03-29`
- A brief **summary paragraph** describing the theme of the release.
- A **Highlights** section with 3-4 bullet points calling out the most important changes.
- Categorized subsections as applicable:
  - **New Features** — new capabilities, grouped by area.
  - **Improvements** — enhancements to existing functionality.
  - **Bug Fixes** — corrected behavior, with enough detail to identify the issue.
  - **Documentation** — new or updated docs.

### Guidelines

- **Base entries on closed GitHub issues.**
  Start by reviewing closed issues for the release.
  Each significant issue should have a corresponding changelog entry.
- **Always include the issue number** (`#NNN`) so entries are traceable.
- **One line per item is usually enough.**
  Keep entries short and concise; the entry is a pointer, not an explanation.
  The detail lives in the linked issue — let the `#NNN` link carry it rather than
  re-describing the change in the changelog. The Highlights section provides
  context; the subsections are a concise inventory.
- **The Highlights section is 3–4 bullets — no more.**
  Highlights call out only the most important changes; everything else belongs in
  the subsections. If you find yourself adding a fifth highlight, it is a subsection
  entry instead.
- **Verify the linked issue actually describes the solution.**
  Because each entry defers detail to its `#NNN` link, that only works if the issue
  contains the detail. Filed issues often describe a *problem* or an early plan, and
  the implemented solution can drift from it (renamed differently, broader/narrower
  scope, a different mechanism than proposed). When writing the changelog, skim each
  cited issue: if it does not adequately describe what actually shipped, **propose a
  short clarifying comment on the issue** capturing the final resolution (and get it
  approved before posting, per the no-unauthorized-GitHub-actions rule). The goal is
  that a reader following the changelog link lands on an accurate account of the
  change, not a stale problem statement.
- **Don't repeat between sections.**
  If something is in Highlights, a brief line in New Features suffices — don't re-explain.
- **Don't list bug fixes for new features.**
  If a feature was introduced and debugged in the same release cycle, that's just implementation.
  Only list bugs that affect functionality present in a prior release.
- **List changes across all implementations**, not just Haskell.
- See previous entries in `CHANGELOG.md` for examples.

## Updating the release index

Update [Releases](https://github.com/CategoricalData/hydra/wiki/Releases)
after the release is published and the package registries expose the new artifacts.
This is a post-publication documentation step, because the page should link to concrete published packages
rather than anticipated package locations.

For each release, add or update:

- the minor-version section, if it does not already exist
- the exact patch release line with date
- the GitHub tag link
- the matching `CHANGELOG.md` heading link
- published package links for every registry used by that release
- a short summary matching the changelog theme

Use version-specific package links when the registry supports them, especially for Maven Central artifacts.
Do not emphasize temporary namespaces such as `net.fortytwo.hydra` in link text; use artifact names like
`hydra-kernel`, `hydra-java`, `hydra-pg`, and `hydra-rdf`.

## Publishing to registries

The per-registry artifacts below — Hackage sdists, Maven Central jars, PyPI wheels, conda packages —
are **convenience binaries**: downstream of, and secondary to, the canonical signed source archive
(see [Release preparation](#release-preparation) Step 12 and [Verifying a release](#verifying-a-release)).
The source archive is the release of record; the registry artifacts are how consumers obtain usable
packages without running the codegen toolchain themselves.

## Haskell releases

We have Haskell code in `packages/hydra-kernel/` (kernel DSL sources),
`packages/hydra-haskell/` (Haskell coder DSL sources),
`heads/haskell/` (runtime, code generation, and sync scripts),
and domain packages (`packages/hydra-pg/`, `packages/hydra-rdf/`, `packages/hydra-ext/`).
The following are Haskell-specific release steps:
* Set up your Haskell environment as described in the
  [Hydra-Haskell README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-haskell/README.md).
* Check for the latest LTS version of Stack [on Stackage](https://www.stackage.org),
  then bump `resolver: lts-xx.yy` in
  [heads/haskell/stack.yaml](https://github.com/CategoricalData/hydra/blob/main/heads/haskell/stack.yaml)
  to this version.
  The next build may take some time, as a new version of GHC is downloaded and installed.
* Update dependency versions (starting with `base`) in
  [heads/haskell/package.yaml](https://github.com/CategoricalData/hydra/blob/main/heads/haskell/package.yaml)
  for compatibility with the new Stack LTS version,
  and also remove any no-longer-needed dependencies.
* Initiate the release of the Haskell packages to Hackage.
  Starting with 0.16, Hydra publishes **per-package** Hackage distributions
  rather than one monolithic `hydra` package (#418), mirroring the per-package
  layout already used for Java (Maven Central) and Python (PyPI).
  The publish set is **derived from the `hydra.json` registry** (deps-first topo
  order) plus the hand-written `hydra` umbrella — so it always covers every Hydra
  Haskell package and grows automatically as packages are added, with no hardcoded
  list to fall out of sync (#376). It began as the 0.16.0 trio (`hydra-kernel`,
  `hydra-haskell`, `hydra`); since 0.17.0 all generated coder packages
  (`hydra-coq`, `hydra-scala`, `hydra-pg`, `hydra-rdf`, …) publish too. The set is
  always **dependency-closed** (asserted before upload).
  * `bin/prepare-release.sh` already produced the upload-ready per-package sdists
    and Haddock-for-Hackage tarballs under `release-artifacts/` (one
    `<pkg>-<version>.tar.gz` + `<pkg>-<version>-docs.tar.gz` per package).
    Each package is assembled by `heads/haskell/bin/assemble-haskell-distribution.sh`,
    which stages the generated `dist/haskell/<pkg>/` tree plus the hand-written
    head modules it needs (for `hydra-kernel`, the `Hydra.Overlay.Haskell.Lib.*` primitive
    implementations and the `Hydra.Settings`/`Hydra.Kernel` entry points) into a
    self-contained tree and runs `stack sdist`. (This replaces the 0.15-era
    `assemble-hackage-sdist.sh`, which flattened everything into one tarball.)
  * **Upload candidates first, then publish.** Hackage candidates are mutable and
    overwritable (re-uploading the same version replaces the candidate), so use them
    to verify rendering before the irreversible publish. A *published* version is
    permanent — it can never be re-uploaded or overwritten (you would have to bump to
    `<version>.1`). The recommended flow:
    ```bash
    heads/haskell/bin/publish-hackage.sh           # assemble only (no upload), catches glitches
    heads/haskell/bin/publish-hackage.sh --upload  # upload CANDIDATES; review on Hackage
    heads/haskell/bin/publish-hackage.sh --publish  # FINAL (irreversible)
    ```
  * **The sdist must bundle `CHANGELOG.md`** or Hackage shows "Change log: None
    available". `assemble-haskell-distribution.sh` copies the repo `CHANGELOG.md` into
    each package's sdist root and the generated `.cabal` lists it under
    `extra-source-files` (added 0.16.0). If you see "None available" on a candidate,
    the CHANGELOG was not bundled — fix the assembler/generator, not the tarball.
  * Upload **leaves first** — order matters, because the umbrella and dependents
    pin their siblings with `== <version>`; uploading a dependent before its
    dependency leaves it transiently unsatisfiable on Hackage. The publish
    script enforces this order:
    ```bash
    heads/haskell/bin/publish-hackage.sh --publish
    ```
    Or upload manually in order (`hydra-kernel`, then `hydra-haskell`, then `hydra`):
    ```bash
    cabal upload --publish release-artifacts/hydra-kernel-<version>.tar.gz
    cabal upload --publish release-artifacts/hydra-haskell-<version>.tar.gz
    cabal upload --publish release-artifacts/hydra-<version>.tar.gz
    ```
    (or click the Upload link at <https://hackage.haskell.org/upload>). Each
    version becomes visible on its package page immediately.
  * Upload pre-built docs (same leaves-first order):
    ```bash
    cabal upload --documentation --publish release-artifacts/hydra-kernel-<version>-docs.tar.gz
    cabal upload --documentation --publish release-artifacts/hydra-haskell-<version>-docs.tar.gz
    cabal upload --documentation --publish release-artifacts/hydra-<version>-docs.tar.gz
    ```
    Hackage's auto-doc-builder is unreliable for packages with many transitive
    deps, so we always upload pre-built docs rather than trusting the
    auto-build. The docs tarballs were already validated by `prepare-release.sh`,
    so this is just an upload — no second build.
  * Check the Hackage releases when ready:
    [hydra](https://hackage.haskell.org/package/hydra),
    [hydra-kernel](https://hackage.haskell.org/package/hydra-kernel),
    [hydra-haskell](https://hackage.haskell.org/package/hydra-haskell).

## Java releases

Hydra-Java is a **complete Hydra implementation** that passes all tests in the common test suite.
It requires Java 11 or later.

We have Java code in `packages/hydra-java` (DSL coder sources) and `heads/java/`
(the runtime, gradle build, and tests).
See the Gradle configuration at
[heads/java/build.gradle](https://github.com/CategoricalData/hydra/blob/main/heads/java/build.gradle)
and [heads/java/settings.gradle](https://github.com/CategoricalData/hydra/blob/main/heads/java/settings.gradle).

Starting with 0.15, the Java release ships **per-package Maven artifacts** rather than a single
`hydra-java` rollup.
Each `dist/java/<pkg>/` directory is a self-contained, publishable Gradle build with a generated
`build.gradle` and `settings.gradle`.
The published artifacts under group `net.fortytwo.hydra.java` (#519) are:

| Artifact | Description | `api` dependencies |
|----------|-------------|--------------------|
| `hydra-kernel` | Core types, terms, DSL, eval, primitives + the Java runtime support classes (`hydra.util.*`, `hydra.lib.*`, `hydra.dsl.*`, `hydra.tools.*`, plus `Adapters`/`Coders`). Self-contained; downstream packages depend on this. | (none) |
| `hydra-build` | Build/packaging model shared across coders. | `hydra-kernel` |
| `hydra-haskell` | Haskell syntax and coder (generates Haskell code from Hydra schemas). | `hydra-kernel` |
| `hydra-jvm` | JVM serde support shared by the JVM coders. | `hydra-kernel` |
| `hydra-java` | Java syntax, serde, and coder (generates Java code from Hydra schemas). | `hydra-kernel` |
| `hydra-python` | Python syntax and coder. | `hydra-kernel` |
| `hydra-scala` | Scala syntax and coder. | `hydra-kernel` |
| `hydra-lisp` | Lisp syntax and the shared coder for the four Lisp dialects (Clojure, Common Lisp, Emacs Lisp, Scheme). | `hydra-kernel` |
| `hydra-typescript` | TypeScript syntax and coder. | `hydra-kernel` |
| `hydra-rdf` | RDF, OWL, SHACL, ShEx, XML Schema models. | `hydra-kernel` |
| `hydra-pg` | Property graph model, coders, GraphSON, TinkerPop. | `hydra-kernel`, `hydra-rdf` |

Starting with 0.16.1, the publish set covers a Java/Maven artifact for **every official Hydra
target language** — one coder package per implementation family
(`hydra-haskell`/`-java`/`-python`/`-scala`/`-lisp`/`-typescript`, covering all nine of the
implementation languages), plus the `hydra-kernel` base, the `hydra-build`/`hydra-jvm` support
packages, and the `hydra-pg`/`hydra-rdf` domain packages (#468, #505, #508). The authoritative Java
publish set lives in `PUBLISH_SET` in `heads/java/bin/publish-maven.sh`. Two related sets are
*intentionally narrower* and no longer identical to it: `batch_emit_packages()` in
`bin/lib/assemble-common.sh` (the coder packages the batch assembler emits a `build.gradle` for —
kernel plus the six coders, omitting `hydra-jvm`/`hydra-build`/`hydra-pg`/`hydra-rdf`) and
`PUBLISHED_HOSTS` in `bin/lib/hydra-packages.py` (which packages resolve to a published *host*
version — now including `hydra-jvm`). The newer `hydra-coq`, `hydra-go`, and `hydra-wasm` targets
do NOT yet qualify.

`hydra-ext` (Avro, Protobuf, GraphQL, Pegasus, etc.) is intentionally NOT in the Java
publish set due to a known Java-coder limitation with parametric union case-elimination on
concretely-instantiated arguments. Track this before adding `hydra-ext` to a future Java publish set.

Each artifact's `build.gradle` is regenerated from `packages/<pkg>/package.json` (which declares
the inter-package `dependencies` array) and `hydra.json:currentVersion` by
`bin/lib/generate-java-package-build.py`.
The kernel additionally has the hand-written runtime support overlaid in from
`overlay/java/hydra-kernel/` by `heads/java/bin/copy-kernel-runtime.sh` (#418).
Both are invoked automatically from `heads/java/bin/assemble-distribution.sh`,
so a clean `bin/sync.sh` produces ready-to-publish trees.

The following are Java-specific release steps:
* Set up your Java environment as described in the
  [Hydra-Java README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-java/README.md).
* Update the JavaDocs.
  * A combined JavaDoc site spanning hydra-kernel, hydra-rdf, hydra-pg, and hydra-java
    is automatically generated and deployed to GitHub Pages when a new tag is pushed.
    The `pages.yml` workflow fires after CI succeeds on the tag, pulls the
    `dist-java` artifact from that CI run, and javadocs the combined source tree
    in a single invocation so cross-package `@link`s resolve.
    No manual steps are needed for JavaDoc publishing.
  * Check the updated combined JavaDocs
    [here](https://categoricaldata.net/hydra/hydra-java/javadoc)
    after the tag is pushed. (hydra-ext is excluded until it ships to Maven Central; #451.)
* Publish each artifact to Maven Central via the [Central Portal](https://central.sonatype.com).
  * **JDK requirement:** the `nmcp` plugin (which the generated `build.gradle` uses to talk
    to the Central Portal publisher API) requires JDK 17+ to *run* Gradle, even though it
    targets Java 11 bytecode.
    Use a native arm64 JDK 18 or 19 on Apple Silicon (the Oracle JDK 17 build is x86_64
    and runs under Rosetta).
  * **One-time setup:** Generate a user token at
    [Central Portal - Account](https://central.sonatype.com/account)
    and add credentials to `~/.gradle/gradle.properties`
    (do NOT check this file in):
    ```
    sonatypeUsername=<token-username>
    sonatypePassword=<token-password>
    signing.keyId=<short key id>
    signing.password=<key passphrase>
    signing.secretKeyRingFile=/Users/<you>/.gnupg/secring.gpg
    ```
  * **Use the orchestrator** `heads/java/bin/publish-maven.sh` (analogous to
    `publish-hackage.sh`). It checks the JDK + credentials + dependency closure, refreshes
    each package into `~/.m2` leaves-first, then runs `publishAggregationToCentralPortal` per
    package. Default is a build-only dry run; `--upload` does the real Central upload:
    ```bash
    export JAVA_HOME=$(/usr/libexec/java_home -v 19)   # JDK 17+ required (nmcp plugin)
    heads/java/bin/publish-maven.sh                    # dry run: build all published artifacts, no upload
    heads/java/bin/publish-maven.sh --upload           # upload pending deployments
    ```
    Or manually, in dependency order, from each `dist/java/<pkg>/`:
    ```bash
    cd dist/java/hydra-kernel && gradle publishAggregationToCentralPortal   # then rdf, pg, java
    ```
    The task chain is: build the jar + Javadoc + sources jars, generate a signed POM,
    package everything into a Central Portal-shaped zip, and upload it via the publisher API.
    The generated `build.gradle` sets `publishingType = "USER_MANAGED"`,
    so each upload lands in the Central Portal UI as a separate pending deployment for review
    (the analog of a Hackage candidate — nothing is live until you click Publish).
  * **Stale `~/.m2` trap (important).** Across a pre-release cycle the version string
    (e.g. `0.16.0`) does not change, so a previously-built same-version jar can linger in
    `~/.m2` and silently satisfy a downstream package's `api 'net.fortytwo.hydra.java:hydra-kernel:0.16.0'`
    dependency — making `hydra-rdf`/`hydra-pg`/`hydra-java` compile against an **old** kernel and
    fail with `cannot find symbol` for classes added/renamed mid-cycle (e.g. `hydra.typing.InferenceContext`,
    `hydra.util.Optional`). `/sync` regenerates the *source* in `dist/java/` but does not touch
    `~/.m2`. The fix is to `publishToMavenLocal` each package leaves-first (with
    `--refresh-dependencies`) before building dependents — `publish-maven.sh` does this
    automatically. If publishing manually, run `gradle publishToMavenLocal` on each package in
    dependency order first.
  * **Javadoc fail-on-error (#449).** The Java coder currently emits unqualified `@link` refs for
    nested case-classes (`{@link hydra.ast.None}` instead of `hydra.ast.Associativity.None`),
    which the strict `withJavadocJar` Javadoc task treats as fatal — failing the build. These are
    cosmetic (the jars compile fine). To publish before #449 is fixed, pass `--allow-javadoc-errors`
    to `publish-maven.sh`, which applies the transient init script
    `heads/java/bin/javadoc-nonfatal.init.gradle` for that run only (the generated build.gradle stays
    strict, so a plain `gradle build` keeps failing on #449 until it is fixed). Remove the flag and
    the init script once #449 lands.
  * Go to [Deployments](https://central.sonatype.com/publishing/deployments) in the Central Portal.
    Find the eleven deployments, verify they have passed validation, then click "Publish" on each.

It will take a short time (a couple of minutes) for validation,
and a longer time (as little as 15 minutes, or as much as nearly an hour) for promotion,
before you can find the release through [Maven Central](https://central.sonatype.com).
Hydra releases are [here](https://central.sonatype.com/search?q=net.fortytwo.hydra).

A consumer of any of the above artifacts only needs to declare the highest-level
dependency they care about; transitive resolution pulls the rest:

```gradle
dependencies {
    implementation 'net.fortytwo.hydra.java:hydra-pg:0.17.0'   // pulls hydra-rdf and hydra-kernel
}
```

## Scala releases

Hydra-Scala is a **complete Hydra implementation** that passes all tests in the common test suite.
It requires Scala 3.3 or later and sbt 1.10+.

Starting with 0.17, the Scala release ships **per-package Maven artifacts** under group `net.fortytwo.hydra.scala` (#519),
mirroring the Java publish set.
Each `dist/scala/<pkg>/` directory is a self-contained, publishable sbt build with a generated
`build.sbt` and `project/`.
The published artifacts mirror the Java set (with `hydra-pg` still excluded, below):

| Artifact | Description | `libraryDependencies` |
|----------|-------------|-----------------------|
| `hydra-kernel_3` | Core types, terms, DSL, eval, primitives + Scala runtime support. Self-contained. | (none) |
| `hydra-build_3` | Build/packaging model shared across coders. | `hydra-kernel_3` |
| `hydra-haskell_3` | Haskell syntax and coder. | `hydra-kernel_3` |
| `hydra-jvm_3` | JVM serde support shared by the JVM coders. | `hydra-kernel_3` |
| `hydra-java_3` | Java syntax, serde, and coder. | `hydra-kernel_3` |
| `hydra-python_3` | Python syntax and coder. | `hydra-kernel_3` |
| `hydra-scala_3` | Scala syntax and coder. | `hydra-kernel_3`, `hydra-java_3` |
| `hydra-lisp_3` | Lisp syntax and the shared coder for the four Lisp dialects. | `hydra-kernel_3` |
| `hydra-typescript_3` | TypeScript syntax and coder. | `hydra-kernel_3` |
| `hydra-rdf_3` | RDF, OWL, SHACL, ShEx, XML Schema models. | `hydra-kernel_3` |
| `hydra-pg_3` _(not yet published)_ | Property graph model, coders, GraphSON, TinkerPop. | `hydra-kernel_3`, `hydra-rdf_3` |

`hydra-pg` is **not** in the current Scala publish set — the generated Scala pg coder has a
type-variable threading issue that prevents standalone compilation. Track the fix separately before
adding it (analogous to `hydra-ext` being excluded from the Java and Python publish sets).

Each artifact's `build.sbt` is regenerated from `packages/<pkg>/package.json` and
`hydra.json:currentVersion` by `bin/lib/generate-scala-package-build.py`, and
wired into `heads/scala/bin/assemble-distribution.sh` so a clean `bin/sync.sh` produces
ready-to-publish trees.

The following are Scala-specific release steps:

* Set up your Scala environment (sbt, JDK 11+, GPG).
* **One-time setup:** Create a Sonatype Central Portal account and obtain a user token at
  <https://central.sonatype.com/account>.
  Store credentials in `~/.sbt/1.0/sonatype.sbt` (do NOT check this file in):
  ```scala
  credentials += Credentials(
    "Sonatype Nexus Repository Manager",
    "central.sonatype.com",
    "<token-username>",
    "<token-password>"
  )
  ```
  Or export `SONATYPE_USERNAME` / `SONATYPE_PASSWORD` in the environment.
  Configure GPG for signing (sbt-pgp reads `~/.gnupg` by default).
* **Use the orchestrator** `heads/scala/bin/publish-sbt.sh` (analogous to `publish-maven.sh`).
  Default is a dry run (builds jars locally, no upload); `--upload` does the real Central upload:
  ```bash
  heads/scala/bin/publish-sbt.sh             # dry run: build all jars, no upload
  heads/scala/bin/publish-sbt.sh --upload    # upload pending deployments
  ```
  The orchestrator checks dependency closure, publishes each package to the local ivy cache
  leaves-first (so downstream builds resolve fresh siblings), then runs `sbt publishSigned`
  per package in topological order.
* Go to [Deployments](https://central.sonatype.com/publishing/deployments) in the Central Portal.
  Find the ten deployments, verify they have passed validation, then click "Publish" on each
  (leaves first).
* **Self-containment gate (#537):** `heads/scala/bin/verify-distribution.sh` publishes the publish
  set to a temp Ivy repo (`sbt --ivy <tmp>`) and builds a throwaway sbt consumer against the
  published `_3` coordinates, proving each jar carries the classes its API needs in isolation from
  the worktree. Run automatically in CI and as a hard gate in `prepare-release.sh` (Step 13); see
  the `verify-distribution.sh` row in the script reference table below.

A Scala consumer adds e.g.:
```scala
libraryDependencies += "net.fortytwo.hydra.scala" % "hydra-kernel_3" % "0.17.0"
```
or, using sbt's `%%` operator (which appends `_3` automatically):
```scala
libraryDependencies += "net.fortytwo.hydra.scala" %% "hydra-kernel" % "0.17.0"
```

## Python releases

Hydra-Python is a **complete Hydra implementation** that passes all tests in the common test suite.
It requires Python 3.12 or later
and uses [uv](https://github.com/astral-sh/uv) for dependency management.

Starting with 0.15, the Python release ships **per-package wheels** rather than a single
`hydra` rollup.
Each `dist/python/<pkg>/` directory is a self-contained build with a generated
`pyproject.toml`.
All Hydra wheels participate in the `hydra` namespace via
[PEP 420 implicit namespace packages](https://peps.python.org/pep-0420/):
no wheel ships a top-level `hydra/__init__.py`,
so multiple wheels install side-by-side and merge their `hydra.*` contents at import time.
Subpackages (`hydra.core`, `hydra.rdf.syntax`, etc.) DO have regular `__init__.py` files;
only the namespace root is implicit.

The published wheels are:

| Distribution | Description | `dependencies` |
|--------------|-------------|----------------|
| `hydra-kernel` | Core types, terms, DSL, eval, primitives + Python runtime support (`hydra.lib.*`, `hydra.dsl.*`, `hydra.sources.*`, `hydra.tools`). Self-contained. | (none) |
| `hydra-build` | Build/packaging model shared across coders. | `hydra-kernel` |
| `hydra-pg` | Property graph model, coders, GraphSON, TinkerPop. | `hydra-kernel`, `hydra-rdf` |
| `hydra-rdf` | RDF, OWL, SHACL, ShEx, XML Schema models. | `hydra-kernel` |
| `hydra-ext` _(not yet published)_ | Avro, Protobuf, GraphQL, Pegasus, C++, Rust, Go extension models. | `hydra-kernel` |
| `hydra-python` | Python syntax, serde, and coder. | `hydra-kernel` |

`hydra-ext` is **not** in the current publish set — it is outside the standard sync matrix and has
not shipped to PyPI (see the publish steps below). The other five are the active set.

Each wheel's `pyproject.toml` is regenerated from `packages/<pkg>/package.json` and
`hydra.json:currentVersion` by `bin/lib/generate-python-package-build.py`.
The kernel additionally has the hand-written runtime support overlaid in from
`overlay/python/hydra-kernel/` by `heads/python/bin/copy-kernel-runtime.sh` (#418).
Both are invoked automatically from `heads/python/bin/assemble-distribution.sh`.

The following are Python-specific release steps:

* Set up your Python environment as described in the
  [Hydra-Python README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-python/README.md).
* **Use the orchestrator** `heads/python/bin/publish-pypi.sh` (analogous to the Haskell/Java
  scripts). It checks dependency closure, builds wheel + sdist for each package into `wheels/`,
  and (with `--upload`) `twine upload`s them. Default is build-only:
  ```bash
  heads/python/bin/publish-pypi.sh            # build all wheels+sdists into wheels/ (no upload)
  twine check wheels/*                        # validate metadata (recommended pre-flight)
  heads/python/bin/publish-pypi.sh --upload   # build + twine upload
  ```
  The publish set is `hydra-kernel`, `hydra-build`, `hydra-rdf`, `hydra-pg`, `hydra-python`.
  **`hydra-ext` is excluded** — it is not in the standard sync matrix (so `dist/python/hydra-ext/`
  is not generated) and was not in the 0.15 PyPI release. (The "published wheels" table above lists
  it aspirationally; the actual publish set is these five.)
  * **Builder portability.** The script prefers `uv build` (hermetic, needs no preinstalled
    `build` module) and falls back to `python3 -m build`. Plain `python -m build` fails with
    `No module named build.__main__` on an interpreter without the `build` package — prefer `uv`.
  * **PyPI uploads are immutable.** Once `<pkg>-<version>` is uploaded it cannot be replaced
    (you would bump to `<version>.post1`). There is no candidate stage as on Hackage, so the
    `twine check` pass + a clean build are the pre-flight; verify before `--upload`.
  * **Dry-run on TestPyPI first.** Because the real upload is irreversible and has no candidate
    stage, validate the whole upload path on [TestPyPI](https://test.pypi.org/) before the
    production upload. TestPyPI is a throwaway sandbox: it exercises authentication, metadata
    acceptance, and project naming end-to-end without consuming a real version number.
    `publish-pypi.sh` has **no TestPyPI flag** — its `--upload` always targets production PyPI —
    so the dry-run runs a direct `twine upload` against the TestPyPI repository URL, over the
    same wheels the script already built into `wheels/`:
    ```bash
    heads/python/bin/publish-pypi.sh                          # build wheels+sdists into wheels/
    twine check wheels/*                                      # metadata pre-flight
    # Dry-run to TestPyPI (needs a SEPARATE test.pypi.org account + token):
    TWINE_USERNAME=__token__ TWINE_PASSWORD='pypi-<TESTPYPI-token>' \
      twine upload --repository-url https://test.pypi.org/legacy/ wheels/*
    # If that succeeds, do the real upload:
    heads/python/bin/publish-pypi.sh --upload
    ```
    To dry-run only **select packages** (e.g. just the `hydra-kernel` leaf, or to re-try one
    package after a TestPyPI version collision), pass explicit artifact paths instead of
    `wheels/*` — TestPyPI keeps versions permanently within a run, so re-uploading the same
    `<pkg>-<version>` there fails just as on production:
    ```bash
    TWINE_USERNAME=__token__ TWINE_PASSWORD='pypi-<TESTPYPI-token>' \
      twine upload --repository-url https://test.pypi.org/legacy/ \
      wheels/hydra_kernel-<version>*
    ```
    Caveats specific to TestPyPI:
    - **Separate credentials.** TestPyPI has its own accounts and API tokens, distinct from
      production PyPI; a production token returns `403` there and vice versa.
    - **Not a release.** TestPyPI is periodically wiped — a successful dry-run does *not* publish
      the version; you still run the production upload afterward.
    - **Inter-package deps won't resolve.** A `pip install` *from* TestPyPI fails to find Hydra's
      own dependencies (e.g. `hydra-python` needs `hydra-kernel`), because those resolve against
      real PyPI. This is expected — the dry-run validates *upload*, not cross-package install.
  * **`403 Forbidden` after the token is accepted** is a *permissions* problem, not a bad token:
    the token's account is not Owner/Maintainer of that project, or the token is scoped to other
    projects. See Access prerequisites. twine stops at the first failure, so check what (if
    anything) landed before retrying. Order does not matter for upload; PyPI accepts each
    independently.
  * **Packaging-boundary smoke gate (`verify-distribution.sh`).** After building each wheel set,
    `publish-pypi.sh` invokes `heads/python/bin/verify-distribution.sh --wheels <dir>` as a hard
    gate before any upload. The gate installs the just-built wheels into a fresh, isolated venv with
    `--no-index --find-links <wheels>` and imports the top-level kernel modules
    (`hydra.codegen`, `hydra.rewriting`, `hydra.encoding`, `hydra.arity`, `hydra.analysis`,
    `hydra.query`, `hydra.predicates`, `hydra.validate.core`, `hydra.validate.packaging`,
    `hydra.python.util`) from a neutral cwd, so the worktree's own `heads/python/` cannot leak
    onto `sys.path` and mask a missing-from-wheel package. A failure aborts the publish.
    This gate exists because 0.16.0 shipped a `hydra-kernel` wheel whose generated modules imported
    `hydra.python.util` from a runtime package that lived only in `heads/python/` and was therefore
    not packaged into the wheel — every local test passed, but `pip install hydra-kernel==0.16.0`
    in a clean venv failed with `ModuleNotFoundError`. The fix landed in #461 (the runtime moved to
    `overlay/python/hydra-kernel/`, which is copied into `dist/python/hydra-kernel/` and packaged);
    the gate is the standing assurance against the same class of break. See
    [#472](https://github.com/CategoricalData/hydra/issues/472). If you add a new top-level kernel
    module that imports a fresh runtime package, add it to `KERNEL_IMPORTS` in the script so the
    gate covers it.
* Publish to conda-forge:

  conda-forge is maintained for Hydra by [@phreed](https://github.com/phreed) (with
  [@joshsh](https://github.com/joshsh) as co-maintainer).
  The feedstock lives at
  [conda-forge/hydra-python-feedstock](https://github.com/conda-forge/hydra-python-feedstock)
  and uses the conda-build v3 recipe format (`recipe/recipe.yaml`).
  The feedstock was established for 0.14.1 as a single-output recipe building only the
  `hydra-python` package (the rollup wheel of the time);
  0.15 is the first release that ships multi-output, so the recipe needs a structural
  update in addition to the usual version + hash bump.

  **Recipe shape** (the multi-output structure below was a one-time conversion done for 0.15;
  it is now in place and subsequent releases are routine version + sha256 bumps):

  - `recipe/recipe.yaml` is a **multi-output recipe**, one conda package per published Python
    wheel. The active outputs are `hydra-kernel`, `hydra-rdf`, `hydra-pg`, `hydra-python`;
    `hydra-ext` is present but **commented out** ("not yet published to PyPI"), matching the
    PyPI publish set. The feedstock repo name (`hydra-python-feedstock`) does NOT change —
    conda-forge convention is one feedstock-with-many-outputs per upstream project, named after
    the most prominent output. Each output declares its own `requirements.run`.
  - Source is the **per-package PyPI sdist** — each output's `source.url` is
    `https://pypi.org/packages/source/h/<pkg>/<pkg_underscored>-${{ version }}.tar.gz`
    with that sdist's `sha256`. (conda-forge is therefore gated on the **PyPI publish**, not
    on the GitHub tag: the recipe's `sha256`s are the hashes of the PyPI-served sdists, so they
    can only be computed *after* PyPI upload.)
  - The downstream outputs (`hydra-rdf`, `hydra-pg`, `hydra-python`) declare `run` requirements
    on `hydra-kernel` and (for `hydra-pg`) `hydra-rdf`. Pin the inter-output dependencies tightly
    to the same version using `${{ pin_subpackage('hydra-kernel', exact=True) }}` so the outputs
    march in lockstep.

  **Per-release update workflow** (after the structural update is in place):

  1. Wait until the packages are live on **PyPI** (the feedstock pulls per-package PyPI sdists;
     the recipe `sha256`s are the hashes of those sdists).
  2. The conda-forge-webservices bot ([regro-cf-autotick-bot](https://github.com/regro-cf-autotick-bot))
     usually opens a version-bump PR within an hour of the new version appearing on PyPI.
     Watch
     [conda-forge/hydra-python-feedstock/pulls](https://github.com/conda-forge/hydra-python-feedstock/pulls)
     and review the PR when it lands.
  3. If the auto-bot doesn't pick up the release (e.g. for the first multi-output release,
     or when the recipe schema changed), open the PR by hand:
     - Fork [conda-forge/hydra-python-feedstock](https://github.com/conda-forge/hydra-python-feedstock).
     - In a feature branch, edit `recipe/recipe.yaml`: bump `context.version`, and update the
       per-output `source.sha256` (one per package) with the SHA256 of each PyPI sdist:
       ```bash
       for pkg in hydra_kernel hydra_rdf hydra_pg hydra_python; do
         curl -sL "https://pypi.org/packages/source/h/${pkg//_/-}/${pkg}-0.16.0.tar.gz" | shasum -a 256
       done
       ```
     - Open a PR against `main`.
       conda-forge's CI will rebuild every output for every supported platform
       (linux-64, osx-64, osx-arm64, win-64) and validate that the recipe is sound.
     - Once green, a feedstock maintainer merges; conda-forge-admin re-renders and the
       packages publish to the `conda-forge` channel within an hour or two.

  Users install with:

  ```bash
  conda install -c conda-forge hydra-pg
  # transitive run deps pull hydra-kernel + hydra-rdf automatically
  ```

## TypeScript releases

Hydra-TypeScript is a **complete Hydra implementation** that passes all tests in the common test suite.
It requires Node.js 20 or later.

Starting with 0.17, the TypeScript release ships **per-package npm tarballs**.
Each `dist/typescript/<pkg>/` directory is a self-contained npm package with a generated
`package.json` and `tsconfig.build.json`.
The published packages are:

| Package | Description | `dependencies` |
|---------|-------------|----------------|
| `hydra-kernel` | Core types, terms, DSL, eval, primitives + TypeScript runtime support (`hydra/runtime.ts`, `hydra/lib/*.ts`, `hydra/primitives.ts`). Self-contained. | (none) |
| `hydra-build` | Build/packaging model shared across coders. | `hydra-kernel` |
| `hydra-rdf` | RDF, OWL, SHACL, ShEx, XML Schema models. | `hydra-kernel` |
| `hydra-pg` | Property graph model, coders, GraphSON, TinkerPop. | `hydra-kernel`, `hydra-rdf` |
| `hydra-typescript` | TypeScript syntax and coder. | `hydra-kernel` |

Each package's `package.json` is generated by `bin/lib/generate-typescript-package-build.py`
from `packages/<pkg>/package.json` and `hydra.json:currentVersion`.
The kernel additionally has the hand-written runtime support overlaid in from
`overlay/typescript/hydra-kernel/` by `heads/typescript/bin/copy-kernel-runtime.sh` (#434).
Both steps run automatically from `heads/typescript/bin/assemble-distribution.sh` during sync.

Each package ships compiled JavaScript (ES2022 modules, `.js` + `.d.ts` declaration files)
compiled by `tsc` during the publish step, not sources. Consumers require no TypeScript toolchain.

The following are TypeScript-specific release steps:

* Set up your TypeScript environment: Node.js 20+, npm, and run
  `cd heads/typescript && npm install` to get `tsc` and `@types/node`.
* **Use the orchestrator** `heads/typescript/bin/publish-npm.sh` (analogous to
  the Haskell/Java/Python scripts). It checks dependency closure, compiles each
  package's TypeScript to JS + `.d.ts`, packs via `npm pack`, smoke-tests the
  packed tarballs, and (with `--upload`) publishes to npm. Default is build-only:
  ```bash
  heads/typescript/bin/publish-npm.sh            # compile + pack into npm-tarballs/ (no upload)
  heads/typescript/bin/publish-npm.sh --upload   # compile + pack + npm publish
  ```
  * **npm uploads are permanent.** Once `<pkg>@<version>` is published it cannot
    be re-uploaded (you would bump to `<version>.1`). Verify the packed tarballs
    (`npm-tarballs/*.tgz`) look correct before `--upload`.
  * **Authentication.** Set `NPM_TOKEN=<token>` in the environment (preferred for CI),
    or run `npm login` in the shell before `--upload`. The token must be an
    **Automation** token with **Read and Publish** access, owned by an account that
    is a package **Owner** of `hydra-kernel`, `hydra-rdf`, `hydra-pg`, and
    `hydra-typescript` on npmjs.com.
  * **Publish order matters.** The orchestrator publishes leaves first
    (`hydra-kernel` → `hydra-rdf` → `hydra-pg`, `hydra-typescript`) so consumers
    can install a dependent immediately after its dependency appears on the registry.
  * Check the published packages at:
    [hydra-kernel](https://www.npmjs.com/package/hydra-kernel),
    [hydra-rdf](https://www.npmjs.com/package/hydra-rdf),
    [hydra-pg](https://www.npmjs.com/package/hydra-pg),
    [hydra-typescript](https://www.npmjs.com/package/hydra-typescript).

A consumer of any of the above packages only needs to declare the highest-level
dependency they care about; npm resolves transitive dependencies automatically:

```bash
npm install hydra-pg   # pulls hydra-rdf and hydra-kernel transitively
```

## Script reference

The scripts below are the ones you directly invoke during a release.
For a complete inventory of
all scripts and Stack executables (including internal ones called by the sync scripts), see the
[Implementation appendix](implementation.md#appendix-build-scripts-and-executables).

| Script | Location | Purpose |
|--------|----------|---------|
| `bump-version.sh` | `bin/` | Bump `hydra.json:currentVersion` and propagate to all config files |
| `sync-all.sh` | `bin/` | Exhaustive regen: every package × every target, with tests. Fails fast. Supports `--no-tests`. |
| `sync-packages.sh` | `bin/` | Per-package orchestrator (Phase 1 → Phase 2 → Phase 3). Supports `--target`, `--from`, `--list`, `--no-tests`. |
| `sync.sh` | `bin/` | Matrix tool for prep before a bootstrapping run. Supports `--hosts`, `--targets`, `--no-tests`. |
| `sync-default.sh` | `bin/` | Shorthand for `sync.sh --hosts haskell,java,python --targets haskell,java,python`. |
| `sync-<lang>.sh` | `bin/` | Per-language wrapper (`sync-java.sh`, `sync-python.sh`, `sync-scala.sh`, `sync-typescript.sh`, `sync-go.sh`, `sync-clojure.sh`, `sync-scheme.sh`, `sync-common-lisp.sh`, `sync-emacs-lisp.sh`). Each sets host == target for one language. |
| `prepare-release.sh` | `bin/` | Cross-implementation pre-release preparation: runs all verification steps and produces upload-ready per-package Hackage sdists + Haddock-for-Hackage docs in `release-artifacts/`. |
| `assemble-distribution.sh` | `heads/<lang>/bin/` | Layer 2 per-package assembler. Takes `<pkg>`, writes `dist/<lang>/<pkg>/`. Called by `sync-packages.sh`. For `hydra-kernel`, also copies the hand-written runtime support; for every package, generates a per-package `build.gradle` (Java), `pyproject.toml` (Python), or `package.json` + `tsconfig.build.json` (TypeScript). |
| `test-distribution.sh` | `heads/<lang>/bin/` | Layer 2.5 per-target tester. |
| `copy-kernel-runtime.sh` | `heads/{java,python,typescript}/bin/` | Per-language helper invoked by `assemble-distribution.sh hydra-kernel` to overlay the hand-written kernel runtime from `overlay/<lang>/hydra-kernel/` into the kernel dist (#418). The overlay tree holds only runtime, so this is a dumb full-tree merge — no selective lists. (Multi-coder drivers, json-io stubs, and the per-language coder stay in `heads/<lang>/src`, not in the overlay.) |
| `assemble-haskell-distribution.sh` | `heads/haskell/bin/` | Layer 2 per-package Haskell assembler (Haskell analog of `assemble-distribution.sh`). Takes any Hydra Haskell distribution package `<pkg>` (`hydra-kernel`, the `hydra` umbrella, or any generated coder — `hydra-coq`, `hydra-scala`, `hydra-pg`, …) and tarballs the already-complete `dist/haskell/<pkg>/` tree (made complete by `sync-haskell.sh`, which overlays the hand-written runtime from `overlay/haskell/`): generates `package.yaml` + `stack.yaml` and runs `stack sdist`. A uniform `dist/` consumer — no per-package special-casing. Replaces the 0.15 monolithic `assemble-hackage-sdist.sh`. |
| `publish-hackage.sh` | `heads/haskell/bin/` | Assembles (and optionally uploads, `--upload`/`--publish`) the per-package Hackage distributions in leaves-first order. Asserts the publish set is dependency-closed before assembling. |
| `publish-maven.sh` | `heads/java/bin/` | Builds (and optionally uploads, `--upload`) the per-package Maven Central distributions leaves-first. Guards JDK 17+ and credentials; refreshes `~/.m2` with freshly-built siblings to avoid the stale-same-version trap. `--allow-javadoc-errors` applies a transient init script for #449. |
| `publish-pypi.sh` | `heads/python/bin/` | Builds (and optionally uploads, `--upload`) the per-package PyPI wheels + sdists. Prefers `uv build`, falls back to `python -m build`. Asserts dependency closure. Invokes `verify-distribution.sh` as a hard gate before any upload. |
| `publish-npm.sh` | `heads/typescript/bin/` | Compiles TypeScript sources to JS + `.d.ts` via `tsc`, packs via `npm pack`, smoke-tests the tarballs, and (with `--upload`) publishes to npm in leaves-first order. Asserts dependency closure. Requires `NPM_TOKEN` or an active `npm login` session for `--upload`. |
| `verify-distribution.sh` | `heads/{haskell,python,java,scala,typescript}/bin/` | **Per-host self-containment gate (uniform name across heads).** Builds the host's publish-set packages from the `dist/` tree alone and proves they are self-contained when consumed as published artifacts, in isolation from the worktree. **Python:** builds the wheels (or `--wheels <dir>`), installs them into a fresh venv with `--no-index`, imports the top-level kernel modules from a neutral cwd (catches #472). **Haskell:** stages the per-package distributions into one multi-package stack project and `stack build`s them, so dependents' `== <version>` pins resolve against the local staged siblings (catches the #473 cold-build class). **Java:** publishes the per-package jars to a temp Maven repo and resolves them from an offline consumer, so a jar missing classes its API needs (or a POM referencing a sibling not in the publish set) fails; hard-fails when no JDK 17+ is present. **Scala:** publishes the per-package artifacts to a temp Ivy repo (`sbt --ivy <tmp>`) and resolves them from an offline sbt consumer against the `_3` cross-version coordinates, same failure mode as Java's gate (#537). **TypeScript:** compiles, packs, installs the tarballs into an isolated node_modules (no registry), and imports the kernel core module. Run by CI per host and as a hard gate in `prepare-release.sh` (Step 13) + `publish-pypi.sh`. |
| `generate-haskell-package-build.py` | `bin/lib/` | Emits a standalone `dist/haskell/<pkg>/package.yaml` (+ `stack.yaml`) from `packages/<pkg>/package.json` (or a built-in spec for the `hydra` umbrella) and `hydra.json:currentVersion`. Inter-Hydra deps emit as exact pins `<dep> == <version>`. |
| `publish-sbt.sh` | `heads/scala/bin/` | Builds (and optionally uploads, `--upload`) the per-package Scala Maven Central jars leaves-first via sbt-sonatype. Guards credentials and GPG key; refreshes `~/.ivy2/local` with freshly-built siblings. |
| `generate-java-package-build.py` | `bin/lib/` | Emits a standalone `dist/java/<pkg>/build.gradle` + `settings.gradle` from `packages/<pkg>/package.json` and `hydra.json:currentVersion`. Inter-Hydra deps emit as `api 'net.fortytwo.hydra.java:<dep>:<version>'`. |
| `generate-python-package-build.py` | `bin/lib/` | Emits a standalone `dist/python/<pkg>/pyproject.toml` from `packages/<pkg>/package.json` and `hydra.json:currentVersion`. Inter-Hydra deps emit as `"<dep> == <version>"`. |
| `generate-scala-package-build.py` | `bin/lib/` | Emits a standalone `dist/scala/<pkg>/build.sbt` + `project/plugins.sbt` + `project/build.properties` from `packages/<pkg>/package.json` and `hydra.json:currentVersion`. Inter-Hydra deps emit as `"net.fortytwo.hydra.scala" % "<dep>_3" % version`. |
| `generate-typescript-package-build.py` | `bin/lib/` | Emits a standalone `dist/typescript/<pkg>/package.json` + `tsconfig.build.json` from `packages/<pkg>/package.json` and `hydra.json:currentVersion`. Inter-Hydra deps emit as `"<dep>": "<version>"` in the npm `dependencies` map. (#492) |
| `transform-json-to-<lang>.sh` | `heads/haskell/bin/` | Layer 1 transform. Thin wrapper over `bootstrap-from-json` for one (pkg, source-set). |
| `transform-haskell-dsl-to-json.sh` | `heads/haskell/bin/` | Layer 1 transform. Supports `--package <pkg>` (one package) or `--all` (batch mode, one universe load). |
