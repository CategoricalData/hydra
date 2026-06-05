# Release workflow

This document is for Hydra developers and release engineers.
It is the canonical procedure for preparing, tagging, publishing, and documenting a Hydra release.

All official Hydra implementations are maintained in a single repository,
and their releases are synchronized,
so that compatibility of different Hydra artifacts can be understood
on the basis of their version numbers alone, regardless of the implementation language.
However, this makes it very important to maintain a unified and consistent release process.

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

## Version synchronization

All implementations share a single version number.
The canonical version lives in the `VERSION` file at the repository root,
and `bin/bump-version.sh` propagates it to all config files.

### Bumping the version

```bash
# Set the new version and propagate to all config files
bin/bump-version.sh 0.13.0

# Or, if you've already edited the VERSION file manually:
bin/bump-version.sh
```

The script validates the version format (X.Y.Z), writes it to the `VERSION` file
(if a version argument is given), and patches all of the following files:

| File | Format |
|------|--------|
| `VERSION` | `0.15.0` |
| `heads/haskell/package.yaml` | `version: 0.15.0` |
| `demos/bootstrapping/resources/haskell/package.yaml` | `version: 0.15.0` |
| `heads/java/build.gradle` | `version = '0.15.0'` |
| `demos/bootstrapping/resources/java/build.gradle` | `version = '0.15.0'` |
| `packages/hydra-scala/build.sbt` | `version := "0.15.0"` |
| `heads/python/pyproject.toml` | `version = "0.15.0"` |
| `demos/bootstrapping/resources/python/pyproject.toml` | `version = "0.15.0"` |

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

1. **Version synchronization** — all version files match (`VERSION`, Haskell, Java, Python, Scala, and bootstrapping resources).
2. **Haskell tests** — `stack test` in `heads/haskell`.
3. **Java tests** — `./gradlew test` from the root (requires Java 11+).
4. **Python tests** — `pytest` plus `ruff check` and `ruff format --check`.
5. **Scala tests** — `sbt test` in `packages/hydra-scala`.
6. **Lisp tests** — runs `run-tests.sh` for each dialect (Clojure, Common Lisp, Emacs Lisp, Scheme).
7. **JSON kernel** — verifies the exported JSON kernel round-trips correctly.
8. **Lexicon freshness** — `docs/hydra-lexicon.txt` matches the current Haskell kernel.
9. **Per-package Hackage sdists on case-sensitive filesystem** —
   the Hackage build infrastructure runs on Linux (case-sensitive ext4),
   while macOS HFS+/APFS hides duplicate-with-different-case directory clashes.
   Hydra publishes per-package Hackage distributions — `hydra-kernel`,
   `hydra-haskell`, and the `hydra` umbrella — rather than one monolithic
   `hydra` sdist. This step assembles all three (leaves first) on a
   case-sensitive disk image (macOS) or plain temp dir (Linux) via
   `heads/haskell/bin/publish-hackage.sh`, extracts them into one
   `cabal.project`, and runs `cabal v2-build --dry-run` to catch GHC-28623
   ("file name does not match module name") errors without a full compile.
   The publish script also asserts the publish set is **dependency-closed**
   (no published package may depend on an unpublished Hydra package).
10. **Per-package Haddock-for-Hackage docs** —
   builds a Haddock tarball per package from the validated sdists.
   Hackage's auto-doc-builder is best-effort and frequently fails on packages
   with many transitive deps, so we pre-build locally and upload via
   `cabal upload --documentation --publish` after release
   (see "Haskell releases" below).

On success the script writes, for each of `hydra-kernel`, `hydra-haskell`, `hydra`:

- `release-artifacts/<pkg>-<version>.tar.gz`      — the Hackage sdist
- `release-artifacts/<pkg>-<version>-docs.tar.gz` — the Haddock-for-Hackage docs

Per-step logs land in `verify-logs/`. All checks must pass before proceeding with the release.

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
  The Highlights section provides context; the subsections are a concise inventory.
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
  The 0.16.0 publish set is the trio `hydra-kernel`, `hydra-haskell`, and the
  `hydra` umbrella (which re-exports the kernel surface plus `moduleToHaskell`);
  later releases may add more packages (`hydra-pg`, `hydra-rdf`, the other
  coders), always **dependency-closed**.
  * `bin/prepare-release.sh` already produced the upload-ready per-package sdists
    and Haddock-for-Hackage tarballs under `release-artifacts/` (one
    `<pkg>-<version>.tar.gz` + `<pkg>-<version>-docs.tar.gz` per package).
    Each package is assembled by `heads/haskell/bin/assemble-haskell-distribution.sh`,
    which stages the generated `dist/haskell/<pkg>/` tree plus the hand-written
    head modules it needs (for `hydra-kernel`, the `Hydra.Haskell.Lib.*` primitive
    implementations and the `Hydra.Settings`/`Hydra.Kernel` entry points) into a
    self-contained tree and runs `stack sdist`. (This replaces the 0.15-era
    `assemble-hackage-sdist.sh`, which flattened everything into one tarball.)
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
The published artifacts under group `net.fortytwo.hydra` are:

| Artifact | Description | `api` dependencies |
|----------|-------------|--------------------|
| `hydra-kernel` | Core types, terms, DSL, eval, primitives + the Java runtime support classes (`hydra.util.*`, `hydra.lib.*`, `hydra.dsl.*`, `hydra.tools.*`, plus `Adapters`/`Coders`). Self-contained; downstream packages depend on this. | (none) |
| `hydra-pg` | Property graph model, coders, GraphSON, TinkerPop. | `hydra-kernel`, `hydra-rdf` |
| `hydra-rdf` | RDF, OWL, SHACL, ShEx, XML Schema models. | `hydra-kernel` |
| `hydra-java` | Java syntax, serde, and coder (generates Java code from Hydra schemas). | `hydra-kernel` |

`hydra-ext` (Avro, Protobuf, GraphQL, Pegasus, etc.) is intentionally NOT in the 0.15 Java
publish set due to a known Java-coder limitation with parametric union case-elimination on
concretely-instantiated arguments. Track this before adding `hydra-ext` to a future Java publish set.

Each artifact's `build.gradle` is regenerated from `packages/<pkg>/package.json` (which declares
the inter-package `dependencies` array) and the worktree's `VERSION` file by
`bin/lib/generate-java-package-build.py`.
The kernel additionally has the hand-written runtime support overlaid in from
`overlay/java/hydra-kernel/` by `heads/java/bin/copy-kernel-runtime.sh` (#418).
Both are invoked automatically from `heads/java/bin/assemble-distribution.sh`,
so a clean `bin/sync.sh` produces ready-to-publish trees.

The following are Java-specific release steps:
* Set up your Java environment as described in the
  [Hydra-Java README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-java/README.md).
* Update the JavaDocs.
  * JavaDocs are automatically generated and deployed to GitHub Pages when a new tag is pushed
    (via the `pages.yml` GitHub Actions workflow).
    No manual steps are needed for JavaDoc publishing.
  * Check the updated hydra-java JavaDocs
    [here](https://categoricaldata.net/hydra/hydra-java/javadoc)
    after the tag is pushed.
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
  * Publish in dependency order so transitive POM resolution works on the consumer side.
    Run from each `dist/java/<pkg>/` directory:
    ```bash
    cd dist/java/hydra-kernel && gradle publishAggregationToCentralPortal
    cd dist/java/hydra-rdf    && gradle publishAggregationToCentralPortal
    cd dist/java/hydra-pg     && gradle publishAggregationToCentralPortal
    cd dist/java/hydra-java   && gradle publishAggregationToCentralPortal
    ```
    The task chain is: build the jar + Javadoc + sources jars, generate a signed POM,
    package everything into a Central Portal-shaped zip, and upload it via the publisher API.
    The generated `build.gradle` sets `publishingType = "USER_MANAGED"`,
    so each upload lands in the Central Portal UI as a separate pending deployment for review.
  * Go to [Deployments](https://central.sonatype.com/publishing/deployments) in the Central Portal.
    Find the four deployments, verify they have passed validation, then click "Publish" on each.

It will take a short time (a couple of minutes) for validation,
and a longer time (as little as 15 minutes, or as much as nearly an hour) for promotion,
before you can find the release through [Maven Central](https://central.sonatype.com).
Hydra releases are [here](https://central.sonatype.com/search?q=net.fortytwo.hydra).

A consumer of any of the above artifacts only needs to declare the highest-level
dependency they care about; transitive resolution pulls the rest:

```gradle
dependencies {
    implementation 'net.fortytwo.hydra:hydra-pg:0.15.0'   // pulls hydra-rdf and hydra-kernel
}
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
| `hydra-pg` | Property graph model, coders, GraphSON, TinkerPop. | `hydra-kernel`, `hydra-rdf` |
| `hydra-rdf` | RDF, OWL, SHACL, ShEx, XML Schema models. | `hydra-kernel` |
| `hydra-ext` | Avro, Protobuf, GraphQL, Pegasus, C++, Rust, Go extension models. | `hydra-kernel` |
| `hydra-python` | Python syntax, serde, and coder. | `hydra-kernel` |

Each wheel's `pyproject.toml` is regenerated from `packages/<pkg>/package.json` and the
worktree's `VERSION` file by `bin/lib/generate-python-package-build.py`.
The kernel additionally has the hand-written runtime support overlaid in from
`overlay/python/hydra-kernel/` by `heads/python/bin/copy-kernel-runtime.sh` (#418).
Both are invoked automatically from `heads/python/bin/assemble-distribution.sh`.

The following are Python-specific release steps:

* Set up your Python environment as described in the
  [Hydra-Python README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-python/README.md).
* Build each wheel from its `dist/python/<pkg>/` directory.
  Any of these tools work; pick one:
  ```bash
  cd dist/python/hydra-kernel && python -m build --wheel --sdist --outdir ../../../wheels
  cd dist/python/hydra-rdf    && python -m build --wheel --sdist --outdir ../../../wheels
  cd dist/python/hydra-pg     && python -m build --wheel --sdist --outdir ../../../wheels
  cd dist/python/hydra-ext    && python -m build --wheel --sdist --outdir ../../../wheels
  cd dist/python/hydra-python && python -m build --wheel --sdist --outdir ../../../wheels
  ```
  Producing both `.whl` and `.tar.gz` (sdist) per package is recommended for maximal
  installer compatibility.
* Publish to PyPI:
  * Use [twine](https://pypi.org/project/twine/) or
    [uv publish](https://docs.astral.sh/uv/guides/publish/) to upload from the wheels directory.
    Order does not matter for upload; PyPI accepts each independently.
  * `twine upload wheels/*.whl wheels/*.tar.gz` (or the `uv publish` equivalent).
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

  **One-time structural update for 0.15** (handled in the same PR as the version bump):

  - Convert the `recipe/recipe.yaml` from the single-output shape used through 0.14
    to a **multi-output recipe** producing one conda package per Python wheel:
    `hydra-kernel`, `hydra-rdf`, `hydra-pg`, `hydra-ext`, `hydra-python`.
    The feedstock repo name (`hydra-python-feedstock`) does NOT change — conda-forge
    convention is one feedstock-with-many-outputs per upstream project, named after
    the most prominent output. The five `package:` outputs each declare their own
    `requirements.run` so transitive deps resolve at install time.
  - Source remains the GitHub release tarball
    (`https://github.com/CategoricalData/hydra/archive/refs/tags/${{ version }}.tar.gz`),
    NOT the PyPI sdists. Each output's build script `cd`s into the relevant subdirectory
    of the unpacked tarball — for 0.15, that's `dist/python/<pkg>/` rather than the old
    top-level `hydra-python/`.
  - The downstream packages (`hydra-rdf`, `hydra-pg`, `hydra-ext`, `hydra-python`) declare
    `run` requirements on `hydra-kernel` and (for `hydra-pg`) `hydra-rdf`.
    Pin the inter-output dependencies tightly to the same version using
    `${{ pin_subpackage('hydra-kernel', exact=True) }}` so the five outputs march in
    lockstep.

  **Per-release update workflow** (after the structural update is in place):

  1. Wait until the GitHub release tag is pushed (the feedstock pulls the source tarball
     from there, not from PyPI).
  2. The conda-forge-webservices bot ([regro-cf-autotick-bot](https://github.com/regro-cf-autotick-bot))
     usually opens a version-bump PR within an hour of the new tag appearing on GitHub.
     Watch
     [conda-forge/hydra-python-feedstock/pulls](https://github.com/conda-forge/hydra-python-feedstock/pulls)
     and review the PR when it lands.
  3. If the auto-bot doesn't pick up the release (e.g. for the first multi-output release,
     or when the recipe schema changed), open the PR by hand:
     - Fork [conda-forge/hydra-python-feedstock](https://github.com/conda-forge/hydra-python-feedstock).
     - In a feature branch, edit `recipe/recipe.yaml`:
       update the `context.version` and the `source.sha256`.
       Compute the SHA256 of the GitHub release tarball with:
       ```bash
       curl -sL https://github.com/CategoricalData/hydra/archive/refs/tags/0.15.0.tar.gz | shasum -a 256
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

## Script reference

The scripts below are the ones you directly invoke during a release.
For a complete inventory of
all scripts and Stack executables (including internal ones called by the sync scripts), see the
[Implementation appendix](implementation.md#appendix-build-scripts-and-executables).

| Script | Location | Purpose |
|--------|----------|---------|
| `bump-version.sh` | `bin/` | Bump version in `VERSION` file and propagate to all config files |
| `sync-all.sh` | `bin/` | Exhaustive regen: every package × every target, with tests. Fails fast. Supports `--no-tests`. |
| `sync-packages.sh` | `bin/` | Per-package orchestrator (Phase 1 → Phase 2 → Phase 3). Supports `--target`, `--from`, `--list`, `--no-tests`. |
| `sync.sh` | `bin/` | Matrix tool for prep before a bootstrapping run. Supports `--hosts`, `--targets`, `--no-tests`. |
| `sync-default.sh` | `bin/` | Shorthand for `sync.sh --hosts haskell,java,python --targets haskell,java,python`. |
| `sync-<lang>.sh` | `bin/` | Per-language wrapper (`sync-java.sh`, `sync-python.sh`, `sync-scala.sh`, `sync-typescript.sh`, `sync-go.sh`, `sync-clojure.sh`, `sync-scheme.sh`, `sync-common-lisp.sh`, `sync-emacs-lisp.sh`). Each sets host == target for one language. |
| `prepare-release.sh` | `bin/` | Cross-implementation pre-release preparation: runs all verification steps and produces upload-ready per-package Hackage sdists + Haddock-for-Hackage docs in `release-artifacts/`. |
| `update-javadoc.sh` | `bin/` | Regenerate JavaDoc HTML for `packages/hydra-java`. |
| `assemble-distribution.sh` | `heads/<lang>/bin/` | Layer 2 per-package assembler. Takes `<pkg>`, writes `dist/<lang>/<pkg>/`. Called by `sync-packages.sh`. For `hydra-kernel`, also copies the hand-written runtime support; for every package, generates a per-package `build.gradle` (Java) or `pyproject.toml` (Python). |
| `test-distribution.sh` | `heads/<lang>/bin/` | Layer 2.5 per-target tester. |
| `copy-kernel-runtime.sh` | `heads/{java,python}/bin/` | Per-language helper invoked by `assemble-distribution.sh hydra-kernel` to overlay the hand-written kernel runtime from `overlay/<lang>/hydra-kernel/` into the kernel dist (#418). The overlay tree holds only runtime, so this is a dumb full-tree merge — no selective lists. (Multi-coder drivers, json-io stubs, and the per-language coder stay in `heads/<lang>/src`, not in the overlay.) |
| `assemble-haskell-distribution.sh` | `heads/haskell/bin/` | Layer 2 per-package Haskell assembler (Haskell analog of `assemble-distribution.sh`). Takes `<pkg>` (`hydra-kernel`/`hydra-haskell`/`hydra`) and tarballs the already-complete `dist/haskell/<pkg>/` tree (made complete by `sync-haskell.sh`, which overlays the hand-written runtime from `overlay/haskell/`): generates `package.yaml` + `stack.yaml` and runs `stack sdist`. A uniform `dist/` consumer — no per-package special-casing. Replaces the 0.15 monolithic `assemble-hackage-sdist.sh`. |
| `publish-hackage.sh` | `heads/haskell/bin/` | Assembles (and optionally uploads, `--upload`/`--publish`) the per-package Hackage distributions in leaves-first order. Asserts the publish set is dependency-closed before assembling. |
| `verify-haskell-distribution.sh` | `heads/haskell/bin/` | Stages all per-package distributions into one multi-package stack project and `stack build`s them, proving the trio compiles as published (the dependents' `== <version>` pins resolve against the local staged siblings). |
| `generate-haskell-package-build.py` | `bin/lib/` | Emits a standalone `dist/haskell/<pkg>/package.yaml` (+ `stack.yaml`) from `packages/<pkg>/package.json` (or a built-in spec for the `hydra` umbrella) and `VERSION`. Inter-Hydra deps emit as exact pins `<dep> == <version>`. |
| `generate-java-package-build.py` | `bin/lib/` | Emits a standalone `dist/java/<pkg>/build.gradle` + `settings.gradle` from `packages/<pkg>/package.json` and `VERSION`. Inter-Hydra deps emit as `api 'net.fortytwo.hydra:<dep>:<version>'`. |
| `generate-python-package-build.py` | `bin/lib/` | Emits a standalone `dist/python/<pkg>/pyproject.toml` from `packages/<pkg>/package.json` and `VERSION`. Inter-Hydra deps emit as `"<dep> == <version>"`. |
| `transform-json-to-<lang>.sh` | `heads/haskell/bin/` | Layer 1 transform. Thin wrapper over `bootstrap-from-json` for one (pkg, source-set). |
| `transform-haskell-dsl-to-json.sh` | `heads/haskell/bin/` | Layer 1 transform. Supports `--package <pkg>` (one package) or `--all` (batch mode, one universe load). |
