# Apache Incubator readiness assessment

This document assesses Hydra against the
[Apache Project Maturity Model](https://community.apache.org/apache-way/apache-project-maturity-model.html),
the checklist the ASF uses to gauge whether a project operates "the Apache Way." It is the artifact to
show a prospective Incubator champion or the Incubator PMC: every maturity-model checkpoint, an honest
**ready / partial / gap** status, and a **link to the evidence on `main`** that backs the claim.

It is the deliverable for Track 1 of [#441](https://github.com/CategoricalData/hydra/issues/441). The
release-and-artifact dimension (Track 7) is covered in depth in the second half — it is the most
mature dimension and doubles as the worked example of how evidence is cited here.

Evidence links point at `main` (`github.com/CategoricalData/hydra/blob/main/…`) so a reviewer can
verify each claim against the live source, not a branch snapshot.

Honest framing: Hydra is **strong on the technical dimensions** (code, licensing, releases, quality,
independence) and **early on the foundation/community dimensions** (governance, consensus, a committer
ladder, a PMC). The latter are exactly what the Incubator *exists to build*, so they are expected gaps
for a pre-podling project, not disqualifiers — but they are recorded plainly, not glossed.

## Maturity-model scorecard

Status key: ✅ ready · ⚠️ partial · ❌ gap (expected for a pre-podling project) · ⏭ deferred (applies
only once incubating). Links resolve on `main`.

### Code (CD)

| ID | Checkpoint | Status | Evidence |
|----|-----------|--------|----------|
| CD10 | Produces OSS for free public distribution | ✅ | [LICENSE](https://github.com/CategoricalData/hydra/blob/main/LICENSE) (Apache-2.0); public on GitHub |
| CD20 | Code is easily discoverable and accessible | ✅ | [README](https://github.com/CategoricalData/hydra/blob/main/README.md); published to Hackage/Maven/PyPI/npm/conda-forge |
| CD30 | Reproducible build with standard tools | ✅ | [contributor-setup.md](https://github.com/CategoricalData/hydra/blob/main/docs/contributor-setup.md), [build-system.md](https://github.com/CategoricalData/hydra/blob/main/docs/build-system.md) |
| CD40 | Full history; any release recreatable from SCM | ✅ | git history + release tags through 0.17.x; [CHANGELOG.md](https://github.com/CategoricalData/hydra/blob/main/CHANGELOG.md) |
| CD50 | Provenance via strong committer authentication | ⚠️ | GitHub authenticated commits; no DCO sign-off / iCLA yet (an Incubator-onboarding step) |

### Licenses and copyright (LC)

| ID | Checkpoint | Status | Evidence |
|----|-----------|--------|----------|
| LC10 | Apache-2.0 covers the released code | ✅ | [LICENSE](https://github.com/CategoricalData/hydra/blob/main/LICENSE); per-host build files declare Apache-2.0 |
| LC20 | Mandatory deps no more restrictive than Apache | ✅ | [Dependency license attestation](#dependency-license-attestation) — all shipped deps Category A |
| LC30 | Those dependencies are open source | ✅ | same attestation (all BSD/MIT/Apache-class) |
| LC40 | Committers bound by an iCLA | ❌ | no iCLA yet — established at podling onboarding (ASF-governed step) |
| LC50 | Copyright ownership documented | ✅ | [NOTICE](https://github.com/CategoricalData/hydra/blob/main/NOTICE) + Apache-2.0 §4(d) |

### Releases (RE)

| ID | Checkpoint | Status | Evidence |
|----|-----------|--------|----------|
| RE10 | Releases are source archives in open formats | ✅ | canonical `git archive` source tarball — [prepare-release.sh](https://github.com/CategoricalData/hydra/blob/main/bin/prepare-release.sh); see [Releases dimension](#releases-dimension--detailed-evidence) |
| RE20 | PMC approves each release | ⏭ | no PMC yet; a `[VOTE]` process is part of podling onboarding |
| RE30 | Releases signed and/or distributed with digests | ✅ | `.sha512` + detached `.asc`; [KEYS](https://github.com/CategoricalData/hydra/blob/main/KEYS) (key registered); [release-workflow.md "Verifying a release"](https://github.com/CategoricalData/hydra/blob/main/docs/release-workflow.md) |
| RE40 | Convenience binaries allowed but not "the release" | ✅ | documented in [release-workflow.md](https://github.com/CategoricalData/hydra/blob/main/docs/release-workflow.md) |
| RE50 | Documented, repeatable release process | ✅ | [release-workflow.md](https://github.com/CategoricalData/hydra/blob/main/docs/release-workflow.md); CI gate [release-verify.yml](https://github.com/CategoricalData/hydra/blob/main/.github/workflows/release-verify.yml) |

### Quality (QU)

| ID | Checkpoint | Status | Evidence |
|----|-----------|--------|----------|
| QU10 | Open/honest about code quality and maturity | ✅ | [README](https://github.com/CategoricalData/hydra/blob/main/README.md) implementation-status table; [Language support](https://github.com/CategoricalData/hydra/wiki/Language-support) wiki page (production / conformance / head-bud tiers) |
| QU20 | High priority on secure software | ✅ | [SECURITY.md](https://github.com/CategoricalData/hydra/blob/main/SECURITY.md); [Security wiki](https://github.com/CategoricalData/hydra/wiki/Security) |
| QU30 | Documented, private security-report channel | ✅ | [SECURITY.md](https://github.com/CategoricalData/hydra/blob/main/SECURITY.md) ("Reporting a vulnerability") |
| QU40 | Backwards-compatibility prioritized + documented | ⚠️ | [CHANGELOG.md](https://github.com/CategoricalData/hydra/blob/main/CHANGELOG.md) + forward-compat contract (#369); no standalone versioning/deprecation policy doc yet |
| QU50 | Timely response to bug reports | ⚠️ | active issue tracker; no stated SLA or issue templates |

### Community (CO)

| ID | Checkpoint | Status | Evidence |
|----|-----------|--------|----------|
| CO10 | Well-known homepage covering the maturity model | ⚠️ | [README](https://github.com/CategoricalData/hydra/blob/main/README.md) + [wiki](https://github.com/CategoricalData/hydra/wiki); no dedicated project domain |
| CO20 | Welcomes good-faith contributions | ✅ | [CONTRIBUTING.md](https://github.com/CategoricalData/hydra/blob/main/CONTRIBUTING.md) + [Contributing wiki](https://github.com/CategoricalData/hydra/wiki/Contributing) |
| CO30 | Values all contribution types | ✅ | [CONTRIBUTING.md](https://github.com/CategoricalData/hydra/blob/main/CONTRIBUTING.md) / wiki |
| CO40 | Strives to be meritocratic | ✅ | meritocratic intent + committer ladder in [GOVERNANCE.md](https://github.com/CategoricalData/hydra/blob/main/GOVERNANCE.md) |
| CO50 | Documents how contributors earn more rights | ✅ | committer ladder in [GOVERNANCE.md](https://github.com/CategoricalData/hydra/blob/main/GOVERNANCE.md) ("Becoming a committer / maintainer") |
| CO60 | Consensus-based; no benevolent dictator | ❌ | honestly lead-maintainer-driven today ([GOVERNANCE.md](https://github.com/CategoricalData/hydra/blob/main/GOVERNANCE.md)); the move to PPMC consensus is an incubation goal (Track 9) |
| CO70 | Timely answers to user questions | ⚠️ | [Discord](https://bit.ly/hydra-on-discord) + GitHub issues; no stated SLA |
| — | Code of Conduct (not a numbered checkpoint; expected) | ✅ | [CODE_OF_CONDUCT.md](https://github.com/CategoricalData/hydra/blob/main/CODE_OF_CONDUCT.md) (Contributor Covenant 2.1; ASF CoC at incubation) |

### Consensus building (CS)

| ID | Checkpoint | Status | Evidence |
|----|-----------|--------|----------|
| CS10 | Public list of contributors with decision power (PMC) | ⚠️ | roles defined in [GOVERNANCE.md](https://github.com/CategoricalData/hydra/blob/main/GOVERNANCE.md); a formal PMC roster awaits incubation (Track 9) |
| CS20 | Decisions by documented consensus on main channel | ⚠️ | decision process documented in [GOVERNANCE.md](https://github.com/CategoricalData/hydra/blob/main/GOVERNANCE.md); lead-maintainer-arbitrated today, PPMC consensus the target |
| CS30 | Documented voting rules | ❌ | ASF-style voting rules adopted at incubation (mapped in [GOVERNANCE.md](https://github.com/CategoricalData/hydra/blob/main/GOVERNANCE.md) "Path to Apache governance") |
| CS40 | Vetoes only on code commits, with justification | ❌ | applies once PPMC voting is in force (Track 9) |
| CS50 | Important discussions async + written on main channel | ⚠️ | GitHub issues/PRs are written + async ([GOVERNANCE.md](https://github.com/CategoricalData/hydra/blob/main/GOVERNANCE.md)); a canonical dev@ list awaits incubation |

### Independence (IN)

| ID | Checkpoint | Status | Evidence |
|----|-----------|--------|----------|
| IN10 | Independent of corporate/organizational influence | ✅ | [GOVERNANCE.md](https://github.com/CategoricalData/hydra/blob/main/GOVERNANCE.md) "Independence"; no single-company control |
| IN20 | Contributors act as themselves | ✅ | individual contributors ([GOVERNANCE.md](https://github.com/CategoricalData/hydra/blob/main/GOVERNANCE.md)); no corporate-representative structure |

### Trademark and branding (TB)

| ID | Checkpoint | Status | Evidence |
|----|-----------|--------|----------|
| TB10 | "Apache Foo™" name used consistently | ⏭ | applies only post-acceptance; pre-podling we deliberately do **not** use "Apache Hydra" (see [Deferred](#deferred-until-podling-acceptance)) |
| TB20 | Homepage at `projectname.apache.org` | ⏭ | applies post-acceptance |
| TB30 | ASF holds trademark rights | ⏭ | transferred at acceptance |
| TB40 | Project monitors brand misuse | ⏭ | post-acceptance |

### How to read this scorecard

The technical dimensions a project must bring *with* it — open code, permissive licensing, a real
reproducible build, signed reproducible releases, a security process, independence — are ✅. The
project also documents its governance, committer ladder, and code of conduct
([GOVERNANCE.md](https://github.com/CategoricalData/hydra/blob/main/GOVERNANCE.md),
[CODE_OF_CONDUCT.md](https://github.com/CategoricalData/hydra/blob/main/CODE_OF_CONDUCT.md)).

The remaining ❌ items are the ones that **structurally require the Foundation to exist**: a formal PMC
roster, ASF voting rules, code-commit vetoes, an iCLA, the PMC release vote (CS30/CS40, LC40, RE20, and
CO60 — the move off lead-maintainer arbitration to collective consensus). These cannot be honestly
satisfied before acceptance; the
[GOVERNANCE.md "Path to Apache governance"](https://github.com/CategoricalData/hydra/blob/main/GOVERNANCE.md)
section records exactly how each maps in. Track 9 of
[#441](https://github.com/CategoricalData/hydra/issues/441) tracks the governance work; the
[Deferred](#deferred-until-podling-acceptance) section lists items that would be premature to adopt
before acceptance.

## Releases dimension — why it matters and what Apache requires

Apache's release policy is one of the foundation's hardest gates: a project cannot make an

Apache's release policy is one of the foundation's hardest gates: a project cannot make an
official release until it can produce a **signed, checksummed source artifact** that a third party
can build from scratch, accompanied by `LICENSE` and `NOTICE` files that account for every
bundled work. Much of this is independent of whether Hydra ever enters the Incubator — it is
simply what a production-grade release looks like — so the work is worth doing regardless.

A few requirements, however, apply **only after** a project is accepted as a podling, and would be
wrong (or premature) to adopt today. Those are called out separately under
[Deferred until podling acceptance](#deferred-until-podling-acceptance) so they are not mistaken
for current gaps.

## What Apache requires of a source release

The authoritative references are the
[ASF Release Policy](https://www.apache.org/legal/release-policy.html),
[Release Distribution Policy](https://infra.apache.org/release-distribution.html), and the
[Incubator Release Management guide](https://incubator.apache.org/guides/releasemanagement.html).
The load-bearing requirements for a source release are:

1. The release is a **source archive** that builds with a documented, reproducible procedure.
2. The archive carries a top-level **`LICENSE`** and **`NOTICE`** consistent with
   [ASF licensing-howto](https://www.apache.org/legal/src-headers.html).
3. Every bundled third-party work is accounted for in `LICENSE`/`NOTICE`, and every dependency
   is under an [ASF Category A](https://www.apache.org/legal/resolved.html) (permissive) license,
   or is handled per the Category B/X rules.
4. The archive is **cryptographically signed** (detached `.asc` from a committer's key in the
   project `KEYS` file) and accompanied by a **checksum** (`.sha256` and/or `.sha512`).
5. The release is **voted on** by the PMC before publication, and the source archive — not a
   convenience binary — is the canonical artifact.

Convenience binaries (Hackage sdists, Maven jars, PyPI wheels, conda packages) are explicitly
*secondary* under Apache policy: they may be published, but the signed source archive is the
release of record.

## Releases dimension — detailed evidence

The Releases (RE) and Licenses (LC) dimensions are the most mature and the most fully evidenced, so
they are worked through in detail here as the depth behind the scorecard rows above. Audited at the
post-0.17 state of `main`; evidence is cited by file path.

### Licensing

| Item | State | Evidence |
|------|-------|----------|
| Root `LICENSE` (Apache-2.0) | ✅ Present | `LICENSE` — full Apache 2.0 text |
| Per-package `LICENSE` copies | ✅ Present | `packages/hydra-{kernel,haskell,java,python}/LICENSE`, `heads/haskell/LICENSE` — all Apache 2.0 |
| Build-metadata license declarations | ✅ Present | Haskell `license: Apache-2.0` (`heads/haskell/package.yaml`); Python `license = { text = "Apache-2.0" }` + OSI classifier (`heads/python/pyproject.toml`); Java POM Apache-2.0, generated by `bin/lib/generate-java-package-build.py`; Scala `licenses := Seq("Apache-2.0" -> …)` (`packages/hydra-scala/build.sbt`) |
| Root `NOTICE` | ✅ Present | `NOTICE` at the repo root (Apache-2.0 §4(d) project notice; see P0 #1) |
| Source-file license headers | ⏭ Deferred | ASF header form presupposes ASF ownership; defer to acceptance (see [P2](#p2--recommended-not-strictly-required)) |

### Artifact integrity

| Item | State | Evidence |
|------|-------|----------|
| Signed source archive (`.asc`) | ⚠️ Registry-specific | Maven Central path **does** GPG-sign via the Gradle `signing` plugin (`heads/java/build.gradle:22`, generated `signing { sign … }` in `generate-java-package-build.py:169`). Hackage and PyPI paths produce **no** signatures: no `gpg`/`sign`/`.asc` in `bin/prepare-release.sh`, `heads/haskell/bin/publish-hackage.sh`, `heads/python/bin/publish-pypi.sh` |
| Checksums (`.sha256`/`.sha512`) | ✅ Present | `bin/prepare-release.sh` Step 12 emits a `.sha512` alongside the canonical source archive (see P0 #2) |
| Single canonical signed source archive | ✅ Present | `bin/prepare-release.sh` Step 12 builds `hydra-<version>-src.tar.gz` via `git archive` with a detached `.asc` signature; this is the release of record (see P0 #2) |
| `KEYS` file (release-manager signing keys) | ✅ Present, populated | [`KEYS`](https://github.com/CategoricalData/hydra/blob/main/KEYS) carries the release manager's rsa4096 signing key (`FC93F19114D72013`) plus the import/verify/append procedure |

### Dependency surface (license compatibility)

Hydra's runtime dependency surface is small and, on inspection, **entirely ASF Category A
compatible** — no copyleft or Category X dependency was found in the core hosts. Evidence:

- **Haskell** (`heads/haskell/package.yaml`): `aeson`, `base`, `base64-bytestring`, `bytestring`,
  `containers`, `directory`, `filepath`, `regex-tdfa`, `scientific`, `SHA`, `split`, `text`,
  `vector` — all BSD-3 / MIT-class.
- **Python** (`heads/python/pyproject.toml`): zero runtime dependencies; `pytest`/`ruff`/`pyright`
  are dev-only.
- **Java** (`heads/java/build.gradle`, `generate-java-package-build.py`): `EXTERNAL_DEPS = {}` —
  the published per-package jars carry no third-party runtime deps. (External integrations are
  host-native overlay source under `overlay/<lang>/<pkg>/`, declaring their third-party deps in
  `build.json`; folded from the former `bindings/` tree in #511. **TODO:** re-audit whether and how
  these overlay-declared deps now reach the published per-package jars — see
  [docs/overlays.md](overlays.md).)
- **Scala** (`packages/hydra-scala/build.sbt`): `org.apache.commons:commons-text` (Apache-2.0),
  `scalactic`/`scalatest` (Apache-2.0, test scope).

The heavier external-dependency surface (rdf4j, ANTLR, Pegasus, Avro/Protobuf tooling) lives in
`demos/` and in the host-native overlays for `hydra-pg`/`hydra-rdf` (rdf4j, ANTLR — folded from the
former `bindings/` in #511). Demos are **not** part of the release publish set; the overlay-declared
integration deps should be re-audited for the publish set (see TODO above) — consistent with the
[Security wiki page](https://github.com/CategoricalData/hydra/wiki/Security)'s dependency-surface
distinction. The formal per-dependency license attestation is now recorded below in
[Dependency license attestation](#dependency-license-attestation); every dependency is ASF Category A
(permissive) except test/dev-scoped JUnit (EPL-2.0, Category B, never shipped at runtime).

### Reproducibility and provenance

| Item | State | Evidence |
|------|-------|----------|
| Documented build-from-source procedure | ✅ Present | `docs/release-workflow.md` + per-package READMEs |
| Dependency lock / pinning | ⚠️ Partial | `heads/python/uv.lock`, `heads/typescript/package-lock.json` present; **no** `cabal.project.freeze` (Haskell) or `gradle.lockfile` (Java/Scala). Haskell pins via Stack resolver + version bounds; Java/Scala pin exact versions in build files |
| SBOM (CycloneDX/SPDX) | ❌ Absent | no SBOM generation anywhere |
| CI release-artifact verification | ✅ Present | `.github/workflows/release-verify.yml` runs on a version tag: asserts `LICENSE`/`NOTICE`/`KEYS`, builds the source archive, asserts both files are inside it, round-trips the SHA-512, and verifies the signature once a key is registered |

## Gaps and recommendations

Ordered by how blocking each is for an Apache-style release. Each is independently actionable.

### P0 — blocks any Apache source release

1. **Add a root `NOTICE` file.** ✅ **Done** — `NOTICE` added at the repo root (Apache-2.0 §4(d)
   project notice; no third-party attributions needed, since the publish set bundles no third-party
   code). **Bundled into all three publish-set hosts' artifacts:** Hackage sdists
   (`assemble-haskell-distribution.sh` + `extra-source-files`), Java jars (`META-INF/{LICENSE,NOTICE}`
   via the generated `build.gradle` `metaInf` block), and Python wheels (`.dist-info/licenses/` via
   PEP 639 `license-files`). Each was verified by building the artifact and confirming both files are
   present with correct content. (Before this, Java/Python carried only metadata license declarations,
   not the files.)
2. **Produce a signed, checksummed canonical source archive.** ✅ **Done** — `bin/prepare-release.sh`
   Step 12 builds `hydra-<version>-src.tar.gz` via `git archive` from `HEAD`, emits a `.sha512`
   checksum and a detached `.asc` GPG signature, and asserts `LICENSE`/`NOTICE` are present. This is
   the "release of record"; the Hackage/Maven/PyPI artifacts are convenience binaries downstream of
   it. Signing degrades to a warning when no key is configured (set `HYDRA_RELEASE_SIGNING_KEY`).
3. **Publish a `KEYS` file** listing the release managers' public signing keys. ✅ **Done** — repo-root
   [`KEYS`](https://github.com/CategoricalData/hydra/blob/main/KEYS) carries the release manager's
   rsa4096 signing key (`FC93F19114D72013`) plus the import/verify/append procedure. The first signed
   release can use it (`HYDRA_RELEASE_SIGNING_KEY=FC93F19114D72013`).

#### Canonical source archive scope (decided 2026-06-11)

`bin/prepare-release.sh` Step 12 builds the archive with `git archive HEAD`, i.e. it ships the **full
tracked source tree** (equivalent to a `git clone` minus `.git`). No `.gitattributes` `export-ignore`
rules are applied, so the archive is a faithful mirror of the tracked repository at the release commit.

This rests on an important fact about Hydra's source model: **`dist/json` is canonical source, not
generated output** — it is the language-neutral representation of the kernel (the kernel *as data*)
from which every host is generated. The upstream Haskell/Java/Python DSLs are effectively the
*generator* that produces `dist/json`; a consumer building from the archive builds *from* `dist/json`,
never from an empty one. So `dist/json` must ship, and does.

`dist/json` is the only tracked tree under `dist/`. Every per-language output tree is gitignored and
regenerated from `dist/json` on each sync — on a cold clone, `dist/haskell` is seeded by the
published-host driver (`heads/haskell/json-driver/`; see
[build-system.md § Bootstrapping dist/haskell/ from the published host](build-system.md#bootstrapping-disthaskell-from-the-published-host)).
This cold rebuild is verified end-to-end (a genuinely cold `git archive` checkout seeds, builds, and
passes `stack test`), so the source archive stays buildable without any per-language tree entering it.

Note also that "a consumer needs a complex multi-language toolchain to build from source" is expected
and acceptable for a source release — convenience without the toolchain is exactly what the published
binaries (Hackage/Maven/PyPI) provide. The source archive optimizes for auditability, not convenience.

### P1 — required for a credible production/Apache posture

4. **Extend signing/checksums to the Hackage and PyPI paths**, or (cleaner) sign the canonical
   source archive (P0 #2) and treat the registry artifacts as unsigned convenience binaries — which
   is acceptable under Apache policy as long as the source archive is the voted artifact.
5. **Add a CI verification job** that, on a tag, asserts: `LICENSE` + `NOTICE` present in the
   assembled source archive; the archive builds; the `.asc`/`.sha512` verify. ✅ **Done** —
   `.github/workflows/release-verify.yml` runs on a version tag (and `workflow_dispatch`): asserts
   `LICENSE`/`NOTICE`/`KEYS` are tracked, builds the `git archive` source tarball, asserts both files
   are inside it, round-trips the SHA-512 checksum, and verifies the GPG signature once a key is
   registered in `KEYS` (skips with a notice until then). Toolchain-free; separate from `ci.yml`'s
   per-language matrix so it does not re-run that matrix on tags.
6. **Add a Scala `licenses` declaration** to `packages/hydra-scala/build.sbt` for parity with the
   other hosts. ✅ **Done** — `packages/hydra-scala/build.sbt` now declares
   `licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))`, matching the
   generated Java POM and the Haskell/Python build files.
7. **Produce a per-dependency license attestation** for the publish-set hosts. ✅ **Done** — recorded
   in [Dependency license attestation](#dependency-license-attestation) below, with each license
   verified against its authoritative source. All shipped runtime deps are Category A; the one
   Category B dep (JUnit, EPL-2.0) is test/dev-scoped only. A generated SBOM (P2) would automate this.

### P2 — recommended, not strictly required

8. **Generate an SBOM** (CycloneDX) per published package for dependency transparency. Low effort
   given the tiny dependency surface; valuable for downstream adopters' supply-chain scanning.
9. **Add dependency locks for Haskell and Java** (`cabal.project.freeze`, `gradle.lockfile`) to
   tighten reproducibility. Lower priority because Stack's resolver and the exact-version Gradle/sbt
   pins already give strong reproducibility.
10. **Add Apache license headers to hand-written source files** (the `overlay/`, `heads/`, and
    `packages/.../src` trees). Generated files carry a "do not edit" banner and can grow an SPDX
    line in the generator; hand-written files would take a header. This is a best-practice polish,
    not a blocker — the root `LICENSE` already governs the whole tree.

## Deferred until podling acceptance

These are **not** current gaps. They apply only once Hydra is an accepted Apache Incubator podling,
and adopting them now would be premature or incorrect:

- **`DISCLAIMER` file** — required for *incubating* releases, asserting podling status. Add it only
  when (if) Hydra is accepted into the Incubator, not before.
- **"-incubating" in artifact/version names** — Apache requires incubating releases to be marked
  as such. Not applicable to the current independent releases.
- **Publication to `dist.apache.org` + Apache mirrors / archive** — the canonical signed source
  archive would move to Apache's distribution infrastructure. Until then, GitHub release assets +
  the registry artifacts are the distribution channel.
- **PMC release vote** — Apache releases require a formal `[VOTE]` thread on the project's dev list.
  Hydra has no PMC yet; this is part of the governance track (Track 9), not the artifact pipeline.

## Dependency license attestation

This records the third-party dependency surface of the publish-set hosts and the license of each, so
the "Category A compatible" claim above is evidenced rather than asserted. Licenses were verified
against each package's authoritative source (Hackage `License` field, Maven POM `<licenses>`, project
LICENSE files), not from memory.

ASF [Category A](https://www.apache.org/legal/resolved.html#category-a) = permissive (Apache-2.0,
BSD-2/3-Clause, MIT, ISC, EDL-1.0); these may be bundled freely. Category B (e.g. EPL, MPL) may be
used in binary/test form but not shipped in source. Nothing copyleft (GPL/LGPL) or Category X was found.

### What counts as "shipped"

The distinction that matters: the **published per-package artifacts** vs. the **developer rollup /
test / binding** code that is not published.

- Published Java jars come from `dist/java/<pkg>/`, generated by `bin/lib/generate-java-package-build.py`
  with `EXTERNAL_DEPS = {}` — **no third-party runtime dependencies**.
- Published Python wheels come from `dist/python/<pkg>/` — `dependencies = []`, **no runtime deps**.
- The `packages/hydra-java/` Gradle build is the **developer rollup** (where `gradle test` runs and the
  bootstrap-demo `headsExtras` are bundled); its JUnit / json-io / commons-csv deps are dev/test/demo
  infrastructure, flagged in-tree as transitional, and do **not** reach the per-package published jars.
- The host-native overlays (`overlay/<lang>/<pkg>/`, folded from the former `bindings/` tree in #511)
  and `demos/` carry the heavier surface (rdf4j, ANTLR, etc.) and are separately publishable, outside
  the core release.

### Haskell (Hackage publish set)

GHC boot libraries ship with the compiler; the rest are Hackage dependencies. All BSD-class.

| Package | Constraint | License | Category | Note |
|---------|-----------|---------|----------|------|
| aeson | >=2.1.0 <2.3 | BSD-3-Clause | A | |
| base64-bytestring | >=1.2.1 <1.3 | BSD-3-Clause | A | |
| regex-tdfa | >=1.3.2 <1.4 | BSD-3-Clause | A | |
| scientific | >=0.3.7 <0.4 | BSD-3-Clause | A | |
| SHA | >=1.6.4 <1.7 | BSD-3-Clause | A | |
| split | >=0.2.3 <0.3 | BSD-3-Clause | A | |
| vector | >=0.12.0 <0.14 | BSD-3-Clause | A | |
| base | >=4.19.0 <4.22 | BSD-3-Clause | A | GHC boot lib |
| bytestring | >=0.11.5 <0.13 | BSD-3-Clause | A | GHC boot lib |
| containers | >=0.6.7 <0.8 | BSD-3-Clause | A | GHC boot lib |
| directory | >=1.3.6 <1.4 | BSD-3-Clause | A | GHC boot lib |
| filepath | >=1.4.200 <1.6 | BSD-3-Clause | A | GHC boot lib |
| text | >=2.0.2 <2.2 | BSD-2-Clause | A | GHC boot lib (BSD-2, not -3) |

### Python (PyPI publish set)

`dependencies = []` — no runtime dependencies. `pytest` / `ruff` / `pyright` are dev dependency-groups,
not shipped in the wheels.

### Java (Maven Central per-package publish set)

`EXTERNAL_DEPS = {}` — the published per-package jars carry no third-party runtime dependencies.
The dev-rollup-only deps (not in the published per-package jars) for completeness:

| Package | Version | License | Category | Scope |
|---------|---------|---------|----------|-------|
| org.junit.jupiter:junit-jupiter | 5.9.2 (bom) | EPL-2.0 | **B** | test / dev rollup — never shipped at runtime |
| com.cedarsoftware:json-io | 4.14.1 | Apache-2.0 | A | dev rollup (bootstrap-demo JsonIoCoder stub); transitional |
| org.apache.commons:commons-csv | 1.10.0 | Apache-2.0 | A | test |

### Scala (not yet a published artifact)

| Package | Version | License | Category | Scope |
|---------|---------|---------|----------|-------|
| org.apache.commons:commons-text | 1.12.0 | Apache-2.0 | A | runtime |
| org.scalactic:scalactic | 3.2.19 | Apache-2.0 | A | runtime |
| org.scalatest:scalatest | 3.2.19 | Apache-2.0 | A | test |

### Conclusion

Every **shipped runtime** dependency across the publish set is ASF Category A. The only Category B
dependency (JUnit, EPL-2.0) is test/dev-scoped and never enters a published runtime artifact, which is
permitted under ASF policy. No copyleft or Category X dependency exists anywhere in the publish set.
Because no third-party code is bundled into the published Java/Python artifacts, no third-party
attribution is required in `NOTICE` (see the NOTICE discussion above).

## Summary

Hydra is in a **strong starting position**: it is already Apache-2.0 licensed end to end, has a
small and (on inspection) fully Category-A-compatible dependency surface, a documented and
reproducible build, and a mature per-package publish pipeline. The release process is not, however,
yet an *Apache* release process. The blocking gaps are concentrated and well-defined: a root
`NOTICE`, a signed + checksummed canonical source archive with a published `KEYS` file, and CI
enforcement of those. The remaining items (Scala license field, SBOM, dependency locks, source
headers) are polish that raises the production-readiness bar without being release-blocking.
