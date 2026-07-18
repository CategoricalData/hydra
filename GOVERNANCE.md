# Hydra governance

This document describes how decisions are made in the Hydra project: who holds decision power
today, how contributors gain it, and how the project intends its governance to evolve. It is written
to be **honest about the current state** rather than to project a structure that is not yet in force.

Hydra is preparing for an [Apache Incubator](https://incubator.apache.org/) proposal, so this
document also records how the current model maps onto "the Apache Way." See
[docs/incubator-readiness.md](docs/incubator-readiness.md) for the full maturity-model assessment;
governance is the dimension that incubation is most designed to develop.

## Current model (pre-Incubator)

Hydra is, today, a **lead-maintainer-driven** open-source project with a small group of contributors.
This is the accurate description of how it operates — not an aspirational one.

- **Lead maintainer.** Joshua Shinavier ([@joshsh](https://github.com/joshsh)) is the founder and lead
  maintainer. He authors the large majority of changes, sets technical direction, and is the final
  arbiter of decisions that lack consensus. This concentration of decision-making is something the
  Apache Way deliberately moves *away from*, toward collective governance; the
  [Path to Apache governance](#path-to-apache-governance) section below describes that transition.
- **Contributors.** Anyone may contribute via pull request (see
  [CONTRIBUTING.md](CONTRIBUTING.md)). Several people have contributed code, fixes, and design input
  over the project's history; the authoritative list is the
  [GitHub contributors graph](https://github.com/CategoricalData/hydra/graphs/contributors).
- **How decisions are made now.** Routine technical decisions are made in pull requests and issues,
  in the open, on GitHub. Most changes use a *commit-then-review* style given the small team; larger
  or cross-cutting changes are discussed in an issue first. When discussion does not converge, the
  lead maintainer decides, with the reasoning recorded in the issue or PR.
- **Where discussion happens.** Public, written, and asynchronous: GitHub
  [issues](https://github.com/CategoricalData/hydra/issues) and pull requests are the primary record;
  the [Hydra Discord](https://bit.ly/hydra-on-discord) is used for lighter-weight community
  discussion. Decisions of record live in the issue/PR history, not in chat.

## Becoming a committer / maintainer

Hydra aims to be **meritocratic**: sustained, high-quality contribution earns greater
responsibility. The current ladder is deliberately simple, and will formalize as the community grows.

1. **Contributor** — anyone who opens an issue or pull request. No prior status required.
2. **Trusted contributor** — someone whose contributions have been consistently sound. Their PRs are
   reviewed with lighter scrutiny and their design input carries weight. Recognition is informal
   today (a function of track record), and is the pool from which committers are drawn.
3. **Committer / maintainer** — granted write access by the lead maintainer on the basis of a
   sustained record of valuable, trustworthy contribution (code, review, documentation, or community
   work — not code alone). Committers may merge changes and are expected to uphold the project's
   quality bar and coding standards.
4. **Release manager** — a committer who runs the release process documented in
   [docs/release-workflow.md](docs/release-workflow.md): version bump, full sync, `prepare-release.sh`,
   tagging, publishing to the registries, and signing the canonical source archive (their key is
   registered in [KEYS](KEYS)). The lead maintainer is the current release manager.

There is no fixed contribution count or time threshold; the test is demonstrated good judgment and
reliability over time. When in doubt, the lead maintainer errs toward extending trust to active,
constructive contributors.

## Path to Apache governance

If Hydra is accepted into the Apache Incubator, its governance changes substantially — and
intentionally. The mapping from the current model to the Apache Way:

| Today | Under Apache (incubating) |
|-------|---------------------------|
| Lead maintainer is final arbiter | A **Podling PMC (PPMC)** holds decision power collectively; no single dictator (Apache maturity CO60) |
| Informal trusted-contributor pool | A **documented committer ladder**; committers and PPMC members listed publicly (CS10) |
| Lead-maintainer decides when discussion stalls | **Lazy consensus + documented voting** on the dev list; vetoes valid only on code commits, with technical justification (CS20–CS40) |
| GitHub issues/PRs + Discord | A canonical **dev@ mailing list** as the channel of record for "important" decisions (CS50) |
| Lead maintainer is release manager | Each release is a **PPMC `[VOTE]`** and an act of the Foundation (RE20) |
| Project-led trademark/branding | **ASF holds the trademark**; the project becomes "Apache Hydra" (TB10–TB40) |

This transition is the core of what incubation provides: mentorship in building a self-governing,
vendor-neutral community. The items above are therefore **not** adopted before acceptance — doing so
would either be premature (an ASF PMC that does not exist) or misleading (claiming "Apache Hydra"
branding). They are recorded here as the deliberate target, evidence that the project understands the
destination.

## Independence

Hydra is independent of any single corporate or organizational control. Contributors act as
individuals. The project has been used in production in industry settings, but no company directs the
project or holds special decision rights (Apache maturity IN10, IN20).

## Code of conduct

All participation is governed by the project [Code of Conduct](CODE_OF_CONDUCT.md).

## Amending this document

Changes to this document are made by pull request and are subject to the same review as any other
change; substantive governance changes are decided by the lead maintainer today, and will move to
PPMC consensus under incubation.
