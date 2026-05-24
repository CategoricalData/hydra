# Release workflow (procedural)

> **Audience:** Hydra release engineers running the release scripts. The
> user-facing release policy — what's coordinated across implementations,
> versioning scheme, what a release means — lives on the wiki at
> [Release process](https://github.com/CategoricalData/hydra/wiki/Release-process).

This document is the procedural companion: the exact sequence of scripts to
run, file paths to verify, and per-language publish steps. It supersedes the
"Synchronizing all implementations," "Release preparation," and per-language
publish sections currently still living on the wiki page. Until that wiki
material is fully migrated here, this doc carries pointers rather than the full
detail; the canonical procedural source for now remains
[Release process on the wiki](https://github.com/CategoricalData/hydra/wiki/Release-process).

## Quick reference

```bash
# 1. Bump the version (propagates to every config file)
bin/bump-version.sh 0.16.0

# 2. Regenerate every implementation (long-running)
bin/sync.sh

# 3. Verify + produce upload-ready artifacts in release-artifacts/
bin/prepare-release.sh

# 4. Update CHANGELOG.md, commit, tag, push
git tag 0.16.0 -m '0.16.0 release' HEAD
git push && git push --tags

# 5. Publish per-platform (see wiki for Hackage / Maven Central / PyPI / conda-forge / npm steps)
```

## Reference

For each step's detail — what `bin/bump-version.sh` patches, which phases
`bin/sync.sh` runs in what order, what `bin/prepare-release.sh` verifies,
per-language publish commands — see the corresponding sections in
[wiki/Release-process](https://github.com/CategoricalData/hydra/wiki/Release-process).

The procedural-content migration from wiki to `docs/` is in progress
(#383). When complete, this file will hold the full procedure and the wiki
page will keep only the policy and cross-implementation coordination
narrative.

## See also

- [Hydra build system](build-system.md) — how `bin/sync.sh` works internally
  (phases, cache model, generator stamps).
- [Code generation recipe](recipes/code-generation.md) — operational entry
  points for the sync scripts.
- [Repository maintenance](recipes/maintenance.md) — stale-file and digest
  hygiene before a release.
