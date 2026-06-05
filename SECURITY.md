# Security

Hydra is preparing for 1.0 and possible Apache Incubator entry.
This file is the GitHub-facing entry point for security reports and security posture.
For the fuller risk model, see the
[Hydra wiki: Security](https://github.com/CategoricalData/hydra/wiki/Security).

## Reporting a vulnerability

If GitHub shows a **Report a vulnerability** link for this repository, use that private reporting path.
If private vulnerability reporting is not available, contact the project maintainer privately before opening
a public issue.

Do not disclose exploitable vulnerabilities in public issues, pull requests, or discussion threads until the
project has had a chance to triage and coordinate a fix.

Please include:

- affected package, host, binding, or demo
- affected version or commit
- steps to reproduce
- expected and actual behavior
- impact and any known workaround

## Dependency surface

Hydra's core hosts are intentionally lightweight in external dependencies.
Most host implementations depend primarily on their language runtime and build toolchain.

The larger third-party dependency surface lives in `bindings/` and `demos/`.
Security reports should distinguish kernel/head issues from binding-specific or demo-specific issues.

The `overlay/` tree contains language-specific source code copied into Hydra's generated distribution packages.
It does not declare or introduce third-party dependencies.
Distribution package dependencies are visible in the generated build/config files under
`dist/<lang>/<package>/` (not version-controlled; these are generated on demand) and in the
[published packages](https://github.com/CategoricalData/hydra/wiki/Releases#published-packages).
