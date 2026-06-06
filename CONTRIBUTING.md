# Contributing to Hydra

Thank you for your interest in contributing to Hydra.
This file exists so GitHub can surface the contribution entry point when someone opens an issue or pull request.

For the full contributor guide, see the
[Hydra wiki: Contributing](https://github.com/CategoricalData/hydra/wiki/Contributing).

Quick links:

- [Contributor setup](docs/contributor-setup.md) — install the toolchain tier you need.
- [Developer recipes](docs/recipes/index.md) — task-oriented guides for changing Hydra.
- [Build system](docs/build-system.md) — sync phases, generated artifacts, and caches.
- [Coding style](https://github.com/CategoricalData/hydra/wiki/Coding-style) — required conventions.
- [Troubleshooting](docs/troubleshooting.md) — common build, sync, and test failures.

Core expectations:

- Do not edit generated files under `dist/` by hand.
- If generated code is wrong, fix the generator or source DSL.
- Do not proceed past failing tests; investigate and fix the failure first.
- Keep changes focused, explain how they were tested, and follow the coding style.
- AI-assisted contributions must meet the same review, testing, and provenance standards as any other change.
