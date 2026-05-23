# Hydra-Go

A Go implementation of the [Hydra](https://github.com/CategoricalData/hydra) type
system and kernel.

**Status: head bud — generator-only, not yet a complete implementation.** The Go
coder lives under `packages/hydra-go/` and can generate Go source from Hydra
modules via `bin/sync-go.sh`, but:

- the coder has emission bugs and the generated output is not yet idiomatic;
- the hand-written runtime under `heads/go/` is mostly placeholder;
- Go does **not** currently host or pass the common test suite;
- Go does **not** yet participate in `bin/run-bootstrapping-demo.sh`.

Track the work on [#289](https://github.com/CategoricalData/hydra/issues/289).
For comparison, the TypeScript head graduated from head-bud to complete
implementation under [#126](https://github.com/CategoricalData/hydra/issues/126);
Go is following a similar trajectory.

Hydra is a type-aware data transformation toolkit which aims to be highly flexible
and portable. It has its roots in graph databases and type theory, and provides
APIs in Haskell, Java, Python, Scala, and Lisp. See the main Hydra
[README](https://github.com/CategoricalData/hydra) for more details.

## Layout

This package contains the Go coder DSL sources:

- `src/main/haskell/Hydra/Sources/Go/` — Go syntax model (`Syntax.hs`), language
  constraints (`Language.hs`), package manifest (`Manifest.hs`).
- `src/main/haskell/Hydra/Go/` — pre-extraction holdovers (`Coder.hs`,
  `Serde.hs`). These still live in the `hydra-go` package but have not yet been
  refactored into `Hydra/Sources/Go/` form. Tracked under the head-bud cleanup.

The runnable Go head (such as it is) lives under
[`heads/go/`](https://github.com/CategoricalData/hydra/tree/main/heads/go).

## Generating Go output

```bash
bin/sync-go.sh        # regenerate the kernel into dist/go/hydra-kernel/
```

`bin/sync.sh` Phase 4 skips Go because `host=go` is not in the active HOSTS list
(saving the ~60-minute self-host phase that doesn't currently work). To exercise
the full sync end-to-end, use the per-language wrapper or the explicit form:

```bash
bin/sync.sh --hosts haskell --targets go
```

Generated output lands in `dist/go/hydra-kernel/`. The `dist/go/` tree is **not**
checked in (kernel and downstream packages alike) — regenerate as needed.

## See also

- **[`hydra-kernel` README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-kernel/README.md)**
  — the core types this coder consumes.
- **[`hydra-typescript` README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-typescript/README.md)**
  — the recently-graduated head; useful as a template for the Go work.
- **[Creating a new Hydra implementation](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/new-implementation.md)**
  — the recipe for getting a head from "bud" to "complete."
- **Issue [#289](https://github.com/CategoricalData/hydra/issues/289)** — Go head
  tracking.
