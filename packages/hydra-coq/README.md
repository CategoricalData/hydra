# hydra-coq

Coq coder for Hydra. Generates Coq source from Hydra kernel modules.

## Status

Generation-only target. The coder emits Coq files from kernel JSON; there
is no Coq-side runtime, harness, or test driver — generated output is
checked by running `coqc` against it, not by reproducing the kernel's
test suite in Coq. Coq does not currently participate in the
mutual-self-hosting set; it is a downstream emission target only.

## Layout

```
packages/hydra-coq/
└── src/main/haskell/Hydra/Sources/Coq/
    ├── Coder.hs          # the main Hydra-to-Coq translation
    ├── Environment.hs    # supporting DSL helpers for emission
    ├── Generate.hs       # higher-level generation entry point
    ├── Language.hs       # Coq-specific language metadata
    ├── Manifest.hs       # the hydra-coq package manifest
    ├── Serde.hs          # Coq syntax serializer
    ├── Syntax.hs         # AST of a subset of Coq
    └── Utils.hs          # shared helpers
```

The coder is a "head bud" in the current docs vocabulary: it lives in
`packages/hydra-coq/` but has no corresponding `heads/coq/` runtime
directory yet, because the Coq toolchain itself plays that role at
output-validation time.

## Generating Coq

The Coq coder is driven by the standard cross-language generation pipeline.
A scoped sync that emits Coq:

```bash
bin/sync.sh --hosts haskell --targets coq
```

(Coq is currently only a target, never a host, since there is no Coq
runtime for the kernel.) Output lands in `dist/coq/`.

## Known limitations

See the open issues tagged with `coq` and the
[design violations](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/maintenance.md) recipe for
ongoing work. The largest current issue is that some terms emit Coq
that exceeds reasonable `coqc` memory bounds (see #326); the underlying
fix is in the Coq coder's handling of inner typeLambdas.

## See also

- [Hydra root README](https://github.com/CategoricalData/hydra/blob/main/README.md) — overall project context
- [`packages/hydra-kernel/`](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-kernel) — the kernel that the
  Coq coder emits
