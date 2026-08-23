# hydra-rust

The Hydra Rust coder.

`packages/hydra-rust/` contains the **coder**: the Hydra DSL sources that
translate kernel modules to Rust source. There is no runtime head yet —
Rust is a generation target only, and this package has not yet been wired
into the sync matrix as a first-class target (see issue #382).

## Layout

```
packages/hydra-rust/      # this package — DSL sources for the Rust coder
  src/main/haskell/Hydra/Sources/Rust/
    Coder.hs       # translation from Hydra modules to Rust source
    Language.hs    # Rust language constraints (supported features)
    Operators.hs   # Rust operator model
    Serde.hs       # Rust-side serialization support
    Syntax.hs      # Rust syntax model
```

## See also

- **[`hydra-kernel` README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-kernel/README.md)**
  — the core types this coder consumes.
- **[`docs/nongc-memory-discipline.md`](https://github.com/CategoricalData/hydra/blob/main/docs/nongc-memory-discipline.md)**
  — memory-management verdict for non-GC hosts, including Rust's `Box`-based
  ownership model (POC-confirmed).
- **[`poc/rust-nongc/`](https://github.com/CategoricalData/hydra/tree/main/poc/rust-nongc)**
  — the standalone proof-of-concept crate backing that verdict.
- **Issue [#382](https://github.com/CategoricalData/hydra/issues/382)** —
  this package split.
