# Rust POC — structural tree-ownership (#678)

Proof-of-concept for [#678](https://github.com/CategoricalData/hydra/issues/678): a recursive,
Hydra-shaped `Term` type in Rust, constructed and dropped, demonstrated **leak-free** using the
`Box` structural-ownership discipline.

See the design note (`docs/nongc-memory-discipline.md`) for the full discipline and per-language verdicts.

## What it demonstrates

- Hydra record → `struct`, union → `enum`, recursive child → `Box<T>`, `list`→`Vec`, `optional`→`Option`.
- **No `Rc`, no `RefCell`, no explicit lifetimes** — unique ownership of an immutable acyclic tree needs none.
- `every_node_dropped_exactly_once` proves the leak-freedom claim with an atomic drop counter: every owned
  node is dropped exactly once when the root is dropped (short count = leak; abort = double-free; neither
  occurs).

## Run

```sh
cargo test
```

For a stronger, allocator-level leak check (optional, requires the nightly `miri` component):

```sh
rustup toolchain install nightly
rustup +nightly component add miri
cargo +nightly miri test
```

`cargo test` alone is sufficient to demonstrate the deterministic drop behavior; `miri` additionally
verifies there is no undefined behavior or leaked allocation under an instrumented interpreter.
