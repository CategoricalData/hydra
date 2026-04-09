# Hydra Rust

Early-stage Rust implementation of Hydra.

## Status

- 104/104 kernel modules generate from Hydra DSL to Rust
- Generated code uses `Rc`-based memory management for all named types
- 13 hand-written primitive libraries in `src/hydra/lib/`
- Work in progress: compilation errors remain

## Memory model

All named types (structs, enums, newtypes) use an `Rc` wrapping pattern:

```rust
// For a union type Term:
pub enum Term_Variant {
    Variable(Name),
    Application(Term, Term),
    Lambda(Name, Option<Type>, Term),
}
pub struct Term(pub Rc<Term_Variant>);

// For a record type Field:
pub struct Field_Variant {
    pub name: Name,
    pub term: Term,
}
pub struct Field(pub Rc<Field_Variant>);
```

This provides:
- Cheap cloning via reference counting (matches Hydra's immutable, shared value semantics)
- Automatic handling of recursive types (no manual `Box` placement needed)
- Uniform representation across all named types

### Future enhancements

- **Unbox non-recursive types**: Currently all named types are `Rc`-wrapped, even simple ones
  like `Name(String)` that have no recursive references.
  A future optimization can use `Schemas.requireType` to detect which types participate in
  recursive cycles and only `Rc`-wrap those, using direct structs/enums for the rest.
- **`Arc` for thread safety**: Replace `Rc` with `Arc` if concurrent access is needed.

## Build

```bash
cargo check    # Type-check without full compilation
cargo build    # Full build
```

Requires the `ordered-float` crate (specified in `Cargo.toml`).

## Code generation

To regenerate the Rust kernel modules:

```bash
cd hydra-ext
stack ghci hydra-ext:lib --ghci-options='+RTS -K256M -A32M -RTS'
> import Hydra.Ext.Generation
> writeRust "../hydra-rust/src/gen-main/rust" kernelModules kernelModules
```
