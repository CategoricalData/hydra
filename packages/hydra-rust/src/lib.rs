// Hydra kernel implementation in Rust
#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![allow(unreachable_patterns)]

/// A custom Either type (Rust's Result has different semantics)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

pub mod hydra;
