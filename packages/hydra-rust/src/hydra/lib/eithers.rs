// hydra.lib.eithers primitives

use crate::Either;

pub fn bind<L, A, B>(e: Either<L, A>, f: impl Fn(A) -> Either<L, B>) -> Either<L, B> {
    match e {
        Either::Left(l) => Either::Left(l),
        Either::Right(a) => f(a),
    }
}

pub fn either_<L, R, A>(fl: impl Fn(L) -> A, fr: impl Fn(R) -> A, e: Either<L, R>) -> A {
    match e {
        Either::Left(l) => fl(l),
        Either::Right(r) => fr(r),
    }
}

pub fn from_left<L: Clone, R>(default: L, e: Either<L, R>) -> L {
    match e {
        Either::Left(l) => l,
        Either::Right(_) => default,
    }
}

pub fn from_right<L, R: Clone>(default: R, e: Either<L, R>) -> R {
    match e {
        Either::Left(_) => default,
        Either::Right(r) => r,
    }
}

pub fn is_left<L, R>(e: Either<L, R>) -> bool {
    matches!(e, Either::Left(_))
}

pub fn is_right<L, R>(e: Either<L, R>) -> bool {
    matches!(e, Either::Right(_))
}

pub fn map<L, A, B>(f: impl Fn(A) -> B, e: Either<L, A>) -> Either<L, B> {
    match e {
        Either::Left(l) => Either::Left(l),
        Either::Right(a) => Either::Right(f(a)),
    }
}

pub fn map_list<L, A, B>(f: impl Fn(A) -> Either<L, B>, l: Vec<A>) -> Either<L, Vec<B>> {
    let mut result = Vec::new();
    for x in l {
        match f(x) {
            Either::Left(l) => return Either::Left(l),
            Either::Right(b) => result.push(b),
        }
    }
    Either::Right(result)
}

pub fn pure_<L, A>(x: A) -> Either<L, A> {
    Either::Right(x)
}
