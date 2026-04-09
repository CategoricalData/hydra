// hydra.lib.pairs primitives

pub fn first<A: Clone, B>(p: (A, B)) -> A {
    p.0
}

pub fn second<A, B: Clone>(p: (A, B)) -> B {
    p.1
}

pub fn swap<A, B>(p: (A, B)) -> (B, A) {
    (p.1, p.0)
}

pub fn map_first<A, B, C>(f: impl Fn(A) -> C, p: (A, B)) -> (C, B) {
    (f(p.0), p.1)
}

pub fn map_second<A, B, C>(f: impl Fn(B) -> C, p: (A, B)) -> (A, C) {
    (p.0, f(p.1))
}
