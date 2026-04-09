// hydra.lib.equality primitives

use std::cmp::Ordering;

pub fn compare<A: Ord>(a: A, b: A) -> Ordering {
    a.cmp(&b)
}

pub fn equal<A: PartialEq>(a: A, b: A) -> bool {
    a == b
}

pub fn gt<A: PartialOrd>(a: A, b: A) -> bool {
    a > b
}

pub fn gte<A: PartialOrd>(a: A, b: A) -> bool {
    a >= b
}

pub fn identity<A>(x: A) -> A {
    x
}

pub fn lt<A: PartialOrd>(a: A, b: A) -> bool {
    a < b
}

pub fn lte<A: PartialOrd>(a: A, b: A) -> bool {
    a <= b
}

pub fn max<A: Ord>(a: A, b: A) -> A {
    std::cmp::max(a, b)
}

pub fn min<A: Ord>(a: A, b: A) -> A {
    std::cmp::min(a, b)
}
