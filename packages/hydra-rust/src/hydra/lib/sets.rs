// hydra.lib.sets primitives

use std::collections::BTreeSet;

pub fn contains<A: Ord>(x: A, s: BTreeSet<A>) -> bool {
    s.contains(&x)
}

pub fn difference<A: Ord + Clone>(s1: BTreeSet<A>, s2: BTreeSet<A>) -> BTreeSet<A> {
    s1.difference(&s2).cloned().collect()
}

pub fn empty<A: Ord>() -> BTreeSet<A> {
    BTreeSet::new()
}

pub fn from_list<A: Ord>(l: Vec<A>) -> BTreeSet<A> {
    l.into_iter().collect()
}

pub fn insert<A: Ord>(x: A, s: BTreeSet<A>) -> BTreeSet<A> {
    let mut result = s;
    result.insert(x);
    result
}

pub fn intersection<A: Ord + Clone>(s1: BTreeSet<A>, s2: BTreeSet<A>) -> BTreeSet<A> {
    s1.intersection(&s2).cloned().collect()
}

pub fn is_empty<A>(s: BTreeSet<A>) -> bool {
    s.is_empty()
}

pub fn map<A: Ord, B: Ord>(f: impl Fn(A) -> B, s: BTreeSet<A>) -> BTreeSet<B> {
    s.into_iter().map(f).collect()
}

pub fn member<A: Ord>(x: A, s: BTreeSet<A>) -> bool {
    s.contains(&x)
}

pub fn singleton<A: Ord>(x: A) -> BTreeSet<A> {
    let mut s = BTreeSet::new();
    s.insert(x);
    s
}

pub fn size<A>(s: BTreeSet<A>) -> i32 {
    s.len() as i32
}

pub fn to_list<A: Clone>(s: BTreeSet<A>) -> Vec<A> {
    s.into_iter().collect()
}

pub fn union<A: Ord + Clone>(s1: BTreeSet<A>, s2: BTreeSet<A>) -> BTreeSet<A> {
    s1.union(&s2).cloned().collect()
}
