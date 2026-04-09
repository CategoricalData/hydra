// hydra.lib.lists primitives

use std::collections::BTreeSet;

pub fn apply<A: Clone, B>(fs: Vec<std::rc::Rc<dyn Fn(A) -> B>>, xs: Vec<A>) -> Vec<B> {
    fs.iter().flat_map(|f| xs.iter().map(|x| f(x.clone())).collect::<Vec<_>>()).collect()
}

pub fn at<A: Clone>(i: i32, l: Vec<A>) -> A {
    l[i as usize].clone()
}

pub fn bind<A, B>(l: Vec<A>, f: impl Fn(A) -> Vec<B>) -> Vec<B> {
    l.into_iter().flat_map(f).collect()
}

pub fn concat<A>(ls: Vec<Vec<A>>) -> Vec<A> {
    ls.into_iter().flatten().collect()
}

pub fn concat2<A>(l1: Vec<A>, l2: Vec<A>) -> Vec<A> {
    let mut result = l1;
    result.extend(l2);
    result
}

pub fn cons<A>(x: A, xs: Vec<A>) -> Vec<A> {
    let mut result = vec![x];
    result.extend(xs);
    result
}

pub fn drop<A>(n: i32, l: Vec<A>) -> Vec<A> {
    l.into_iter().skip(n as usize).collect()
}

pub fn drop_while<A: Clone>(f: impl Fn(A) -> bool, l: Vec<A>) -> Vec<A> {
    let mut dropping = true;
    l.into_iter().filter(|x| {
        if dropping && f(x.clone()) { false } else { dropping = false; true }
    }).collect()
}

pub fn elem<A: PartialEq>(x: A, l: Vec<A>) -> bool {
    l.contains(&x)
}

pub fn filter<A: Clone>(f: impl Fn(A) -> bool, l: Vec<A>) -> Vec<A> {
    l.into_iter().filter(|x| f(x.clone())).collect()
}

pub fn find<A: Clone>(f: impl Fn(A) -> bool, l: Vec<A>) -> Option<A> {
    l.iter().find(|x| f((*x).clone())).cloned()
}

pub fn foldl<A, B>(f: impl Fn(B, A) -> B, init: B, l: Vec<A>) -> B {
    l.into_iter().fold(init, |acc, x| f(acc, x))
}

pub fn foldr<A, B>(f: impl Fn(A, B) -> B, init: B, l: Vec<A>) -> B {
    l.into_iter().rev().fold(init, |acc, x| f(x, acc))
}

pub fn head<A: Clone>(l: Vec<A>) -> A {
    l[0].clone()
}

pub fn init<A>(l: Vec<A>) -> Vec<A> {
    let mut v = l;
    v.pop();
    v
}

pub fn intercalate<A: Clone>(sep: Vec<A>, ls: Vec<Vec<A>>) -> Vec<A> {
    let mut result = Vec::new();
    for (i, l) in ls.into_iter().enumerate() {
        if i > 0 { result.extend(sep.clone()); }
        result.extend(l);
    }
    result
}

pub fn intersperse<A: Clone>(sep: A, l: Vec<A>) -> Vec<A> {
    let mut result = Vec::new();
    for (i, x) in l.into_iter().enumerate() {
        if i > 0 { result.push(sep.clone()); }
        result.push(x);
    }
    result
}

pub fn last<A: Clone>(l: Vec<A>) -> A {
    l.last().unwrap().clone()
}

pub fn length<A>(l: Vec<A>) -> i32 {
    l.len() as i32
}

pub fn map<A, B>(f: impl Fn(A) -> B, l: Vec<A>) -> Vec<B> {
    l.into_iter().map(f).collect()
}

pub fn nub<A: PartialEq + Clone>(l: Vec<A>) -> Vec<A> {
    let mut seen = Vec::new();
    for x in l {
        if !seen.contains(&x) { seen.push(x); }
    }
    seen
}

pub fn null<A>(l: Vec<A>) -> bool {
    l.is_empty()
}

pub fn pure_<A>(x: A) -> Vec<A> {
    vec![x]
}

pub fn partition<A: Clone>(f: impl Fn(A) -> bool, l: Vec<A>) -> (Vec<A>, Vec<A>) {
    l.into_iter().partition(|x| f(x.clone()))
}

pub fn replicate<A: Clone>(n: i32, x: A) -> Vec<A> {
    vec![x; n as usize]
}

pub fn reverse<A>(l: Vec<A>) -> Vec<A> {
    let mut v = l;
    v.reverse();
    v
}

pub fn safe_head<A: Clone>(l: Vec<A>) -> Option<A> {
    l.first().cloned()
}

pub fn singleton<A>(x: A) -> Vec<A> {
    vec![x]
}

pub fn sort<A: Ord>(l: Vec<A>) -> Vec<A> {
    let mut v = l;
    v.sort();
    v
}

pub fn sort_on<A: Clone, B: Ord>(f: impl Fn(A) -> B, l: Vec<A>) -> Vec<A> {
    let mut v = l;
    v.sort_by_key(|x| f(x.clone()));
    v
}

pub fn tail<A>(l: Vec<A>) -> Vec<A> {
    l.into_iter().skip(1).collect()
}

pub fn take<A>(n: i32, l: Vec<A>) -> Vec<A> {
    l.into_iter().take(n as usize).collect()
}

pub fn zip<A, B>(l1: Vec<A>, l2: Vec<B>) -> Vec<(A, B)> {
    l1.into_iter().zip(l2.into_iter()).collect()
}

pub fn zip_with<A, B, C>(f: impl Fn(A, B) -> C, l1: Vec<A>, l2: Vec<B>) -> Vec<C> {
    l1.into_iter().zip(l2.into_iter()).map(|(a, b)| f(a, b)).collect()
}
