// hydra.lib.maps primitives

use std::collections::BTreeMap;
use std::collections::BTreeSet;

pub fn empty<K: Ord, V>() -> BTreeMap<K, V> {
    BTreeMap::new()
}

pub fn filter<K: Ord + Clone, V: Clone>(f: impl Fn(V) -> bool, m: BTreeMap<K, V>) -> BTreeMap<K, V> {
    m.into_iter().filter(|(_, v)| f(v.clone())).collect()
}

pub fn from_list<K: Ord, V>(pairs: Vec<(K, V)>) -> BTreeMap<K, V> {
    pairs.into_iter().collect()
}

pub fn insert<K: Ord, V>(k: K, v: V, m: BTreeMap<K, V>) -> BTreeMap<K, V> {
    let mut result = m;
    result.insert(k, v);
    result
}

pub fn is_empty<K, V>(m: BTreeMap<K, V>) -> bool {
    m.is_empty()
}

pub fn keys<K: Clone, V>(m: BTreeMap<K, V>) -> Vec<K> {
    m.keys().cloned().collect()
}

pub fn lookup<K: Ord, V: Clone>(k: K, m: BTreeMap<K, V>) -> Option<V> {
    m.get(&k).cloned()
}

pub fn map<K: Ord, V, W>(f: impl Fn(V) -> W, m: BTreeMap<K, V>) -> BTreeMap<K, W> {
    m.into_iter().map(|(k, v)| (k, f(v))).collect()
}

pub fn map_keys<K: Ord, L: Ord, V>(f: impl Fn(K) -> L, m: BTreeMap<K, V>) -> BTreeMap<L, V> {
    m.into_iter().map(|(k, v)| (f(k), v)).collect()
}

pub fn remove<K: Ord, V>(k: K, m: BTreeMap<K, V>) -> BTreeMap<K, V> {
    let mut result = m;
    result.remove(&k);
    result
}

pub fn singleton<K: Ord, V>(k: K, v: V) -> BTreeMap<K, V> {
    let mut m = BTreeMap::new();
    m.insert(k, v);
    m
}

pub fn size<K, V>(m: BTreeMap<K, V>) -> i32 {
    m.len() as i32
}

pub fn to_list<K: Clone, V: Clone>(m: BTreeMap<K, V>) -> Vec<(K, V)> {
    m.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
}

pub fn union<K: Ord, V>(m1: BTreeMap<K, V>, m2: BTreeMap<K, V>) -> BTreeMap<K, V> {
    let mut result = m1;
    result.extend(m2);
    result
}

pub fn values<K, V: Clone>(m: BTreeMap<K, V>) -> Vec<V> {
    m.values().cloned().collect()
}
