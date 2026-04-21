/**
 * Map primitive functions for Hydra-TypeScript.
 */

import type { Maybe } from "../core.js";
import { just, nothing } from "../core.js";

export function alter<K, V>(
  f: (mv: Maybe<V>) => Maybe<V>,
): (k: K) => (m: ReadonlyMap<K, V>) => ReadonlyMap<K, V> {
  return (k) => (m) => {
    const result = new Map(m);
    const current: Maybe<V> = result.has(k) ? just(result.get(k)!) : nothing();
    const updated = f(current);
    if (updated.tag === "just") {
      result.set(k, updated.value);
    } else {
      result.delete(k);
    }
    return result;
  };
}

export function bimap<K1, K2, V1, V2>(
  fk: (k: K1) => K2,
): (fv: (v: V1) => V2) => (m: ReadonlyMap<K1, V1>) => ReadonlyMap<K2, V2> {
  return (fv) => (m) => {
    const result = new Map<K2, V2>();
    for (const [k, v] of m) {
      result.set(fk(k), fv(v));
    }
    return result;
  };
}

export function delete_<K, V>(k: K): (m: ReadonlyMap<K, V>) => ReadonlyMap<K, V> {
  return (m) => {
    const result = new Map(m);
    result.delete(k);
    return result;
  };
}

export function elems<K, V>(m: ReadonlyMap<K, V>): ReadonlyArray<V> {
  return [...m.values()];
}

export function empty<K, V>(): ReadonlyMap<K, V> {
  return new Map();
}

export function filter<K, V>(
  f: (v: V) => boolean,
): (m: ReadonlyMap<K, V>) => ReadonlyMap<K, V> {
  return (m) => {
    const result = new Map<K, V>();
    for (const [k, v] of m) {
      if (f(v)) result.set(k, v);
    }
    return result;
  };
}

export function filterWithKey<K, V>(
  f: (k: K) => (v: V) => boolean,
): (m: ReadonlyMap<K, V>) => ReadonlyMap<K, V> {
  return (m) => {
    const result = new Map<K, V>();
    for (const [k, v] of m) {
      if (f(k)(v)) result.set(k, v);
    }
    return result;
  };
}

export function findWithDefault<K, V>(
  def: V,
): (k: K) => (m: ReadonlyMap<K, V>) => V {
  return (k) => (m) => {
    const v = m.get(k);
    return v !== undefined ? v : def;
  };
}

export function fromList<K, V>(
  entries: ReadonlyArray<readonly [K, V]>,
): ReadonlyMap<K, V> {
  return new Map(entries);
}

export function insert<K, V>(
  k: K,
): (v: V) => (m: ReadonlyMap<K, V>) => ReadonlyMap<K, V> {
  return (v) => (m) => {
    const result = new Map(m);
    result.set(k, v);
    return result;
  };
}

export function keys<K, V>(m: ReadonlyMap<K, V>): ReadonlyArray<K> {
  return [...m.keys()];
}

export function lookup<K, V>(k: K): (m: ReadonlyMap<K, V>) => Maybe<V> {
  return (m) => {
    const v = m.get(k);
    return v !== undefined ? just(v) : nothing();
  };
}

export function map<K, V1, V2>(
  f: (v: V1) => V2,
): (m: ReadonlyMap<K, V1>) => ReadonlyMap<K, V2> {
  return (m) => {
    const result = new Map<K, V2>();
    for (const [k, v] of m) {
      result.set(k, f(v));
    }
    return result;
  };
}

export function mapKeys<K1, K2, V>(
  f: (k: K1) => K2,
): (m: ReadonlyMap<K1, V>) => ReadonlyMap<K2, V> {
  return (m) => {
    const result = new Map<K2, V>();
    for (const [k, v] of m) {
      result.set(f(k), v);
    }
    return result;
  };
}

export function member<K, V>(k: K): (m: ReadonlyMap<K, V>) => boolean {
  return (m) => m.has(k);
}

export function null_<K, V>(m: ReadonlyMap<K, V>): boolean {
  return m.size === 0;
}

export function singleton<K, V>(k: K): (v: V) => ReadonlyMap<K, V> {
  return (v) => new Map([[k, v]]);
}

export function size<K, V>(m: ReadonlyMap<K, V>): number {
  return m.size;
}

export function toList<K, V>(
  m: ReadonlyMap<K, V>,
): ReadonlyArray<readonly [K, V]> {
  return [...m.entries()];
}

export function union<K, V>(
  a: ReadonlyMap<K, V>,
): (b: ReadonlyMap<K, V>) => ReadonlyMap<K, V> {
  return (b) => {
    const result = new Map(b);
    for (const [k, v] of a) {
      result.set(k, v);
    }
    return result;
  };
}
