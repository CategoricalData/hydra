/**
 * Map primitive functions for Hydra-TypeScript.
 */

import type { Maybe } from "../core.js";
import { just, nothing } from "../core.js";

export function empty<K, V>(): ReadonlyMap<K, V> {
  return new Map();
}

export function singleton<K, V>(k: K, v: V): ReadonlyMap<K, V> {
  return new Map([[k, v]]);
}

export function fromList<K, V>(
  entries: ReadonlyArray<readonly [K, V]>,
): ReadonlyMap<K, V> {
  return new Map(entries);
}

export function toList<K, V>(
  m: ReadonlyMap<K, V>,
): ReadonlyArray<readonly [K, V]> {
  return [...m.entries()];
}

export function insert<K, V>(
  k: K,
  v: V,
  m: ReadonlyMap<K, V>,
): ReadonlyMap<K, V> {
  const result = new Map(m);
  result.set(k, v);
  return result;
}

export function lookup<K, V>(k: K, m: ReadonlyMap<K, V>): Maybe<V> {
  const v = m.get(k);
  return v !== undefined ? just(v) : nothing();
}

export function member<K, V>(k: K, m: ReadonlyMap<K, V>): boolean {
  return m.has(k);
}

export function size<K, V>(m: ReadonlyMap<K, V>): number {
  return m.size;
}

export function keys<K, V>(m: ReadonlyMap<K, V>): ReadonlyArray<K> {
  return [...m.keys()];
}

export function values<K, V>(m: ReadonlyMap<K, V>): ReadonlyArray<V> {
  return [...m.values()];
}

export function mapValues<K, V, V2>(
  f: (v: V) => V2,
  m: ReadonlyMap<K, V>,
): ReadonlyMap<K, V2> {
  const result = new Map<K, V2>();
  for (const [k, v] of m) {
    result.set(k, f(v));
  }
  return result;
}
