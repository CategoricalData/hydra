// Hand-written runtime: hydra.lib.maps primitives.
//
// Backed by JavaScript's `Map`, but with a value-equality wrapper for
// non-primitive keys. Hydra map keys are often wrapped values like
// `Name = { value: "..." }` — two structurally-equal Name objects from
// different constructions would fail JS's SameValueZero equality, so we
// canonicalize keys to JSON strings under the hood and store the
// original key alongside the value for retrieval.
//
// All operations are non-mutating: each modifier returns a fresh map.

import type { Maybe } from "../core.js";
import { Just, Nothing } from "../core.js";

const canon = (k: unknown): string => {
  switch (typeof k) {
    case "string": return "s:" + k;
    case "number": return "n:" + k;
    case "bigint": return "B:" + k.toString();
    case "boolean": return "b:" + k;
    case "undefined": return "u:";
    case "object":
      if (k === null) return "N:";
      // Sets and Maps as keys are rare but possible; fall through to JSON.
      try { return "o:" + JSON.stringify(k, (_kk, v) =>
        typeof v === "bigint" ? `__big__${v.toString()}` : v); }
      catch { return "o:" + String(k); }
    default: return String(k);
  }
};

interface InternalEntry<K, V> { readonly key: K; readonly value: V; }
type InternalMap<K, V> = Map<string, InternalEntry<K, V>>;

// Tag our internal value-keyed maps so we can distinguish them from a plain
// ReadonlyMap. The kernel-generated `Map_<K,V>` interface (if any) lives at
// the type level and is erased at runtime.
const TAG = Symbol.for("hydra.canonMap");
interface CanonMap<K, V> {
  readonly [TAG]: true;
  readonly _internal: InternalMap<K, V>;
}

const isCanon = <K, V>(m: unknown): m is CanonMap<K, V> =>
  !!m && typeof m === "object" && (m as { [TAG]?: boolean })[TAG] === true;

const mkCanon = <K, V>(internal: InternalMap<K, V>): CanonMap<K, V> =>
  ({ [TAG]: true as const, _internal: internal });

// Convert any ReadonlyMap to a CanonMap (preserving entry order).
const toCanon = <K, V>(m: ReadonlyMap<K, V> | CanonMap<K, V>): CanonMap<K, V> => {
  if (isCanon<K, V>(m)) return m;
  const internal: InternalMap<K, V> = new Map();
  for (const [k, v] of (m as ReadonlyMap<K, V>).entries()) {
    internal.set(canon(k), { key: k, value: v });
  }
  return mkCanon(internal);
};

// ===== Hydra-facing API. Inputs may be plain ReadonlyMap or CanonMap; outputs are always CanonMap. =====

// Hydra exposes `empty` as a value, not a function.
export const empty: ReadonlyMap<never, never> = mkCanon<never, never>(new Map()) as unknown as ReadonlyMap<never, never>;

export const fromList = <K, V>(pairs: readonly (readonly [K, V])[]): ReadonlyMap<K, V> => {
  const internal: InternalMap<K, V> = new Map();
  for (const [k, v] of pairs) internal.set(canon(k), { key: k, value: v });
  return mkCanon(internal) as unknown as ReadonlyMap<K, V>;
};

// Maps (matching Haskell's Data.Map) are conceptually ordered. Sort by
// the original key value (numeric for numbers, lexicographic for strings,
// etc.) rather than the canonicalized JSON string — otherwise numbers
// like `10` sort before `2` (because "n:10" < "n:2" lexically).
const compareKeys = (a: unknown, b: unknown): number => {
  if (typeof a === "number" && typeof b === "number") return a - b;
  if (typeof a === "bigint" && typeof b === "bigint") return a < b ? -1 : a > b ? 1 : 0;
  if (typeof a === "string" && typeof b === "string") return a < b ? -1 : a > b ? 1 : 0;
  // Object keys (Names etc.): fall back to canonical string compare.
  const ca = canon(a), cb = canon(b);
  return ca < cb ? -1 : ca > cb ? 1 : 0;
};

export const toList = <K, V>(m: ReadonlyMap<K, V>): readonly (readonly [K, V])[] => {
  const c = toCanon(m);
  const entries = [...c._internal.entries()];
  entries.sort((a, b) => compareKeys(a[1].key, b[1].key));
  return entries.map((e) => [e[1].key, e[1].value] as const);
};

export const insert = <K, V>(k: K) => (v: V) => (m: ReadonlyMap<K, V>): ReadonlyMap<K, V> => {
  const c = toCanon(m);
  const next: InternalMap<K, V> = new Map(c._internal);
  next.set(canon(k), { key: k, value: v });
  return mkCanon(next) as unknown as ReadonlyMap<K, V>;
};

export const delete_ = <K, V>(k: K) => (m: ReadonlyMap<K, V>): ReadonlyMap<K, V> => {
  const c = toCanon(m);
  const next: InternalMap<K, V> = new Map(c._internal);
  next.delete(canon(k));
  return mkCanon(next) as unknown as ReadonlyMap<K, V>;
};

export const lookup = <K, V>(k: K) => (m: ReadonlyMap<K, V>): Maybe<V> => {
  const c = toCanon(m);
  const e = c._internal.get(canon(k));
  return e === undefined ? Nothing : Just(e.value);
};

export const member = <K, V>(k: K) => (m: ReadonlyMap<K, V>): boolean =>
  toCanon(m)._internal.has(canon(k));

export const size = <K, V>(m: ReadonlyMap<K, V>): number => toCanon(m)._internal.size;

export const keys = <K, V>(m: ReadonlyMap<K, V>): readonly K[] =>
  toList(m).map((p) => p[0]);

export const values = <K, V>(m: ReadonlyMap<K, V>): readonly V[] =>
  toList(m).map((p) => p[1]);

// Left-biased union (Haskell Data.Map.union): on key collision, the
// value from `a` is kept.
export const union = <K, V>(a: ReadonlyMap<K, V>) => (b: ReadonlyMap<K, V>): ReadonlyMap<K, V> => {
  const ca = toCanon(a);
  const cb = toCanon(b);
  const next: InternalMap<K, V> = new Map(cb._internal);
  for (const [ck, e] of ca._internal) next.set(ck, e);
  return mkCanon(next) as unknown as ReadonlyMap<K, V>;
};

export const filter = <K, V>(p: (v: V) => boolean) => (m: ReadonlyMap<K, V>): ReadonlyMap<K, V> => {
  const c = toCanon(m);
  const next: InternalMap<K, V> = new Map();
  for (const [ck, e] of c._internal) if (p(e.value)) next.set(ck, e);
  return mkCanon(next) as unknown as ReadonlyMap<K, V>;
};

export const filterWithKey = <K, V>(p: (k: K) => (v: V) => boolean) => (m: ReadonlyMap<K, V>): ReadonlyMap<K, V> => {
  const c = toCanon(m);
  const next: InternalMap<K, V> = new Map();
  for (const [ck, e] of c._internal) if (p(e.key)(e.value)) next.set(ck, e);
  return mkCanon(next) as unknown as ReadonlyMap<K, V>;
};

export const map = <K, V, W>(f: (v: V) => W) => (m: ReadonlyMap<K, V>): ReadonlyMap<K, W> => {
  const c = toCanon(m);
  const next: InternalMap<K, W> = new Map();
  for (const [ck, e] of c._internal) next.set(ck, { key: e.key, value: f(e.value) });
  return mkCanon(next) as unknown as ReadonlyMap<K, W>;
};

export const singleton = <K, V>(k: K) => (v: V): ReadonlyMap<K, V> => {
  const next: InternalMap<K, V> = new Map();
  next.set(canon(k), { key: k, value: v });
  return mkCanon(next) as unknown as ReadonlyMap<K, V>;
};

export const alter = <K, V>(f: (m: Maybe<V>) => Maybe<V>) => (k: K) => (m: ReadonlyMap<K, V>): ReadonlyMap<K, V> => {
  const c = toCanon(m);
  const ck = canon(k);
  const cur: Maybe<V> = c._internal.has(ck) ? Just(c._internal.get(ck)!.value) : Nothing;
  const nextVal = f(cur);
  const next: InternalMap<K, V> = new Map(c._internal);
  if (nextVal.tag === "just") next.set(ck, { key: k, value: nextVal.value }); else next.delete(ck);
  return mkCanon(next) as unknown as ReadonlyMap<K, V>;
};

export const bimap = <K1, V1, K2, V2>(fk: (k: K1) => K2) => (fv: (v: V1) => V2) => (m: ReadonlyMap<K1, V1>): ReadonlyMap<K2, V2> => {
  const c = toCanon(m);
  const next: InternalMap<K2, V2> = new Map();
  for (const [, e] of c._internal) {
    const nk = fk(e.key);
    next.set(canon(nk), { key: nk, value: fv(e.value) });
  }
  return mkCanon(next) as unknown as ReadonlyMap<K2, V2>;
};

export const elems = <K, V>(m: ReadonlyMap<K, V>): readonly V[] => values(m);

// `findWithDefault` is lazy in its default position — see
// docs/recipes/new-implementation.md "Lazy evaluation and thunking".
export const findWithDefault = <K, V>(d: V | (() => V)) => (k: K) => (m: ReadonlyMap<K, V>): V => {
  const c = toCanon(m);
  const e = c._internal.get(canon(k));
  return e === undefined ? (typeof d === "function" ? (d as () => V)() : d) : e.value;
};

export const mapKeys = <K1, K2, V>(f: (k: K1) => K2) => (m: ReadonlyMap<K1, V>): ReadonlyMap<K2, V> => {
  const c = toCanon(m);
  const next: InternalMap<K2, V> = new Map();
  for (const [, e] of c._internal) {
    const nk = f(e.key);
    next.set(canon(nk), { key: nk, value: e.value });
  }
  return mkCanon(next) as unknown as ReadonlyMap<K2, V>;
};

export const null_ = <K, V>(m: ReadonlyMap<K, V>): boolean => toCanon(m)._internal.size === 0;
