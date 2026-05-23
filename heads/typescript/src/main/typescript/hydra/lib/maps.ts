// Hand-written runtime: hydra.lib.maps primitives.
//
// Signatures are flat (positional), matching Python's heads/python/lib/maps.py.
//
// Backed by JavaScript's `Map`, but with a value-equality wrapper for
// non-primitive keys. Hydra map keys are often wrapped values like
// `Name = { value: "..." }` — two structurally-equal Name objects from
// different constructions would fail JS's SameValueZero equality, so we
// canonicalize keys to JSON strings under the hood and store the
// original key alongside the value for retrieval.
//
// All operations are non-mutating: each modifier returns a fresh map.

import type { Maybe } from "../runtime.js";
import { Just, Nothing } from "../runtime.js";

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
const toCanon = <K, V>(m: any | CanonMap<K, V>): CanonMap<K, V> => {
  if (isCanon<K, V>(m)) return m;
  const internal: InternalMap<K, V> = new Map();
  for (const [k, v] of (m as ReadonlyMap<K, V>).entries()) {
    internal.set(canon(k), { key: k, value: v });
  }
  return mkCanon(internal);
};

// ===== Hydra-facing API. Inputs may be plain ReadonlyMap or CanonMap; outputs are always CanonMap. =====

// Hydra exposes `empty` as a value, not a function.
export const empty: any = mkCanon<never, never>(new Map()) as unknown as ReadonlyMap<never, never>;

export const fromList = <K, V>(pairs: readonly (readonly [K, V])[]): any => {
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

export const toList = (m: any): readonly (readonly [any, any])[] => {
  const c = toCanon(m);
  const entries = [...c._internal.entries()];
  entries.sort((a, b) => compareKeys(a[1].key, b[1].key));
  return entries.map((e) => [e[1].key, e[1].value] as const);
};

export const insert = <K, V>(k: K, v: V, m: any): any => {
  const c = toCanon(m);
  const next: any = new Map(c._internal);
  next.set(canon(k), { key: k, value: v });
  return mkCanon(next) as unknown as ReadonlyMap<K, V>;
};

export const delete_ = <K, V>(k: K, m: any): any => {
  const c = toCanon(m);
  const next: any = new Map(c._internal);
  next.delete(canon(k));
  return mkCanon(next) as unknown as ReadonlyMap<K, V>;
};

export const lookup = (k: any, m: any): Maybe<any> => {
  const c = toCanon(m);
  const e = c._internal.get(canon(k));
  return e === undefined ? Nothing : Just(e.value);
};

export const member = <K, V>(k: K, m: any): boolean =>
  toCanon(m)._internal.has(canon(k));

export const size = <K, V>(m: any): number => toCanon(m)._internal.size;

export const keys = (m: any): readonly any[] =>
  toList(m).map((p) => p[0]);

export const values = (m: any): readonly any[] =>
  toList(m).map((p) => p[1]);

// Left-biased union (Haskell Data.Map.union): on key collision, the
// value from `a` is kept.
export const union = (a: any, b: any): any => {
  const ca = toCanon(a);
  const cb = toCanon(b);
  const next: any = new Map(cb._internal);
  for (const [ck, e] of ca._internal) next.set(ck, e);
  return mkCanon(next);
};

export const filter = (p: (v: any) => boolean, m: any): any => {
  const c = toCanon(m);
  const next: any = new Map();
  for (const [ck, e] of c._internal) if (p(e.value)) next.set(ck, e);
  return mkCanon(next);
};

export const filterWithKey = (p: (k: any, v: any) => boolean, m: any): any => {
  const c = toCanon(m);
  const next: any = new Map();
  for (const [ck, e] of c._internal) if (p(e.key, e.value)) next.set(ck, e);
  return mkCanon(next);
};

export const map = (f: (v: any) => any, m: any): any => {
  const c = toCanon(m);
  const next: any = new Map();
  for (const [ck, e] of c._internal) next.set(ck, { key: e.key, value: f(e.value) });
  return mkCanon(next);
};

export const singleton = <K, V>(k: K, v: V): any => {
  const next: any = new Map();
  next.set(canon(k), { key: k, value: v });
  return mkCanon(next) as unknown as ReadonlyMap<K, V>;
};

// `f` is typed `(m: any) => any` (not `(Maybe<any>) => Maybe<any>`) so
// kernel-generated callsites that wrap an unknown value back into a
// Maybe-shaped object literal pass type-check; the runtime only reads
// `f(cur).tag`, which works for any object-shaped return value.
export const alter = (f: (m: any) => any, k: any, m: any): any => {
  const c = toCanon(m);
  const ck = canon(k);
  const cur: Maybe<any> = c._internal.has(ck) ? Just(c._internal.get(ck)!.value) : Nothing;
  const nextVal = f(cur);
  const next: any = new Map(c._internal);
  if (nextVal.tag === "just") next.set(ck, { key: k, value: nextVal.value }); else next.delete(ck);
  return mkCanon(next);
};

export const bimap = (fk: (k: any) => any, fv: (v: any) => any, m: any): any => {
  const c = toCanon(m);
  const next: any = new Map();
  for (const [, e] of c._internal) {
    const nk = fk(e.key);
    next.set(canon(nk), { key: nk, value: fv(e.value) });
  }
  return mkCanon(next);
};

export const elems = (m: any): readonly any[] => values(m);

// `findWithDefault` is lazy in its default position — see
// docs/recipes/new-implementation.md "Lazy evaluation and thunking".
export const findWithDefault = (d: any, k: any, m: any): any => {
  const c = toCanon(m);
  const e = c._internal.get(canon(k));
  return e === undefined ? (typeof d === "function" ? (d as () => any)() : d) : e.value;
};

export const mapKeys = (f: (k: any) => any, m: any): any => {
  const c = toCanon(m);
  const next: any = new Map();
  for (const [, e] of c._internal) {
    const nk = f(e.key);
    next.set(canon(nk), { key: nk, value: e.value });
  }
  return mkCanon(next);
};

export const null_ = (m: any): boolean => toCanon(m)._internal.size === 0;
