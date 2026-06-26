// Hand-written runtime: hydra.lib.sets primitives.
//
// Value-keyed sets, mirroring the structure of `hydra/lib/maps.ts`. JS's
// SameValueZero equality is inadequate for Hydra's wrapped types
// (Name = { value: "..." } etc.), so internally we canonicalize each
// element to a JSON string and keep the original element alongside.

const canon = (k: unknown): string => {
  switch (typeof k) {
    case "string": return "s:" + k;
    case "number": return "n:" + k;
    case "bigint": return "B:" + k.toString();
    case "boolean": return "b:" + k;
    case "undefined": return "u:";
    case "object":
      if (k === null) return "N:";
      try { return "o:" + JSON.stringify(k, (_kk, v) =>
        typeof v === "bigint" ? `__big__${v.toString()}` : v); }
      catch { return "o:" + String(k); }
    default: return String(k);
  }
};

type Internal<A> = Map<string, A>;

const TAG = Symbol.for("hydra.canonSet");
interface CanonSet<A> {
  readonly [TAG]: true;
  readonly _internal: Internal<A>;
}

const isCanon = <A>(s: unknown): s is CanonSet<A> =>
  !!s && typeof s === "object" && (s as { [TAG]?: boolean })[TAG] === true;

const mkCanon = <A>(internal: Internal<A>): CanonSet<A> =>
  ({ [TAG]: true as const, _internal: internal });

const toCanon = <A>(s: any | CanonSet<A>): CanonSet<A> => {
  if (isCanon<A>(s)) return s;
  const internal: Internal<A> = new Map();
  for (const x of s as ReadonlySet<A>) internal.set(canon(x), x);
  return mkCanon(internal);
};

// ===== API =====

export const empty: any = mkCanon<never>(new Map() as Internal<never>) as unknown as ReadonlySet<never>;

export const fromList = <A>(xs: readonly A[]): any => {
  const internal: Internal<A> = new Map();
  for (const x of xs) internal.set(canon(x), x);
  return mkCanon(internal) as unknown as ReadonlySet<A>;
};

// Sets are conceptually ordered (matching Haskell's Data.Set). Sort by
// the original element value (numeric/lexicographic) — canonicalized
// strings would put "n:10" before "n:2".
const compareElems = (a: unknown, b: unknown): number => {
  if (typeof a === "number" && typeof b === "number") return a - b;
  if (typeof a === "bigint" && typeof b === "bigint") return a < b ? -1 : a > b ? 1 : 0;
  if (typeof a === "string" && typeof b === "string") return a < b ? -1 : a > b ? 1 : 0;
  const ca = canon(a), cb = canon(b);
  return ca < cb ? -1 : ca > cb ? 1 : 0;
};

export const toList = (s: any): readonly any[] => {
  const c = toCanon(s);
  const entries = [...c._internal.entries()];
  entries.sort((a, b) => compareElems(a[1], b[1]));
  return entries.map((e) => e[1]);
};

export const insert = <A>(x: A, s: any): any => {
  const c = toCanon(s);
  const next: any = new Map(c._internal);
  next.set(canon(x), x);
  return mkCanon(next) as unknown as ReadonlySet<A>;
};

export const delete_ = <A>(x: A, s: any): any => {
  const c = toCanon(s);
  const next: any = new Map(c._internal);
  next.delete(canon(x));
  return mkCanon(next) as unknown as ReadonlySet<A>;
};

export const member = <A>(x: A, s: any): boolean => toCanon(s)._internal.has(canon(x));

export const size = <A>(s: any): number => toCanon(s)._internal.size;

export const union = <A>(a: any, b: any): any => {
  const ca = toCanon(a);
  const cb = toCanon(b);
  const next: any = new Map(ca._internal);
  for (const [ck, v] of cb._internal) next.set(ck, v);
  return mkCanon(next) as unknown as ReadonlySet<A>;
};

export const intersection = <A>(a: any, b: any): any => {
  const ca = toCanon(a);
  const cb = toCanon(b);
  const next: any = new Map();
  for (const [ck, v] of ca._internal) if (cb._internal.has(ck)) next.set(ck, v);
  return mkCanon(next) as unknown as ReadonlySet<A>;
};

export const difference = <A>(a: any, b: any): any => {
  const ca = toCanon(a);
  const cb = toCanon(b);
  const next: any = new Map();
  for (const [ck, v] of ca._internal) if (!cb._internal.has(ck)) next.set(ck, v);
  return mkCanon(next) as unknown as ReadonlySet<A>;
};

export const singleton = <A>(x: A): any => {
  const next: any = new Map();
  next.set(canon(x), x);
  return mkCanon(next) as unknown as ReadonlySet<A>;
};

export const map = (f: (a: any) => any, s: any): any => {
  const c = toCanon(s);
  const next: any = new Map();
  for (const x of c._internal.values()) { const nx = f(x); next.set(canon(nx), nx); }
  return mkCanon(next);
};

export const null_ = (s: any): boolean => toCanon(s)._internal.size === 0;

export const unions = (ss: any): any => {
  const next: any = new Map();
  for (const s of (ss as readonly any[])) for (const [ck, v] of toCanon(s)._internal) next.set(ck, v);
  return mkCanon(next);
};
