// Hand-written runtime: hydra.lib.lists primitives.
//
// Functions are curried — every primitive of arity > 1 is emitted by the
// coder as a sequence of single-argument applications.

import type { Maybe } from "../core.js";
import { Just, Nothing } from "../core.js";

export const apply = <A, B>(fs: readonly ((a: A) => B)[]) => (xs: readonly A[]): readonly B[] => {
  const out: B[] = [];
  for (const f of fs) for (const x of xs) out.push(f(x));
  return out;
};

export const bind = <A, B>(xs: readonly A[]) => (f: (a: A) => readonly B[]): readonly B[] => {
  const out: B[] = [];
  for (const x of xs) out.push(...f(x));
  return out;
};

export const concat = <A>(xss: readonly (readonly A[])[]): readonly A[] => xss.flat();

export const concat2 = <A>(xs: readonly A[]) => (ys: readonly A[]): readonly A[] => [...xs, ...ys];

export const cons = <A>(x: A) => (xs: readonly A[]): readonly A[] => [x, ...xs];

export const drop = <A>(n: number) => (xs: readonly A[]): readonly A[] =>
  n <= 0 ? xs : xs.slice(n);

export const dropWhile = <A>(p: (a: A) => boolean) => (xs: readonly A[]): readonly A[] => {
  let i = 0;
  while (i < xs.length && p(xs[i]!)) i++;
  return xs.slice(i);
};

export const elem = <A>(x: A) => (xs: readonly A[]): boolean => xs.includes(x);

export const filter = <A>(p: (a: A) => boolean) => (xs: readonly A[]): readonly A[] => xs.filter(p);

export const find = <A>(p: (a: A) => boolean) => (xs: readonly A[]): Maybe<A> => {
  for (const x of xs) if (p(x)) return Just(x);
  return Nothing;
};

export const foldl = <A, B>(f: (b: B) => (a: A) => B) => (init: B) => (xs: readonly A[]): B =>
  xs.reduce((acc, x) => f(acc)(x), init);

export const foldr = <A, B>(f: (a: A) => (b: B) => B) => (init: B) => (xs: readonly A[]): B =>
  xs.reduceRight((acc, x) => f(x)(acc), init);

export const group = <A>(xs: readonly A[]): readonly (readonly A[])[] => {
  if (xs.length === 0) return [];
  const out: A[][] = [];
  let cur: A[] = [xs[0]!];
  for (let i = 1; i < xs.length; i++) {
    if (xs[i] === xs[i - 1]) cur.push(xs[i]!);
    else { out.push(cur); cur = [xs[i]!]; }
  }
  out.push(cur);
  return out;
};

export const head = <A>(xs: readonly A[]): A => {
  if (xs.length === 0) throw new Error("head: empty list");
  return xs[0]!;
};

export const intercalate = <A>(sep: readonly A[]) => (xss: readonly (readonly A[])[]): readonly A[] => {
  const out: A[] = [];
  xss.forEach((xs, i) => {
    if (i > 0) out.push(...sep);
    out.push(...xs);
  });
  return out;
};

export const intersperse = <A>(sep: A) => (xs: readonly A[]): readonly A[] => {
  if (xs.length === 0) return [];
  const out: A[] = [xs[0]!];
  for (let i = 1; i < xs.length; i++) { out.push(sep); out.push(xs[i]!); }
  return out;
};

export const length = <A>(xs: readonly A[]): number => xs.length;

export const map = <A, B>(f: (a: A) => B) => (xs: readonly A[]): readonly B[] => xs.map(f);

export const maybeAt = <A>(i: number) => (xs: readonly A[]): Maybe<A> =>
  (i >= 0 && i < xs.length) ? Just(xs[i]!) : Nothing;

export const maybeHead = <A>(xs: readonly A[]): Maybe<A> =>
  xs.length === 0 ? Nothing : Just(xs[0]!);

export const maybeInit = <A>(xs: readonly A[]): Maybe<readonly A[]> =>
  xs.length === 0 ? Nothing : Just(xs.slice(0, -1));

export const maybeLast = <A>(xs: readonly A[]): Maybe<A> =>
  xs.length === 0 ? Nothing : Just(xs[xs.length - 1]!);

export const maybeTail = <A>(xs: readonly A[]): Maybe<readonly A[]> =>
  xs.length === 0 ? Nothing : Just(xs.slice(1));

export const nub = <A>(xs: readonly A[]): readonly A[] => {
  const seen = new Set<A>();
  const out: A[] = [];
  for (const x of xs) if (!seen.has(x)) { seen.add(x); out.push(x); }
  return out;
};

// `null` is a reserved word in TS, so the coder emits `null_`. The runtime
// matches that name.
export const null_ = <A>(xs: readonly A[]): boolean => xs.length === 0;

export const partition = <A>(p: (a: A) => boolean) => (xs: readonly A[]): readonly [readonly A[], readonly A[]] => {
  const yes: A[] = [];
  const no: A[] = [];
  for (const x of xs) (p(x) ? yes : no).push(x);
  return [yes, no] as const;
};

export const pure = <A>(x: A): readonly A[] => [x];

export const replicate = <A>(n: number) => (x: A): readonly A[] => Array.from({ length: n }, () => x);

export const reverse = <A>(xs: readonly A[]): readonly A[] => [...xs].reverse();

export const singleton = <A>(x: A): readonly A[] => [x];

const defaultCompare = (a: unknown, b: unknown): number => {
  if (a === b) return 0;
  return (a as never) < (b as never) ? -1 : 1;
};

export const sort = <A>(xs: readonly A[]): readonly A[] =>
  [...xs].sort(defaultCompare);

export const sortOn = <A, B>(key: (a: A) => B) => (xs: readonly A[]): readonly A[] =>
  [...xs].sort((x, y) => defaultCompare(key(x), key(y)));

export const span = <A>(p: (a: A) => boolean) => (xs: readonly A[]): readonly [readonly A[], readonly A[]] => {
  for (let i = 0; i < xs.length; i++) if (!p(xs[i]!)) return [xs.slice(0, i), xs.slice(i)] as const;
  return [xs, []] as const;
};

export const take = <A>(n: number) => (xs: readonly A[]): readonly A[] =>
  n <= 0 ? [] : xs.slice(0, n);

export const transpose = <A>(xss: readonly (readonly A[])[]): readonly (readonly A[])[] => {
  if (xss.length === 0) return [];
  const w = Math.max(...xss.map((r) => r.length));
  const out: A[][] = [];
  for (let i = 0; i < w; i++) {
    const row: A[] = [];
    for (const r of xss) if (i < r.length) row.push(r[i]!);
    out.push(row);
  }
  return out;
};

export const uncons = <A>(xs: readonly A[]): Maybe<readonly [A, readonly A[]]> =>
  xs.length === 0 ? Nothing : Just([xs[0]!, xs.slice(1)] as const);

export const zip = <A, B>(xs: readonly A[]) => (ys: readonly B[]): readonly (readonly [A, B])[] => {
  const n = Math.min(xs.length, ys.length);
  return Array.from({ length: n }, (_, i) => [xs[i]!, ys[i]!] as const);
};

export const zipWith = <A, B, C>(f: (a: A) => (b: B) => C) => (xs: readonly A[]) => (ys: readonly B[]): readonly C[] => {
  const n = Math.min(xs.length, ys.length);
  return Array.from({ length: n }, (_, i) => f(xs[i]!)(ys[i]!));
};
