// Hand-written runtime: hydra.lib.lists primitives.
//
// Signatures are flat (positional), matching Python's heads/python/lib/lists.py
// and the convention the new analyze-based TypeScript coder emits. Higher-order
// callbacks (those that take A → B) remain curried only because the *callback*
// itself, not the primitive, is curried in the kernel ABI.
//
// Array-shaped parameters accept `unknown` rather than `readonly A[]` so that
// kernel-generated code (which routes arrays through `unknown`/`any`-typed
// continuations) can call these without TS2345 noise. Runtime behavior is
// correct for any array-shaped input.

import type { Maybe } from "../runtime.js";
import { Just, Nothing } from "../runtime.js";

export const apply = <A, B>(fs: readonly ((a: A) => B)[], xs: any): readonly B[] => {
  const out: B[] = [];
  for (const f of fs) for (const x of xs) out.push(f(x));
  return out;
};

export const bind = <A, B>(xs: any, f: (a: A) => readonly B[]): readonly B[] => {
  const out: B[] = [];
  for (const x of xs) out.push(...f(x));
  return out;
};

// Return type defaults to `readonly any[]` (rather than `readonly unknown[]`)
// so the result is implicitly assignable into nominally-typed positions
// like `readonly Binding[]` — the kernel-generated code threads collections
// through `any`-typed continuations, so unifying at `any` removes TS2345
// "readonly unknown[] not assignable to readonly Binding[]" noise.
export const concat = (xss: unknown): readonly any[] => (xss as readonly any[][]).flat();

export const concat2 = (xs: unknown, ys: unknown): readonly any[] => [...(xs as readonly any[]), ...(ys as readonly any[])];

export const cons = (x: any, xs: unknown): readonly any[] => [x, ...(xs as readonly any[])];

export const drop = (n: number, xs: any): readonly any[] =>
  n <= 0 ? xs : xs.slice(n);

export const dropWhile = (p: (a: any) => boolean, xs: any): readonly any[] => {
  let i = 0;
  while (i < xs.length && p(xs[i]!)) i++;
  return xs.slice(i);
};

export const elem = <A>(x: A, xs: any): boolean => xs.includes(x);

export const filter = (p: (a: any) => boolean, xs: any): readonly any[] => xs.filter(p);

export const find = <A>(p: (a: A) => boolean, xs: any): Maybe<A> => {
  for (const x of xs) if (p(x)) return Just(x);
  return Nothing;
};

export const foldl = <A, B>(f: (b: B, a: A) => B, init: B, xs: any): B =>
  (xs as readonly A[]).reduce<B>((acc, x) => f(acc, x), init);

export const foldr = <A, B>(f: (a: A, b: B) => B, init: B, xs: any): B =>
  (xs as readonly A[]).reduceRight<B>((acc, x) => f(x, acc), init);

export const group = <A>(xs: any): readonly (readonly A[])[] => {
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

export const head = <A>(xs: any): A => {
  if (xs.length === 0) throw new Error("head: empty list");
  return xs[0]!;
};

export const intercalate = <A>(sep: readonly A[], xss: readonly (readonly A[])[]): readonly A[] => {
  const out: A[] = [];
  xss.forEach((xs, i) => {
    if (i > 0) out.push(...sep);
    out.push(...xs);
  });
  return out;
};

export const intersperse = (sep: any, xs: any): readonly any[] => {
  if (xs.length === 0) return [];
  const out: any[] = [xs[0]!];
  for (let i = 1; i < xs.length; i++) { out.push(sep); out.push(xs[i]!); }
  return out;
};

export const length = <A>(xs: any): number => xs.length;

export const map = (f: (a: any) => any, xs: any): readonly any[] => xs.map(f);

export const maybeAt = (i: number, xs: any): Maybe<any> =>
  (i >= 0 && i < xs.length) ? Just(xs[i]!) : Nothing;

export const maybeHead = (xs: any): Maybe<any> =>
  xs.length === 0 ? Nothing : Just(xs[0]!);

export const maybeInit = (xs: any): Maybe<readonly any[]> =>
  xs.length === 0 ? Nothing : Just(xs.slice(0, -1));

export const maybeLast = (xs: any): Maybe<any> =>
  xs.length === 0 ? Nothing : Just(xs[xs.length - 1]!);

export const maybeTail = (xs: any): Maybe<readonly any[]> =>
  xs.length === 0 ? Nothing : Just(xs.slice(1));

export const nub = (xs: any): readonly any[] => {
  const seen = new Set<any>();
  const out: any[] = [];
  for (const x of xs) if (!seen.has(x)) { seen.add(x); out.push(x); }
  return out;
};

// `null` is a reserved word in TS, so the coder emits `null_`. The runtime
// matches that name.
export const null_ = (xs: any): boolean => xs.length === 0;

export const partition = <A>(p: (a: A) => boolean, xs: any): readonly [readonly A[], readonly A[]] => {
  const yes: A[] = [];
  const no: A[] = [];
  for (const x of xs) (p(x) ? yes : no).push(x);
  return [yes, no] as const;
};

export const pure = <A>(x: A): readonly A[] => [x];

export const replicate = <A>(n: number, x: A): readonly A[] => Array.from({ length: n }, () => x);

export const reverse = (xs: any): readonly any[] => [...xs].reverse();

export const singleton = <A>(x: A): readonly A[] => [x];

const defaultCompare = (a: unknown, b: unknown): number => {
  if (a === b) return 0;
  return (a as never) < (b as never) ? -1 : 1;
};

export const sort = (xs: any): readonly any[] =>
  [...xs].sort(defaultCompare);

export const sortOn = <A, B>(key: (a: A) => B, xs: any): readonly A[] =>
  [...xs].sort((x, y) => defaultCompare(key(x), key(y)));

export const span = <A>(p: (a: A) => boolean, xs: any): readonly [readonly A[], readonly A[]] => {
  for (let i = 0; i < xs.length; i++) if (!p(xs[i]!)) return [xs.slice(0, i), xs.slice(i)] as const;
  return [xs, []] as const;
};

export const take = (n: number, xs: any): readonly any[] =>
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

export const uncons = <A>(xs: any): Maybe<readonly [A, readonly A[]]> =>
  xs.length === 0 ? Nothing : Just([xs[0]!, xs.slice(1)] as const);

// Return type is `readonly any[]` (rather than `readonly (readonly [unknown, unknown])[]`)
// so the result is implicitly assignable into nominally-typed pair-array
// positions like `readonly (readonly [Name, TypeScheme])[]`.
export const zip = (xs: any, ys: any): readonly any[] => {
  const n = Math.min(xs.length, ys.length);
  return Array.from({ length: n }, (_, i) => [xs[i]!, ys[i]!] as const);
};

export const zipWith = (f: (a: any, b: any) => any, xs: any, ys: any): readonly any[] => {
  const n = Math.min(xs.length, ys.length);
  return Array.from({ length: n }, (_, i) => f(xs[i]!, ys[i]!));
};
