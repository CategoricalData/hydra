// Hand-written runtime: hydra.lib.eithers primitives.
//
// Signatures are flat (positional), matching Python's heads/python/lib/eithers.py.
//
// Callback and Either parameters take `any` rather than precise `Either<L,R>`
// because the kernel-generated coder routes Either values through positions
// typed as `any`. Tightening these wastes type-checker time on synthesized
// code without catching real bugs; the runtime checks `tag` directly.

import type { Either, Maybe } from "../runtime.js";
import { Left, Right, Just, Nothing } from "../runtime.js";

export const bimap = (fl: (l: any) => any, fr: (r: any) => any, e: any): Either<any, any> =>
  e.tag === "left" ? Left(fl(e.value)) : Right(fr(e.value));

export const bind = (e: any, f: (r: any) => any): Either<any, any> =>
  e.tag === "right" ? f(e.value) : (e as unknown as Either<any, any>);

export const either = (fl: (l: any) => any, fr: (r: any) => any, e: any): any =>
  e.tag === "left" ? fl(e.value) : fr(e.value);

export const either_ = either;

export const isLeft = (e: any): boolean => e.tag === "left";
export const isRight = (e: any): boolean => e.tag === "right";

export const map = (f: (r: any) => any, e: any): Either<any, any> =>
  e.tag === "right" ? Right(f(e.value)) : (e as unknown as Either<any, any>);

export const mapList = <A, L, B>(f: (a: A) => Either<L, B>, xs: readonly A[]): Either<L, readonly B[]> => {
  const out: B[] = [];
  for (const x of xs) {
    const r = f(x);
    if (r.tag === "left") return r as unknown as Either<L, readonly B[]>;
    out.push(r.value);
  }
  return Right(out);
};

export const pure = <R, L = never>(x: R): Either<L, R> => Right(x);

// `fromLeft` / `fromRight` are lazy in their default position — see
// docs/recipes/new-implementation.md "Lazy evaluation and thunking".
const force = <A>(x: A | (() => A)): A =>
  typeof x === "function" ? (x as () => A)() : x;

export const fromLeft = <L, R>(d: L | (() => L), e: Either<L, R>): L =>
  e.tag === "left" ? e.value : force(d);

export const fromRight = <L, R>(d: R | (() => R), e: Either<L, R>): R =>
  e.tag === "right" ? e.value : force(d);

export const lefts = <L, R>(es: readonly Either<L, R>[]): readonly L[] => {
  const out: L[] = [];
  for (const e of es) if (e.tag === "left") out.push(e.value);
  return out;
};

export const rights = <L, R>(es: readonly Either<L, R>[]): readonly R[] => {
  const out: R[] = [];
  for (const e of es) if (e.tag === "right") out.push(e.value);
  return out;
};

export const mapMaybe = (f: (a: any) => any, m: any): Either<any, Maybe<any>> => {
  if (m.tag === "nothing") return Right(Nothing);
  const r = f(m.value);
  if (r.tag === "left") return r as unknown as Either<any, Maybe<any>>;
  return Right(Just(r.value));
};

export const mapSet = <A, L, B>(f: (a: A) => Either<L, B>, s: ReadonlySet<A>): Either<L, ReadonlySet<B>> => {
  const out = new Set<B>();
  for (const x of s) {
    const r = f(x);
    if (r.tag === "left") return r as unknown as Either<L, ReadonlySet<B>>;
    out.add(r.value);
  }
  return Right(out);
};

export const partitionEithers = <L, R>(es: readonly Either<L, R>[]): readonly [readonly L[], readonly R[]] => {
  const ls: L[] = [];
  const rs: R[] = [];
  for (const e of es) (e.tag === "left" ? ls : rs).push(e.value as never);
  return [ls, rs] as const;
};

// Left-fold over a list with an Either-returning function, short-
// circuiting on Left. Mirrors Python's `eithers.foldl`. The kernel
// emits `f` curried (`(acc) => (x) => Either<L, A>`); call accordingly.
export const foldl = (f: (acc: any) => (x: any) => any, acc: any, xs: readonly any[]): any => {
  let r = acc;
  for (const x of xs) {
    const e = f(r)(x);
    if (e.tag === "left") return e;
    r = e.value;
  }
  return Right(r);
};
