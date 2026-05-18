// Hand-written runtime: hydra.lib.eithers primitives.

import type { Either, Maybe } from "../core.js";
import { Left, Right, Just, Nothing } from "../core.js";

export const bimap = <L, R, L2, R2>(fl: (l: L) => L2) => (fr: (r: R) => R2) => (e: Either<L, R>): Either<L2, R2> =>
  e.tag === "left" ? Left(fl(e.value)) : Right(fr(e.value));

export const bind = <L, R, R2>(e: Either<L, R>) => (f: (r: R) => Either<L, R2>): Either<L, R2> =>
  e.tag === "right" ? f(e.value) : (e as unknown as Either<L, R2>);

export const either = <L, R, B>(fl: (l: L) => B) => (fr: (r: R) => B) => (e: Either<L, R>): B =>
  e.tag === "left" ? fl(e.value) : fr(e.value);

export const either_ = either;

export const isLeft = <L, R>(e: Either<L, R>): boolean => e.tag === "left";
export const isRight = <L, R>(e: Either<L, R>): boolean => e.tag === "right";

export const map = <L, R, R2>(f: (r: R) => R2) => (e: Either<L, R>): Either<L, R2> =>
  e.tag === "right" ? Right(f(e.value)) : (e as unknown as Either<L, R2>);

export const mapList = <A, L, B>(f: (a: A) => Either<L, B>) => (xs: readonly A[]): Either<L, readonly B[]> => {
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

export const fromLeft = <L>(d: L | (() => L)) => <R>(e: Either<L, R>): L =>
  e.tag === "left" ? e.value : force(d);

export const fromRight = <R>(d: R | (() => R)) => <L>(e: Either<L, R>): R =>
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

export const mapMaybe = <A, L, B>(f: (a: A) => Either<L, B>) => (m: Maybe<A>): Either<L, Maybe<B>> => {
  if (m.tag === "nothing") return Right(Nothing);
  const r = f(m.value);
  if (r.tag === "left") return r as unknown as Either<L, Maybe<B>>;
  return Right(Just(r.value));
};

export const mapSet = <A, L, B>(f: (a: A) => Either<L, B>) => (s: ReadonlySet<A>): Either<L, ReadonlySet<B>> => {
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
