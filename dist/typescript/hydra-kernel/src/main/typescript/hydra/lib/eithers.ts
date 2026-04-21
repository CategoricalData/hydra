/**
 * Either primitive functions for Hydra-TypeScript.
 */

import type { Either, Maybe } from "../core.js";
import { left, right, just, nothing } from "../core.js";

export function bind<X, Y, Z>(
  e: Either<X, Y>,
): (f: (y: Y) => Either<X, Z>) => Either<X, Z> {
  return (f) => (e.tag === "right" ? f(e.value) : e);
}

export function bimap<X, Y, Z, W>(
  f: (x: X) => Z,
): (g: (y: Y) => W) => (e: Either<X, Y>) => Either<Z, W> {
  return (g) => (e) =>
    e.tag === "left" ? left(f(e.value)) : right(g(e.value));
}

export function either<X, Y, Z>(
  f: (x: X) => Z,
): (g: (y: Y) => Z) => (e: Either<X, Y>) => Z {
  return (g) => (e) => (e.tag === "left" ? f(e.value) : g(e.value));
}

export function foldl<X, Y, Z>(
  f: (x: X) => (y: Y) => Either<Z, X>,
): (init: X) => (xs: ReadonlyArray<Y>) => Either<Z, X> {
  return (init) => (xs) => {
    let acc: X = init;
    for (const y of xs) {
      const result = f(acc)(y);
      if (result.tag === "left") return result;
      acc = result.value;
    }
    return right(acc);
  };
}

export function fromLeft<L, R>(def: L): (e: Either<L, R>) => L {
  return (e) => (e.tag === "left" ? e.value : def);
}

export function fromRight<L, R>(def: R): (e: Either<L, R>) => R {
  return (e) => (e.tag === "right" ? e.value : def);
}

export function isLeft<L, R>(e: Either<L, R>): boolean {
  return e.tag === "left";
}

export function isRight<L, R>(e: Either<L, R>): boolean {
  return e.tag === "right";
}

export function lefts<L, R>(es: ReadonlyArray<Either<L, R>>): ReadonlyArray<L> {
  const result: L[] = [];
  for (const e of es) {
    if (e.tag === "left") result.push(e.value);
  }
  return result;
}

export function map<X, Y, Z>(
  f: (x: X) => Y,
): (e: Either<Z, X>) => Either<Z, Y> {
  return (e) => (e.tag === "right" ? right(f(e.value)) : e);
}

export function mapList<X, Y, Z>(
  f: (x: X) => Either<Z, Y>,
): (xs: ReadonlyArray<X>) => Either<Z, ReadonlyArray<Y>> {
  return (xs) => {
    const result: Y[] = [];
    for (const x of xs) {
      const r = f(x);
      if (r.tag === "left") return r;
      result.push(r.value);
    }
    return right(result);
  };
}

export function mapMaybe<X, Y, Z>(
  f: (x: X) => Either<Z, Y>,
): (mx: Maybe<X>) => Either<Z, Maybe<Y>> {
  return (mx) => {
    if (mx.tag === "nothing") return right(nothing());
    const r = f(mx.value);
    if (r.tag === "left") return r;
    return right(just(r.value));
  };
}

export function mapSet<X, Y, Z>(
  f: (x: X) => Either<Z, Y>,
): (xs: ReadonlySet<X>) => Either<Z, ReadonlySet<Y>> {
  return (xs) => {
    const result = new Set<Y>();
    for (const x of xs) {
      const r = f(x);
      if (r.tag === "left") return r;
      result.add(r.value);
    }
    return right(result);
  };
}

export function partitionEithers<L, R>(
  es: ReadonlyArray<Either<L, R>>,
): readonly [ReadonlyArray<L>, ReadonlyArray<R>] {
  const ls: L[] = [];
  const rs: R[] = [];
  for (const e of es) {
    if (e.tag === "left") ls.push(e.value);
    else rs.push(e.value);
  }
  return [ls, rs] as const;
}

export function rights<L, R>(es: ReadonlyArray<Either<L, R>>): ReadonlyArray<R> {
  const result: R[] = [];
  for (const e of es) {
    if (e.tag === "right") result.push(e.value);
  }
  return result;
}
