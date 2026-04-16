/**
 * List primitive functions for Hydra-TypeScript.
 */

import type { Maybe } from "../core.js";
import { just, nothing } from "../core.js";

export function concat<T>(
  lists: ReadonlyArray<ReadonlyArray<T>>,
): ReadonlyArray<T> {
  return lists.flat();
}

export function cons<T>(x: T, xs: ReadonlyArray<T>): ReadonlyArray<T> {
  return [x, ...xs];
}

export function head<T>(xs: ReadonlyArray<T>): T {
  if (xs.length === 0) {
    throw new Error("head of empty list");
  }
  return xs[0]!;
}

export function intercalate<T>(
  sep: ReadonlyArray<T>,
  lists: ReadonlyArray<ReadonlyArray<T>>,
): ReadonlyArray<T> {
  if (lists.length === 0) return [];
  const result: T[] = [...lists[0]!];
  for (let i = 1; i < lists.length; i++) {
    result.push(...sep, ...lists[i]!);
  }
  return result;
}

export function isEmpty<T>(xs: ReadonlyArray<T>): boolean {
  return xs.length === 0;
}

export function last<T>(xs: ReadonlyArray<T>): T {
  if (xs.length === 0) {
    throw new Error("last of empty list");
  }
  return xs[xs.length - 1]!;
}

export function length<T>(xs: ReadonlyArray<T>): number {
  return xs.length;
}

export function map<A, B>(
  f: (a: A) => B,
  xs: ReadonlyArray<A>,
): ReadonlyArray<B> {
  return xs.map(f);
}

export function filter<T>(
  f: (t: T) => boolean,
  xs: ReadonlyArray<T>,
): ReadonlyArray<T> {
  return xs.filter(f);
}

export function foldl<A, B>(
  f: (acc: B, a: A) => B,
  init: B,
  xs: ReadonlyArray<A>,
): B {
  return xs.reduce(f, init);
}

export function foldr<A, B>(
  f: (a: A, acc: B) => B,
  init: B,
  xs: ReadonlyArray<A>,
): B {
  return xs.reduceRight((acc, a) => f(a, acc), init);
}

export function lookup<T>(
  index: number,
  xs: ReadonlyArray<T>,
): Maybe<T> {
  if (index >= 0 && index < xs.length) {
    return just(xs[index]!);
  }
  return nothing();
}

export function null_<T>(xs: ReadonlyArray<T>): boolean {
  return xs.length === 0;
}

export function reverse<T>(xs: ReadonlyArray<T>): ReadonlyArray<T> {
  return [...xs].reverse();
}

export function sort<T>(xs: ReadonlyArray<T>): ReadonlyArray<T> {
  return [...xs].sort();
}

export function sortBy<T>(
  cmp: (a: T, b: T) => number,
  xs: ReadonlyArray<T>,
): ReadonlyArray<T> {
  return [...xs].sort(cmp);
}

export function tail<T>(xs: ReadonlyArray<T>): ReadonlyArray<T> {
  if (xs.length === 0) {
    throw new Error("tail of empty list");
  }
  return xs.slice(1);
}

export function take<T>(n: number, xs: ReadonlyArray<T>): ReadonlyArray<T> {
  return xs.slice(0, n);
}

export function drop<T>(n: number, xs: ReadonlyArray<T>): ReadonlyArray<T> {
  return xs.slice(n);
}

export function zip<A, B>(
  as: ReadonlyArray<A>,
  bs: ReadonlyArray<B>,
): ReadonlyArray<readonly [A, B]> {
  const len = Math.min(as.length, bs.length);
  const result: Array<readonly [A, B]> = [];
  for (let i = 0; i < len; i++) {
    result.push([as[i]!, bs[i]!] as const);
  }
  return result;
}
