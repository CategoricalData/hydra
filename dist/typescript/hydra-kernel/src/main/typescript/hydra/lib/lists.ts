/**
 * List primitive functions for Hydra-TypeScript.
 */

import type { Maybe } from "../core.js";
import { just, nothing } from "../core.js";

export function apply<A, B>(
  fs: ReadonlyArray<(a: A) => B>,
): (xs: ReadonlyArray<A>) => ReadonlyArray<B> {
  return (xs) => {
    const result: B[] = [];
    for (const f of fs) {
      for (const x of xs) {
        result.push(f(x));
      }
    }
    return result;
  };
}

export function at<A>(index: number): (xs: ReadonlyArray<A>) => A {
  return (xs) => {
    if (index < 0 || index >= xs.length) {
      throw new Error(`Index ${index} out of bounds for list of length ${xs.length}`);
    }
    return xs[index]!;
  };
}

export function bind<A, B>(
  xs: ReadonlyArray<A>,
): (f: (a: A) => ReadonlyArray<B>) => ReadonlyArray<B> {
  return (f) => xs.flatMap(f);
}

export function concat<T>(
  lists: ReadonlyArray<ReadonlyArray<T>>,
): ReadonlyArray<T> {
  return lists.flat();
}

export function concat2<T>(
  xs: ReadonlyArray<T>,
): (ys: ReadonlyArray<T>) => ReadonlyArray<T> {
  return (ys) => [...xs, ...ys];
}

export function cons<T>(x: T): (xs: ReadonlyArray<T>) => ReadonlyArray<T> {
  return (xs) => [x, ...xs];
}

export function drop<T>(n: number): (xs: ReadonlyArray<T>) => ReadonlyArray<T> {
  return (xs) => xs.slice(n);
}

export function dropWhile<T>(
  f: (t: T) => boolean,
): (xs: ReadonlyArray<T>) => ReadonlyArray<T> {
  return (xs) => {
    let i = 0;
    while (i < xs.length && f(xs[i]!)) i++;
    return xs.slice(i);
  };
}

export function elem<T>(x: T): (xs: ReadonlyArray<T>) => boolean {
  return (xs) => xs.includes(x);
}

export function filter<T>(
  f: (t: T) => boolean,
): (xs: ReadonlyArray<T>) => ReadonlyArray<T> {
  return (xs) => xs.filter(f);
}

export function find<T>(
  f: (t: T) => boolean,
): (xs: ReadonlyArray<T>) => Maybe<T> {
  return (xs) => {
    const found = xs.find(f);
    return found !== undefined ? just(found) : nothing();
  };
}

export function foldl<A, B>(
  f: (acc: B) => (a: A) => B,
): (init: B) => (xs: ReadonlyArray<A>) => B {
  return (init) => (xs) => xs.reduce((acc, a) => f(acc)(a), init);
}

export function foldr<A, B>(
  f: (a: A) => (acc: B) => B,
): (init: B) => (xs: ReadonlyArray<A>) => B {
  return (init) => (xs) => xs.reduceRight((acc, a) => f(a)(acc), init);
}

export function group<T>(xs: ReadonlyArray<T>): ReadonlyArray<ReadonlyArray<T>> {
  if (xs.length === 0) return [];
  const result: T[][] = [];
  let current: T[] = [xs[0]!];
  for (let i = 1; i < xs.length; i++) {
    if (xs[i] === xs[i - 1]) {
      current.push(xs[i]!);
    } else {
      result.push(current);
      current = [xs[i]!];
    }
  }
  result.push(current);
  return result;
}

export function head<T>(xs: ReadonlyArray<T>): T {
  if (xs.length === 0) throw new Error("head of empty list");
  return xs[0]!;
}

export function init<T>(xs: ReadonlyArray<T>): ReadonlyArray<T> {
  if (xs.length === 0) throw new Error("init of empty list");
  return xs.slice(0, -1);
}

export function intercalate<T>(
  sep: ReadonlyArray<T>,
): (lists: ReadonlyArray<ReadonlyArray<T>>) => ReadonlyArray<T> {
  return (lists) => {
    if (lists.length === 0) return [];
    const result: T[] = [...lists[0]!];
    for (let i = 1; i < lists.length; i++) {
      result.push(...sep, ...lists[i]!);
    }
    return result;
  };
}

export function intersperse<T>(sep: T): (xs: ReadonlyArray<T>) => ReadonlyArray<T> {
  return (xs) => {
    if (xs.length <= 1) return [...xs];
    const result: T[] = [xs[0]!];
    for (let i = 1; i < xs.length; i++) {
      result.push(sep, xs[i]!);
    }
    return result;
  };
}

export function last<T>(xs: ReadonlyArray<T>): T {
  if (xs.length === 0) throw new Error("last of empty list");
  return xs[xs.length - 1]!;
}

export function length<T>(xs: ReadonlyArray<T>): number {
  return xs.length;
}

export function map<A, B>(
  f: (a: A) => B,
): (xs: ReadonlyArray<A>) => ReadonlyArray<B> {
  return (xs) => xs.map(f);
}

export function maybeAt<T>(index: number): (xs: ReadonlyArray<T>) => Maybe<T> {
  return (xs) => {
    if (index >= 0 && index < xs.length) return just(xs[index]!);
    return nothing();
  };
}

export function maybeHead<T>(xs: ReadonlyArray<T>): Maybe<T> {
  return xs.length > 0 ? just(xs[0]!) : nothing();
}

export function maybeInit<T>(xs: ReadonlyArray<T>): Maybe<ReadonlyArray<T>> {
  return xs.length > 0 ? just(xs.slice(0, -1)) : nothing();
}

export function maybeLast<T>(xs: ReadonlyArray<T>): Maybe<T> {
  return xs.length > 0 ? just(xs[xs.length - 1]!) : nothing();
}

export function maybeTail<T>(xs: ReadonlyArray<T>): Maybe<ReadonlyArray<T>> {
  return xs.length > 0 ? just(xs.slice(1)) : nothing();
}

export function nub<T>(xs: ReadonlyArray<T>): ReadonlyArray<T> {
  return [...new Set(xs)];
}

export function null_<T>(xs: ReadonlyArray<T>): boolean {
  return xs.length === 0;
}

export function partition<T>(
  f: (t: T) => boolean,
): (xs: ReadonlyArray<T>) => readonly [ReadonlyArray<T>, ReadonlyArray<T>] {
  return (xs) => {
    const yes: T[] = [];
    const no: T[] = [];
    for (const x of xs) {
      if (f(x)) yes.push(x);
      else no.push(x);
    }
    return [yes, no] as const;
  };
}

export function pure<T>(x: T): ReadonlyArray<T> {
  return [x];
}

export function replicate<T>(n: number): (x: T) => ReadonlyArray<T> {
  return (x) => Array.from({ length: n }, () => x);
}

export function reverse<T>(xs: ReadonlyArray<T>): ReadonlyArray<T> {
  return [...xs].reverse();
}

export function safeHead<T>(xs: ReadonlyArray<T>): Maybe<T> {
  return xs.length > 0 ? just(xs[0]!) : nothing();
}

export function singleton<T>(x: T): ReadonlyArray<T> {
  return [x];
}

export function sort<T>(xs: ReadonlyArray<T>): ReadonlyArray<T> {
  return [...xs].sort();
}

export function sortOn<A, B>(
  f: (a: A) => B,
): (xs: ReadonlyArray<A>) => ReadonlyArray<A> {
  return (xs) =>
    [...xs].sort((a, b) => {
      const fa = f(a);
      const fb = f(b);
      return fa < fb ? -1 : fa > fb ? 1 : 0;
    });
}

export function span<T>(
  f: (t: T) => boolean,
): (xs: ReadonlyArray<T>) => readonly [ReadonlyArray<T>, ReadonlyArray<T>] {
  return (xs) => {
    let i = 0;
    while (i < xs.length && f(xs[i]!)) i++;
    return [xs.slice(0, i), xs.slice(i)] as const;
  };
}

export function tail<T>(xs: ReadonlyArray<T>): ReadonlyArray<T> {
  if (xs.length === 0) throw new Error("tail of empty list");
  return xs.slice(1);
}

export function take<T>(n: number): (xs: ReadonlyArray<T>) => ReadonlyArray<T> {
  return (xs) => xs.slice(0, n);
}

export function transpose<T>(
  xss: ReadonlyArray<ReadonlyArray<T>>,
): ReadonlyArray<ReadonlyArray<T>> {
  if (xss.length === 0) return [];
  const maxLen = Math.max(...xss.map((xs) => xs.length));
  const result: T[][] = [];
  for (let i = 0; i < maxLen; i++) {
    const row: T[] = [];
    for (const xs of xss) {
      if (i < xs.length) row.push(xs[i]!);
    }
    result.push(row);
  }
  return result;
}

export function zip<A, B>(
  as: ReadonlyArray<A>,
): (bs: ReadonlyArray<B>) => ReadonlyArray<readonly [A, B]> {
  return (bs) => {
    const len = Math.min(as.length, bs.length);
    const result: Array<readonly [A, B]> = [];
    for (let i = 0; i < len; i++) {
      result.push([as[i]!, bs[i]!] as const);
    }
    return result;
  };
}

export function zipWith<A, B, C>(
  f: (a: A) => (b: B) => C,
): (as: ReadonlyArray<A>) => (bs: ReadonlyArray<B>) => ReadonlyArray<C> {
  return (as) => (bs) => {
    const len = Math.min(as.length, bs.length);
    const result: C[] = [];
    for (let i = 0; i < len; i++) {
      result.push(f(as[i]!)(bs[i]!));
    }
    return result;
  };
}
