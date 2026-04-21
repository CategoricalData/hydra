/**
 * Maybe primitive functions for Hydra-TypeScript.
 */

import type { Maybe } from "../core.js";
import { just, nothing } from "../core.js";

export function apply<A, B>(
  mf: Maybe<(a: A) => B>,
): (ma: Maybe<A>) => Maybe<B> {
  return (ma) => {
    if (mf.tag === "nothing" || ma.tag === "nothing") return nothing();
    return just(mf.value(ma.value));
  };
}

export function bind<A, B>(
  ma: Maybe<A>,
): (f: (a: A) => Maybe<B>) => Maybe<B> {
  return (f) => (ma.tag === "just" ? f(ma.value) : nothing());
}

export function cases<A, B>(
  ma: Maybe<A>,
): (def: B) => (f: (a: A) => B) => B {
  return (def) => (f) => (ma.tag === "just" ? f(ma.value) : def);
}

export function cat<T>(ms: ReadonlyArray<Maybe<T>>): ReadonlyArray<T> {
  const result: T[] = [];
  for (const m of ms) {
    if (m.tag === "just") result.push(m.value);
  }
  return result;
}

export function compose<A, B, C>(
  f: (a: A) => Maybe<B>,
): (g: (b: B) => Maybe<C>) => (a: A) => Maybe<C> {
  return (g) => (a) => {
    const mb = f(a);
    return mb.tag === "just" ? g(mb.value) : nothing();
  };
}

export function fromJust<T>(m: Maybe<T>): T {
  if (m.tag === "nothing") throw new Error("fromJust called on Nothing");
  return m.value;
}

export function fromMaybe<T>(def: T): (m: Maybe<T>) => T {
  return (m) => (m.tag === "just" ? m.value : def);
}

export function isJust<T>(m: Maybe<T>): boolean {
  return m.tag === "just";
}

export function isNothing<T>(m: Maybe<T>): boolean {
  return m.tag === "nothing";
}

export function map<A, B>(f: (a: A) => B): (m: Maybe<A>) => Maybe<B> {
  return (m) => (m.tag === "just" ? just(f(m.value)) : nothing());
}

export function mapMaybe<A, B>(
  f: (a: A) => Maybe<B>,
): (xs: ReadonlyArray<A>) => ReadonlyArray<B> {
  return (xs) => {
    const result: B[] = [];
    for (const x of xs) {
      const r = f(x);
      if (r.tag === "just") result.push(r.value);
    }
    return result;
  };
}

export function maybe<A, B>(
  def: B,
): (f: (a: A) => B) => (m: Maybe<A>) => B {
  return (f) => (m) => (m.tag === "just" ? f(m.value) : def);
}

export function pure<T>(x: T): Maybe<T> {
  return just(x);
}

export function toList<T>(m: Maybe<T>): ReadonlyArray<T> {
  return m.tag === "just" ? [m.value] : [];
}
