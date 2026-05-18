// Hand-written runtime: hydra.lib.maybes primitives.
//
// `cases`, `maybe`, and `fromMaybe` have lazy default positions — see
// docs/recipes/new-implementation.md "Lazy evaluation and thunking".
// The coder wraps those defaults in nullary arrows `() => expr`; we
// force the value here only when needed. Plain (non-function) defaults
// are accepted unchanged.

import type { Maybe } from "../core.js";
import { Just, Nothing } from "../core.js";

const force = <A>(x: A | (() => A)): A =>
  typeof x === "function" ? (x as () => A)() : x;

export const apply = <A, B>(f: Maybe<(a: A) => B>) => (m: Maybe<A>): Maybe<B> =>
  f.tag === "just" && m.tag === "just" ? Just(f.value(m.value)) : Nothing;

export const bind = <A, B>(m: Maybe<A>) => (f: (a: A) => Maybe<B>): Maybe<B> =>
  m.tag === "just" ? f(m.value) : Nothing;

export const cases = <A, B>(m: Maybe<A>) => (n: B | (() => B)) => (j: (a: A) => B): B =>
  m.tag === "just" ? j(m.value) : force(n);

export const cat = <A>(ms: readonly Maybe<A>[]): readonly A[] => {
  const out: A[] = [];
  for (const m of ms) if (m.tag === "just") out.push(m.value);
  return out;
};

export const compose = <A, B, C>(f: (a: A) => Maybe<B>) => (g: (b: B) => Maybe<C>) => (x: A): Maybe<C> => {
  const r = f(x);
  return r.tag === "just" ? g(r.value) : Nothing;
};

export const fromMaybe = <A>(d: A | (() => A)) => (m: Maybe<A>): A =>
  m.tag === "just" ? m.value : force(d);

export const isJust = <A>(m: Maybe<A>): boolean => m.tag === "just";

export const isNothing = <A>(m: Maybe<A>): boolean => m.tag === "nothing";

export const map = <A, B>(f: (a: A) => B) => (m: Maybe<A>): Maybe<B> =>
  m.tag === "just" ? Just(f(m.value)) : Nothing;

export const mapMaybe = <A, B>(f: (a: A) => Maybe<B>) => (xs: readonly A[]): readonly B[] => {
  const out: B[] = [];
  for (const x of xs) { const y = f(x); if (y.tag === "just") out.push(y.value); }
  return out;
};

export const maybe = <A, B>(d: B | (() => B)) => (f: (a: A) => B) => (m: Maybe<A>): B =>
  m.tag === "just" ? f(m.value) : force(d);

export const pure = <A>(x: A): Maybe<A> => Just(x);

export const toList = <A>(m: Maybe<A>): readonly A[] =>
  m.tag === "just" ? [m.value] : [];
