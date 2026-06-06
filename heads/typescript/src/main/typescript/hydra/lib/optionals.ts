// Hand-written runtime: hydra.lib.optionals primitives.
//
// Signatures are flat (positional), matching Python's heads/python/lib/optionals.py.
// `cases` and `fromOptional` have lazy default positions — see
// docs/recipes/new-implementation.md "Lazy evaluation and thunking". The coder
// wraps those defaults in nullary arrows `() => expr`; we force the value
// here only when needed.
//
// Maybe and callback parameters take `any` rather than precise `Maybe<A>` —
// the kernel-generated coder routes Maybes through `any`-typed positions
// (continuation args, anonymous reducer accumulators). The runtime checks
// `tag` directly so this loosening is safe and silences TS2345 churn.

import type { Maybe } from "../runtime.js";
import { Just, Nothing } from "../runtime.js";

// `force` is inlined into each consumer (`cases`, `fromOptional`)
// to save one JS frame per call. See lib/logic.ts ifElse for the same
// rationale and feature_126_typescript-plan.md for measurements.

export const apply = (f: any, m: any): Maybe<any> =>
  f.tag === "just" && m.tag === "just" ? Just(f.value(m.value)) : Nothing;

export const bind = (m: any, f: (a: any) => any): Maybe<any> =>
  m.tag === "just" ? f(m.value) : Nothing;

export const cases = (m: any, n: any, j: (a: any) => any): any =>
  m.tag === "just" ? j(m.value) : (typeof n === "function" ? n() : n);

export const cat = (ms: any): readonly any[] => {
  const out: any[] = [];
  for (const m of (ms as readonly Maybe<any>[])) if (m.tag === "just") out.push(m.value);
  return out;
};

export const compose = (f: (a: any) => any, g: (b: any) => any, x: any): Maybe<any> => {
  const r = f(x);
  return r.tag === "just" ? g(r.value) : Nothing;
};

export const fromOptional = (d: any, m: any): any =>
  m.tag === "just" ? m.value : (typeof d === "function" ? d() : d);

export const isGiven = (m: any): boolean => m.tag === "just";

export const isNone = (m: any): boolean => m.tag === "nothing";

export const map = (f: (a: any) => any, m: any): Maybe<any> =>
  m.tag === "just" ? Just(f(m.value)) : Nothing;

export const mapOptional = (f: (a: any) => any, xs: any): readonly any[] => {
  const out: any[] = [];
  for (const x of (xs as readonly any[])) { const y = f(x); if (y.tag === "just") out.push(y.value); }
  return out;
};

export const pure = <A>(x: A): Maybe<A> => Just(x);

export const toList = (m: any): readonly any[] =>
  m.tag === "just" ? [m.value] : [];
