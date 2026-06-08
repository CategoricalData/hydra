// Hand-written runtime: hydra.lib.optionals primitives.
//
// Signatures are flat (positional), matching Python's heads/python/lib/optionals.py.
// `cases` and `fromOptional` have lazy default positions — see
// docs/recipes/new-implementation.md "Lazy evaluation and thunking". The coder
// wraps those defaults in nullary arrows `() => expr`; we force the value
// here only when needed.
//
// Optional and callback parameters take `any` rather than precise `Optional<A>` —
// the kernel-generated coder routes Optionals through `any`-typed positions
// (continuation args, anonymous reducer accumulators). The runtime checks
// `tag` directly so this loosening is safe and silences TS2345 churn.

import type { Optional } from "../runtime.js";
import { Given, None } from "../runtime.js";

// `force` is inlined into each consumer (`cases`, `fromOptional`)
// to save one JS frame per call. See lib/logic.ts ifElse for the same
// rationale and feature_126_typescript-plan.md for measurements.

export const apply = (f: any, m: any): Optional<any> =>
  f.tag === "given" && m.tag === "given" ? Given(f.value(m.value)) : None;

export const bind = (m: any, f: (a: any) => any): Optional<any> =>
  m.tag === "given" ? f(m.value) : None;

export const cases = (m: any, n: any, j: (a: any) => any): any =>
  m.tag === "given" ? j(m.value) : (typeof n === "function" ? n() : n);

export const cat = (ms: any): readonly any[] => {
  const out: any[] = [];
  for (const m of (ms as readonly Optional<any>[])) if (m.tag === "given") out.push(m.value);
  return out;
};

export const compose = (f: (a: any) => any, g: (b: any) => any, x: any): Optional<any> => {
  const r = f(x);
  return r.tag === "given" ? g(r.value) : None;
};

export const fromOptional = (d: any, m: any): any =>
  m.tag === "given" ? m.value : (typeof d === "function" ? d() : d);

export const isGiven = (m: any): boolean => m.tag === "given";

export const isNone = (m: any): boolean => m.tag === "none";

export const map = (f: (a: any) => any, m: any): Optional<any> =>
  m.tag === "given" ? Given(f(m.value)) : None;

export const mapOptional = (f: (a: any) => any, xs: any): readonly any[] => {
  const out: any[] = [];
  for (const x of (xs as readonly any[])) { const y = f(x); if (y.tag === "given") out.push(y.value); }
  return out;
};

export const pure = <A>(x: A): Optional<A> => Given(x);

export const toList = (m: any): readonly any[] =>
  m.tag === "given" ? [m.value] : [];
