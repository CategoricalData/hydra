// Hand-written runtime: hydra.lib.effects primitives.
//
// In TypeScript the Hydra type `effect<t>` is transparent (the TS target lacks a
// TypeVariantEffect), so `effect<t>` is just `t`: effectful programs are ordinary
// eager native code and "running the effect" simply means forcing the value. These
// primitives therefore reduce to ordinary applications. Mirrors the Python reference
// in heads/python/.../lib/effects.py and the Haskell reference Hydra.Lib.Effects
// (where effect<t> = IO t). For #494/#507.
//
// Optional and callback parameters take `any` rather than precise types — the
// kernel-generated coder routes these through `any`-typed positions; the runtime
// checks `tag` directly. Same loosening as the other overlay libs (see optionals.ts).

import type { Optional } from "../../../runtime.js";
import { Given, None } from "../../../runtime.js";

// Applicative apply: since effects are transparent, apply f to a.
export const apply = (f: (a: any) => any, a: any): any => f(a);

// Sequence two effectful computations: transparent, so apply f to a.
export const bind = (a: any, f: (a: any) => any): any => f(a);

// Kleisli composition: run f, then g on its result.
export const compose = (f: (a: any) => any, g: (b: any) => any, a: any): any => g(f(a));

// Left-fold over a list with an effect-returning function.
export const foldl = (f: (acc: any, x: any) => any, acc: any, values: readonly any[]): any => {
  let result = acc;
  for (const x of values) result = f(result, x);
  return result;
};

// Map a pure function over the result of an effect: transparent, so apply f.
export const map = (f: (a: any) => any, a: any): any => f(a);

// Map an effect-returning function over a list, collecting the results.
export const mapList = (f: (a: any) => any, values: readonly any[]): readonly any[] =>
  values.map((x) => f(x));

// Map an effect-returning function over an optional.
export const mapOptional = (f: (a: any) => any, x: any): Optional<any> =>
  x.tag === "given" ? Given(f(x.value)) : None;

// Lift a pure value into an effect: transparent, so identity.
export const pure = (a: any): any => a;
