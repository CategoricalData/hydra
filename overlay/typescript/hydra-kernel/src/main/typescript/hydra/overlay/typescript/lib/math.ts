// Hand-written runtime: hydra.lib.math primitives.
//
// Signatures are flat (positional), matching Python's heads/python/lib/math.py.

import type { Optional } from "../runtime.js";
import { Given, None } from "../runtime.js";

export const add = (a: number, b: number): number => a + b;
export const sub = (a: number, b: number): number => a - b;
export const mul = (a: number, b: number): number => a * b;
export const div = (a: number, b: number): number => a / b;
export const mod = (a: number, b: number): number => a % b;
export const neg = (a: number): number => -a;
export const negate = (a: number): number => -a;
export const abs = (a: number): number => Math.abs(a);

export const max_ = (a: number, b: number): number => Math.max(a, b);
export const min_ = (a: number, b: number): number => Math.min(a, b);
// Aliases so generated code that emits .max/.min (without trailing _) works.
export const max = max_;
export const min = min_;

export const ceil = (f: number): number => Math.ceil(f);
export const floor = (f: number): number => Math.floor(f);

export const maybeDiv = (a: number, b: number): Optional<number> =>
  b === 0 ? None : Given(Math.trunc(a / b));

export const maybeMod = (a: number, b: number): Optional<number> =>
  b === 0 ? None : Given(a % b);

// Haskell-style inclusive range `[a..b]`: includes both endpoints.
// `range 1 3 = [1,2,3]`; `range 1 1 = [1]`; `range 2 1 = []`.
export const range = (a: number, b: number): readonly number[] => {
  const out: number[] = [];
  for (let i = a; i <= b; i++) out.push(i);
  return out;
};
