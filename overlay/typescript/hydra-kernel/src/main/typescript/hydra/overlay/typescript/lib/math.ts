// Hand-written runtime: hydra.lib.math primitives.
//
// Signatures are flat (positional), matching Python's heads/python/lib/math.py.

import type { Optional } from "../../../runtime.js";
import { Given, None } from "../../../runtime.js";
import { floorDivBig, floorModBig, wrapInt } from "./libraries.js";

export const add = (a: number, b: number): number => a + b;
export const sub = (a: number, b: number): number => a - b;
export const mul = (a: number, b: number): number => a * b;
export const neg = (a: number): number => -a;
export const negate = (a: number): number => -a;
export const abs = (a: number): number => Math.abs(a);

export const ceil = (f: number): number => Math.ceil(f);
export const floor = (f: number): number => Math.floor(f);

// Integral division (floor) / modulus (Knuth, sign follows the divisor), matching the kernel
// contract (Test/Lib/Math.hs) and the registry's `integralBinary` (libraries.ts). This flat
// surface has no width tag on its plain-`number` operands, so it is treated as int32 (the range
// of every known call site, e.g. Generation.hs's hex-digit encoder); the `minBound / -1` overflow
// guard and wraparound mirror the registry's int32 handling. Delegates to the same bigint helpers
// as the registry so the two surfaces cannot drift again (#677).
export const div = (a: number, b: number): Optional<number> => {
  if (b === 0) return None;
  const ba = BigInt(a), bb = BigInt(b);
  const r = (a === -2147483648 && b === -1) ? ba : floorDivBig(ba, bb);
  return Given(wrapInt("int32", r) as number);
};

export const mod = (a: number, b: number): Optional<number> => {
  if (b === 0) return None;
  return Given(wrapInt("int32", floorModBig(BigInt(a), BigInt(b))) as number);
};

// Half-open range `[a, b)`: includes the lower bound, excludes the upper bound.
// `range 1 4 = [1,2,3]`; `range 1 1 = []`; `range 2 1 = []`.
export const range = (a: number, b: number): readonly number[] => {
  const out: number[] = [];
  for (let i = a; i < b; i++) out.push(i);
  return out;
};
