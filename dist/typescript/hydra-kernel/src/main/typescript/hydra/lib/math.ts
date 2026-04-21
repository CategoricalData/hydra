/**
 * Math primitive functions for Hydra-TypeScript.
 */

import type { Maybe } from "../core.js";
import { just, nothing } from "../core.js";

// --- Int32 arithmetic ---

export function abs(a: number): number {
  return Math.abs(a);
}

export function add(a: number): (b: number) => number {
  return (b) => a + b;
}

export function addFloat64(a: number): (b: number) => number {
  return (b) => a + b;
}

export function div(a: number): (b: number) => number {
  return (b) => {
    if (b === 0) throw new Error("Division by zero");
    return Math.trunc(a / b);
  };
}

export function even(n: number): boolean {
  return n % 2 === 0;
}

export function max(a: number): (b: number) => number {
  return (b) => Math.max(a, b);
}

export function maybeDiv(a: number): (b: number) => Maybe<number> {
  return (b) => (b === 0 ? nothing() : just(Math.trunc(a / b)));
}

export function maybeMod(a: number): (b: number) => Maybe<number> {
  return (b) => (b === 0 ? nothing() : just(((a % b) + b) % b));
}

export function maybePred(n: number): Maybe<number> {
  return n <= -2147483648 ? nothing() : just(n - 1);
}

export function maybeRem(a: number): (b: number) => Maybe<number> {
  return (b) => (b === 0 ? nothing() : just(a % b));
}

export function maybeSucc(n: number): Maybe<number> {
  return n >= 2147483647 ? nothing() : just(n + 1);
}

export function min(a: number): (b: number) => number {
  return (b) => Math.min(a, b);
}

export function mod(a: number): (b: number) => number {
  return (b) => ((a % b) + b) % b;
}

export function mul(a: number): (b: number) => number {
  return (b) => a * b;
}

export function mulFloat64(a: number): (b: number) => number {
  return (b) => a * b;
}

export function negate(a: number): number {
  return -a;
}

export function negateFloat64(a: number): number {
  return -a;
}

export function odd(n: number): boolean {
  return n % 2 !== 0;
}

export function pred(n: number): number {
  return n - 1;
}

export function range(lo: number): (hi: number) => ReadonlyArray<number> {
  return (hi) => {
    const result: number[] = [];
    for (let i = lo; i < hi; i++) result.push(i);
    return result;
  };
}

export function rem(a: number): (b: number) => number {
  return (b) => a % b;
}

export function signum(n: number): number {
  return n > 0 ? 1 : n < 0 ? -1 : 0;
}

export function sub(a: number): (b: number) => number {
  return (b) => a - b;
}

export function subFloat64(a: number): (b: number) => number {
  return (b) => a - b;
}

export function succ(n: number): number {
  return n + 1;
}

// --- Float64 math ---

export function acos(x: number): number {
  return Math.acos(x);
}

export function acosh(x: number): number {
  return Math.acosh(x);
}

export function asin(x: number): number {
  return Math.asin(x);
}

export function asinh(x: number): number {
  return Math.asinh(x);
}

export function atan(x: number): number {
  return Math.atan(x);
}

export function atan2(y: number): (x: number) => number {
  return (x) => Math.atan2(y, x);
}

export function atanh(x: number): number {
  return Math.atanh(x);
}

export function ceiling(x: number): number {
  return Math.ceil(x);
}

export function cos(x: number): number {
  return Math.cos(x);
}

export function cosh(x: number): number {
  return Math.cosh(x);
}

export function e(): number {
  return Math.E;
}

export function exp(x: number): number {
  return Math.exp(x);
}

export function floor(x: number): number {
  return Math.floor(x);
}

export function log(x: number): number {
  return Math.log(x);
}

export function logBase(base: number): (x: number) => number {
  return (x) => Math.log(x) / Math.log(base);
}

export function pi(): number {
  return Math.PI;
}

export function pow(base: number): (exp: number) => number {
  return (exp) => Math.pow(base, exp);
}

export function round(x: number): number {
  return Math.round(x);
}

export function roundBigfloat(places: number): (x: number) => number {
  return (x) => {
    const factor = Math.pow(10, places);
    return Math.round(x * factor) / factor;
  };
}

export function roundFloat32(places: number): (x: number) => number {
  return (x) => {
    const factor = Math.pow(10, places);
    return Math.fround(Math.round(x * factor) / factor);
  };
}

export function roundFloat64(places: number): (x: number) => number {
  return (x) => {
    const factor = Math.pow(10, places);
    return Math.round(x * factor) / factor;
  };
}

export function sin(x: number): number {
  return Math.sin(x);
}

export function sinh(x: number): number {
  return Math.sinh(x);
}

export function sqrt(x: number): number {
  return Math.sqrt(x);
}

export function tan(x: number): number {
  return Math.tan(x);
}

export function tanh(x: number): number {
  return Math.tanh(x);
}

export function truncate(x: number): number {
  return Math.trunc(x);
}
