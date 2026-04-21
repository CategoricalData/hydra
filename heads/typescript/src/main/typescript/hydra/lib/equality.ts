/**
 * Equality and comparison primitive functions for Hydra-TypeScript.
 */

export type Comparison = "lessThan" | "equalTo" | "greaterThan";

export function compare<T>(a: T): (b: T) => Comparison {
  return (b) => {
    if (a < b) return "lessThan";
    if (a > b) return "greaterThan";
    return "equalTo";
  };
}

export function equal<T>(a: T): (b: T) => boolean {
  return (b) => a === b;
}

export function gt<T>(a: T): (b: T) => boolean {
  return (b) => a > b;
}

export function gte<T>(a: T): (b: T) => boolean {
  return (b) => a >= b;
}

export function identity<T>(a: T): T {
  return a;
}

export function lt<T>(a: T): (b: T) => boolean {
  return (b) => a < b;
}

export function lte<T>(a: T): (b: T) => boolean {
  return (b) => a <= b;
}

export function max<T>(a: T): (b: T) => T {
  return (b) => (a >= b ? a : b);
}

export function min<T>(a: T): (b: T) => T {
  return (b) => (a <= b ? a : b);
}
