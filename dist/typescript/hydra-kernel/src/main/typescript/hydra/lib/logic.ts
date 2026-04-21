/**
 * Logic primitive functions for Hydra-TypeScript.
 */

export function and(a: boolean): (b: boolean) => boolean {
  return (b) => a && b;
}

export function or(a: boolean): (b: boolean) => boolean {
  return (b) => a || b;
}

export function not(a: boolean): boolean {
  return !a;
}

export function ifElse<T>(cond: boolean): (ifTrue: T) => (ifFalse: T) => T {
  return (ifTrue) => (ifFalse) => cond ? ifTrue : ifFalse;
}
