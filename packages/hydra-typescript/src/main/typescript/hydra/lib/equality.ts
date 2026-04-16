/**
 * Equality primitive functions for Hydra-TypeScript.
 */

export function equal<T>(a: T, b: T): boolean {
  return a === b;
}

export function notEqual<T>(a: T, b: T): boolean {
  return a !== b;
}

export function compareStrings(a: string, b: string): number {
  return a < b ? -1 : a > b ? 1 : 0;
}
