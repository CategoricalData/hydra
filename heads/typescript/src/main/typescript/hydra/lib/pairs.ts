/**
 * Pair primitive functions for Hydra-TypeScript.
 */

export function bimap<A, B, C, D>(
  f: (a: A) => C,
): (g: (b: B) => D) => (p: readonly [A, B]) => readonly [C, D] {
  return (g) => (p) => [f(p[0]), g(p[1])] as const;
}

export function first<A, B>(p: readonly [A, B]): A {
  return p[0];
}

export function second<A, B>(p: readonly [A, B]): B {
  return p[1];
}
