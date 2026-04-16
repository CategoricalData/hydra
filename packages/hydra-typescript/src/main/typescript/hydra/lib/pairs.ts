/**
 * Pair primitive functions for Hydra-TypeScript.
 */

export function fst<A, B>(pair: readonly [A, B]): A {
  return pair[0];
}

export function snd<A, B>(pair: readonly [A, B]): B {
  return pair[1];
}

export function pair<A, B>(a: A, b: B): readonly [A, B] {
  return [a, b] as const;
}

export function mapFirst<A, A2, B>(
  f: (a: A) => A2,
  p: readonly [A, B],
): readonly [A2, B] {
  return [f(p[0]), p[1]] as const;
}

export function mapSecond<A, B, B2>(
  f: (b: B) => B2,
  p: readonly [A, B],
): readonly [A, B2] {
  return [p[0], f(p[1])] as const;
}
