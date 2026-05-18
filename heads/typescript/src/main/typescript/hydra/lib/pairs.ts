// Hand-written runtime: hydra.lib.pairs primitives.

export const first = <A, B>(p: readonly [A, B]): A => p[0];
export const second = <A, B>(p: readonly [A, B]): B => p[1];

export const swap = <A, B>(p: readonly [A, B]): readonly [B, A] => [p[1], p[0]];

export const bimap = <A, B, A2, B2>(f: (a: A) => A2) => (g: (b: B) => B2) => (p: readonly [A, B]): readonly [A2, B2] =>
  [f(p[0]), g(p[1])];
