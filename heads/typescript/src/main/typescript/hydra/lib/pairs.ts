// Hand-written runtime: hydra.lib.pairs primitives.
//
// `first` and `second` accept `unknown` (so kernel-routed `unknown`
// pairs type-check) and return `any` (so callers can use the result
// without TS18046 "X is of type unknown"). They're total at runtime for
// any 2-element array input.
export const first = (p: unknown): any => (p as readonly [any, any])[0];
export const second = (p: unknown): any => (p as readonly [any, any])[1];

export const swap = <A, B>(p: readonly [A, B]): readonly [B, A] => [p[1], p[0]];

export const bimap = <A, B, A2, B2>(f: (a: A) => A2, g: (b: B) => B2, p: readonly [A, B]): readonly [A2, B2] =>
  [f(p[0]), g(p[1])];
