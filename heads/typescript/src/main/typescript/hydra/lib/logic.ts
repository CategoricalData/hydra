// Hand-written runtime: hydra.lib.logic primitives.

export const and = (a: boolean) => (b: boolean): boolean => a && b;
export const or = (a: boolean) => (b: boolean): boolean => a || b;
export const not = (a: boolean): boolean => !a;

// `ifElse` is one of Hydra's lazy primitives (see
// docs/recipes/new-implementation.md "Lazy evaluation and thunking").
// The TypeScript coder wraps both branches in nullary arrow functions
// `() => expr`; we force whichever branch the condition selects. We also
// accept a plain value (for cases where the call wasn't rewritten,
// e.g. when generated code calls `ifElse` indirectly via a higher-order
// position), mirroring Python's `callable()` check.
const force = <A>(x: A | (() => A)): A =>
  typeof x === "function" ? (x as () => A)() : x;

export const ifElse = <A>(cond: boolean) => (t: A | (() => A)) => (e: A | (() => A)): A =>
  cond ? force(t) : force(e);
