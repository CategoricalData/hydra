// Hand-written runtime: hydra.lib.logic primitives.
//
// Signatures are flat (positional), matching Python's heads/python/lib/logic.py
// and the convention the new analyze-based TypeScript coder emits. Calls
// look like `ifElse(cond, t, e)` rather than `ifElse(cond)(t)(e)`.

export const and = (a: boolean, b: boolean): boolean => a && b;
export const or = (a: boolean, b: boolean): boolean => a || b;
export const not = (a: boolean): boolean => !a;

// `ifElse` is one of Hydra's lazy primitives (see
// docs/recipes/new-implementation.md "Lazy evaluation and thunking").
// The TypeScript coder wraps both branches in nullary arrow functions
// `() => expr`; we force whichever branch the condition selects. We also
// accept a plain value (for cases where the call wasn't rewritten,
// e.g. when generated code calls `ifElse` indirectly via a higher-order
// position), mirroring Python's `callable()` check.
//
// Both branches accept `any` rather than a unified `A`. tsc would
// otherwise try to unify the literal types of the two branches and
// reject diagnostics like `() => Right(_)` vs `() => Left(_)` because
// the inferred discriminated-union tag differs — even though the
// runtime call returns one OR the other, never both.
//
// The thunk-or-value branching is inlined directly here (rather than
// delegated to a `force` helper) so that each `ifElse` call consumes
// only one extra JS frame instead of two. The TS coder emits many
// `ifElse` calls per Hydra term; on deeply-nested terms the saved
// frames are the difference between completing codegen and a V8
// stack overflow. See feature_126_typescript-plan.md for measurements.
export const ifElse = (cond: boolean, t: any, e: any): any => {
  const x = cond ? t : e;
  return typeof x === "function" ? x() : x;
};
