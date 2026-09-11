// Hand-written runtime: hydra.lib.equality primitives.
//
// Hydra equality is structural (docs/specification/ordering-and-equality.md):
// two values are equal exactly when `compare` (hydra.lib.ordering, ./ordering.ts)
// returns `equalTo`. Delegate to the structural comparator there rather than
// reimplementing equality separately -- see ordering.ts for the rationale
// (print-based/stringify comparison is not spec-conformant for structured
// values: it is sensitive to JSON key order, float/decimal rendering, and
// set/map iteration order).

import { compareValues } from "./ordering.js";

export const equal = <A>(a: A, b: A): boolean => {
  if (Object.is(a, b)) return true;
  return compareValues(a, b) === 0;
};
