// Hand-written runtime: hydra.lib.equality primitives.
//
// Hydra equality is structural — JS reference equality only works for primitives.
// For objects we use JSON.stringify as a pragmatic fallback; this matches the
// behavior of Hydra's encode-and-compare semantics for serializable data.

export const equal = <A>(a: A, b: A): boolean => {
  if (Object.is(a, b)) return true;
  if (typeof a !== "object" || typeof b !== "object" || a === null || b === null) return false;
  return JSON.stringify(a) === JSON.stringify(b);
};

// Unwrap a Hydra Term value to its underlying scalar for ordered
// comparison. The kernel test primitives often compare Term-encoded
// integers/floats/strings; raw structural JSON comparison fails for
// these (e.g. "...1}}}" lex-compares wrong against "...100}}}").
const unwrapForOrdering = (x: unknown): unknown => {
  if (x === null || typeof x !== "object") return x;
  const o = x as { tag?: string; value?: unknown };
  // Term_literal → unwrap to the literal value.
  if (o.tag === "literal" && o.value && typeof o.value === "object") {
    const lit = o.value as { tag?: string; value?: unknown };
    if (lit.tag === "string" || lit.tag === "binary") return lit.value;
    if (lit.tag === "boolean") return lit.value;
    // Integer/float wrap one more level.
    if (lit.tag === "integer" || lit.tag === "float") {
      const inner = lit.value as { tag?: string; value?: unknown };
      return inner?.value;
    }
    if (lit.tag === "decimal") return lit.value;
  }
  // Wrap (newtype Name etc.): { value: <inner> }.
  if (Object.keys(o).length === 1 && "value" in o) return unwrapForOrdering(o.value);
  return x;
};

export const lt = <A>(a: A, b: A): boolean => {
  const ua = unwrapForOrdering(a);
  const ub = unwrapForOrdering(b);
  if (typeof ua === "number" && typeof ub === "number") return ua < ub;
  if (typeof ua === "string" && typeof ub === "string") return ua < ub;
  if (typeof ua === "bigint" && typeof ub === "bigint") return ua < ub;
  // Fallback: original structural comparison.
  return JSON.stringify(a) < JSON.stringify(b);
};

export const lte = <A>(a: A, b: A): boolean => equal(a, b) || lt(a, b);
export const gt = <A>(a: A, b: A): boolean => lt(b, a);
export const gte = <A>(a: A, b: A): boolean => equal(a, b) || gt(a, b);

// Hydra `compare` returns a hydra.util.Comparison value:
//   { tag: "lessThan" } | { tag: "equalTo" } | { tag: "greaterThan" }
// The kernel pattern-matches on the tag (see hydra/show/util.ts), so
// the runtime value must use this discriminated-union shape — not a
// raw -1/0/1 number.
export type Comparison =
  | { readonly tag: "lessThan" }
  | { readonly tag: "equalTo" }
  | { readonly tag: "greaterThan" };

export const compare = <A>(a: A, b: A): Comparison => {
  if (equal(a, b)) return { tag: "equalTo" } as const;
  return lt(a, b) ? { tag: "lessThan" } as const : { tag: "greaterThan" } as const;
};

export const identity = <A>(a: A): A => a;

export const min = <A>(a: A, b: A): A => lt(a, b) ? a : b;
export const max = <A>(a: A, b: A): A => lt(b, a) ? a : b;
