// Hand-written runtime: hydra.lib.equality primitives.
//
// Hydra equality is structural — JS reference equality only works for primitives.
// For objects we use JSON.stringify as a pragmatic fallback; this matches the
// behavior of Hydra's encode-and-compare semantics for serializable data.

// Structural stringify that tolerates bigint (plain JSON.stringify throws on
// bigint). Records like Timespec carry nested bigint fields (int64/uint32 encode
// as bigint in TS), so any structural compare over them must be bigint-safe.
// Exported for reuse by hydra.lib.ordering (see ./ordering.ts).
export const stableStringify = (x: unknown): string =>
  JSON.stringify(x, (_k, v) => (typeof v === "bigint" ? `${v}n` : v));

export const equal = <A>(a: A, b: A): boolean => {
  if (Object.is(a, b)) return true;
  if (typeof a !== "object" || typeof b !== "object" || a === null || b === null) return false;
  return stableStringify(a) === stableStringify(b);
};
