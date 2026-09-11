// Hand-written runtime: hydra.lib.ordering primitives.
//
// Structural comparison per docs/specification/ordering-and-equality.md: records
// compare field-by-field in declaration order, unions by declared-variant order
// then payload, wrappers by wrapped value, decimals by numeric value then scale,
// with no print-based fallback. Mirrors the Java/Python/Scala overlay fixes (#718)
// -- see those for the reference structural-comparator pattern.
//
// TypeScript unions carry no runtime ordinal (unlike Java's generated
// `hydraOrdinal()`), so kernel union types get a hand-authored declared-order
// table below (VARIANT_ORDER), transcribed from the generated
// hydra/reflect.ts (termVariants, typeVariants, etc.) -- mirroring Python's
// _VARIANT_ORDER table (overlay/python/.../util/_compare.py), which is the
// established precedent: #718 hand-authored variant order for kernel types
// only. Non-kernel (user-schema) union types fall back to a still-deterministic
// but non-declared-order comparison (tag string order) -- the same scope
// limitation Python/Scala already carry (arbitrary cross-variant comparison of
// user-defined unions is not fully solved by #718 on any host but Java, whose
// hydraOrdinal() is universal because it is generated per-value).

import type { Decimal } from "./literals.js";

const isDecimal = (x: unknown): x is Decimal =>
  typeof x === "object" && x !== null && typeof (x as Decimal).coefficient === "bigint" &&
  typeof (x as Decimal).scale === "number";

// Ordered comparison of two decimals: numeric value first, then scale
// ascending as a tiebreak (1.1 < 1.10 < 1.100), per
// docs/specification/ordering-and-equality.md.
const compareDecimals = (a: Decimal, b: Decimal): number => {
  // Cross-multiply to compare a.coefficient/10^a.scale against
  // b.coefficient/10^b.scale without floating-point error.
  const maxScale = Math.max(a.scale, b.scale);
  const na = a.coefficient * (10n ** BigInt(maxScale - a.scale));
  const nb = b.coefficient * (10n ** BigInt(maxScale - b.scale));
  if (na < nb) return -1;
  if (na > nb) return 1;
  return a.scale - b.scale;
};

// IEEE 754 extended total order (docs/specification/ordering-and-equality.md):
// -inf < negative finite < -0.0 < +0.0 < positive finite < +inf < NaN (equal
// to itself). Native `<`/`>` disagree at NaN (unordered) and signed zero
// (-0.0 === 0.0).
const compareFloats = (a: number, b: number): number => {
  const aNaN = Number.isNaN(a), bNaN = Number.isNaN(b);
  if (aNaN || bNaN) return aNaN && bNaN ? 0 : aNaN ? 1 : -1;
  if (a < b) return -1;
  if (a > b) return 1;
  // a === b as numbers: distinguish signed zero (-0.0 < +0.0).
  if (a === 0) {
    const aNeg = Object.is(a, -0), bNeg = Object.is(b, -0);
    if (aNeg !== bNeg) return aNeg ? -1 : 1;
  }
  return 0;
};

// Declared-variant order for each hydra.core union family, transcribed from
// the generated hydra/reflect.ts (termVariants, typeVariants, literalVariants,
// integerTypes, floatTypes, literalTypes), which in turn come from the
// hand-authored hydra.reflect DSL module (Sources/Kernel/Terms/Reflect.hs).
// Keyed by tag string; every family's tags are assumed mutually exclusive
// enough that a same-position comparison (guaranteed by Hydra's static
// typing -- see the spec's "comparisons are defined only between values of
// the same type") never crosses families. Regenerate this table if the
// kernel's core union declarations change.
const VARIANT_ORDER: Record<string, readonly string[]> = {
  term: [
    "annotated", "application", "cases", "either", "lambda", "let", "list",
    "literal", "map", "optional", "pair", "project", "record", "set",
    "typeLambda", "typeApplication", "inject", "unit", "unwrap", "variable", "wrap",
  ],
  type: [
    "annotated", "application", "effect", "either", "forall", "function", "list",
    "literal", "map", "optional", "pair", "record", "set", "union", "unit",
    "variable", "void", "wrap",
  ],
  literal: ["binary", "boolean", "decimal", "float", "integer", "string"],
  integer: ["bigint", "int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64"],
  float: ["float32", "float64"],
};
// family -> tag -> ordinal, built once.
const FAMILY_ORDINAL: ReadonlyMap<string, ReadonlyMap<string, number>> = new Map(
  Object.entries(VARIANT_ORDER).map(([family, tags]) => [family, new Map(tags.map((t, i) => [t, i]))]),
);

// Determine which known kernel union family two tags share, and their
// ordinals within it, by checking each family's order table until one
// contains BOTH tags (tags collide across unrelated families -- e.g.
// "literal", "map", "unit" appear in both "term" and "type" -- so a single
// flat tag->ordinal table would be unsound; this resolves the family from
// both sides together instead). Returns undefined if no single family's
// table contains both tags (unknown/non-kernel family, or a family mismatch
// that shouldn't arise under Hydra's static typing -- callers fall back to
// tag-string order in that case).
const sameFamilyOrdinals = (aTag: string, bTag: string): readonly [number, number] | undefined => {
  for (const table of FAMILY_ORDINAL.values()) {
    const ai = table.get(aTag);
    if (ai === undefined) continue;
    const bi = table.get(bTag);
    if (bi === undefined) continue;
    return [ai, bi];
  }
  return undefined;
};

const CANON_MAP_TAG = Symbol.for("hydra.canonMap");
const CANON_SET_TAG = Symbol.for("hydra.canonSet");
const isCanonMap = (x: unknown): x is { readonly _internal: Map<string, { key: unknown; value: unknown }> } =>
  !!x && typeof x === "object" && (x as any)[CANON_MAP_TAG] === true;
const isCanonSet = (x: unknown): x is { readonly _internal: Map<string, unknown> } =>
  !!x && typeof x === "object" && (x as any)[CANON_SET_TAG] === true;

// Structural comparison per docs/specification/ordering-and-equality.md.
// Returns -1, 0, or 1. Exported for reuse by hydra.lib.equality (equal(a, b)
// := compareValues(a, b) === 0).
export const compareValues = (a: unknown, b: unknown): number => {
  if (Object.is(a, b)) return 0;

  // Decimals: numeric value first, then scale tiebreak.
  if (isDecimal(a) && isDecimal(b)) return compareDecimals(a, b);

  // Bigints (int64/uint64/bigint literal values): numeric.
  if (typeof a === "bigint" && typeof b === "bigint") return a < b ? -1 : a > b ? 1 : 0;

  // Numbers: IEEE extended total order (covers float32/float64; also
  // correct -- and simpler than a dedicated path -- for the int8..uint32
  // widths, which never carry NaN/signed-zero and compare identically
  // under both rules).
  if (typeof a === "number" && typeof b === "number") return compareFloats(a, b);

  if (typeof a === "string" && typeof b === "string") return a < b ? -1 : a > b ? 1 : 0;
  if (typeof a === "boolean" && typeof b === "boolean") return a === b ? 0 : a ? 1 : -1;

  // Arrays: List, and the tuple encoding of Pair -- lexicographic,
  // shorter-is-prefix-less.
  if (Array.isArray(a) && Array.isArray(b)) {
    const len = Math.min(a.length, b.length);
    for (let i = 0; i < len; i++) {
      const c = compareValues(a[i], b[i]);
      if (c !== 0) return c;
    }
    return a.length - b.length;
  }

  if (isCanonMap(a) && isCanonMap(b)) {
    const ea = [...a._internal.values()].sort((x, y) => compareValues(x.key, y.key));
    const eb = [...b._internal.values()].sort((x, y) => compareValues(x.key, y.key));
    const len = Math.min(ea.length, eb.length);
    for (let i = 0; i < len; i++) {
      const ck = compareValues(ea[i].key, eb[i].key);
      if (ck !== 0) return ck;
      const cv = compareValues(ea[i].value, eb[i].value);
      if (cv !== 0) return cv;
    }
    return ea.length - eb.length;
  }

  if (isCanonSet(a) && isCanonSet(b)) {
    const ea = [...a._internal.values()].sort(compareValues);
    const eb = [...b._internal.values()].sort(compareValues);
    const len = Math.min(ea.length, eb.length);
    for (let i = 0; i < len; i++) {
      const c = compareValues(ea[i], eb[i]);
      if (c !== 0) return c;
    }
    return ea.length - eb.length;
  }

  if (a === null || b === null || typeof a !== "object" || typeof b !== "object") {
    // Mismatched/non-object primitives that reach here (shouldn't happen
    // under Hydra's static typing -- same-type comparisons only) still need
    // a total order; fall through to a stable, non-print-based tag compare.
    return String(a) < String(b) ? -1 : String(a) > String(b) ? 1 : 0;
  }

  // Discriminated unions: Optional ({tag:"given"|"none"}), Either
  // ({tag:"left"|"right"}), and hydra.core unions (Term, Type, Literal, ...).
  // "given"/"none": none < given (spec). "left"/"right": every left < every
  // right (spec). Kernel unions: declared-variant order via VARIANT_ORDER.
  const aTag = (a as { tag?: unknown }).tag;
  const bTag = (b as { tag?: unknown }).tag;
  if (typeof aTag === "string" && typeof bTag === "string") {
    if (aTag === "none" || aTag === "given") {
      if (aTag !== bTag) return aTag === "none" ? -1 : 1;
    } else if (aTag === "left" || aTag === "right") {
      if (aTag !== bTag) return aTag === "left" ? -1 : 1;
    } else {
      const ordinals = sameFamilyOrdinals(aTag, bTag);
      if (ordinals !== undefined) {
        const [ai, bi] = ordinals;
        if (ai !== bi) return ai - bi;
      } else if (aTag !== bTag) {
        // Unknown (non-kernel, user-schema) union family: no declared-order
        // table available (the TS coder does not emit per-type ordinal
        // metadata for arbitrary unions -- see the header comment). Tag
        // string order is deterministic and total, but not necessarily the
        // declared order; this is the same scope limitation #718 carries on
        // every host but Java.
        return aTag < bTag ? -1 : 1;
      }
    }
    if ("value" in a && "value" in b) {
      return compareValues((a as { value: unknown }).value, (b as { value: unknown }).value);
    }
    return 0;
  }

  // Records and wrappers: plain objects, fields in declaration order.
  // ES2015+ guarantees string-key insertion order, and the coder always
  // constructs/destructures fields in declared order, so Object.keys()
  // reflects declaration order without a separate field-order table.
  const aKeys = Object.keys(a as object);
  const bKeys = Object.keys(b as object);
  const len = Math.min(aKeys.length, bKeys.length);
  for (let i = 0; i < len; i++) {
    const c = compareValues((a as any)[aKeys[i]], (b as any)[bKeys[i]]);
    if (c !== 0) return c;
  }
  return aKeys.length - bKeys.length;
};

export const lt = <A>(a: A, b: A): boolean => compareValues(a, b) < 0;
export const lte = <A>(a: A, b: A): boolean => compareValues(a, b) <= 0;
export const gt = <A>(a: A, b: A): boolean => compareValues(a, b) > 0;
export const gte = <A>(a: A, b: A): boolean => compareValues(a, b) >= 0;

// Hydra `compare` returns a hydra.util.Comparison value:
//   { tag: "lessThan" } | { tag: "equalTo" } | { tag: "greaterThan" }
// The kernel pattern-matches on the tag (see hydra/print/util.ts), so
// the runtime value must use this discriminated-union shape — not a
// raw -1/0/1 number.
export type Comparison =
  | { readonly tag: "lessThan" }
  | { readonly tag: "equalTo" }
  | { readonly tag: "greaterThan" };

export const compare = <A>(a: A, b: A): Comparison => {
  const c = compareValues(a, b);
  return c < 0 ? { tag: "lessThan" } as const : c > 0 ? { tag: "greaterThan" } as const : { tag: "equalTo" } as const;
};

export const min = <A>(a: A, b: A): A => lt(a, b) ? a : b;
export const max = <A>(a: A, b: A): A => lt(b, a) ? a : b;
