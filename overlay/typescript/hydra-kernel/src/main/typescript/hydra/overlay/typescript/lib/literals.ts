// Hand-written runtime: hydra.lib.literals primitives.
//
// Hydra's literals module exposes generic show/read functions that work
// across the kernel's literal sub-types (IntegerType, FloatType,
// IntegerValue, FloatValue, Literal). The TypeScript runtime represents:
//   - IntegerType / FloatType as discriminated unions { tag, value? }
//   - IntegerValue / FloatValue as discriminated unions { tag, value }
//   - Literal as a discriminated union over its kinds
//
// The functions below mirror the Python implementation in
// heads/python/src/main/python/hydra/lib/literals.py.

import type { Optional } from "../../../runtime.js";
import { Given, None } from "../../../runtime.js";

// === show family ===

// Haskell `show` for String: ASCII printable characters are emitted as
// themselves (with `\"` and `\\` escaped); recognized control characters
// use named escapes (\NUL, \a, \b, \t, \n, \v, \f, \r, \DEL); other
// control / non-ASCII characters are emitted as `\NNN` (decimal escape).
const HASKELL_CTRL_ESCAPES: Record<number, string> = {
  0: "\\NUL", 1: "\\SOH", 2: "\\STX", 3: "\\ETX", 4: "\\EOT",
  5: "\\ENQ", 6: "\\ACK", 7: "\\a", 8: "\\b", 9: "\\t",
  10: "\\n", 11: "\\v", 12: "\\f", 13: "\\r", 14: "\\SO",
  15: "\\SI", 16: "\\DLE", 17: "\\DC1", 18: "\\DC2", 19: "\\DC3",
  20: "\\DC4", 21: "\\NAK", 22: "\\SYN", 23: "\\ETB", 24: "\\CAN",
  25: "\\EM", 26: "\\SUB", 27: "\\ESC", 28: "\\FS", 29: "\\GS",
  30: "\\RS", 31: "\\US", 127: "\\DEL",
};

export const printString = (s: string): string => {
  let out = '"';
  for (const ch of s) {
    const cp = ch.codePointAt(0)!;
    if (ch === '"') out += '\\"';
    else if (ch === "\\") out += "\\\\";
    else if (HASKELL_CTRL_ESCAPES[cp]) out += HASKELL_CTRL_ESCAPES[cp];
    else if (cp < 127) out += ch;
    else out += "\\" + cp;
  }
  out += '"';
  return out;
};
export const printBoolean = (b: boolean): string => (b ? "true" : "false");

// Generic show for an IntegerValue: `<n>:<typeTag>`.
type IntegerValue = { tag: string; value: number | bigint };
export const showInt = (v: IntegerValue): string => `${v.value}:${v.tag}`;
export const showUint = (v: IntegerValue): string => `${v.value}:${v.tag}`;
// printBigint is called by the kernel in two contexts:
//   - As `lib_literals.printBigint` from `show.core.literal`, with an
//     IntegerValue wrapper `{tag: "bigint", value: n}` — prints "n:bigint".
//   - As a raw integer renderer from `json.writer.valueToExpr`, with a
//     bare JS `bigint` — prints just "n".
export const printBigint = (v: IntegerValue | bigint): string =>
  typeof v === "bigint" ? v.toString() : `${v.value}:${v.tag}`;

type FloatValue = { tag: string; value: number };
export const showFloat = (v: FloatValue): string => `${v.value}:${v.tag}`;

// A scale-preserving arbitrary-precision decimal: the numeric value is
// `coefficient * 10^-scale`. Mirrors java.math.BigDecimal's (unscaledValue,
// scale) convention, so "1.10" is { coefficient: 110n, scale: 2 } and "1.1"
// is { coefficient: 11n, scale: 1 } -- distinct values per the kernel spec
// (docs/specification/ordering-and-equality.md: 1.1 != 1.10).
export type Decimal = { readonly coefficient: bigint; readonly scale: number };

export const mkDecimal = (coefficient: bigint, scale: number): Decimal => ({ coefficient, scale });

// Representation-faithful DECIMAL rendering (NOT float `show`): per the kernel
// printDecimal (docs/specification/syntax.md §2.6, json-format.md), a decimal
// prints in positional form when the adjusted exponent is in [-6, 21) and in
// exponent form otherwise, with NO mandatory trailing ".0" on whole values
// ("42" not "42.0", "0" not "0.0", "0.01" not "1.0e-2", "100000000000000000000"
// not "1.0e20"), coefficient digits (including trailing zeros) preserved
// exactly, and zero printed per its scale ("0", "0.0", "0.00").
//
// Parameter typed `unknown` (not `Decimal`) because the generated
// hydra.adapt module's hoisted decimal-conversion helpers (adapt.ts) type
// their captured value as `unknown` -- an artifact of the TS coder's
// hoisting logic, which cannot express a concrete domain type for these
// inner helper functions. Narrowed internally via `asDecimal`.
const asDecimal = (v: unknown): Decimal => v as Decimal;
export const printDecimal = (d0: unknown): string => {
  const d = asDecimal(d0);
  const neg = d.coefficient < 0n;
  const digits = (neg ? -d.coefficient : d.coefficient).toString();
  const sign = neg ? "-" : "";
  // Adjusted exponent: decimal position of the leading significant digit,
  // i.e. exponent of digits[0] when the value is written as d.ddd... * 10^a.
  const adjustedExp = digits.length - 1 - d.scale;
  if (adjustedExp >= -6 && adjustedExp < 21) {
    // Positional form.
    if (d.scale <= 0) {
      // Whole value: pad with trailing zeros (no fraction, no ".0").
      return sign + digits + "0".repeat(-d.scale);
    }
    if (d.scale < digits.length) {
      const intPart = digits.slice(0, digits.length - d.scale);
      const fracPart = digits.slice(digits.length - d.scale);
      return `${sign}${intPart}.${fracPart}`;
    }
    return sign + "0." + "0".repeat(d.scale - digits.length) + digits;
  }
  // Exponent form: always one digit before the point AND a fractional part
  // (spec: "one digit before the point"), so a single-digit coefficient
  // still prints with a synthetic ".0" ("1.0e-20", not "1e-20") -- otherwise
  // coefficient digits are preserved exactly.
  const leadDigit = digits[0];
  const rest = digits.slice(1);
  const mantissa = rest.length > 0 ? `${leadDigit}.${rest}` : `${leadDigit}.0`;
  const expSign = adjustedExp < 0 ? "-" : "+";
  return `${sign}${mantissa}e${expSign}${Math.abs(adjustedExp)}`;
};

// === read family ===

export const readInt = (s: string): Optional<number> => {
  const n = parseInt(s, 10);
  return Number.isFinite(n) && /^-?\d+$/.test(s.trim()) ? Given(n) : None;
};

export const readUint = (s: string): Optional<number> => {
  const n = parseInt(s, 10);
  return Number.isFinite(n) && n >= 0 && /^\d+$/.test(s.trim()) ? Given(n) : None;
};

export const parseBigint = (s: string): Optional<bigint> => {
  try {
    return /^-?\d+$/.test(s.trim()) ? Given(BigInt(s.trim())) : None;
  } catch { return None; }
};

export const readFloat = (s: string): Optional<number> => {
  // Accept NaN, Infinity, and -Infinity as valid float literals so the
  // JSON decoder's special-float path (parseSpecialFloat) succeeds.
  if (s === "NaN") return Given(NaN);
  if (s === "Infinity") return Given(Infinity);
  if (s === "-Infinity") return Given(-Infinity);
  if (s === "-0.0") return Given(-0);
  const n = parseFloat(s);
  return Number.isNaN(n) ? None : Given(n);
};

// Parse the JSON number grammar (docs/specification/syntax.md §2.6) into a
// scale-preserving Decimal: an optional sign, integer digits, an optional
// fraction part, and an optional exponent part. Scale-preserving means
// "1.10" and "1.1" parse to distinct values (scale 2 vs scale 1).
const DECIMAL_PATTERN = /^(-?)(\d+)(?:\.(\d+))?(?:[eE]([+-]?\d+))?$/;
export const parseDecimal = (s: string): Optional<Decimal> => {
  const m = DECIMAL_PATTERN.exec(s.trim());
  if (!m) return None;
  const [, sign, intDigits, fracDigits, expPart] = m;
  const digits = intDigits + (fracDigits ?? "");
  const scale = (fracDigits?.length ?? 0) - (expPart ? parseInt(expPart, 10) : 0);
  let coefficient = BigInt((sign === "-" ? "-" : "") + digits);
  let normScale = scale;
  // A negative scale means trailing zeros beyond the decimal point implied by
  // the exponent; fold them into the coefficient so `scale` is never negative
  // (matches BigDecimal's normalized (unscaledValue, scale) representation).
  if (normScale < 0) {
    coefficient = coefficient * (10n ** BigInt(-normScale));
    normScale = 0;
  }
  return Given(mkDecimal(coefficient, normScale));
};

// Parse a known-good canonical decimal digit string (e.g. one produced by
// printDecimal) into a Decimal. Used by the TypeScript coder to embed decimal
// literals in generated source (mirrors how the Python coder emits
// `Decimal('<printDecimal string>')`).
export const decimalFromString = (s: string): Decimal => {
  const m = parseDecimal(s);
  if (m.tag !== "given") throw new Error(`decimalFromString: not a valid decimal literal: ${s}`);
  return m.value;
};

export const parseBoolean = (s: string): Optional<boolean> =>
  s === "true" ? Given(true) : s === "false" ? Given(false) : None;

// === conversions ===

// "bigint" here means Hydra's BigInteger value — represented as JS bigint.
export const bigintToInt = (n: bigint): number => Number(n);
export const bigintToUint = (n: bigint): number => Number(n);
export const bigintToDecimal = (n: bigint): Decimal => mkDecimal(n, 0);

// `decimalToBigint` rounds to the nearest integer, ties to even (banker's
// rounding) -- matches the Python/Java reference hosts' behavior, despite
// the kernel primitive's doc comment saying "truncating" (stale relative to
// the actual cross-host test suite, e.g. 42.7 rounds to 43, not 42).
export const decimalToBigint = (d: Decimal): bigint => {
  if (d.scale <= 0) return d.coefficient * (10n ** BigInt(-d.scale));
  const divisor = 10n ** BigInt(d.scale);
  const neg = d.coefficient < 0n;
  const abs = neg ? -d.coefficient : d.coefficient;
  const quotient = abs / divisor;
  const remainder = abs % divisor;
  const twiceRemainder = remainder * 2n;
  let rounded = quotient;
  if (twiceRemainder > divisor || (twiceRemainder === divisor && quotient % 2n === 1n)) {
    rounded = quotient + 1n;
  }
  return neg ? -rounded : rounded;
};

// Exact conversion to a JS double (float64), rounding to the nearest
// representable value (ties-to-even, via Number(string)).
export const decimalToFloat = (d: unknown): number => Number(printDecimal(d));

// === wrappers for primitive constructors ===
// These return the canonical IntegerValue / FloatValue shape so that
// generated code can construct Hydra literals through the library.

export const int = (n: number): IntegerValue => ({ tag: "int32", value: n });
export const uint = (n: number): IntegerValue => ({ tag: "uint32", value: n });
export const float = (f: number): FloatValue => ({ tag: "float64", value: f });

// === binary <-> string ===
// Hydra binary literals are byte strings; the TS runtime uses Uint8Array.

// `binaryToBase64` produces a base64 string from the binary content.
// In the TypeScript runtime, binary literal values are stored as base64
// strings already (because that's how the coder emits them in TS source),
// so this is the identity for string inputs. For Uint8Array (e.g. when
// constructed programmatically), encode to base64. Treats null/undefined
// as the empty binary.
export const binaryToBase64 = (b: Uint8Array | ReadonlyArray<number> | string | null | undefined): string => {
  if (b === null || b === undefined) return "";
  if (typeof b === "string") return b;
  // Uint8Array or readonly number[] → base64
  let raw = "";
  for (const byte of b) raw += String.fromCharCode(byte);
  return typeof btoa !== "undefined"
    ? btoa(raw)
    : Buffer.from(raw, "binary").toString("base64");
};

// `base64ToBinary` is the inverse: given a base64 string, return the
// binary representation. Since the TypeScript runtime represents binary
// content as base64 strings at the Term level, this is also the identity
// for the round-trip — caller has the base64 string back as the binary
// value, which is what `binaryToBase64` will accept.
export const base64ToBinary = (s: string): string => s;

// `binaryToBytes` decodes a binary value into a list of byte values
// (0-255). Mirrors Python's `binary_to_bytes`. The TS runtime stores
// binary as base64-encoded strings, so we decode then return byte ints.
export const binaryToBytes = (b: Uint8Array | ReadonlyArray<number> | string | null | undefined): readonly number[] => {
  if (b === null || b === undefined) return [];
  if (typeof b !== "string") return Array.from(b);
  // Decode base64. Use atob in browser, Buffer in Node.
  const raw = typeof atob !== "undefined" ? atob(b) : Buffer.from(b, "base64").toString("binary");
  const out: number[] = [];
  for (let i = 0; i < raw.length; i++) out.push(raw.charCodeAt(i));
  return out;
};

// `bytesToBinary` is the inverse of `binaryToBytes`: pack a list of
// byte values (0-255) into a base64-encoded string.
export const bytesToBinary = (bytes: readonly number[]): string => {
  let raw = "";
  for (const b of bytes) raw += String.fromCharCode(b & 0xff);
  return typeof btoa !== "undefined" ? btoa(raw) : Buffer.from(raw, "binary").toString("base64");
};

// === typed show helpers (used by encodeLiteral in the coder) ===

export const printInt8 = (n: number): string => n.toString();
export const printInt16 = (n: number): string => n.toString();
export const printInt32 = (n: number): string => n.toString();
export const printInt64 = (n: bigint): string => n.toString();
export const printUint8 = (n: number): string => n.toString();
export const printUint16 = (n: number): string => n.toString();
export const printUint32 = (n: number): string => n.toString();
export const printUint64 = (n: bigint): string => n.toString();
// Show a float at ~12 significant digits, matching the convention used
// by the kernel test fixtures. Haskell's `show` for `Double` uses
// exponential notation when `abs(x) < 0.1` and the standard non-
// exponential form otherwise. For exp form the mantissa is normalized
// to a single non-zero digit before the dot (e.g. `5.0e-2`, not
// `0.05`). Returns "NaN" / "Infinity" / "-Infinity" for edge cases and
// `<n>.0` for integer-valued floats.
const _showFloatPrecise = (f: number): string => {
  if (Number.isNaN(f)) return "NaN";
  if (f === Infinity) return "Infinity";
  if (f === -Infinity) return "-Infinity";
  if (f === 0) return Object.is(f, -0) ? "-0.0" : "0.0";
  const abs = Math.abs(f);
  if (abs < 0.1) {
    // Haskell shows values < 0.1 in exponential notation
    // (e.g. `5.0e-2`, `1.22464679915e-16`). Use toExponential() for the
    // shortest round-trip mantissa, then normalize to Haskell style.
    // toExponential() with no arg picks the shortest exact representation.
    let s = f.toExponential();
    const eIdx = s.indexOf("e");
    const mantissa = s.slice(0, eIdx);
    const expPart = s.slice(eIdx + 1);
    // Ensure mantissa contains a '.', so `5e-2` → `5.0e-2`.
    const fixedMantissa = mantissa.includes(".") ? mantissa : `${mantissa}.0`;
    // Strip leading "+" and zero-padding from exponent: e+05 → e5, e-05 → e-5.
    const sign = expPart.startsWith("-") ? "-" : "";
    const digits = expPart.replace(/^[+-]?0*/, "") || "0";
    return `${fixedMantissa}e${sign}${digits}`;
  }
  // For values >= 0.1, use shortest round-trip representation via toString().
  // This matches Haskell's `show :: Double -> String` for finite values.
  let s = f.toString();
  if (s.includes("e")) {
    s = s.replace(/(\.\d*?)0+e/, "$1e").replace(/\.e/, ".0e");
    if (!s.includes(".")) s = s.replace(/e/, ".0e");
    s = s.replace(/e\+?(-?)0*(\d)/, "e$1$2");
    return s;
  }
  if (s.includes(".")) {
    s = s.replace(/0+$/, "");
    if (s.endsWith(".")) s += "0";
  } else {
    s += ".0";
  }
  return s;
};

// float32 has ~7 significant digits of precision; render at that
// precision to match what the kernel test fixtures expect (the
// canonical round-trip representation of the float32 value, not the
// full float64 approximation).
export const printFloat32 = (f: number): string => _showFloatPreciseSig(f, 7);
export const printFloat64 = (f: number): string => _showFloatPrecise(f);

const _showFloatPreciseSig = (f: number, sig: number): string => {
  if (Number.isNaN(f)) return "NaN";
  if (f === Infinity) return "Infinity";
  if (f === -Infinity) return "-Infinity";
  if (f === 0) return Object.is(f, -0) ? "-0.0" : "0.0";
  const abs = Math.abs(f);
  if (abs < 0.1) {
    let s = f.toExponential(sig - 1);
    s = s.replace(/(\.\d*?)0+e/, "$1e").replace(/\.e/, ".0e");
    if (!s.includes(".")) s = s.replace(/e/, ".0e");
    s = s.replace(/e\+?(-?)0*(\d)/, "e$1$2");
    return s;
  }
  let s = f.toPrecision(sig);
  if (s.includes("e")) {
    s = s.replace(/(\.\d*?)0+e/, "$1e").replace(/\.e/, ".0e");
    if (!s.includes(".")) s = s.replace(/e/, ".0e");
    s = s.replace(/e\+?(-?)0*(\d)/, "e$1$2");
    return s;
  }
  if (s.includes(".")) {
    s = s.replace(/0+$/, "");
    if (s.endsWith(".")) s += "0";
  } else {
    s += ".0";
  }
  return s;
};

// (Legacy aliases retained for the encoder until it switches to the
// generic show* family.)
export const bigintToInt32 = (n: bigint): number => Number(n);
export const int32ToBigint = (n: number): bigint => BigInt(n);

// float64/float32 -> Decimal is exact: every IEEE 754 double has a finite
// decimal expansion. Use the shortest round-tripping decimal string
// (String(f)) and parse that back, rather than expanding the full binary
// fraction, matching printFloat64's own shortest-round-trip convention.
export const float64ToDecimal = (f: number): Decimal => {
  const parsed = parseDecimal(floatDigitString(f));
  return parsed.tag === "given" ? parsed.value : mkDecimal(0n, 0);
};
export const decimalToFloat64 = decimalToFloat;

// Render a finite float as a plain decimal digit string (no "NaN"/"Infinity"
// sentinels -- callers only reach here for finite values) suitable for
// parseDecimal, normalizing String(f)'s exponent spelling to the JSON
// number grammar ("e+"/"e-", no leading zeros).
const floatDigitString = (f: number): string =>
  String(f).replace(/e\+?(-?)0*(\d)/, "e$1$2");

// === Width-specialized parse aliases ===
//
// The kernel emits direct references to `lib_literals.parseFloat32`, etc.,
// at the runtime layer (not via the primitives registry). They all
// delegate to the underlying parser; the width tag is a hint about
// where the result will be stored, not about the parsing rules.
// `parseFloat32` narrows the parsed value to single-precision so the
// round-tripped string matches what a real float32 would store.
export const parseFloat32 = (s: string): Optional<number> => {
  const m = readFloat(s);
  return m.tag === "given" ? { tag: "given" as const, value: Math.fround(m.value) } : m;
};
export const parseFloat64 = readFloat;
export const parseInt8 = readInt;
export const parseInt16 = readInt;
export const parseInt32 = readInt;
export const parseInt64 = parseBigint;
export const parseUint8 = readUint;
export const parseUint16 = readUint;
export const parseUint32 = readUint;
export const parseUint64 = parseBigint;

// === Width-specialized show aliases ===
//
// Similar to the read family: the kernel calls these directly. show*
// already exists for fixed widths above; expose them with their
// runtime names.

// === Width-specialized conversions ===
//
// Convert between fixed-width integer/float types. JS numbers are
// always Float64, so the conversions are essentially identity except
// for explicit downcasts.
export const bigintToInt8 = (n: bigint): number => Number(n);
export const bigintToInt16 = (n: bigint): number => Number(n);
export const bigintToInt64 = (n: bigint): bigint => n;
export const bigintToUint8 = (n: bigint): number => Number(n);
export const bigintToUint16 = (n: bigint): number => Number(n);
export const bigintToUint32 = (n: bigint): number => Number(n);
export const bigintToUint64 = (n: bigint): bigint => n;
export const int8ToBigint = (n: number): bigint => BigInt(n);
export const int16ToBigint = (n: number): bigint => BigInt(n);
export const int64ToBigint = (n: bigint): bigint => n;
export const uint8ToBigint = (n: number): bigint => BigInt(n);
export const uint16ToBigint = (n: number): bigint => BigInt(n);
export const uint32ToBigint = (n: number): bigint => BigInt(n);
export const uint64ToBigint = (n: bigint): bigint => n;
export const decimalToFloat32 = (d: Decimal): number => Math.fround(decimalToFloat(d));
export const float32ToDecimal = (f: number): Decimal => float64ToDecimal(Math.fround(f));
export const float32ToFloat64 = (f: number): number => f;
export const float64ToFloat32 = (f: number): number => Math.fround(f);
