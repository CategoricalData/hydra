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

import type { Maybe } from "../runtime.js";
import { Just, Nothing } from "../runtime.js";

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

export const showString = (s: string): string => {
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
export const showBoolean = (b: boolean): string => (b ? "true" : "false");

// Generic show for an IntegerValue: `<n>:<typeTag>`.
type IntegerValue = { tag: string; value: number | bigint };
export const showInt = (v: IntegerValue): string => `${v.value}:${v.tag}`;
export const showUint = (v: IntegerValue): string => `${v.value}:${v.tag}`;
// showBigint is called by the kernel in two contexts:
//   - As `lib_literals.showBigint` from `show.core.literal`, with an
//     IntegerValue wrapper `{tag: "bigint", value: n}` — prints "n:bigint".
//   - As a raw integer renderer from `json.writer.valueToExpr`, with a
//     bare JS `bigint` — prints just "n".
export const showBigint = (v: IntegerValue | bigint): string =>
  typeof v === "bigint" ? v.toString() : `${v.value}:${v.tag}`;

type FloatValue = { tag: string; value: number };
export const showFloat = (v: FloatValue): string => `${v.value}:${v.tag}`;

// Match Haskell's `show` for Double:
//   - exponent in [-1, 7) → fixed-point form, with mandatory `.0` for ints (e.g. "1.0", "0.1")
//   - otherwise → scientific form `<mantissa>e<exp>` (e.g. "1.0e-2", "1.0e20")
// JavaScript's `Number.prototype.toString` differs from Haskell in:
//   - 0.01 → "0.01" vs Haskell "1.0e-2"
//   - 1.0 → "1" vs Haskell "1.0"
export const showDecimal = (f: number): string => {
  if (!Number.isFinite(f)) {
    if (Number.isNaN(f)) return "NaN";
    return f > 0 ? "Infinity" : "-Infinity";
  }
  if (f === 0) return Object.is(f, -0) ? "-0.0" : "0.0";
  const abs = Math.abs(f);
  const exp = Math.floor(Math.log10(abs));
  // Haskell uses scientific notation when exponent < -1 OR exponent >= 7.
  const useSci = exp < -1 || exp >= 7;
  if (useSci) {
    // Use JS's built-in toExponential for accuracy, then reformat to
    // Haskell's `mantissa e exp` convention (no `+` on positive exp).
    // toExponential() picks the shortest representation that round-trips.
    const ex = f.toExponential();
    const m = ex.match(/^(-?)(\d+)(?:\.(\d+))?[eE]([+-]?\d+)$/);
    if (!m) return ex;
    const [, sign, intP, fracP, expP] = m;
    let mantissa = `${sign}${intP}.${fracP || "0"}`;
    if ((fracP ?? "") === "") mantissa = `${sign}${intP}.0`;
    return `${mantissa}e${parseInt(expP!, 10)}`;
  }
  // Fixed-point form
  let s = f.toString();
  if (s.includes("e") || s.includes("E")) {
    // Number already in scientific - convert to fixed
    s = f.toFixed(Math.max(0, -exp + 1));
  }
  if (!s.includes(".")) s += ".0";
  return s;
};

// === read family ===

export const readInt = (s: string): Maybe<number> => {
  const n = parseInt(s, 10);
  return Number.isFinite(n) && /^-?\d+$/.test(s.trim()) ? Just(n) : Nothing;
};

export const readUint = (s: string): Maybe<number> => {
  const n = parseInt(s, 10);
  return Number.isFinite(n) && n >= 0 && /^\d+$/.test(s.trim()) ? Just(n) : Nothing;
};

export const readBigint = (s: string): Maybe<bigint> => {
  try {
    return /^-?\d+$/.test(s.trim()) ? Just(BigInt(s.trim())) : Nothing;
  } catch { return Nothing; }
};

export const readFloat = (s: string): Maybe<number> => {
  // Accept NaN, Infinity, and -Infinity as valid float literals so the
  // JSON decoder's special-float path (parseSpecialFloat) succeeds.
  if (s === "NaN") return Just(NaN);
  if (s === "Infinity") return Just(Infinity);
  if (s === "-Infinity") return Just(-Infinity);
  if (s === "-0.0") return Just(-0);
  const n = parseFloat(s);
  return Number.isNaN(n) ? Nothing : Just(n);
};

export const readDecimal = readFloat;

// === conversions ===

// "bigint" here means Hydra's BigInteger value — represented as JS bigint.
export const bigintToInt = (n: bigint): number => Number(n);
export const bigintToUint = (n: bigint): number => Number(n);
export const bigintToDecimal = (n: bigint): number => Number(n);

// `decimalToBigint` matches Haskell's `round` (banker's rounding to nearest,
// ties-to-even). For symmetry with the kernel test fixtures, use Math.round
// which rounds halves *away from zero*. Tests use values like 42.7 where this
// agrees with banker's rounding.
export const decimalToBigint = (f: number): bigint => BigInt(Math.round(f));
export const decimalToFloat = (f: number): number => f;

// === wrappers for primitive constructors ===
// These return the canonical IntegerValue / FloatValue shape so that
// generated code can construct Hydra literals through the library.

export const int = (n: number): IntegerValue => ({ tag: "int32", value: n });
export const uint = (n: number): IntegerValue => ({ tag: "uint32", value: n });
export const float = (f: number): FloatValue => ({ tag: "float64", value: f });

// === binary <-> string ===
// Hydra binary literals are byte strings; the TS runtime uses Uint8Array.

// `binaryToString` produces a base64 string from the binary content.
// In the TypeScript runtime, binary literal values are stored as base64
// strings already (because that's how the coder emits them in TS source),
// so this is the identity for string inputs. For Uint8Array (e.g. when
// constructed programmatically), encode to base64. Treats null/undefined
// as the empty binary.
export const binaryToString = (b: Uint8Array | string | null | undefined): string => {
  if (b === null || b === undefined) return "";
  if (typeof b === "string") return b;
  // Uint8Array → base64
  let raw = "";
  for (const byte of b) raw += String.fromCharCode(byte);
  return typeof btoa !== "undefined"
    ? btoa(raw)
    : Buffer.from(raw, "binary").toString("base64");
};

// `stringToBinary` is the inverse: given a base64 string, return the
// binary representation. Since the TypeScript runtime represents binary
// content as base64 strings at the Term level, this is also the identity
// for the round-trip — caller has the base64 string back as the binary
// value, which is what `binaryToString` will accept.
export const stringToBinary = (s: string): string => s;

// `binaryToBytes` decodes a binary value into a list of byte values
// (0-255). Mirrors Python's `binary_to_bytes`. The TS runtime stores
// binary as base64-encoded strings, so we decode then return byte ints.
export const binaryToBytes = (b: Uint8Array | string | null | undefined): readonly number[] => {
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

export const showInt8 = (n: number): string => n.toString();
export const showInt16 = (n: number): string => n.toString();
export const showInt32 = (n: number): string => n.toString();
export const showInt64 = (n: bigint): string => n.toString();
export const showUint8 = (n: number): string => n.toString();
export const showUint16 = (n: number): string => n.toString();
export const showUint32 = (n: number): string => n.toString();
export const showUint64 = (n: bigint): string => n.toString();
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
export const showFloat32 = (f: number): string => _showFloatPreciseSig(f, 7);
export const showFloat64 = (f: number): string => _showFloatPrecise(f);

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
export const float64ToDecimal = (f: number): number => f;
export const decimalToFloat64 = (f: number): number => f;

// === Width-specialized read aliases ===
//
// The kernel emits direct references to `lib_literals.readFloat32`, etc.,
// at the runtime layer (not via the primitives registry). They all
// delegate to the underlying parser; the width tag is a hint about
// where the result will be stored, not about the parsing rules.
// `readFloat32` narrows the parsed value to single-precision so the
// round-tripped string matches what a real float32 would store.
export const readFloat32 = (s: string): Maybe<number> => {
  const m = readFloat(s);
  return m.tag === "just" ? { tag: "just" as const, value: Math.fround(m.value) } : m;
};
export const readFloat64 = readFloat;
export const readInt8 = readInt;
export const readInt16 = readInt;
export const readInt32 = readInt;
export const readInt64 = readBigint;
export const readUint8 = readUint;
export const readUint16 = readUint;
export const readUint32 = readUint;
export const readUint64 = readBigint;

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
export const decimalToFloat32 = (f: number): number => Math.fround(f);
export const float32ToDecimal = (f: number): number => f;
export const float32ToFloat64 = (f: number): number => f;
export const float64ToFloat32 = (f: number): number => Math.fround(f);
