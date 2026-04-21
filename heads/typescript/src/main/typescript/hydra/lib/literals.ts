/**
 * Literal conversion primitive functions for Hydra-TypeScript.
 *
 * TypeScript uses `number` for all integer/float types except bigint.
 * The narrowing conversions (e.g. bigintToInt8) clamp to the target range.
 */

import type { Maybe } from "../core.js";
import { just, nothing } from "../core.js";

// --- Bigfloat conversions (bigfloat ≈ number in TS) ---

export function bigfloatToBigint(n: number): bigint {
  return BigInt(Math.trunc(n));
}

export function bigfloatToFloat32(n: number): number {
  return Math.fround(n);
}

export function bigfloatToFloat64(n: number): number {
  return n;
}

// --- Bigint conversions ---

export function bigintToBigfloat(n: bigint): number {
  return Number(n);
}

export function bigintToInt8(n: bigint): number {
  return Number(BigInt.asIntN(8, n));
}

export function bigintToInt16(n: bigint): number {
  return Number(BigInt.asIntN(16, n));
}

export function bigintToInt32(n: bigint): number {
  return Number(BigInt.asIntN(32, n));
}

export function bigintToInt64(n: bigint): bigint {
  return BigInt.asIntN(64, n);
}

export function bigintToUint8(n: bigint): number {
  return Number(BigInt.asUintN(8, n));
}

export function bigintToUint16(n: bigint): number {
  return Number(BigInt.asUintN(16, n));
}

export function bigintToUint32(n: bigint): number {
  return Number(BigInt.asUintN(32, n));
}

export function bigintToUint64(n: bigint): bigint {
  return BigInt.asUintN(64, n);
}

// --- Binary conversions ---

export function binaryToBytes(b: Uint8Array): ReadonlyArray<number> {
  return Array.from(b);
}

export function binaryToString(b: Uint8Array): string {
  return new TextDecoder().decode(b);
}

// --- Float-to-bigfloat ---

export function float32ToBigfloat(n: number): number {
  return n;
}

export function float64ToBigfloat(n: number): number {
  return n;
}

// --- Int/Uint to bigint ---

export function int8ToBigint(n: number): bigint {
  return BigInt(n);
}

export function int16ToBigint(n: number): bigint {
  return BigInt(n);
}

export function int32ToBigint(n: number): bigint {
  return BigInt(n);
}

export function int64ToBigint(n: bigint): bigint {
  return n;
}

export function uint8ToBigint(n: number): bigint {
  return BigInt(n);
}

export function uint16ToBigint(n: number): bigint {
  return BigInt(n);
}

export function uint32ToBigint(n: number): bigint {
  return BigInt(n);
}

export function uint64ToBigint(n: bigint): bigint {
  return n;
}

// --- String-to-binary ---

export function stringToBinary(s: string): Uint8Array {
  return new TextEncoder().encode(s);
}

// --- Read functions (string -> Maybe<value>) ---

export function readBigfloat(s: string): Maybe<number> {
  const n = parseFloat(s);
  return isNaN(n) && s !== "NaN" ? nothing() : just(n);
}

export function readBigint(s: string): Maybe<bigint> {
  try {
    return just(BigInt(s));
  } catch {
    return nothing();
  }
}

export function readBoolean(s: string): Maybe<boolean> {
  if (s === "true") return just(true);
  if (s === "false") return just(false);
  return nothing();
}

export function readFloat32(s: string): Maybe<number> {
  const n = parseFloat(s);
  return isNaN(n) && s !== "NaN" ? nothing() : just(Math.fround(n));
}

export function readFloat64(s: string): Maybe<number> {
  const n = parseFloat(s);
  return isNaN(n) && s !== "NaN" ? nothing() : just(n);
}

export function readInt8(s: string): Maybe<number> {
  const n = parseInt(s, 10);
  return isNaN(n) || n < -128 || n > 127 ? nothing() : just(n);
}

export function readInt16(s: string): Maybe<number> {
  const n = parseInt(s, 10);
  return isNaN(n) || n < -32768 || n > 32767 ? nothing() : just(n);
}

export function readInt32(s: string): Maybe<number> {
  const n = parseInt(s, 10);
  return isNaN(n) ? nothing() : just(n | 0);
}

export function readInt64(s: string): Maybe<bigint> {
  try {
    return just(BigInt(s));
  } catch {
    return nothing();
  }
}

export function readString(s: string): Maybe<string> {
  return just(s);
}

export function readUint8(s: string): Maybe<number> {
  const n = parseInt(s, 10);
  return isNaN(n) || n < 0 || n > 255 ? nothing() : just(n);
}

export function readUint16(s: string): Maybe<number> {
  const n = parseInt(s, 10);
  return isNaN(n) || n < 0 || n > 65535 ? nothing() : just(n);
}

export function readUint32(s: string): Maybe<number> {
  const n = parseInt(s, 10);
  return isNaN(n) || n < 0 ? nothing() : just(n >>> 0);
}

export function readUint64(s: string): Maybe<bigint> {
  try {
    const n = BigInt(s);
    return n < 0n ? nothing() : just(n);
  } catch {
    return nothing();
  }
}

// --- Show functions (value -> string) ---

export function showBigfloat(n: number): string {
  return String(n);
}

export function showBigint(n: bigint): string {
  return String(n);
}

export function showBoolean(b: boolean): string {
  return b ? "true" : "false";
}

export function showFloat32(n: number): string {
  return String(n);
}

export function showFloat64(n: number): string {
  return String(n);
}

export function showInt8(n: number): string {
  return String(n);
}

export function showInt16(n: number): string {
  return String(n);
}

export function showInt32(n: number): string {
  return String(n);
}

export function showInt64(n: bigint): string {
  return String(n);
}

export function showString(s: string): string {
  return s;
}

export function showUint8(n: number): string {
  return String(n);
}

export function showUint16(n: number): string {
  return String(n);
}

export function showUint32(n: number): string {
  return String(n);
}

export function showUint64(n: bigint): string {
  return String(n);
}
