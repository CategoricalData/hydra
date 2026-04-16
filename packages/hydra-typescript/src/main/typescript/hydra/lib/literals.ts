/**
 * Literal conversion primitive functions for Hydra-TypeScript.
 */

export function showInt32(n: number): string {
  return String(n);
}

export function showInt64(n: bigint): string {
  return String(n);
}

export function showFloat32(n: number): string {
  return String(n);
}

export function showFloat64(n: number): string {
  return String(n);
}

export function showBigint(n: bigint): string {
  return String(n);
}

export function showBoolean(b: boolean): string {
  return b ? "true" : "false";
}

export function readInt32(s: string): number {
  const n = parseInt(s, 10);
  if (isNaN(n)) {
    throw new Error(`Invalid int32: ${s}`);
  }
  return n;
}

export function readFloat64(s: string): number {
  const n = parseFloat(s);
  if (isNaN(n) && s !== "NaN") {
    throw new Error(`Invalid float64: ${s}`);
  }
  return n;
}
