/**
 * Character primitive functions for Hydra-TypeScript.
 */

export function isAlpha(c: number): boolean {
  return (c >= 65 && c <= 90) || (c >= 97 && c <= 122);
}

export function isAlphaNum(c: number): boolean {
  return isAlpha(c) || isDigit(c);
}

export function isDigit(c: number): boolean {
  return c >= 48 && c <= 57;
}

export function isLower(c: number): boolean {
  return c >= 97 && c <= 122;
}

export function isSpace(c: number): boolean {
  return c === 32 || (c >= 9 && c <= 13);
}

export function isUpper(c: number): boolean {
  return c >= 65 && c <= 90;
}

export function toLower(c: number): number {
  return isUpper(c) ? c + 32 : c;
}

export function toUpper(c: number): number {
  return isLower(c) ? c - 32 : c;
}
