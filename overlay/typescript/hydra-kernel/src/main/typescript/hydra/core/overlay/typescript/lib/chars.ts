// Hand-written runtime: hydra.core.lib.chars primitives.
//
// Characters are represented as Unicode code points (`number`), matching
// Hydra's representation.

export const isAlpha = (c: number): boolean => {
  const ch = String.fromCodePoint(c);
  return /\p{Letter}/u.test(ch);
};

export const isDigit = (c: number): boolean => c >= 48 && c <= 57;
export const isAlnum = (c: number): boolean => isAlpha(c) || isDigit(c);
export const isAlphaNum = isAlnum;
export const isSpace = (c: number): boolean => /\s/.test(String.fromCodePoint(c));
export const isUpper = (c: number): boolean => c >= 65 && c <= 90;
export const isLower = (c: number): boolean => c >= 97 && c <= 122;

// JS has no simple (one-to-one) case-mapping API; String.prototype.toUpperCase/toLowerCase
// perform full Unicode case folding, which can turn one code point into several (e.g. "ß" ->
// "SS"). Detect that case and fall back to the input unchanged, per the simple-mapping spec.
// That fallback is correct for every such code point except the ones below, where a length-1
// simple mapping exists but the full mapping is longer. Found by diffing JS's
// toLowerCase/toUpperCase against Java's Character.toLowerCase/toUpperCase (which expose
// Unicode's simple mapping directly) across every code point that changes case.
const SIMPLE_LOWER_OVERRIDES: ReadonlyMap<number, number> = new Map([
  [0x0130, 0x0069], // İ LATIN CAPITAL LETTER I WITH DOT ABOVE -> i
]);
// Greek letters with iota subscript: simple uppercase mapping adds the iota as a capital
// letter (iota subscript -> capital iota), while the full mapping spells out the base
// letter's own uppercase form followed by a capital iota (2 code points).
const SIMPLE_UPPER_OVERRIDES: ReadonlyMap<number, number> = new Map([
  [0x1f80, 0x1f88], [0x1f81, 0x1f89], [0x1f82, 0x1f8a], [0x1f83, 0x1f8b],
  [0x1f84, 0x1f8c], [0x1f85, 0x1f8d], [0x1f86, 0x1f8e], [0x1f87, 0x1f8f],
  [0x1f90, 0x1f98], [0x1f91, 0x1f99], [0x1f92, 0x1f9a], [0x1f93, 0x1f9b],
  [0x1f94, 0x1f9c], [0x1f95, 0x1f9d], [0x1f96, 0x1f9e], [0x1f97, 0x1f9f],
  [0x1fa0, 0x1fa8], [0x1fa1, 0x1fa9], [0x1fa2, 0x1faa], [0x1fa3, 0x1fab],
  [0x1fa4, 0x1fac], [0x1fa5, 0x1fad], [0x1fa6, 0x1fae], [0x1fa7, 0x1faf],
  [0x1fb3, 0x1fbc], [0x1fc3, 0x1fcc], [0x1ff3, 0x1ffc],
]);

const simpleCase = (c: number, map: (s: string) => string): number => {
  const mapped = map(String.fromCodePoint(c));
  return [...mapped].length === 1 ? mapped.codePointAt(0)! : c;
};

export const toUpper = (c: number): number =>
  SIMPLE_UPPER_OVERRIDES.get(c) ?? simpleCase(c, (s) => s.toUpperCase());
export const toLower = (c: number): number =>
  SIMPLE_LOWER_OVERRIDES.get(c) ?? simpleCase(c, (s) => s.toLowerCase());
