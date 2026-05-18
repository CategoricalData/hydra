// Hand-written runtime: hydra.lib.chars primitives.
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

export const toUpper = (c: number): number => String.fromCodePoint(c).toUpperCase().codePointAt(0)!;
export const toLower = (c: number): number => String.fromCodePoint(c).toLowerCase().codePointAt(0)!;
