// Hand-written runtime: hydra.lib.strings primitives.
//
// Operates on JavaScript strings. Code points are represented as `number`,
// matching Hydra's `Int` for characters.

export const concat = (xs: readonly string[]): string => xs.join("");
export const concat2 = (a: string, b: string): string => a + b;

export const join = (sep: string, xs: readonly string[]): string => xs.join(sep);

// Counts Unicode code points (not UTF-16 code units), matching the
// kernel test fixtures' "length of string" expectation.
export const length = (s: string): number => [...s].length;

// `splitOn` mirrors Haskell's Data.List.Split.splitOn. The empty
// separator case is special: it yields `["", c0, c1, ..., cN-1, ""]` —
// the input split between every code point with empty boundary strings.
// For non-empty seps we delegate to JS's String.split.
export const splitOn = (sep: string, s: string): readonly string[] => {
  if (sep === "") {
    if (s === "") return [""];
    return ["", ...s.split("")];
  }
  return s.split(sep);
};

export const fromList = (cps: readonly number[]): string => String.fromCodePoint(...cps);

export const toList = (s: string): readonly number[] => {
  const out: number[] = [];
  for (const ch of s) out.push(ch.codePointAt(0)!);
  return out;
};

export const isEmpty_ = (s: string): boolean => s.length === 0;

export const toUpper = (s: string): string => s.toUpperCase();
export const toLower = (s: string): string => s.toLowerCase();

export const trim = (s: string): string => s.trim();

export const charAt = (i: number, s: string): import("../../../runtime.js").Optional<number> => {
  const cp = s.codePointAt(i);
  return cp === undefined ? { tag: "none" } : { tag: "given", value: cp };
};

export const isPrefix = (pre: string, s: string): boolean => s.startsWith(pre);
export const isSuffix = (suf: string, s: string): boolean => s.endsWith(suf);
