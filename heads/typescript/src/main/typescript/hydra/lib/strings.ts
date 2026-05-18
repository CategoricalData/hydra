// Hand-written runtime: hydra.lib.strings primitives.
//
// Operates on JavaScript strings. Code points are represented as `number`,
// matching Hydra's `Int` for characters.

export const cat = (xs: readonly string[]): string => xs.join("");
export const cat2 = (a: string) => (b: string): string => a + b;

export const intercalate = (sep: string) => (xs: readonly string[]): string => xs.join(sep);

// Counts Unicode code points (not UTF-16 code units), matching the
// kernel test fixtures' "length of string" expectation.
export const length = (s: string): number => [...s].length;

// `splitOn` mirrors Haskell's Data.List.Split.splitOn. The empty
// separator case is special: it yields `["", c0, c1, ..., cN-1, ""]` —
// the input split between every code point with empty boundary strings.
// For non-empty seps we delegate to JS's String.split.
export const splitOn = (sep: string) => (s: string): readonly string[] => {
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

export const null_ = (s: string): boolean => s.length === 0;

export const toUpper = (s: string): string => s.toUpperCase();
export const toLower = (s: string): string => s.toLowerCase();

export const trim = (s: string): string => s.trim();

// `lines` mirrors Haskell's `Data.List.lines`: an empty input yields
// `[]`; a trailing newline does NOT add an empty final element.
export const lines = (s: string): readonly string[] => {
  if (s === "") return [];
  const parts = s.split("\n");
  // Drop trailing empty caused by a final "\n".
  if (parts[parts.length - 1] === "") parts.pop();
  return parts;
};
// `unlines` appends "\n" after EVERY element (including the last),
// matching Haskell's `Data.List.unlines`.
export const unlines = (xs: readonly string[]): string => xs.map((x) => x + "\n").join("");

export const maybeCharAt = (i: number) => (s: string): import("../core.js").Maybe<number> => {
  const cp = s.codePointAt(i);
  return cp === undefined ? { tag: "nothing" } : { tag: "just", value: cp };
};

export const isPrefix = (pre: string) => (s: string): boolean => s.startsWith(pre);
export const isSuffix = (suf: string) => (s: string): boolean => s.endsWith(suf);
