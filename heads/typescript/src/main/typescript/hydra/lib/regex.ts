// Hand-written runtime: hydra.lib.regex primitives.
//
// Signatures are flat (positional), matching Python's heads/python/lib/regex.py.
// Operates on JavaScript strings using the built-in RegExp.

// Hydra `matches` is anchored (must match the WHOLE string), matching
// Haskell's `Text.Regex.PCRE.match` convention used by the kernel.
export const matches = (pattern: string, s: string): boolean => {
  const m = s.match(new RegExp(pattern));
  return m !== null && m[0] === s;
};

export const replace = (pattern: string, replacement: string, s: string): string =>
  s.replace(new RegExp(pattern, "g"), replacement);

export const findAll = (pattern: string, s: string): readonly string[] =>
  Array.from(s.matchAll(new RegExp(pattern, "g")), (m) => m[0]);
