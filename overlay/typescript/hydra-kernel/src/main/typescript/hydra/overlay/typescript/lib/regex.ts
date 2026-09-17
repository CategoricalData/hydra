// Hand-written runtime: hydra.lib.regex primitives.
//
// Signatures are flat (positional), matching Python's heads/python/lib/regex.py.
// Operates on JavaScript strings using the built-in RegExp.
//
// Patterns are Hydra-defined and translingual (docs/specification/regex.md). Each primitive first
// runs the pattern through hydra.parse.regex, then renders the AST to PCRE syntax via
// hydra.print.pcre.regex (ECMA-262 RegExp shares the byte-identical minimal-core rendering with
// PCRE), before handing the rendered pattern to the native engine. An ill-formed pattern (rejected
// by hydra.parse.regex) is treated as "no match" -- the same portable-failure convention as an empty
// match. See issue #603.

import { parseRegex } from "../../../parse/regex.js";
import { printRegex } from "../../../print/pcre/regex.js";

type HydraOptional<T> = { tag: "given"; value: T } | { tag: "none" };

// Translates a Hydra regex pattern to ECMA/PCRE syntax, or undefined if the pattern does not parse.
const toNative = (pattern: string): string | undefined => {
  const parsed = parseRegex(pattern) as HydraOptional<unknown>;
  return parsed.tag === "given" ? (printRegex(parsed.value as any) as string) : undefined;
};

// Hydra `matches` is anchored (must match the WHOLE string), matching
// Haskell's `Text.Regex.PCRE.match` convention used by the kernel.
export const matches = (pattern: string, s: string): boolean => {
  const native = toNative(pattern);
  if (native === undefined) {
    return false;
  }
  const m = s.match(new RegExp(native));
  return m !== null && m[0] === s;
};

export const find = (pattern: string, s: string): string | undefined => {
  const native = toNative(pattern);
  if (native === undefined) {
    return undefined;
  }
  const m = s.match(new RegExp(native));
  return m === null ? undefined : m[0];
};

export const findAll = (pattern: string, s: string): readonly string[] => {
  const native = toNative(pattern);
  if (native === undefined) {
    return [];
  }
  return Array.from(s.matchAll(new RegExp(native, "g")), (m) => m[0]);
};

export const replace = (pattern: string, replacement: string, s: string): string => {
  const native = toNative(pattern);
  return native === undefined ? s : s.replace(new RegExp(native), replacement);
};

export const replaceAll = (pattern: string, replacement: string, s: string): string => {
  const native = toNative(pattern);
  return native === undefined ? s : s.replace(new RegExp(native, "g"), replacement);
};

export const split = (pattern: string, s: string): readonly string[] => {
  const native = toNative(pattern);
  return native === undefined ? [s] : s.split(new RegExp(native));
};
