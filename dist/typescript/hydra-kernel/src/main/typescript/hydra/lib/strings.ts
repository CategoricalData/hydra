/**
 * String primitive functions for Hydra-TypeScript.
 */

import type { Maybe } from "../core.js";
import { just, nothing } from "../core.js";

export function cat(parts: ReadonlyArray<string>): string {
  return parts.join("");
}

export function cat2(a: string): (b: string) => string {
  return (b) => a + b;
}

export function charAt(index: number): (s: string) => number {
  return (s) => {
    if (index < 0 || index >= s.length) {
      throw new Error(`charAt: index ${index} out of bounds for string of length ${s.length}`);
    }
    return s.charCodeAt(index);
  };
}

export function fromList(codes: ReadonlyArray<number>): string {
  return String.fromCharCode(...codes);
}

export function intercalate(sep: string): (parts: ReadonlyArray<string>) => string {
  return (parts) => parts.join(sep);
}

export function length(s: string): number {
  return s.length;
}

export function lines(s: string): ReadonlyArray<string> {
  return s.split("\n");
}

export function maybeCharAt(index: number): (s: string) => Maybe<number> {
  return (s) => {
    if (index < 0 || index >= s.length) return nothing();
    return just(s.charCodeAt(index));
  };
}

export function null_(s: string): boolean {
  return s.length === 0;
}

export function splitOn(sep: string): (s: string) => ReadonlyArray<string> {
  return (s) => s.split(sep);
}

export function toList(s: string): ReadonlyArray<number> {
  const result: number[] = [];
  for (let i = 0; i < s.length; i++) {
    result.push(s.charCodeAt(i));
  }
  return result;
}

export function toLower(s: string): string {
  return s.toLowerCase();
}

export function toUpper(s: string): string {
  return s.toUpperCase();
}

export function unlines(ls: ReadonlyArray<string>): string {
  return ls.join("\n");
}
