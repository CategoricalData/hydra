/**
 * Regular expression primitive functions for Hydra-TypeScript.
 */

import type { Maybe } from "../core.js";
import { just, nothing } from "../core.js";

export function find(pattern: string): (input: string) => Maybe<string> {
  return (input) => {
    const m = input.match(new RegExp(pattern));
    return m ? just(m[0]!) : nothing();
  };
}

export function findAll(pattern: string): (input: string) => ReadonlyArray<string> {
  return (input) => {
    const matches = input.match(new RegExp(pattern, "g"));
    return matches ?? [];
  };
}

export function matches(pattern: string): (input: string) => boolean {
  return (input) => new RegExp(pattern).test(input);
}

export function replace(
  pattern: string,
): (replacement: string) => (input: string) => string {
  return (replacement) => (input) =>
    input.replace(new RegExp(pattern), replacement);
}

export function replaceAll(
  pattern: string,
): (replacement: string) => (input: string) => string {
  return (replacement) => (input) =>
    input.replace(new RegExp(pattern, "g"), replacement);
}

export function split(pattern: string): (input: string) => ReadonlyArray<string> {
  return (input) => input.split(new RegExp(pattern));
}
