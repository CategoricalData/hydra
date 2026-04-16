/**
 * Maybe primitive functions for Hydra-TypeScript.
 */

import type { Maybe } from "../core.js";
import { just, nothing } from "../core.js";

export function fromMaybe<T>(def: T, m: Maybe<T>): T {
  return m.tag === "just" ? m.value : def;
}

export function isJust<T>(m: Maybe<T>): boolean {
  return m.tag === "just";
}

export function isNothing<T>(m: Maybe<T>): boolean {
  return m.tag === "nothing";
}

export function maybe<T, R>(
  def: R,
  f: (t: T) => R,
  m: Maybe<T>,
): R {
  return m.tag === "just" ? f(m.value) : def;
}

export function mapMaybe<T, R>(f: (t: T) => R, m: Maybe<T>): Maybe<R> {
  return m.tag === "just" ? just(f(m.value)) : nothing();
}

export function flatMapMaybe<T, R>(
  f: (t: T) => Maybe<R>,
  m: Maybe<T>,
): Maybe<R> {
  return m.tag === "just" ? f(m.value) : nothing();
}

export function catMaybes<T>(ms: ReadonlyArray<Maybe<T>>): ReadonlyArray<T> {
  const result: T[] = [];
  for (const m of ms) {
    if (m.tag === "just") {
      result.push(m.value);
    }
  }
  return result;
}
