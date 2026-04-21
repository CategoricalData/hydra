/**
 * Computation primitives for Hydra-TypeScript.
 *
 * Provides Either-based error handling and basic combinators
 * matching the kernel's computation model.
 */

import type { Either, Term, Graph, Name } from "./core.js";
import { left, right } from "./core.js";

export function mapEither<L, A, B>(
  f: (a: A) => B,
  e: Either<L, A>,
): Either<L, B> {
  return e.tag === "right" ? right(f(e.value)) : e;
}

export function flatMapEither<L, A, B>(
  f: (a: A) => Either<L, B>,
  e: Either<L, A>,
): Either<L, B> {
  return e.tag === "right" ? f(e.value) : e;
}

export function pureEither<L, A>(a: A): Either<L, A> {
  return right(a);
}

export function failEither<A>(msg: string): Either<string, A> {
  return left(msg);
}

export function fromMaybe<A>(
  err: string,
  m: { readonly tag: "nothing" } | { readonly tag: "just"; readonly value: A },
): Either<string, A> {
  return m.tag === "just" ? right(m.value) : left(err);
}

export function lookupElement(
  name: Name,
  graph: Graph,
): Either<string, Term> {
  const el = graph.elements.get(name);
  if (el === undefined) {
    return left(`Element not found: ${name}`);
  }
  return right(el);
}
