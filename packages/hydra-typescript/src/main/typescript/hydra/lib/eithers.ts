/**
 * Either primitive functions for Hydra-TypeScript.
 */

import type { Either } from "../core.js";
import { left, right } from "../core.js";

export function fromLeft<L, R>(def: L, e: Either<L, R>): L {
  return e.tag === "left" ? e.value : def;
}

export function fromRight<L, R>(def: R, e: Either<L, R>): R {
  return e.tag === "right" ? e.value : def;
}

export function isLeft<L, R>(e: Either<L, R>): boolean {
  return e.tag === "left";
}

export function isRight<L, R>(e: Either<L, R>): boolean {
  return e.tag === "right";
}

export function mapLeft<L, L2, R>(
  f: (l: L) => L2,
  e: Either<L, R>,
): Either<L2, R> {
  return e.tag === "left" ? left(f(e.value)) : e;
}

export function mapRight<L, R, R2>(
  f: (r: R) => R2,
  e: Either<L, R>,
): Either<L, R2> {
  return e.tag === "right" ? right(f(e.value)) : e;
}
