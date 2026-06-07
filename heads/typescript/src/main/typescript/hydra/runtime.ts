// Hand-written core types for the Hydra TypeScript runtime.
//
// These wrap the (later, code-generated) kernel types in TypeScript-native
// shapes. The encoding choices match `docs/dsl-guide.md` and the per-language
// counterparts in `heads/python` and `heads/scala`:
//
//   * Records ............ `readonly interface`
//   * Discriminated unions  `{ readonly tag: "...", readonly value: T }`
//   * Newtype wrappers .... `readonly` interfaces with a `_brand`
//   * Maps / sets ......... `ReadonlyMap` / `ReadonlySet`
//
// The shapes here are stable; the code generator emits modules that import
// from this file.

/** A Hydra namespace, the fully-qualified prefix shared by every element of a module. */
export interface Namespace {
  readonly value: string;
}

export function Namespace(value: string): Namespace {
  return { value };
}

/** A fully-qualified Hydra name; structurally compatible with the
 * generated kernel `Name` (both declare `{ value: string }`). */
export interface Name {
  readonly value: string;
}

export function Name(value: string): Name {
  return { value };
}

/** Optional<T> — discriminated union mirroring Hydra's `optional`. */
export type Optional<T> =
  | { readonly tag: "given"; readonly value: T }
  | { readonly tag: "none" };

export const None: Optional<never> = Object.freeze({ tag: "none" });

export function Given<T>(value: T): Optional<T> {
  return { tag: "given", value };
}

export function isGiven<T>(m: Optional<T>): m is { tag: "given"; value: T } {
  return m.tag === "given";
}

export function fromOptional<T>(fallback: T, m: Optional<T>): T {
  return m.tag === "given" ? m.value : fallback;
}

/** Either<L, R> — discriminated union mirroring Hydra's `either`. */
export type Either<L, R> =
  | { readonly tag: "left"; readonly value: L }
  | { readonly tag: "right"; readonly value: R };

export function Left<L, R = never>(value: L): Either<L, R> {
  return { tag: "left", value };
}

export function Right<R, L = never>(value: R): Either<L, R> {
  return { tag: "right", value };
}

export function isLeft<L, R>(e: Either<L, R>): e is { tag: "left"; value: L } {
  return e.tag === "left";
}

export function isRight<L, R>(e: Either<L, R>): e is { tag: "right"; value: R } {
  return e.tag === "right";
}

/** A Hydra pair, `(fst, snd)`. */
export interface Pair<A, B> {
  readonly fst: A;
  readonly snd: B;
}

export function Pair<A, B>(fst: A, snd: B): Pair<A, B> {
  return { fst, snd };
}

/**
 * The unit type. Hydra's `unit` becomes a singleton record so it survives
 * structural comparisons without colliding with `void` (which TypeScript does
 * not let you observe).
 */
export const Unit = Object.freeze({ _brand: "Unit" as const });
export type Unit = typeof Unit;
