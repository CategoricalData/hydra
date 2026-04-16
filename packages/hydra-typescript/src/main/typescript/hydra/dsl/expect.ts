/**
 * DSL helpers for extracting values from Hydra terms, failing with descriptive errors.
 */

import type { Either, Term, Field, Name } from "../core.js";
import { left, right } from "../core.js";

export function expectString(t: Term): Either<string, string> {
  if (t.tag === "literal" && t.value.tag === "string") {
    return right(t.value.value);
  }
  return left(`Expected string literal, got ${t.tag}`);
}

export function expectBoolean(t: Term): Either<string, boolean> {
  if (t.tag === "literal" && t.value.tag === "boolean") {
    return right(t.value.value);
  }
  return left(`Expected boolean literal, got ${t.tag}`);
}

export function expectInt32(t: Term): Either<string, number> {
  if (
    t.tag === "literal" &&
    t.value.tag === "integer" &&
    t.value.value.tag === "int32"
  ) {
    return right(t.value.value.value);
  }
  return left(`Expected int32 literal, got ${t.tag}`);
}

export function expectFloat64(t: Term): Either<string, number> {
  if (
    t.tag === "literal" &&
    t.value.tag === "float" &&
    t.value.value.tag === "float64"
  ) {
    return right(t.value.value.value);
  }
  return left(`Expected float64 literal, got ${t.tag}`);
}

export function expectList(t: Term): Either<string, ReadonlyArray<Term>> {
  if (t.tag === "list") {
    return right(t.value);
  }
  return left(`Expected list, got ${t.tag}`);
}

export function expectRecord(t: Term): Either<string, ReadonlyArray<Field>> {
  if (t.tag === "record") {
    return right(t.value.fields);
  }
  return left(`Expected record, got ${t.tag}`);
}

export function expectUnit(t: Term): Either<string, undefined> {
  if (t.tag === "unit") {
    return right(undefined);
  }
  return left(`Expected unit, got ${t.tag}`);
}
