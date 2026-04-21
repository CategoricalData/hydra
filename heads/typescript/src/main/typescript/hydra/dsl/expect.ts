/**
 * DSL helpers for extracting values from Hydra terms, failing with descriptive errors.
 */

import type { Either, Maybe, Term, Field, Name } from "../core.js";
import { left, right, just, nothing } from "../core.js";

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

export function expectInt8(t: Term): Either<string, number> {
  if (t.tag === "literal" && t.value.tag === "integer" && t.value.value.tag === "int8") {
    return right(t.value.value.value);
  }
  return left(`Expected int8 literal, got ${t.tag}`);
}

export function expectInt16(t: Term): Either<string, number> {
  if (t.tag === "literal" && t.value.tag === "integer" && t.value.value.tag === "int16") {
    return right(t.value.value.value);
  }
  return left(`Expected int16 literal, got ${t.tag}`);
}

export function expectInt64(t: Term): Either<string, bigint> {
  if (t.tag === "literal" && t.value.tag === "integer" && t.value.value.tag === "int64") {
    return right(t.value.value.value);
  }
  return left(`Expected int64 literal, got ${t.tag}`);
}

export function expectUint8(t: Term): Either<string, number> {
  if (t.tag === "literal" && t.value.tag === "integer" && t.value.value.tag === "uint8") {
    return right(t.value.value.value);
  }
  return left(`Expected uint8 literal, got ${t.tag}`);
}

export function expectUint16(t: Term): Either<string, number> {
  if (t.tag === "literal" && t.value.tag === "integer" && t.value.value.tag === "uint16") {
    return right(t.value.value.value);
  }
  return left(`Expected uint16 literal, got ${t.tag}`);
}

export function expectUint32(t: Term): Either<string, number> {
  if (t.tag === "literal" && t.value.tag === "integer" && t.value.value.tag === "uint32") {
    return right(t.value.value.value);
  }
  return left(`Expected uint32 literal, got ${t.tag}`);
}

export function expectUint64(t: Term): Either<string, bigint> {
  if (t.tag === "literal" && t.value.tag === "integer" && t.value.value.tag === "uint64") {
    return right(t.value.value.value);
  }
  return left(`Expected uint64 literal, got ${t.tag}`);
}

export function expectBigint(t: Term): Either<string, bigint> {
  if (t.tag === "literal" && t.value.tag === "integer" && t.value.value.tag === "bigint") {
    return right(t.value.value.value);
  }
  return left(`Expected bigint literal, got ${t.tag}`);
}

export function expectFloat32(t: Term): Either<string, number> {
  if (t.tag === "literal" && t.value.tag === "float" && t.value.value.tag === "float32") {
    return right(t.value.value.value);
  }
  return left(`Expected float32 literal, got ${t.tag}`);
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

export function expectBigfloat(t: Term): Either<string, number> {
  if (t.tag === "literal" && t.value.tag === "float" && t.value.value.tag === "bigfloat") {
    return right(t.value.value.value);
  }
  return left(`Expected bigfloat literal, got ${t.tag}`);
}

export function expectBinary(t: Term): Either<string, Uint8Array> {
  if (t.tag === "literal" && t.value.tag === "binary") {
    return right(t.value.value);
  }
  return left(`Expected binary literal, got ${t.tag}`);
}

export function expectList(t: Term): Either<string, ReadonlyArray<Term>> {
  if (t.tag === "list") {
    return right(t.value);
  }
  return left(`Expected list, got ${t.tag}`);
}

export function expectSet(t: Term): Either<string, ReadonlySet<Term>> {
  if (t.tag === "set") {
    return right(t.value);
  }
  return left(`Expected set, got ${t.tag}`);
}

export function expectMap(t: Term): Either<string, ReadonlyMap<Term, Term>> {
  if (t.tag === "map") {
    return right(t.value);
  }
  return left(`Expected map, got ${t.tag}`);
}

export function expectPair(t: Term): Either<string, readonly [Term, Term]> {
  if (t.tag === "pair") {
    return right(t.value);
  }
  return left(`Expected pair, got ${t.tag}`);
}

export function expectMaybe(t: Term): Either<string, Maybe<Term>> {
  if (t.tag === "maybe") {
    return right(t.value);
  }
  return left(`Expected maybe, got ${t.tag}`);
}

export function expectEither(t: Term): Either<string, Either<Term, Term>> {
  if (t.tag === "either") {
    return right(t.value);
  }
  return left(`Expected either, got ${t.tag}`);
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

export function expectNArgs(name: string, n: number, args: ReadonlyArray<Term>): Either<string, true> {
  if (args.length !== n) {
    return left(`Primitive ${name} expected ${n} arguments, got ${args.length}`);
  }
  return right(true);
}
