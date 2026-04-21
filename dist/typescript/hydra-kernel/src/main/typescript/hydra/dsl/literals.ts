/**
 * DSL helpers for constructing Hydra literal values in TypeScript.
 */

import type { Literal, FloatValue, IntegerValue } from "../core.js";

export function booleanLiteral(b: boolean): Literal {
  return { tag: "boolean", value: b };
}

export function stringLiteral(s: string): Literal {
  return { tag: "string", value: s };
}

export function int32Literal(n: number): Literal {
  return { tag: "integer", value: { tag: "int32", value: n } };
}

export function int64Literal(n: bigint): Literal {
  return { tag: "integer", value: { tag: "int64", value: n } };
}

export function float32Literal(n: number): Literal {
  return { tag: "float", value: { tag: "float32", value: n } };
}

export function float64Literal(n: number): Literal {
  return { tag: "float", value: { tag: "float64", value: n } };
}

export function bigintLiteral(n: bigint): Literal {
  return { tag: "integer", value: { tag: "bigint", value: n } };
}

export function binaryLiteral(b: Uint8Array): Literal {
  return { tag: "binary", value: b };
}
