/**
 * DSL helpers for constructing Hydra literal types in TypeScript.
 */

import type { LiteralType, FloatType, IntegerType } from "../core.js";

export const booleanType: LiteralType = { tag: "boolean" };
export const stringType: LiteralType = { tag: "string" };
export const binaryType: LiteralType = { tag: "binary" };

export function floatType(ft: FloatType): LiteralType {
  return { tag: "float", value: ft };
}

export function integerType(it: IntegerType): LiteralType {
  return { tag: "integer", value: it };
}

export const float32Type: LiteralType = floatType({ tag: "float32" });
export const float64Type: LiteralType = floatType({ tag: "float64" });
export const bigfloatType: LiteralType = floatType({ tag: "bigfloat" });

export const int8Type: LiteralType = integerType({ tag: "int8" });
export const int16Type: LiteralType = integerType({ tag: "int16" });
export const int32Type: LiteralType = integerType({ tag: "int32" });
export const int64Type: LiteralType = integerType({ tag: "int64" });
export const uint8Type: LiteralType = integerType({ tag: "uint8" });
export const uint16Type: LiteralType = integerType({ tag: "uint16" });
export const uint32Type: LiteralType = integerType({ tag: "uint32" });
export const uint64Type: LiteralType = integerType({ tag: "uint64" });
export const bigintType: LiteralType = integerType({ tag: "bigint" });
