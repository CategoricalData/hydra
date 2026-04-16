/**
 * A DSL for constructing Hydra types in TypeScript.
 *
 * Mirrors the Haskell DSL in Hydra.Dsl.Types.
 */

import type {
  FieldType, ForallType, FunctionType, MapType, PairType, Type,
} from "../core.js";
import { Name } from "../core.js";

export function annotated(t: Type, annotations: ReadonlyMap<string, unknown>): Type {
  return { tag: "annotated", value: { body: t, annotation: new Map() } };
}

export function application(f: Type, a: Type): Type {
  return { tag: "application", value: { function: f, argument: a } };
}

export function boolean(): Type {
  return { tag: "literal", value: { tag: "boolean" } };
}

export function either(l: Type, r: Type): Type {
  return { tag: "either", value: { left: l, right: r } };
}

export function float32(): Type {
  return { tag: "literal", value: { tag: "float", value: { tag: "float32" } } };
}

export function float64(): Type {
  return { tag: "literal", value: { tag: "float", value: { tag: "float64" } } };
}

export function forall(variable: string, body: Type): Type {
  return { tag: "forall", value: { variable: Name(variable), body } };
}

export function func(domain: Type, codomain: Type): Type {
  return { tag: "function", value: { domain, codomain } };
}

export function int8(): Type {
  return { tag: "literal", value: { tag: "integer", value: { tag: "int8" } } };
}

export function int16(): Type {
  return { tag: "literal", value: { tag: "integer", value: { tag: "int16" } } };
}

export function int32(): Type {
  return { tag: "literal", value: { tag: "integer", value: { tag: "int32" } } };
}

export function int64(): Type {
  return { tag: "literal", value: { tag: "integer", value: { tag: "int64" } } };
}

export function bigint_(): Type {
  return { tag: "literal", value: { tag: "integer", value: { tag: "bigint" } } };
}

export function list(element: Type): Type {
  return { tag: "list", value: element };
}

export function map(keys: Type, values: Type): Type {
  return { tag: "map", value: { keys, values } };
}

export function maybe(t: Type): Type {
  return { tag: "maybe", value: t };
}

export function pair(first: Type, second: Type): Type {
  return { tag: "pair", value: { first, second } };
}

export function record(fields: ReadonlyArray<FieldType>): Type {
  return { tag: "record", value: fields };
}

export function set(element: Type): Type {
  return { tag: "set", value: element };
}

export function string_(): Type {
  return { tag: "literal", value: { tag: "string" } };
}

export function union(fields: ReadonlyArray<FieldType>): Type {
  return { tag: "union", value: fields };
}

export function unit(): Type {
  return { tag: "unit" };
}

export function variable(name: string): Type {
  return { tag: "variable", value: Name(name) };
}

export function void_(): Type {
  return { tag: "void" };
}

export function wrap(name: string): Type {
  return { tag: "wrap", value: Name(name) };
}

export function field(name: string, type: Type): FieldType {
  return { name: Name(name), type };
}

export function binary(): Type {
  return { tag: "literal", value: { tag: "binary" } };
}
