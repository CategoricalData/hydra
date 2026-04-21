// Note: this is an automatically generated file. Do not edit.

/**
 * A JSON syntax model. See the BNF at https://www.json.org
 */



import * as Core from "../core.js";

export type Value =
  | { readonly tag: "array"; readonly value: ReadonlyArray<Value> }
  | { readonly tag: "boolean"; readonly value: boolean }
  | { readonly tag: "null" }
  | { readonly tag: "number"; readonly value: number }
  | { readonly tag: "object"; readonly value: ReadonlyMap<string, Value> }
  | { readonly tag: "string"; readonly value: string };
