// Note: this is an automatically generated file. Do not edit.

/**
 * Variant types which describe the structure of Hydra core types and terms.
 */



import * as Core from "./core.js";

export type EliminationVariant =
  | { readonly tag: "record" }
  | { readonly tag: "union" }
  | { readonly tag: "wrap" };

export type FunctionVariant =
  | { readonly tag: "elimination" }
  | { readonly tag: "lambda" };

export type LiteralVariant =
  | { readonly tag: "binary" }
  | { readonly tag: "boolean" }
  | { readonly tag: "float" }
  | { readonly tag: "integer" }
  | { readonly tag: "string" };

export type TermVariant =
  | { readonly tag: "annotated" }
  | { readonly tag: "application" }
  | { readonly tag: "cases" }
  | { readonly tag: "either" }
  | { readonly tag: "inject" }
  | { readonly tag: "lambda" }
  | { readonly tag: "let" }
  | { readonly tag: "list" }
  | { readonly tag: "literal" }
  | { readonly tag: "map" }
  | { readonly tag: "maybe" }
  | { readonly tag: "pair" }
  | { readonly tag: "project" }
  | { readonly tag: "record" }
  | { readonly tag: "set" }
  | { readonly tag: "typeApplication" }
  | { readonly tag: "typeLambda" }
  | { readonly tag: "unit" }
  | { readonly tag: "unwrap" }
  | { readonly tag: "variable" }
  | { readonly tag: "wrap" };

export type TypeVariant =
  | { readonly tag: "annotated" }
  | { readonly tag: "application" }
  | { readonly tag: "either" }
  | { readonly tag: "forall" }
  | { readonly tag: "function" }
  | { readonly tag: "list" }
  | { readonly tag: "literal" }
  | { readonly tag: "map" }
  | { readonly tag: "maybe" }
  | { readonly tag: "pair" }
  | { readonly tag: "record" }
  | { readonly tag: "set" }
  | { readonly tag: "union" }
  | { readonly tag: "unit" }
  | { readonly tag: "variable" }
  | { readonly tag: "void" }
  | { readonly tag: "wrap" };
