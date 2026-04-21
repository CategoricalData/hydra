// Note: this is an automatically generated file. Do not edit.

/**
 * General-purpose utility types used across Hydra.
 */



import * as Core from "./core.js";

export type CaseConvention =
  | { readonly tag: "camel" }
  | { readonly tag: "pascal" }
  | { readonly tag: "lowerSnake" }
  | { readonly tag: "upperSnake" };

export type Comparison =
  | { readonly tag: "lessThan" }
  | { readonly tag: "equalTo" }
  | { readonly tag: "greaterThan" };

export type Precision =
  | { readonly tag: "arbitrary" }
  | { readonly tag: "bits"; readonly value: number };
