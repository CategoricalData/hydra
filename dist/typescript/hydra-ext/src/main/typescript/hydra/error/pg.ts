// Note: this is an automatically generated file. Do not edit.

/**
 * Error types for property graph validation
 */



import * as PgModel from "../pg/model.js";

export type InvalidEdgeError =
  | { readonly tag: "id"; readonly value: InvalidValueError }
  | { readonly tag: "inVertexLabel"; readonly value: WrongVertexLabelError }
  | { readonly tag: "inVertexNotFound" }
  | { readonly tag: "label"; readonly value: NoSuchEdgeLabelError }
  | { readonly tag: "outVertexLabel"; readonly value: WrongVertexLabelError }
  | { readonly tag: "outVertexNotFound" }
  | { readonly tag: "property"; readonly value: InvalidElementPropertyError };

export interface InvalidElementPropertyError {
  readonly key: PgModel.PropertyKey;
  readonly error: InvalidPropertyError;
}

export interface InvalidGraphEdgeError<v> {
  readonly id: v;
  readonly error: InvalidEdgeError;
}

export type InvalidGraphError<v> =
  | { readonly tag: "edge"; readonly value: InvalidGraphEdgeError<v> }
  | { readonly tag: "vertex"; readonly value: InvalidGraphVertexError<v> };

export interface InvalidGraphVertexError<v> {
  readonly id: v;
  readonly error: InvalidVertexError;
}

export type InvalidPropertyError =
  | { readonly tag: "invalidValue"; readonly value: InvalidValueError }
  | { readonly tag: "missingRequired"; readonly value: PgModel.PropertyKey }
  | { readonly tag: "unexpectedKey"; readonly value: PgModel.PropertyKey };

export interface InvalidValueError {
  readonly expectedType: string;
  readonly value: string;
}

export type InvalidVertexError =
  | { readonly tag: "id"; readonly value: InvalidValueError }
  | { readonly tag: "label"; readonly value: NoSuchVertexLabelError }
  | { readonly tag: "property"; readonly value: InvalidElementPropertyError };

export interface NoSuchEdgeLabelError {
  readonly label: PgModel.EdgeLabel;
}

export interface NoSuchVertexLabelError {
  readonly label: PgModel.VertexLabel;
}

export interface WrongVertexLabelError {
  readonly expected: PgModel.VertexLabel;
  readonly actual: PgModel.VertexLabel;
}
