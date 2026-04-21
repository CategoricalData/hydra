// Note: this is an automatically generated file. Do not edit.

/**
 * A basic YAML representation model. Based on:
 *   https://yaml.org/spec/1.2/spec.html
 * The Serialization and Presentation properties of YAML,
 * including directives, comments, anchors, style, formatting, and aliases, are not supported by this model.
 * In addition, tags are omitted from this model, and non-standard scalars are unsupported.
 */



import * as Core from "../core.js";

export type Node =
  | { readonly tag: "mapping"; readonly value: ReadonlyMap<Node, Node> }
  | { readonly tag: "scalar"; readonly value: Scalar }
  | { readonly tag: "sequence"; readonly value: ReadonlyArray<Node> };

export type Scalar =
  | { readonly tag: "bool"; readonly value: boolean }
  | { readonly tag: "float"; readonly value: number }
  | { readonly tag: "int"; readonly value: bigint }
  | { readonly tag: "null" }
  | { readonly tag: "str"; readonly value: string };
