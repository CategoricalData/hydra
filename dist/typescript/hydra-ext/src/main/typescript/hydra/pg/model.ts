// Note: this is an automatically generated file. Do not edit.

/**
 * A typed property graph data model. Property graphs are parameterized a type for property and id values, while property graph schemas are parameterized by a type for property and id types
 */



import * as Core from "../core.js";

export interface AdjacentEdge<v> {
  readonly label: EdgeLabel;
  readonly id: v;
  readonly vertex: v;
  readonly properties: ReadonlyMap<PropertyKey, v>;
}

export type Direction =
  | { readonly tag: "out" }
  | { readonly tag: "in" }
  | { readonly tag: "both" }
  | { readonly tag: "undirected" };

export interface Edge<v> {
  readonly label: EdgeLabel;
  readonly id: v;
  readonly out: v;
  readonly in: v;
  readonly properties: ReadonlyMap<PropertyKey, v>;
}

export type EdgeLabel = string & { readonly __brand: "EdgeLabel" };

export interface EdgeType<t> {
  readonly label: EdgeLabel;
  readonly id: t;
  readonly out: VertexLabel;
  readonly in: VertexLabel;
  readonly properties: ReadonlyArray<PropertyType<t>>;
}

export type Element<v> =
  | { readonly tag: "vertex"; readonly value: Vertex<v> }
  | { readonly tag: "edge"; readonly value: Edge<v> };

export type ElementKind =
  | { readonly tag: "vertex" }
  | { readonly tag: "edge" };

export interface ElementTree<v> {
  readonly self: Element<v>;
  readonly dependencies: ReadonlyArray<ElementTree<v>>;
}

export type ElementType<t> =
  | { readonly tag: "vertex"; readonly value: VertexType<t> }
  | { readonly tag: "edge"; readonly value: EdgeType<t> };

export interface ElementTypeTree<t> {
  readonly self: ElementType<t>;
  readonly dependencies: ReadonlyArray<ElementTypeTree<t>>;
}

export interface Graph<v> {
  readonly vertices: ReadonlyMap<v, Vertex<v>>;
  readonly edges: ReadonlyMap<v, Edge<v>>;
}

export interface GraphSchema<t> {
  readonly vertices: ReadonlyMap<VertexLabel, VertexType<t>>;
  readonly edges: ReadonlyMap<EdgeLabel, EdgeType<t>>;
}

export type Label =
  | { readonly tag: "vertex"; readonly value: VertexLabel }
  | { readonly tag: "edge"; readonly value: EdgeLabel };

export interface LazyGraph<v> {
  readonly vertices: ReadonlyArray<Vertex<v>>;
  readonly edges: ReadonlyArray<Edge<v>>;
}

export interface Property<v> {
  readonly key: PropertyKey;
  readonly value: v;
}

export type PropertyKey = string & { readonly __brand: "PropertyKey" };

export interface PropertyType<t> {
  readonly key: PropertyKey;
  readonly value: t;
  readonly required: boolean;
}

export interface Vertex<v> {
  readonly label: VertexLabel;
  readonly id: v;
  readonly properties: ReadonlyMap<PropertyKey, v>;
}

export type VertexLabel = string & { readonly __brand: "VertexLabel" };

export interface VertexType<t> {
  readonly label: VertexLabel;
  readonly id: t;
  readonly properties: ReadonlyArray<PropertyType<t>>;
}

export interface VertexWithAdjacentEdges<v> {
  readonly vertex: Vertex<v>;
  readonly ins: ReadonlyArray<AdjacentEdge<v>>;
  readonly outs: ReadonlyArray<AdjacentEdge<v>>;
}
