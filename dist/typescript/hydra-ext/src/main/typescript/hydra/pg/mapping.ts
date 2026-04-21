// Note: this is an automatically generated file. Do not edit.

/**
 * A model for property graph mapping specifications. See https://github.com/CategoricalData/hydra/wiki/Property-graphs
 */



import * as Coders from "../coders.js";
import * as Core from "../core.js";
import * as PgModel from "./model.js";

export interface AnnotationSchema {
  readonly vertexLabel: string;
  readonly edgeLabel: string;
  readonly vertexId: string;
  readonly edgeId: string;
  readonly propertyKey: string;
  readonly propertyValue: string;
  readonly outVertex: string;
  readonly outVertexLabel: string;
  readonly inVertex: string;
  readonly inVertexLabel: string;
  readonly outEdge: string;
  readonly outEdgeLabel: string;
  readonly inEdge: string;
  readonly inEdgeLabel: string;
  readonly ignore: string;
}

export interface EdgeSpec {
  readonly label: PgModel.EdgeLabel;
  readonly id: ValueSpec;
  readonly out: ValueSpec;
  readonly in: ValueSpec;
  readonly properties: ReadonlyArray<PropertySpec>;
}

export type ElementSpec =
  | { readonly tag: "vertex"; readonly value: VertexSpec }
  | { readonly tag: "edge"; readonly value: EdgeSpec };

export interface PropertySpec {
  readonly key: PgModel.PropertyKey;
  readonly value: ValueSpec;
}

export interface Schema<s, t, v> {
  readonly vertexIdTypes: Coders.Coder<Core.Type, t>;
  readonly vertexIds: Coders.Coder<Core.Term, v>;
  readonly edgeIdTypes: Coders.Coder<Core.Type, t>;
  readonly edgeIds: Coders.Coder<Core.Term, v>;
  readonly propertyTypes: Coders.Coder<Core.Type, t>;
  readonly propertyValues: Coders.Coder<Core.Term, v>;
  readonly annotations: AnnotationSchema;
  readonly defaultVertexId: v;
  readonly defaultEdgeId: v;
}

export type ValueSpec =
  | { readonly tag: "value" }
  | { readonly tag: "pattern"; readonly value: string };

export interface VertexSpec {
  readonly label: PgModel.VertexLabel;
  readonly id: ValueSpec;
  readonly properties: ReadonlyArray<PropertySpec>;
}
