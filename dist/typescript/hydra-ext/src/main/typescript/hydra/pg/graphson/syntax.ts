// Note: this is an automatically generated file. Do not edit.

/**
 * A syntax model for TinkerPop's GraphSON format. This model is designed to be as inclusive as possible, supporting GraphSON 4.0 as well as earlier versions. See https://github.com/apache/tinkerpop/blob/master/docs/src/dev/io/graphson.asciidoc.
 */



import * as Core from "../../core.js";

export type BigDecimalValue = string & { readonly __brand: "BigDecimalValue" };

export interface CompositeTypedValue {
  readonly type: TypeName;
  readonly fields: Map;
}

export type DateTime = string & { readonly __brand: "DateTime" };

export type DoubleValue =
  | { readonly tag: "finite"; readonly value: number }
  | { readonly tag: "infinity" }
  | { readonly tag: "negativeInfinity" }
  | { readonly tag: "notANumber" };

export type Duration = string & { readonly __brand: "Duration" };

export type EdgeLabel = string & { readonly __brand: "EdgeLabel" };

export type FloatValue =
  | { readonly tag: "finite"; readonly value: number }
  | { readonly tag: "infinity" }
  | { readonly tag: "negativeInfinity" }
  | { readonly tag: "notANumber" };

export type Map = ReadonlyArray<ValuePair> & { readonly __brand: "Map" };

export interface AdjacentEdge {
  readonly id: Value;
  readonly vertexId: Value;
  readonly properties: ReadonlyMap<PropertyKey, Value>;
}

export interface PrimitiveTypedValue {
  readonly type: TypeName;
  readonly value: string;
}

export type PropertyKey = string & { readonly __brand: "PropertyKey" };

export type TypeName = string & { readonly __brand: "TypeName" };

export type Uuid = string & { readonly __brand: "Uuid" };

export type Value =
  | { readonly tag: "bigDecimal"; readonly value: BigDecimalValue }
  | { readonly tag: "bigInteger"; readonly value: bigint }
  | { readonly tag: "binary"; readonly value: string }
  | { readonly tag: "boolean"; readonly value: boolean }
  | { readonly tag: "byte"; readonly value: bigint }
  | { readonly tag: "char"; readonly value: bigint }
  | { readonly tag: "composite"; readonly value: CompositeTypedValue }
  | { readonly tag: "dateTime"; readonly value: DateTime }
  | { readonly tag: "double"; readonly value: DoubleValue }
  | { readonly tag: "duration"; readonly value: Duration }
  | { readonly tag: "float"; readonly value: FloatValue }
  | { readonly tag: "integer"; readonly value: number }
  | { readonly tag: "list"; readonly value: ReadonlyArray<Value> }
  | { readonly tag: "long"; readonly value: bigint }
  | { readonly tag: "map"; readonly value: Map }
  | { readonly tag: "null" }
  | { readonly tag: "primitive"; readonly value: PrimitiveTypedValue }
  | { readonly tag: "set"; readonly value: ReadonlyArray<Value> }
  | { readonly tag: "short"; readonly value: bigint }
  | { readonly tag: "string"; readonly value: string }
  | { readonly tag: "uuid"; readonly value: Uuid };

export interface ValuePair {
  readonly first: Value;
  readonly second: Value;
}

export interface Vertex {
  readonly id: Value;
  readonly label: VertexLabel | null;
  readonly inEdges: ReadonlyMap<EdgeLabel, ReadonlyArray<AdjacentEdge>>;
  readonly outEdges: ReadonlyMap<EdgeLabel, ReadonlyArray<AdjacentEdge>>;
  readonly properties: ReadonlyMap<PropertyKey, ReadonlyArray<VertexPropertyValue>>;
}

export type VertexLabel = string & { readonly __brand: "VertexLabel" };

export interface VertexPropertyValue {
  readonly id: Value | null;
  readonly value: Value;
}
