// Note: this is an automatically generated file. Do not edit.

/**
 * A model derived from TinkerPop's Graph.Features. See
 *   https://tinkerpop.apache.org/javadocs/current/core/org/apache/tinkerpop/gremlin/structure/Graph.Features.html
 *
 * An interface that represents the capabilities of a Graph implementation.
 * By default all methods of features return true and it is up to implementers to disable feature they don't support.
 * Users should check features prior to using various functions of TinkerPop to help ensure code portability across implementations.
 * For example, a common usage would be to check if a graph supports transactions prior to calling the commit method on Graph.tx().
 */



import * as Core from "../core.js";

export interface DataTypeFeatures {
  readonly supportsBooleanArrayValues: boolean;
  readonly supportsBooleanValues: boolean;
  readonly supportsByteArrayValues: boolean;
  readonly supportsByteValues: boolean;
  readonly supportsDoubleArrayValues: boolean;
  readonly supportsDoubleValues: boolean;
  readonly supportsFloatArrayValues: boolean;
  readonly supportsFloatValues: boolean;
  readonly supportsIntegerArrayValues: boolean;
  readonly supportsIntegerValues: boolean;
  readonly supportsLongArrayValues: boolean;
  readonly supportsLongValues: boolean;
  readonly supportsMapValues: boolean;
  readonly supportsMixedListValues: boolean;
  readonly supportsSerializableValues: boolean;
  readonly supportsStringArrayValues: boolean;
  readonly supportsStringValues: boolean;
  readonly supportsUniformListValues: boolean;
}

export interface EdgeFeatures {
  readonly elementFeatures: ElementFeatures;
  readonly properties: EdgePropertyFeatures;
  readonly supportsAddEdges: boolean;
  readonly supportsRemoveEdges: boolean;
  readonly supportsUpsert: boolean;
}

export interface EdgePropertyFeatures {
  readonly propertyFeatures: PropertyFeatures;
}

export interface ElementFeatures {
  readonly supportsAddProperty: boolean;
  readonly supportsAnyIds: boolean;
  readonly supportsCustomIds: boolean;
  readonly supportsNumericIds: boolean;
  readonly supportsRemoveProperty: boolean;
  readonly supportsStringIds: boolean;
  readonly supportsUserSuppliedIds: boolean;
  readonly supportsUuidIds: boolean;
}

export interface ExtraFeatures<a> {
  readonly supportsMapKey: ((x: Core.Type) => boolean);
}

export interface Features {
  readonly edge: EdgeFeatures;
  readonly graph: GraphFeatures;
  readonly vertex: VertexFeatures;
}

export interface GraphFeatures {
  readonly supportsComputer: boolean;
  readonly supportsConcurrentAccess: boolean;
  readonly supportsIoRead: boolean;
  readonly supportsIoWrite: boolean;
  readonly supportsPersistence: boolean;
  readonly supportsThreadedTransactions: boolean;
  readonly supportsTransactions: boolean;
  readonly variables: VariableFeatures;
}

export interface PropertyFeatures {
  readonly dataTypeFeatures: DataTypeFeatures;
  readonly supportsProperties: boolean;
}

export interface VariableFeatures {
  readonly dataTypeFeatures: DataTypeFeatures;
  readonly supportsVariables: boolean;
}

export interface VertexFeatures {
  readonly elementFeatures: ElementFeatures;
  readonly properties: VertexPropertyFeatures;
  readonly supportsAddVertices: boolean;
  readonly supportsDuplicateMultiProperties: boolean;
  readonly supportsMetaProperties: boolean;
  readonly supportsMultiProperties: boolean;
  readonly supportsRemoveVertices: boolean;
  readonly supportsUpsert: boolean;
}

export interface VertexPropertyFeatures {
  readonly dataTypeFeatures: DataTypeFeatures;
  readonly propertyFeatures: PropertyFeatures;
  readonly elementFeatures: ElementFeatures;
  readonly supportsRemove: boolean;
}
