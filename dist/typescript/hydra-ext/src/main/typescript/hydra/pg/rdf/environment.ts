// Note: this is an automatically generated file. Do not edit.

/**
 * Environment types for property graph to RDF mapping
 */



import * as Core from "../../core.js";
import * as PgModel from "../model.js";
import * as RdfSyntax from "../../rdf/syntax.js";

export interface PgRdfEnvironment<v> {
  readonly encodeVertexId: ((x: v) => RdfSyntax.Iri);
  readonly encodeVertexLabel: ((x: PgModel.VertexLabel) => RdfSyntax.Iri);
  readonly encodeEdgeId: ((x: v) => RdfSyntax.Iri);
  readonly encodeEdgeLabel: ((x: PgModel.EdgeLabel) => RdfSyntax.Iri);
  readonly encodePropertyKey: ((x: PgModel.PropertyKey) => RdfSyntax.Iri);
  readonly encodePropertyValue: ((x: v) => RdfSyntax.Literal);
}
