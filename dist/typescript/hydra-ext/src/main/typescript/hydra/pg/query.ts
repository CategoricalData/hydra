// Note: this is an automatically generated file. Do not edit.

/**
 * A common model for pattern-matching queries over property graphs
 */



import * as Core from "../core.js";
import * as PgModel from "./model.js";

export type AggregationQuery =
  | { readonly tag: "count" };

export type ApplicationQuery = ReadonlyArray<Query> & { readonly __brand: "ApplicationQuery" };

export interface AssociativeExpression {
  readonly operator: BinaryOperator;
  readonly operands: ReadonlyArray<Expression>;
}

export interface BinaryExpression {
  readonly left: Expression;
  readonly operator: BinaryOperator;
  readonly right: Expression;
}

export type BinaryBooleanOperator =
  | { readonly tag: "and" }
  | { readonly tag: "or" }
  | { readonly tag: "xor" };

export type BinaryOperator =
  | { readonly tag: "boolean"; readonly value: BinaryBooleanOperator }
  | { readonly tag: "comparison"; readonly value: ComparisonOperator }
  | { readonly tag: "power" };

export interface Binding {
  readonly key: Variable;
  readonly value: Query;
}

export type ComparisonOperator =
  | { readonly tag: "eq" }
  | { readonly tag: "neq" }
  | { readonly tag: "lt" }
  | { readonly tag: "lte" }
  | { readonly tag: "gt" }
  | { readonly tag: "gte" };

export interface EdgeProjectionPattern {
  readonly direction: PgModel.Direction;
  readonly label: PgModel.EdgeLabel | null;
  readonly properties: ReadonlyArray<PropertyPattern>;
  readonly vertex: VertexPattern | null;
}

export type Expression =
  | { readonly tag: "associative"; readonly value: AssociativeExpression }
  | { readonly tag: "binary"; readonly value: BinaryExpression }
  | { readonly tag: "property"; readonly value: PropertyProjection }
  | { readonly tag: "unary"; readonly value: UnaryExpression }
  | { readonly tag: "variable"; readonly value: Variable }
  | { readonly tag: "vertex"; readonly value: VertexPattern };

export interface LetQuery {
  readonly bindings: ReadonlyArray<Binding>;
  readonly environment: Query;
}

export interface MatchQuery {
  readonly optional: boolean;
  readonly pattern: ReadonlyArray<Projection>;
  readonly where: Expression | null;
}

export interface Projection {
  readonly value: Expression;
  readonly as: Variable | null;
}

export interface Projections {
  readonly all: boolean;
  readonly explicit: ReadonlyArray<Projection>;
}

export interface PropertyPattern {
  readonly key: PgModel.PropertyKey;
  readonly value: PropertyValuePattern;
}

export interface PropertyProjection {
  readonly base: Expression;
  readonly key: PgModel.PropertyKey;
}

export type PropertyValue = string & { readonly __brand: "PropertyValue" };

export type PropertyValuePattern =
  | { readonly tag: "variable"; readonly value: PgModel.PropertyKey }
  | { readonly tag: "value"; readonly value: string };

export type Query =
  | { readonly tag: "application"; readonly value: ApplicationQuery }
  | { readonly tag: "aggregate"; readonly value: AggregationQuery }
  | { readonly tag: "letQuery"; readonly value: LetQuery }
  | { readonly tag: "match"; readonly value: MatchQuery }
  | { readonly tag: "select"; readonly value: SelectQuery }
  | { readonly tag: "value"; readonly value: string };

export interface SelectQuery {
  readonly distinct: boolean;
  readonly projection: Projections;
}

export interface UnaryExpression {
  readonly operator: UnaryOperator;
  readonly operand: Expression;
}

export type UnaryOperator =
  | { readonly tag: "negate" };

export type Variable = string & { readonly __brand: "Variable" };

export interface VertexPattern {
  readonly variable: Variable | null;
  readonly label: PgModel.VertexLabel | null;
  readonly properties: ReadonlyArray<PropertyPattern>;
  readonly edges: ReadonlyArray<EdgeProjectionPattern>;
}
