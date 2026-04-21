// Note: this is an automatically generated file. Do not edit.

/**
 * A SHACL syntax model. See https://www.w3.org/TR/shacl
 */



import * as Core from "../core.js";
import * as RdfSyntax from "../rdf/syntax.js";

export interface Closed {
  readonly isClosed: boolean;
  readonly ignoredProperties: ReadonlySet<RdfSyntax.Property> | null;
}

export type CommonConstraint =
  | { readonly tag: "and"; readonly value: ReadonlySet<Reference<Shape>> }
  | { readonly tag: "closed"; readonly value: Closed }
  | { readonly tag: "class"; readonly value: ReadonlySet<RdfSyntax.RdfsClass> }
  | { readonly tag: "datatype"; readonly value: RdfSyntax.Iri }
  | { readonly tag: "disjoint"; readonly value: ReadonlySet<RdfSyntax.Property> }
  | { readonly tag: "equals"; readonly value: ReadonlySet<RdfSyntax.Property> }
  | { readonly tag: "hasValue"; readonly value: ReadonlySet<RdfSyntax.Node> }
  | { readonly tag: "in"; readonly value: ReadonlyArray<RdfSyntax.Node> }
  | { readonly tag: "languageIn"; readonly value: ReadonlySet<RdfSyntax.LanguageTag> }
  | { readonly tag: "nodeKind"; readonly value: NodeKind }
  | { readonly tag: "node"; readonly value: ReadonlySet<Reference<NodeShape>> }
  | { readonly tag: "not"; readonly value: ReadonlySet<Reference<Shape>> }
  | { readonly tag: "maxExclusive"; readonly value: RdfSyntax.Literal }
  | { readonly tag: "maxInclusive"; readonly value: RdfSyntax.Literal }
  | { readonly tag: "maxLength"; readonly value: bigint }
  | { readonly tag: "minExclusive"; readonly value: RdfSyntax.Literal }
  | { readonly tag: "minInclusive"; readonly value: RdfSyntax.Literal }
  | { readonly tag: "minLength"; readonly value: bigint }
  | { readonly tag: "pattern"; readonly value: Pattern }
  | { readonly tag: "property"; readonly value: ReadonlySet<Reference<PropertyShape>> }
  | { readonly tag: "or"; readonly value: ReadonlySet<Reference<Shape>> }
  | { readonly tag: "xone"; readonly value: ReadonlySet<Reference<Shape>> };

export interface CommonProperties {
  readonly constraints: ReadonlySet<CommonConstraint>;
  readonly deactivated: boolean | null;
  readonly message: RdfSyntax.LangStrings;
  readonly severity: Severity;
  readonly targetClass: ReadonlySet<RdfSyntax.RdfsClass>;
  readonly targetNode: ReadonlySet<RdfSyntax.IriOrLiteral>;
  readonly targetObjectsOf: ReadonlySet<RdfSyntax.Property>;
  readonly targetSubjectsOf: ReadonlySet<RdfSyntax.Property>;
}

export interface Definition<a> {
  readonly iri: RdfSyntax.Iri;
  readonly target: a;
}

export type NodeKind =
  | { readonly tag: "blankNode" }
  | { readonly tag: "iri" }
  | { readonly tag: "literal" }
  | { readonly tag: "blankNodeOrIri" }
  | { readonly tag: "blankNodeOrLiteral" }
  | { readonly tag: "iriOrLiteral" };

export interface NodeShape {
  readonly common: CommonProperties;
}

export interface Pattern {
  readonly regex: string;
  readonly flags: string | null;
}

export interface PropertyShape {
  readonly common: CommonProperties;
  readonly constraints: ReadonlySet<PropertyShapeConstraint>;
  readonly defaultValue: RdfSyntax.Node | null;
  readonly description: RdfSyntax.LangStrings;
  readonly name: RdfSyntax.LangStrings;
  readonly order: bigint | null;
  readonly path: RdfSyntax.Iri;
}

export type PropertyShapeConstraint =
  | { readonly tag: "lessThan"; readonly value: ReadonlySet<RdfSyntax.Property> }
  | { readonly tag: "lessThanOrEquals"; readonly value: ReadonlySet<RdfSyntax.Property> }
  | { readonly tag: "maxCount"; readonly value: bigint }
  | { readonly tag: "minCount"; readonly value: bigint }
  | { readonly tag: "uniqueLang"; readonly value: boolean }
  | { readonly tag: "qualifiedValueShape"; readonly value: QualifiedValueShape };

export interface QualifiedValueShape {
  readonly qualifiedValueShape: Reference<Shape>;
  readonly qualifiedMaxCount: bigint;
  readonly qualifiedMinCount: bigint;
  readonly qualifiedValueShapesDisjoint: boolean | null;
}

export type Reference<a> =
  | { readonly tag: "named"; readonly value: RdfSyntax.Iri }
  | { readonly tag: "anonymous"; readonly value: a }
  | { readonly tag: "definition"; readonly value: Definition<a> };

export type Severity =
  | { readonly tag: "info" }
  | { readonly tag: "warning" }
  | { readonly tag: "violation" };

export type Shape =
  | { readonly tag: "node"; readonly value: NodeShape }
  | { readonly tag: "property"; readonly value: PropertyShape };

export type ShapesGraph = ReadonlySet<Definition<Shape>> & { readonly __brand: "ShapesGraph" };
