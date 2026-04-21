// Note: this is an automatically generated file. Do not edit.

/**
 * A model for language-agnostic graph pattern queries
 */



import * as Core from "./core.js";

export type ComparisonConstraint =
  | { readonly tag: "equal" }
  | { readonly tag: "notEqual" }
  | { readonly tag: "lessThan" }
  | { readonly tag: "greaterThan" }
  | { readonly tag: "lessThanOrEqual" }
  | { readonly tag: "greaterThanOrEqual" };

export interface Edge {
  readonly type: Core.Name;
  readonly out: Core.Name | null;
  readonly in: Core.Name | null;
}

export interface GraphPattern {
  readonly graph: Core.Name;
  readonly patterns: ReadonlyArray<Pattern>;
}

export type Node =
  | { readonly tag: "term"; readonly value: Core.Term }
  | { readonly tag: "variable"; readonly value: Variable }
  | { readonly tag: "wildcard" };

export type Path =
  | { readonly tag: "step"; readonly value: Step }
  | { readonly tag: "regex"; readonly value: RegexSequence }
  | { readonly tag: "inverse"; readonly value: Path };

export interface PathEquation {
  readonly left: Path;
  readonly right: Path;
}

export type Pattern =
  | { readonly tag: "triple"; readonly value: TriplePattern }
  | { readonly tag: "negation"; readonly value: Pattern }
  | { readonly tag: "conjunction"; readonly value: ReadonlyArray<Pattern> }
  | { readonly tag: "disjunction"; readonly value: ReadonlyArray<Pattern> }
  | { readonly tag: "graph"; readonly value: GraphPattern };

export interface PatternImplication {
  readonly antecedent: Pattern;
  readonly consequent: Pattern;
}

export interface Query {
  readonly variables: ReadonlyArray<Variable>;
  readonly patterns: ReadonlyArray<Pattern>;
}

export interface Range {
  readonly min: number;
  readonly max: number;
}

export type RegexQuantifier =
  | { readonly tag: "one" }
  | { readonly tag: "zeroOrOne" }
  | { readonly tag: "zeroOrMore" }
  | { readonly tag: "oneOrMore" }
  | { readonly tag: "exactly"; readonly value: number }
  | { readonly tag: "atLeast"; readonly value: number }
  | { readonly tag: "range"; readonly value: Range };

export interface RegexSequence {
  readonly path: Path;
  readonly quantifier: RegexQuantifier;
}

export type Step =
  | { readonly tag: "edge"; readonly value: Edge }
  | { readonly tag: "project"; readonly value: Core.Projection }
  | { readonly tag: "compare"; readonly value: ComparisonConstraint };

export interface TriplePattern {
  readonly subject: Node;
  readonly predicate: Path;
  readonly object: Node;
}

export type Variable = string & { readonly __brand: "Variable" };
