// Note: this is an automatically generated file. Do not edit.

/**
 * A model for subterm and subtype access patterns
 */



import * as Core from "./core.js";

export interface SubtermEdge {
  readonly source: SubtermNode;
  readonly path: SubtermPath;
  readonly target: SubtermNode;
}

export interface SubtermGraph {
  readonly nodes: ReadonlyArray<SubtermNode>;
  readonly edges: ReadonlyArray<SubtermEdge>;
}

export interface SubtermNode {
  readonly name: Core.Name;
  readonly label: string;
  readonly id: string;
}

export type SubtermPath = ReadonlyArray<SubtermStep> & { readonly __brand: "SubtermPath" };

export type SubtermStep =
  | { readonly tag: "annotatedBody" }
  | { readonly tag: "applicationFunction" }
  | { readonly tag: "applicationArgument" }
  | { readonly tag: "lambdaBody" }
  | { readonly tag: "unionCasesDefault" }
  | { readonly tag: "unionCasesBranch"; readonly value: Core.Name }
  | { readonly tag: "letBody" }
  | { readonly tag: "letBinding"; readonly value: Core.Name }
  | { readonly tag: "listElement"; readonly value: number }
  | { readonly tag: "mapKey"; readonly value: number }
  | { readonly tag: "mapValue"; readonly value: number }
  | { readonly tag: "maybeTerm" }
  | { readonly tag: "productTerm"; readonly value: number }
  | { readonly tag: "recordField"; readonly value: Core.Name }
  | { readonly tag: "setElement"; readonly value: number }
  | { readonly tag: "sumTerm" }
  | { readonly tag: "typeLambdaBody" }
  | { readonly tag: "typeApplicationTerm" }
  | { readonly tag: "injectionTerm" }
  | { readonly tag: "wrappedTerm" };

export interface SubtypeEdge {
  readonly source: SubtypeNode;
  readonly path: SubtypePath;
  readonly target: SubtypeNode;
}

export interface SubtypeGraph {
  readonly nodes: ReadonlyArray<SubtypeNode>;
  readonly edges: ReadonlyArray<SubtypeEdge>;
}

export interface SubtypeNode {
  readonly name: Core.Name;
  readonly label: string;
  readonly id: string;
}

export type SubtypePath = ReadonlyArray<SubtypeStep> & { readonly __brand: "SubtypePath" };

export type SubtypeStep =
  | { readonly tag: "annotatedBody" }
  | { readonly tag: "applicationFunction" }
  | { readonly tag: "applicationArgument" }
  | { readonly tag: "eitherLeft" }
  | { readonly tag: "eitherRight" }
  | { readonly tag: "forallBody" }
  | { readonly tag: "functionDomain" }
  | { readonly tag: "functionCodomain" }
  | { readonly tag: "listElement" }
  | { readonly tag: "mapKeys" }
  | { readonly tag: "mapValues" }
  | { readonly tag: "maybeElement" }
  | { readonly tag: "pairFirst" }
  | { readonly tag: "pairSecond" }
  | { readonly tag: "recordField"; readonly value: Core.Name }
  | { readonly tag: "setElement" }
  | { readonly tag: "unionField"; readonly value: Core.Name }
  | { readonly tag: "wrappedType" };
