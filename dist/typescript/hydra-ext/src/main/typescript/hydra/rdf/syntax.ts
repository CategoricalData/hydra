// Note: this is an automatically generated file. Do not edit.

/**
 * An RDF 1.1 syntax model
 */



import * as Core from "../core.js";

export type BlankNode = string & { readonly __brand: "BlankNode" };

export type RdfsClass = void & { readonly __brand: "RdfsClass" };

export type Dataset = ReadonlySet<Quad> & { readonly __brand: "Dataset" };

export interface Description {
  readonly subject: Node;
  readonly graph: Graph;
}

export type Graph = ReadonlySet<Triple> & { readonly __brand: "Graph" };

export type Iri = string & { readonly __brand: "Iri" };

export type IriOrLiteral =
  | { readonly tag: "iri"; readonly value: Iri }
  | { readonly tag: "literal"; readonly value: Literal };

export type LangStrings = ReadonlyMap<LanguageTag | null, string> & { readonly __brand: "LangStrings" };

export type LanguageTag = string & { readonly __brand: "LanguageTag" };

export interface Literal {
  readonly lexicalForm: string;
  readonly datatypeIri: Iri;
  readonly languageTag: LanguageTag | null;
}

export type Node =
  | { readonly tag: "iri"; readonly value: Iri }
  | { readonly tag: "bnode"; readonly value: BlankNode }
  | { readonly tag: "literal"; readonly value: Literal };

export interface Property {
  readonly domain: ReadonlySet<RdfsClass>;
  readonly range: ReadonlySet<RdfsClass>;
  readonly subPropertyOf: ReadonlySet<Property>;
}

export interface Quad {
  readonly subject: Resource;
  readonly predicate: Iri;
  readonly object: Node;
  readonly graph: Iri | null;
}

export type Resource =
  | { readonly tag: "iri"; readonly value: Iri }
  | { readonly tag: "bnode"; readonly value: BlankNode };

export interface Triple {
  readonly subject: Resource;
  readonly predicate: Iri;
  readonly object: Node;
}
