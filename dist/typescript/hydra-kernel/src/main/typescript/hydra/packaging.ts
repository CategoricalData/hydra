// Note: this is an automatically generated file. Do not edit.

/**
 * A model for Hydra namespaces, modules, and packages
 */



import * as Core from "./core.js";
import * as Graph from "./graph.js";

export type Definition =
  | { readonly tag: "term"; readonly value: TermDefinition }
  | { readonly tag: "type"; readonly value: TypeDefinition };

export type FileExtension = string & { readonly __brand: "FileExtension" };

export interface Library {
  readonly namespace: Namespace;
  readonly prefix: string;
  readonly primitives: ReadonlyArray<Graph.Primitive>;
}

export interface Module {
  readonly namespace: Namespace;
  readonly definitions: ReadonlyArray<Definition>;
  readonly termDependencies: ReadonlyArray<Namespace>;
  readonly typeDependencies: ReadonlyArray<Namespace>;
  readonly description: string | null;
}

export type Namespace = string & { readonly __brand: "Namespace" };

export interface Namespaces<n> {
  readonly focus: readonly [Namespace, n];
  readonly mapping: ReadonlyMap<Namespace, n>;
}

export interface Package {
  readonly name: PackageName;
  readonly modules: ReadonlyArray<Module>;
  readonly dependencies: ReadonlyArray<PackageName>;
  readonly description: string | null;
}

export type PackageName = string & { readonly __brand: "PackageName" };

export interface QualifiedName {
  readonly namespace: Namespace | null;
  readonly local: string;
}

export interface TermDefinition {
  readonly name: Core.Name;
  readonly term: Core.Term;
  readonly type: Core.TypeScheme | null;
}

export interface TypeDefinition {
  readonly name: Core.Name;
  readonly type: Core.TypeScheme;
}
