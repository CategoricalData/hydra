// Note: this is an automatically generated file. Do not edit.

/**
 * Phantom types for use with Hydra DSLs
 */



import * as Core from "./core.js";

export interface TBinding<a> {
  readonly name: Core.Name;
  readonly term: TTerm<a>;
}

export type TTerm<a> = Core.Term & { readonly __brand: "TTerm" };

export interface TTermDefinition<a> {
  readonly name: Core.Name;
  readonly term: TTerm<a>;
}
