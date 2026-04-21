// Note: this is an automatically generated file. Do not edit.

/**
 * Types supporting type inference and type reconstruction.
 */



import * as Context from "./context.js";
import * as Core from "./core.js";

export interface FunctionStructure<env> {
  readonly typeParams: ReadonlyArray<Core.Name>;
  readonly params: ReadonlyArray<Core.Name>;
  readonly bindings: ReadonlyArray<Core.Binding>;
  readonly body: Core.Term;
  readonly domains: ReadonlyArray<Core.Type>;
  readonly codomain: Core.Type | null;
  readonly environment: env;
}

export interface InferenceResult {
  readonly term: Core.Term;
  readonly type: Core.Type;
  readonly subst: TypeSubst;
  readonly classConstraints: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>;
  readonly context: Context.Context;
}

export type TermSubst = ReadonlyMap<Core.Name, Core.Term> & { readonly __brand: "TermSubst" };

export interface TypeConstraint {
  readonly left: Core.Type;
  readonly right: Core.Type;
  readonly comment: string;
}

export type TypeSubst = ReadonlyMap<Core.Name, Core.Type> & { readonly __brand: "TypeSubst" };
