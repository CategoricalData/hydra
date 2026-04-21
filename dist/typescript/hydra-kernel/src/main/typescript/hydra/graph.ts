// Note: this is an automatically generated file. Do not edit.

/**
 * The extension to graphs of Hydra's core type system (hydra.core)
 */



import * as Context from "./context.js";
import * as Core from "./core.js";
import * as Errors from "./errors.js";

export interface Graph {
  readonly boundTerms: ReadonlyMap<Core.Name, Core.Term>;
  readonly boundTypes: ReadonlyMap<Core.Name, Core.TypeScheme>;
  readonly classConstraints: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>;
  readonly lambdaVariables: ReadonlySet<Core.Name>;
  readonly metadata: ReadonlyMap<Core.Name, Core.Term>;
  readonly primitives: ReadonlyMap<Core.Name, Primitive>;
  readonly schemaTypes: ReadonlyMap<Core.Name, Core.TypeScheme>;
  readonly typeVariables: ReadonlySet<Core.Name>;
}

export interface Primitive {
  readonly name: Core.Name;
  readonly type: Core.TypeScheme;
  readonly implementation: ((x: Context.Context) => ((x: Graph) => ((x: ReadonlyArray<Core.Term>) => Errors.Error | Core.Term)));
}

export interface TermCoder<a> {
  readonly type: Core.Type;
  readonly encode: ((x: Context.Context) => ((x: Graph) => ((x: Core.Term) => Errors.Error | a)));
  readonly decode: ((x: Context.Context) => ((x: a) => Errors.Error | Core.Term));
}
