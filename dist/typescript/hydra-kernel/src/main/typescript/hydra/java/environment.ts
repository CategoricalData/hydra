// Note: this is an automatically generated file. Do not edit.

/**
 * Environment types for Java code generation
 */



import * as Core from "../core.js";
import * as Graph from "../graph.js";
import * as JavaSyntax from "./syntax.js";
import * as Packaging from "../packaging.js";
import * as Typing from "../typing.js";

export type JavaSymbolClass =
  | { readonly tag: "constant" }
  | { readonly tag: "nullaryFunction" }
  | { readonly tag: "hoistedLambda"; readonly value: number }
  | { readonly tag: "unaryFunction" }
  | { readonly tag: "localVariable" };

export interface JavaFeatures {
  readonly supportsDiamondOperator: boolean;
}

export interface Aliases {
  readonly currentNamespace: Packaging.Namespace;
  readonly packages: ReadonlyMap<Packaging.Namespace, JavaSyntax.PackageName>;
  readonly branchVars: ReadonlySet<Core.Name>;
  readonly recursiveVars: ReadonlySet<Core.Name>;
  readonly inScopeTypeParams: ReadonlySet<Core.Name>;
  readonly polymorphicLocals: ReadonlySet<Core.Name>;
  readonly inScopeJavaVars: ReadonlySet<Core.Name>;
  readonly varRenames: ReadonlyMap<Core.Name, Core.Name>;
  readonly lambdaVars: ReadonlySet<Core.Name>;
  readonly typeVarSubst: ReadonlyMap<Core.Name, Core.Name>;
  readonly trustedTypeVars: ReadonlySet<Core.Name>;
  readonly methodCodomain: Core.Type | null;
  readonly thunkedVars: ReadonlySet<Core.Name>;
}

export interface JavaEnvironment {
  readonly aliases: Aliases;
  readonly graph: Graph.Graph;
}
