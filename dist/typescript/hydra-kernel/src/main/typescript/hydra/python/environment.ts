// Note: this is an automatically generated file. Do not edit.

/**
 * Environment types for Python code generation
 */



import * as Core from "../core.js";
import * as Graph from "../graph.js";
import * as Packaging from "../packaging.js";
import * as PythonSyntax from "./syntax.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";

export type PythonVersion =
  | { readonly tag: "python310" }
  | { readonly tag: "python312" };

export interface PythonEnvironment {
  readonly namespaces: Packaging.Namespaces<PythonSyntax.DottedName>;
  readonly boundTypeVariables: readonly [ReadonlyArray<Core.Name>, ReadonlyMap<Core.Name, PythonSyntax.Name>];
  readonly graph: Graph.Graph;
  readonly nullaryBindings: ReadonlySet<Core.Name>;
  readonly version: PythonVersion;
  readonly skipCasts: boolean;
  readonly inlineVariables: ReadonlySet<Core.Name>;
}

export interface PythonModuleMetadata {
  readonly namespaces: Packaging.Namespaces<PythonSyntax.DottedName>;
  readonly typeVariables: ReadonlySet<Core.Name>;
  readonly usesAnnotated: boolean;
  readonly usesCallable: boolean;
  readonly usesCast: boolean;
  readonly usesLruCache: boolean;
  readonly usesTypeAlias: boolean;
  readonly usesDataclass: boolean;
  readonly usesDecimal: boolean;
  readonly usesEither: boolean;
  readonly usesEnum: boolean;
  readonly usesFrozenDict: boolean;
  readonly usesFrozenList: boolean;
  readonly usesGeneric: boolean;
  readonly usesJust: boolean;
  readonly usesLeft: boolean;
  readonly usesMaybe: boolean;
  readonly usesName: boolean;
  readonly usesNode: boolean;
  readonly usesNothing: boolean;
  readonly usesRight: boolean;
  readonly usesTypeVar: boolean;
}

export interface PyGraph {
  readonly graph: Graph.Graph;
  readonly metadata: PythonModuleMetadata;
}
