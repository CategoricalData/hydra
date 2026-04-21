// Note: this is an automatically generated file. Do not edit.

/**
 * Abstractions for paired transformations between languages
 */



import * as Context from "./context.js";
import * as Core from "./core.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as Variants from "./variants.js";

export interface Adapter<t1, t2, v1, v2> {
  readonly isLossy: boolean;
  readonly source: t1;
  readonly target: t2;
  readonly coder: Coder<v1, v2>;
}

export interface AdapterContext {
  readonly graph: Graph.Graph;
  readonly language: Language;
  readonly adapters: ReadonlyMap<Core.Name, Adapter<Core.Type, Core.Type, Core.Term, Core.Term>>;
}

export interface Bicoder<t1, t2, v1, v2> {
  readonly encode: ((x: t1) => Adapter<t1, t2, v1, v2>);
  readonly decode: ((x: t2) => Adapter<t2, t1, v2, v1>);
}

export interface Coder<v1, v2> {
  readonly encode: ((x: Context.Context) => ((x: v1) => Errors.Error | v2));
  readonly decode: ((x: Context.Context) => ((x: v2) => Errors.Error | v1));
}

export type CoderDirection =
  | { readonly tag: "encode" }
  | { readonly tag: "decode" };

export interface Language {
  readonly name: LanguageName;
  readonly constraints: LanguageConstraints;
}

export interface LanguageConstraints {
  readonly eliminationVariants: ReadonlySet<Variants.EliminationVariant>;
  readonly literalVariants: ReadonlySet<Variants.LiteralVariant>;
  readonly floatTypes: ReadonlySet<Core.FloatType>;
  readonly functionVariants: ReadonlySet<Variants.FunctionVariant>;
  readonly integerTypes: ReadonlySet<Core.IntegerType>;
  readonly termVariants: ReadonlySet<Variants.TermVariant>;
  readonly typeVariants: ReadonlySet<Variants.TypeVariant>;
  readonly types: ((x: Core.Type) => boolean);
}

export type LanguageName = string & { readonly __brand: "LanguageName" };

export type SymmetricAdapter<t, v> = Adapter<t, t, v, v>;

export type TraversalOrder =
  | { readonly tag: "pre" }
  | { readonly tag: "post" };

export type TypeAdapter = ((x: AdapterContext) => ((x: Core.Type) => string | SymmetricAdapter<Core.Type, Core.Term>));
