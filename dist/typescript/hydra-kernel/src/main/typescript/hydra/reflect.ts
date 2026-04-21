// Note: this is an automatically generated file. Do not edit.

/**
 * Reflection functions for working with term, type, and literal type variants, as well as numeric precision.
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as LibLists from "./lib/lists.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export const eliminationVariants: ReadonlyArray<Variants.EliminationVariant> = [({ tag: "record" }), ({ tag: "union" }), ({ tag: "wrap" })];

export function floatTypePrecision(v1: Core.FloatType): Util.Precision {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => ({ tag: "arbitrary" }))((_m as any).value);
    case "float32": return ((_: void) => ({ tag: "bits", value: 32 }))((_m as any).value);
    case "float64": return ((_: void) => ({ tag: "bits", value: 64 }))((_m as any).value);
  }
})();
}

export const floatTypes: ReadonlyArray<Core.FloatType> = [({ tag: "bigfloat" }), ({ tag: "float32" }), ({ tag: "float64" })];

export function floatValueType(v1: Core.FloatValue): Core.FloatType {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigfloat": return ((_: number) => ({ tag: "bigfloat" }))((_m as any).value);
    case "float32": return ((_: number) => ({ tag: "float32" }))((_m as any).value);
    case "float64": return ((_: number) => ({ tag: "float64" }))((_m as any).value);
  }
})();
}

export const functionVariants: ReadonlyArray<Variants.FunctionVariant> = [({ tag: "elimination" }), ({ tag: "lambda" })];

export function integerTypeIsSigned(v1: Core.IntegerType): boolean {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigint": return ((_: void) => true)((_m as any).value);
    case "int8": return ((_: void) => true)((_m as any).value);
    case "int16": return ((_: void) => true)((_m as any).value);
    case "int32": return ((_: void) => true)((_m as any).value);
    case "int64": return ((_: void) => true)((_m as any).value);
    case "uint8": return ((_: void) => false)((_m as any).value);
    case "uint16": return ((_: void) => false)((_m as any).value);
    case "uint32": return ((_: void) => false)((_m as any).value);
    case "uint64": return ((_: void) => false)((_m as any).value);
  }
})();
}

export function integerTypePrecision(v1: Core.IntegerType): Util.Precision {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigint": return ((_: void) => ({ tag: "arbitrary" }))((_m as any).value);
    case "int8": return ((_: void) => ({ tag: "bits", value: 8 }))((_m as any).value);
    case "int16": return ((_: void) => ({ tag: "bits", value: 16 }))((_m as any).value);
    case "int32": return ((_: void) => ({ tag: "bits", value: 32 }))((_m as any).value);
    case "int64": return ((_: void) => ({ tag: "bits", value: 64 }))((_m as any).value);
    case "uint8": return ((_: void) => ({ tag: "bits", value: 8 }))((_m as any).value);
    case "uint16": return ((_: void) => ({ tag: "bits", value: 16 }))((_m as any).value);
    case "uint32": return ((_: void) => ({ tag: "bits", value: 32 }))((_m as any).value);
    case "uint64": return ((_: void) => ({ tag: "bits", value: 64 }))((_m as any).value);
  }
})();
}

export const integerTypes: ReadonlyArray<Core.IntegerType> = [({ tag: "bigint" }), ({ tag: "int8" }), ({ tag: "int16" }), ({ tag: "int32" }), ({ tag: "int64" }), ({ tag: "uint8" }), ({ tag: "uint16" }), ({ tag: "uint32" }), ({ tag: "uint64" })];

export function integerValueType(v1: Core.IntegerValue): Core.IntegerType {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigint": return ((_: bigint) => ({ tag: "bigint" }))((_m as any).value);
    case "int8": return ((_: number) => ({ tag: "int8" }))((_m as any).value);
    case "int16": return ((_: bigint) => ({ tag: "int16" }))((_m as any).value);
    case "int32": return ((_: number) => ({ tag: "int32" }))((_m as any).value);
    case "int64": return ((_: bigint) => ({ tag: "int64" }))((_m as any).value);
    case "uint8": return ((_: bigint) => ({ tag: "uint8" }))((_m as any).value);
    case "uint16": return ((_: number) => ({ tag: "uint16" }))((_m as any).value);
    case "uint32": return ((_: bigint) => ({ tag: "uint32" }))((_m as any).value);
    case "uint64": return ((_: bigint) => ({ tag: "uint64" }))((_m as any).value);
  }
})();
}

export function literalType(v1: Core.Literal): Core.LiteralType {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "binary": return ((_: Uint8Array) => ({ tag: "binary" }))((_m as any).value);
    case "boolean": return ((_: boolean) => ({ tag: "boolean" }))((_m as any).value);
    case "float": return ((arg_: Core.FloatValue) => ({ tag: "float", value: floatValueType(arg_) }))((_m as any).value);
    case "integer": return ((arg_: Core.IntegerValue) => ({ tag: "integer", value: integerValueType(arg_) }))((_m as any).value);
    case "string": return ((_: string) => ({ tag: "string" }))((_m as any).value);
  }
})();
}

export function literalTypeVariant(v1: Core.LiteralType): Variants.LiteralVariant {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "binary": return ((_: void) => ({ tag: "binary" }))((_m as any).value);
    case "boolean": return ((_: void) => ({ tag: "boolean" }))((_m as any).value);
    case "float": return ((_: Core.FloatType) => ({ tag: "float" }))((_m as any).value);
    case "integer": return ((_: Core.IntegerType) => ({ tag: "integer" }))((_m as any).value);
    case "string": return ((_: void) => ({ tag: "string" }))((_m as any).value);
  }
})();
}

export const literalTypes: ReadonlyArray<Core.LiteralType> = LibLists.concat([[({ tag: "binary" }), ({ tag: "boolean" })], LibLists.map(((x: Core.FloatType) => ({ tag: "float", value: x })))(floatTypes), LibLists.map(((x: Core.IntegerType) => ({ tag: "integer", value: x })))(integerTypes), [({ tag: "string" })]]);

export function literalVariant(arg_: Core.Literal): Variants.LiteralVariant {
  return literalTypeVariant(literalType(arg_));
}

export const literalVariants: ReadonlyArray<Variants.LiteralVariant> = [({ tag: "binary" }), ({ tag: "boolean" }), ({ tag: "float" }), ({ tag: "integer" }), ({ tag: "string" })];

export function termVariant(v1: Core.Term): Variants.TermVariant {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((_: Core.AnnotatedTerm) => ({ tag: "annotated" }))((_m as any).value);
    case "application": return ((_: Core.Application) => ({ tag: "application" }))((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => ({ tag: "cases" }))((_m as any).value);
    case "either": return ((_: Core.Term | Core.Term) => ({ tag: "either" }))((_m as any).value);
    case "lambda": return ((_: Core.Lambda) => ({ tag: "lambda" }))((_m as any).value);
    case "let": return ((_: Core.Let) => ({ tag: "let" }))((_m as any).value);
    case "list": return ((_: ReadonlyArray<Core.Term>) => ({ tag: "list" }))((_m as any).value);
    case "literal": return ((_: Core.Literal) => ({ tag: "literal" }))((_m as any).value);
    case "map": return ((_: ReadonlyMap<Core.Term, Core.Term>) => ({ tag: "map" }))((_m as any).value);
    case "maybe": return ((_: Core.Term | null) => ({ tag: "maybe" }))((_m as any).value);
    case "pair": return ((_: readonly [Core.Term, Core.Term]) => ({ tag: "pair" }))((_m as any).value);
    case "project": return ((_: Core.Projection) => ({ tag: "project" }))((_m as any).value);
    case "record": return ((_: Core.Record) => ({ tag: "record" }))((_m as any).value);
    case "set": return ((_: ReadonlySet<Core.Term>) => ({ tag: "set" }))((_m as any).value);
    case "typeApplication": return ((_: Core.TypeApplicationTerm) => ({ tag: "typeApplication" }))((_m as any).value);
    case "typeLambda": return ((_: Core.TypeLambda) => ({ tag: "typeLambda" }))((_m as any).value);
    case "inject": return ((_: Core.Injection) => ({ tag: "inject" }))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "unit" }))((_m as any).value);
    case "unwrap": return ((_: Core.Name) => ({ tag: "unwrap" }))((_m as any).value);
    case "variable": return ((_: Core.Name) => ({ tag: "variable" }))((_m as any).value);
    case "wrap": return ((_: Core.WrappedTerm) => ({ tag: "wrap" }))((_m as any).value);
  }
})();
}

export const termVariants: ReadonlyArray<Variants.TermVariant> = [({ tag: "annotated" }), ({ tag: "application" }), ({ tag: "cases" }), ({ tag: "either" }), ({ tag: "lambda" }), ({ tag: "let" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "project" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "typeLambda" }), ({ tag: "typeApplication" }), ({ tag: "inject" }), ({ tag: "unit" }), ({ tag: "unwrap" }), ({ tag: "variable" }), ({ tag: "wrap" })];

export function typeVariant(v1: Core.Type): Variants.TypeVariant {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((_: Core.AnnotatedType) => ({ tag: "annotated" }))((_m as any).value);
    case "application": return ((_: Core.ApplicationType) => ({ tag: "application" }))((_m as any).value);
    case "either": return ((_: Core.EitherType) => ({ tag: "either" }))((_m as any).value);
    case "function": return ((_: Core.FunctionType) => ({ tag: "function" }))((_m as any).value);
    case "forall": return ((_: Core.ForallType) => ({ tag: "forall" }))((_m as any).value);
    case "list": return ((_: Core.Type) => ({ tag: "list" }))((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => ({ tag: "literal" }))((_m as any).value);
    case "map": return ((_: Core.MapType) => ({ tag: "map" }))((_m as any).value);
    case "maybe": return ((_: Core.Type) => ({ tag: "maybe" }))((_m as any).value);
    case "pair": return ((_: Core.PairType) => ({ tag: "pair" }))((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "record" }))((_m as any).value);
    case "set": return ((_: Core.Type) => ({ tag: "set" }))((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "union" }))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "unit" }))((_m as any).value);
    case "variable": return ((_: Core.Name) => ({ tag: "variable" }))((_m as any).value);
    case "void": return ((_: void) => ({ tag: "void" }))((_m as any).value);
    case "wrap": return ((_: Core.Type) => ({ tag: "wrap" }))((_m as any).value);
  }
})();
}

export const typeVariants: ReadonlyArray<Variants.TypeVariant> = [({ tag: "annotated" }), ({ tag: "application" }), ({ tag: "either" }), ({ tag: "function" }), ({ tag: "forall" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "wrap" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "union" }), ({ tag: "unit" }), ({ tag: "variable" }), ({ tag: "void" })];
