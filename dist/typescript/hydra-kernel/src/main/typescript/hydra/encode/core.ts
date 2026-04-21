// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.core
 */



import * as Core from "../core.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibLists from "../lib/lists.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";

export function annotatedTerm(x: Core.AnnotatedTerm): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedTerm",
    fields: [({
    name: "body",
    term: term(((_x) => _x.body)(x))
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.bimap(name)(term)(((_x) => _x.annotation)(x)) })
  })]
  }) });
}

export function annotatedType(x: Core.AnnotatedType): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.AnnotatedType",
    fields: [({
    name: "body",
    term: type(((_x) => _x.body)(x))
  }), ({
    name: "annotation",
    term: ({ tag: "map", value: LibMaps.bimap(name)(term)(((_x) => _x.annotation)(x)) })
  })]
  }) });
}

export function application(x: Core.Application): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.Application",
    fields: [({
    name: "function",
    term: term(((_x) => _x.function)(x))
  }), ({
    name: "argument",
    term: term(((_x) => _x.argument)(x))
  })]
  }) });
}

export function applicationType(x: Core.ApplicationType): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.ApplicationType",
    fields: [({
    name: "function",
    term: type(((_x) => _x.function)(x))
  }), ({
    name: "argument",
    term: type(((_x) => _x.argument)(x))
  })]
  }) });
}

export function binding(x: Core.Binding): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.Binding",
    fields: [({
    name: "name",
    term: name(((_x) => _x.name)(x))
  }), ({
    name: "term",
    term: term(((_x) => _x.term)(x))
  }), ({
    name: "type",
    term: ({ tag: "maybe", value: LibMaybes.map(typeScheme)(((_x) => _x.type)(x)) })
  })]
  }) });
}

export function caseStatement(x: Core.CaseStatement): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.CaseStatement",
    fields: [({
    name: "typeName",
    term: name(((_x) => _x.typeName)(x))
  }), ({
    name: "default",
    term: ({ tag: "maybe", value: LibMaybes.map(term)(((_x) => _x.default)(x)) })
  }), ({
    name: "cases",
    term: ({ tag: "list", value: LibLists.map(field)(((_x) => _x.cases)(x)) })
  })]
  }) });
}

export function eitherType(x: Core.EitherType): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.EitherType",
    fields: [({
    name: "left",
    term: type(((_x) => _x.left)(x))
  }), ({
    name: "right",
    term: type(((_x) => _x.right)(x))
  })]
  }) });
}

export function field(x: Core.Field): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.Field",
    fields: [({
    name: "name",
    term: name(((_x) => _x.name)(x))
  }), ({
    name: "term",
    term: term(((_x) => _x.term)(x))
  })]
  }) });
}

export function fieldType(x: Core.FieldType): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.FieldType",
    fields: [({
    name: "name",
    term: name(((_x) => _x.name)(x))
  }), ({
    name: "type",
    term: type(((_x) => _x.type)(x))
  })]
  }) });
}

export function floatType(v1: Core.FloatType): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigfloat": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.FloatType",
    field: ({
    name: "bigfloat",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "float32": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.FloatType",
    field: ({
    name: "float32",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "float64": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.FloatType",
    field: ({
    name: "float64",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function floatValue(v1: Core.FloatValue): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigfloat": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.core.FloatValue",
    field: ({
    name: "bigfloat",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "float32": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.core.FloatValue",
    field: ({
    name: "float32",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "float64": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.core.FloatValue",
    field: ({
    name: "float64",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: y }) }) })
  })
  }) }))((_m as any).value);
  }
})();
}

export function forallType(x: Core.ForallType): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.ForallType",
    fields: [({
    name: "parameter",
    term: name(((_x) => _x.parameter)(x))
  }), ({
    name: "body",
    term: type(((_x) => _x.body)(x))
  })]
  }) });
}

export function functionType(x: Core.FunctionType): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.FunctionType",
    fields: [({
    name: "domain",
    term: type(((_x) => _x.domain)(x))
  }), ({
    name: "codomain",
    term: type(((_x) => _x.codomain)(x))
  })]
  }) });
}

export function injection(x: Core.Injection): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.Injection",
    fields: [({
    name: "typeName",
    term: name(((_x) => _x.typeName)(x))
  }), ({
    name: "field",
    term: field(((_x) => _x.field)(x))
  })]
  }) });
}

export function integerType(v1: Core.IntegerType): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigint": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerType",
    field: ({
    name: "bigint",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "int8": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerType",
    field: ({
    name: "int8",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "int16": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerType",
    field: ({
    name: "int16",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "int32": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerType",
    field: ({
    name: "int32",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "int64": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerType",
    field: ({
    name: "int64",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "uint8": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerType",
    field: ({
    name: "uint8",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "uint16": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerType",
    field: ({
    name: "uint16",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "uint32": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerType",
    field: ({
    name: "uint32",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "uint64": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerType",
    field: ({
    name: "uint64",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function integerValue(v1: Core.IntegerValue): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigint": return ((y: bigint) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "bigint",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "int8": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int8",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "int16": return ((y: bigint) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int16",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "int32": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "int64": return ((y: bigint) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "int64",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "uint8": return ((y: bigint) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "uint8",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "uint16": return ((y: number) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "uint16",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "uint32": return ((y: bigint) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "uint32",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: y }) }) })
  })
  }) }))((_m as any).value);
    case "uint64": return ((y: bigint) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: "uint64",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64", value: y }) }) })
  })
  }) }))((_m as any).value);
  }
})();
}

export function lambda(x: Core.Lambda): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.Lambda",
    fields: [({
    name: "parameter",
    term: name(((_x) => _x.parameter)(x))
  }), ({
    name: "domain",
    term: ({ tag: "maybe", value: LibMaybes.map(type)(((_x) => _x.domain)(x)) })
  }), ({
    name: "body",
    term: term(((_x) => _x.body)(x))
  })]
  }) });
}

export function let_(x: Core.Let): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.Let",
    fields: [({
    name: "bindings",
    term: ({ tag: "list", value: LibLists.map(binding)(((_x) => _x.bindings)(x)) })
  }), ({
    name: "body",
    term: term(((_x) => _x.body)(x))
  })]
  }) });
}

export function literal(v1: Core.Literal): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "binary": return ((y: Uint8Array) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "binary",
    term: ({ tag: "literal", value: ({ tag: "binary", value: y }) })
  })
  }) }))((_m as any).value);
    case "boolean": return ((y: boolean) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "boolean",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: y }) })
  })
  }) }))((_m as any).value);
    case "float": return ((y: Core.FloatValue) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "float",
    term: floatValue(y)
  })
  }) }))((_m as any).value);
    case "integer": return ((y: Core.IntegerValue) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: integerValue(y)
  })
  }) }))((_m as any).value);
    case "string": return ((y: string) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "literal", value: ({ tag: "string", value: y }) })
  })
  }) }))((_m as any).value);
  }
})();
}

export function literalType(v1: Core.LiteralType): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "binary": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.LiteralType",
    field: ({
    name: "binary",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "boolean": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.LiteralType",
    field: ({
    name: "boolean",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "float": return ((y: Core.FloatType) => ({ tag: "inject", value: ({
    typeName: "hydra.core.LiteralType",
    field: ({
    name: "float",
    term: floatType(y)
  })
  }) }))((_m as any).value);
    case "integer": return ((y: Core.IntegerType) => ({ tag: "inject", value: ({
    typeName: "hydra.core.LiteralType",
    field: ({
    name: "integer",
    term: integerType(y)
  })
  }) }))((_m as any).value);
    case "string": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.LiteralType",
    field: ({
    name: "string",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})();
}

export function mapType(x: Core.MapType): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.MapType",
    fields: [({
    name: "keys",
    term: type(((_x) => _x.keys)(x))
  }), ({
    name: "values",
    term: type(((_x) => _x.values)(x))
  })]
  }) });
}

export function name(x: Core.Name): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function pairType(x: Core.PairType): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.PairType",
    fields: [({
    name: "first",
    term: type(((_x) => _x.first)(x))
  }), ({
    name: "second",
    term: type(((_x) => _x.second)(x))
  })]
  }) });
}

export function projection(x: Core.Projection): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.Projection",
    fields: [({
    name: "typeName",
    term: name(((_x) => _x.typeName)(x))
  }), ({
    name: "field",
    term: name(((_x) => _x.field)(x))
  })]
  }) });
}

export function record(x: Core.Record): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.Record",
    fields: [({
    name: "typeName",
    term: name(((_x) => _x.typeName)(x))
  }), ({
    name: "fields",
    term: ({ tag: "list", value: LibLists.map(field)(((_x) => _x.fields)(x)) })
  })]
  }) });
}

export function term(v1: Core.Term): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((y: Core.AnnotatedTerm) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "annotated",
    term: annotatedTerm(y)
  })
  }) }))((_m as any).value);
    case "application": return ((y: Core.Application) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "application",
    term: application(y)
  })
  }) }))((_m as any).value);
    case "cases": return ((y: Core.CaseStatement) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "cases",
    term: caseStatement(y)
  })
  }) }))((_m as any).value);
    case "either": return ((y: Core.Term | Core.Term) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "either",
    term: ({ tag: "either", value: LibEithers.bimap(term)(term)(y) })
  })
  }) }))((_m as any).value);
    case "inject": return ((y: Core.Injection) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "inject",
    term: injection(y)
  })
  }) }))((_m as any).value);
    case "lambda": return ((y: Core.Lambda) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "lambda",
    term: lambda(y)
  })
  }) }))((_m as any).value);
    case "let": return ((y: Core.Let) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "let",
    term: let_(y)
  })
  }) }))((_m as any).value);
    case "list": return ((y: ReadonlyArray<Core.Term>) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "list",
    term: ({ tag: "list", value: LibLists.map(term)(y) })
  })
  }) }))((_m as any).value);
    case "literal": return ((y: Core.Literal) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: literal(y)
  })
  }) }))((_m as any).value);
    case "map": return ((y: ReadonlyMap<Core.Term, Core.Term>) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "map",
    term: ({ tag: "map", value: LibMaps.bimap(term)(term)(y) })
  })
  }) }))((_m as any).value);
    case "maybe": return ((y: Core.Term | null) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "maybe",
    term: ({ tag: "maybe", value: LibMaybes.map(term)(y) })
  })
  }) }))((_m as any).value);
    case "pair": return ((y: readonly [Core.Term, Core.Term]) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "pair",
    term: ({ tag: "pair", value: LibPairs.bimap(term)(term)(y) })
  })
  }) }))((_m as any).value);
    case "project": return ((y: Core.Projection) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "project",
    term: projection(y)
  })
  }) }))((_m as any).value);
    case "record": return ((y: Core.Record) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "record",
    term: record(y)
  })
  }) }))((_m as any).value);
    case "set": return ((y: ReadonlySet<Core.Term>) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "set",
    term: ({ tag: "set", value: LibSets.map(term)(y) })
  })
  }) }))((_m as any).value);
    case "typeApplication": return ((y: Core.TypeApplicationTerm) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "typeApplication",
    term: typeApplicationTerm(y)
  })
  }) }))((_m as any).value);
    case "typeLambda": return ((y: Core.TypeLambda) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "typeLambda",
    term: typeLambda(y)
  })
  }) }))((_m as any).value);
    case "unit": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "unit",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "unwrap": return ((y: Core.Name) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "unwrap",
    term: name(y)
  })
  }) }))((_m as any).value);
    case "variable": return ((y: Core.Name) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "variable",
    term: name(y)
  })
  }) }))((_m as any).value);
    case "wrap": return ((y: Core.WrappedTerm) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "wrap",
    term: wrappedTerm(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function type(v1: Core.Type): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((y: Core.AnnotatedType) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "annotated",
    term: annotatedType(y)
  })
  }) }))((_m as any).value);
    case "application": return ((y: Core.ApplicationType) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "application",
    term: applicationType(y)
  })
  }) }))((_m as any).value);
    case "either": return ((y: Core.EitherType) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "either",
    term: eitherType(y)
  })
  }) }))((_m as any).value);
    case "forall": return ((y: Core.ForallType) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "forall",
    term: forallType(y)
  })
  }) }))((_m as any).value);
    case "function": return ((y: Core.FunctionType) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "function",
    term: functionType(y)
  })
  }) }))((_m as any).value);
    case "list": return ((y: Core.Type) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "list",
    term: type(y)
  })
  }) }))((_m as any).value);
    case "literal": return ((y: Core.LiteralType) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "literal",
    term: literalType(y)
  })
  }) }))((_m as any).value);
    case "map": return ((y: Core.MapType) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "map",
    term: mapType(y)
  })
  }) }))((_m as any).value);
    case "maybe": return ((y: Core.Type) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "maybe",
    term: type(y)
  })
  }) }))((_m as any).value);
    case "pair": return ((y: Core.PairType) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "pair",
    term: pairType(y)
  })
  }) }))((_m as any).value);
    case "record": return ((y: ReadonlyArray<Core.FieldType>) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "record",
    term: ({ tag: "list", value: LibLists.map(fieldType)(y) })
  })
  }) }))((_m as any).value);
    case "set": return ((y: Core.Type) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "set",
    term: type(y)
  })
  }) }))((_m as any).value);
    case "union": return ((y: ReadonlyArray<Core.FieldType>) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "union",
    term: ({ tag: "list", value: LibLists.map(fieldType)(y) })
  })
  }) }))((_m as any).value);
    case "unit": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "unit",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "variable": return ((y: Core.Name) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "variable",
    term: name(y)
  })
  }) }))((_m as any).value);
    case "void": return ((y: void) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "void",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "wrap": return ((y: Core.Type) => ({ tag: "inject", value: ({
    typeName: "hydra.core.Type",
    field: ({
    name: "wrap",
    term: type(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function typeApplicationTerm(x: Core.TypeApplicationTerm): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.TypeApplicationTerm",
    fields: [({
    name: "body",
    term: term(((_x) => _x.body)(x))
  }), ({
    name: "type",
    term: type(((_x) => _x.type)(x))
  })]
  }) });
}

export function typeLambda(x: Core.TypeLambda): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.TypeLambda",
    fields: [({
    name: "parameter",
    term: name(((_x) => _x.parameter)(x))
  }), ({
    name: "body",
    term: term(((_x) => _x.body)(x))
  })]
  }) });
}

export function typeScheme(x: Core.TypeScheme): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.TypeScheme",
    fields: [({
    name: "variables",
    term: ({ tag: "list", value: LibLists.map(name)(((_x) => _x.variables)(x)) })
  }), ({
    name: "type",
    term: type(((_x) => _x.type)(x))
  }), ({
    name: "constraints",
    term: ({ tag: "maybe", value: LibMaybes.map(((m: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => ({ tag: "map", value: LibMaps.bimap(name)(typeVariableMetadata)(m) })))(((_x) => _x.constraints)(x)) })
  })]
  }) });
}

export function typeVariableMetadata(x: Core.TypeVariableMetadata): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.TypeVariableMetadata",
    fields: [({
    name: "classes",
    term: ({ tag: "set", value: LibSets.map(name)(((_x) => _x.classes)(x)) })
  })]
  }) });
}

export function wrappedTerm(x: Core.WrappedTerm): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.core.WrappedTerm",
    fields: [({
    name: "typeName",
    term: name(((_x) => _x.typeName)(x))
  }), ({
    name: "body",
    term: term(((_x) => _x.body)(x))
  })]
  }) });
}
