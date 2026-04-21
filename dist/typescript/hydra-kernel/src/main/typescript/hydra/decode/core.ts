// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.core
 */



import * as Core from "../core.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibStrings from "../lib/strings.js";
import * as Rewriting from "../rewriting.js";
import * as Util from "../util.js";

export function annotatedTerm(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.AnnotatedTerm) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("body")(term)(fieldMap)(cx))(((field_body: Core.Term) => LibEithers.bind(ExtractCore.requireField("annotation")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(name)(term)(v1)(v2))))(fieldMap)(cx))(((field_annotation: ReadonlyMap<Core.Name, Core.Term>) => ({ tag: "right", value: ({
    body: field_body,
    annotation: field_annotation
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function annotatedType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.AnnotatedType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("body")(type)(fieldMap)(cx))(((field_body: Core.Type) => LibEithers.bind(ExtractCore.requireField("annotation")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(name)(term)(v1)(v2))))(fieldMap)(cx))(((field_annotation: ReadonlyMap<Core.Name, Core.Term>) => ({ tag: "right", value: ({
    body: field_body,
    annotation: field_annotation
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function application(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Application) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("function")(term)(fieldMap)(cx))(((field_function: Core.Term) => LibEithers.bind(ExtractCore.requireField("argument")(term)(fieldMap)(cx))(((field_argument: Core.Term) => ({ tag: "right", value: ({
    function: field_function,
    argument: field_argument
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function applicationType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.ApplicationType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("function")(type)(fieldMap)(cx))(((field_function: Core.Type) => LibEithers.bind(ExtractCore.requireField("argument")(type)(fieldMap)(cx))(((field_argument: Core.Type) => ({ tag: "right", value: ({
    function: field_function,
    argument: field_argument
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function binding(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Binding) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(name)(fieldMap)(cx))(((field_name: Core.Name) => LibEithers.bind(ExtractCore.requireField("term")(term)(fieldMap)(cx))(((field_term: Core.Term) => LibEithers.bind(ExtractCore.requireField("type")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(typeScheme)(v1)(v2))))(fieldMap)(cx))(((field_type: Core.TypeScheme | null) => ({ tag: "right", value: ({
    name: field_name,
    term: field_term,
    type: field_type
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function caseStatement(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.CaseStatement) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("typeName")(name)(fieldMap)(cx))(((field_typeName: Core.Name) => LibEithers.bind(ExtractCore.requireField("default")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(term)(v1)(v2))))(fieldMap)(cx))(((field_default: Core.Term | null) => LibEithers.bind(ExtractCore.requireField("cases")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(field)(v1)(v2))))(fieldMap)(cx))(((field_cases: ReadonlyArray<Core.Field>) => ({ tag: "right", value: ({
    typeName: field_typeName,
    default: field_default,
    cases: field_cases
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function eitherType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.EitherType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("left")(type)(fieldMap)(cx))(((field_left: Core.Type) => LibEithers.bind(ExtractCore.requireField("right")(type)(fieldMap)(cx))(((field_right: Core.Type) => ({ tag: "right", value: ({
    left: field_left,
    right: field_right
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function field(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Field) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(name)(fieldMap)(cx))(((field_name: Core.Name) => LibEithers.bind(ExtractCore.requireField("term")(term)(fieldMap)(cx))(((field_term: Core.Term) => ({ tag: "right", value: ({
    name: field_name,
    term: field_term
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function fieldType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.FieldType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(name)(fieldMap)(cx))(((field_name: Core.Name) => LibEithers.bind(ExtractCore.requireField("type")(type)(fieldMap)(cx))(((field_type: Core.Type) => ({ tag: "right", value: ({
    name: field_name,
    type: field_type
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function floatType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.FloatType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["bigfloat", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "bigfloat", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["float32", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "float32", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["float64", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "float64", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Core.FloatType)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function floatValue(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.FloatValue) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["bigfloat", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "bigfloat", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "float": return ((v1: Core.FloatValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigfloat": return ((f: number) => ({ tag: "right", value: f }))((_m as any).value);
    default: return ({ tag: "left", value: "expected bigfloat value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected bigfloat literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["float32", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "float32", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "float": return ((v1: Core.FloatValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "float32": return ((f: number) => ({ tag: "right", value: f }))((_m as any).value);
    default: return ({ tag: "left", value: "expected float32 value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected float32 literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["float64", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "float64", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "float": return ((v1: Core.FloatValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "float64": return ((f: number) => ({ tag: "right", value: f }))((_m as any).value);
    default: return ({ tag: "left", value: "expected float64 value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected float64 literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Core.FloatValue)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function forallType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.ForallType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("parameter")(name)(fieldMap)(cx))(((field_parameter: Core.Name) => LibEithers.bind(ExtractCore.requireField("body")(type)(fieldMap)(cx))(((field_body: Core.Type) => ({ tag: "right", value: ({
    parameter: field_parameter,
    body: field_body
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function functionType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.FunctionType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("domain")(type)(fieldMap)(cx))(((field_domain: Core.Type) => LibEithers.bind(ExtractCore.requireField("codomain")(type)(fieldMap)(cx))(((field_codomain: Core.Type) => ({ tag: "right", value: ({
    domain: field_domain,
    codomain: field_codomain
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function injection(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Injection) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("typeName")(name)(fieldMap)(cx))(((field_typeName: Core.Name) => LibEithers.bind(ExtractCore.requireField("field")(field)(fieldMap)(cx))(((field_field: Core.Field) => ({ tag: "right", value: ({
    typeName: field_typeName,
    field: field_field
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function integerType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.IntegerType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["bigint", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "bigint", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["int8", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "int8", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["int16", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "int16", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["int32", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "int32", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["int64", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "int64", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["uint8", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "uint8", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["uint16", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "uint16", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["uint32", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "uint32", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["uint64", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "uint64", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Core.IntegerType)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function integerValue(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.IntegerValue) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["bigint", ((input: Core.Term) => LibEithers.map(((t: bigint) => ({ tag: "bigint", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v1: Core.IntegerValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "bigint": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: "expected bigint value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected bigint literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["int8", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "int8", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v1: Core.IntegerValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "int8": return ((i: number) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: "expected int8 value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected int8 literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["int16", ((input: Core.Term) => LibEithers.map(((t: bigint) => ({ tag: "int16", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v1: Core.IntegerValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "int16": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: "expected int16 value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected int16 literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["int32", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "int32", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v1: Core.IntegerValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "int32": return ((i: number) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: "expected int32 value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected int32 literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["int64", ((input: Core.Term) => LibEithers.map(((t: bigint) => ({ tag: "int64", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v1: Core.IntegerValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "int64": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: "expected int64 value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected int64 literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["uint8", ((input: Core.Term) => LibEithers.map(((t: bigint) => ({ tag: "uint8", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v1: Core.IntegerValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "uint8": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: "expected uint8 value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected uint8 literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["uint16", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "uint16", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v1: Core.IntegerValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "uint16": return ((i: number) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: "expected uint16 value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected uint16 literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["uint32", ((input: Core.Term) => LibEithers.map(((t: bigint) => ({ tag: "uint32", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v1: Core.IntegerValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "uint32": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: "expected uint32 value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected uint32 literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["uint64", ((input: Core.Term) => LibEithers.map(((t: bigint) => ({ tag: "uint64", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((v1: Core.IntegerValue) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "uint64": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: "expected uint64 value" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected uint64 literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Core.IntegerValue)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function lambda(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Lambda) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("parameter")(name)(fieldMap)(cx))(((field_parameter: Core.Name) => LibEithers.bind(ExtractCore.requireField("domain")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(type)(v1)(v2))))(fieldMap)(cx))(((field_domain: Core.Type | null) => LibEithers.bind(ExtractCore.requireField("body")(term)(fieldMap)(cx))(((field_body: Core.Term) => ({ tag: "right", value: ({
    parameter: field_parameter,
    domain: field_domain,
    body: field_body
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function let_(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Let) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("bindings")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(binding)(v1)(v2))))(fieldMap)(cx))(((field_bindings: ReadonlyArray<Core.Binding>) => LibEithers.bind(ExtractCore.requireField("body")(term)(fieldMap)(cx))(((field_body: Core.Term) => ({ tag: "right", value: ({
    bindings: field_bindings,
    body: field_body
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function literal(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Literal) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["binary", ((input: Core.Term) => LibEithers.map(((t: Uint8Array) => ({ tag: "binary", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "binary": return ((b: Uint8Array) => ({ tag: "right", value: b }))((_m as any).value);
    default: return ({ tag: "left", value: "expected binary literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["boolean", ((input: Core.Term) => LibEithers.map(((t: boolean) => ({ tag: "boolean", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "boolean": return ((b: boolean) => ({ tag: "right", value: b }))((_m as any).value);
    default: return ({ tag: "left", value: "expected boolean literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["float", ((input: Core.Term) => LibEithers.map(((t: Core.FloatValue) => ({ tag: "float", value: t })))(floatValue(cx)(input)))], ["integer", ((input: Core.Term) => LibEithers.map(((t: Core.IntegerValue) => ({ tag: "integer", value: t })))(integerValue(cx)(input)))], ["string", ((input: Core.Term) => LibEithers.map(((t: string) => ({ tag: "string", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "right", value: s }))((_m as any).value);
    default: return ({ tag: "left", value: "expected string literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(input))))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Core.Literal)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function literalType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.LiteralType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["binary", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "binary", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["boolean", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "boolean", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["float", ((input: Core.Term) => LibEithers.map(((t: Core.FloatType) => ({ tag: "float", value: t })))(floatType(cx)(input)))], ["integer", ((input: Core.Term) => LibEithers.map(((t: Core.IntegerType) => ({ tag: "integer", value: t })))(integerType(cx)(input)))], ["string", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "string", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Core.LiteralType)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function mapType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.MapType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("keys")(type)(fieldMap)(cx))(((field_keys: Core.Type) => LibEithers.bind(ExtractCore.requireField("values")(type)(fieldMap)(cx))(((field_values: Core.Type) => ({ tag: "right", value: ({
    keys: field_keys,
    values: field_values
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function name(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Name) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: string) => b))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "right", value: s }))((_m as any).value);
    default: return ({ tag: "left", value: "expected string literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(((_x) => _x.body)(wrappedTerm)))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function pairType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.PairType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("first")(type)(fieldMap)(cx))(((field_first: Core.Type) => LibEithers.bind(ExtractCore.requireField("second")(type)(fieldMap)(cx))(((field_second: Core.Type) => ({ tag: "right", value: ({
    first: field_first,
    second: field_second
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function projection(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Projection) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("typeName")(name)(fieldMap)(cx))(((field_typeName: Core.Name) => LibEithers.bind(ExtractCore.requireField("field")(name)(fieldMap)(cx))(((field_field: Core.Name) => ({ tag: "right", value: ({
    typeName: field_typeName,
    field: field_field
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function record(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Record) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("typeName")(name)(fieldMap)(cx))(((field_typeName: Core.Name) => LibEithers.bind(ExtractCore.requireField("fields")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(field)(v1)(v2))))(fieldMap)(cx))(((field_fields: ReadonlyArray<Core.Field>) => ({ tag: "right", value: ({
    typeName: field_typeName,
    fields: field_fields
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function term(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Term) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["annotated", ((input: Core.Term) => LibEithers.map(((t: Core.AnnotatedTerm) => ({ tag: "annotated", value: t })))(annotatedTerm(cx)(input)))], ["application", ((input: Core.Term) => LibEithers.map(((t: Core.Application) => ({ tag: "application", value: t })))(application(cx)(input)))], ["cases", ((input: Core.Term) => LibEithers.map(((t: Core.CaseStatement) => ({ tag: "cases", value: t })))(caseStatement(cx)(input)))], ["either", ((input: Core.Term) => LibEithers.map(((t: Core.Term | Core.Term) => ({ tag: "either", value: t })))(ExtractCore.decodeEither(term)(term)(cx)(input)))], ["inject", ((input: Core.Term) => LibEithers.map(((t: Core.Injection) => ({ tag: "inject", value: t })))(injection(cx)(input)))], ["lambda", ((input: Core.Term) => LibEithers.map(((t: Core.Lambda) => ({ tag: "lambda", value: t })))(lambda(cx)(input)))], ["let", ((input: Core.Term) => LibEithers.map(((t: Core.Let) => ({ tag: "let", value: t })))(let_(cx)(input)))], ["list", ((input: Core.Term) => LibEithers.map(((t: ReadonlyArray<Core.Term>) => ({ tag: "list", value: t })))(ExtractCore.decodeList(term)(cx)(input)))], ["literal", ((input: Core.Term) => LibEithers.map(((t: Core.Literal) => ({ tag: "literal", value: t })))(literal(cx)(input)))], ["map", ((input: Core.Term) => LibEithers.map(((t: ReadonlyMap<Core.Term, Core.Term>) => ({ tag: "map", value: t })))(ExtractCore.decodeMap(term)(term)(cx)(input)))], ["maybe", ((input: Core.Term) => LibEithers.map(((t: Core.Term | null) => ({ tag: "maybe", value: t })))(ExtractCore.decodeMaybe(term)(cx)(input)))], ["pair", ((input: Core.Term) => LibEithers.map(((t: readonly [Core.Term, Core.Term]) => ({ tag: "pair", value: t })))(ExtractCore.decodePair(term)(term)(cx)(input)))], ["project", ((input: Core.Term) => LibEithers.map(((t: Core.Projection) => ({ tag: "project", value: t })))(projection(cx)(input)))], ["record", ((input: Core.Term) => LibEithers.map(((t: Core.Record) => ({ tag: "record", value: t })))(record(cx)(input)))], ["set", ((input: Core.Term) => LibEithers.map(((t: ReadonlySet<Core.Term>) => ({ tag: "set", value: t })))(ExtractCore.decodeSet(term)(cx)(input)))], ["typeApplication", ((input: Core.Term) => LibEithers.map(((t: Core.TypeApplicationTerm) => ({ tag: "typeApplication", value: t })))(typeApplicationTerm(cx)(input)))], ["typeLambda", ((input: Core.Term) => LibEithers.map(((t: Core.TypeLambda) => ({ tag: "typeLambda", value: t })))(typeLambda(cx)(input)))], ["unit", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "unit", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["unwrap", ((input: Core.Term) => LibEithers.map(((t: Core.Name) => ({ tag: "unwrap", value: t })))(name(cx)(input)))], ["variable", ((input: Core.Term) => LibEithers.map(((t: Core.Name) => ({ tag: "variable", value: t })))(name(cx)(input)))], ["wrap", ((input: Core.Term) => LibEithers.map(((t: Core.WrappedTerm) => ({ tag: "wrap", value: t })))(wrappedTerm(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Core.Term)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function type(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Type) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["annotated", ((input: Core.Term) => LibEithers.map(((t: Core.AnnotatedType) => ({ tag: "annotated", value: t })))(annotatedType(cx)(input)))], ["application", ((input: Core.Term) => LibEithers.map(((t: Core.ApplicationType) => ({ tag: "application", value: t })))(applicationType(cx)(input)))], ["either", ((input: Core.Term) => LibEithers.map(((t: Core.EitherType) => ({ tag: "either", value: t })))(eitherType(cx)(input)))], ["forall", ((input: Core.Term) => LibEithers.map(((t: Core.ForallType) => ({ tag: "forall", value: t })))(forallType(cx)(input)))], ["function", ((input: Core.Term) => LibEithers.map(((t: Core.FunctionType) => ({ tag: "function", value: t })))(functionType(cx)(input)))], ["list", ((input: Core.Term) => LibEithers.map(((t: Core.Type) => ({ tag: "list", value: t })))(type(cx)(input)))], ["literal", ((input: Core.Term) => LibEithers.map(((t: Core.LiteralType) => ({ tag: "literal", value: t })))(literalType(cx)(input)))], ["map", ((input: Core.Term) => LibEithers.map(((t: Core.MapType) => ({ tag: "map", value: t })))(mapType(cx)(input)))], ["maybe", ((input: Core.Term) => LibEithers.map(((t: Core.Type) => ({ tag: "maybe", value: t })))(type(cx)(input)))], ["pair", ((input: Core.Term) => LibEithers.map(((t: Core.PairType) => ({ tag: "pair", value: t })))(pairType(cx)(input)))], ["record", ((input: Core.Term) => LibEithers.map(((t: ReadonlyArray<Core.FieldType>) => ({ tag: "record", value: t })))(ExtractCore.decodeList(fieldType)(cx)(input)))], ["set", ((input: Core.Term) => LibEithers.map(((t: Core.Type) => ({ tag: "set", value: t })))(type(cx)(input)))], ["union", ((input: Core.Term) => LibEithers.map(((t: ReadonlyArray<Core.FieldType>) => ({ tag: "union", value: t })))(ExtractCore.decodeList(fieldType)(cx)(input)))], ["unit", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "unit", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["variable", ((input: Core.Term) => LibEithers.map(((t: Core.Name) => ({ tag: "variable", value: t })))(name(cx)(input)))], ["void", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "void", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["wrap", ((input: Core.Term) => LibEithers.map(((t: Core.Type) => ({ tag: "wrap", value: t })))(type(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Core.Type)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function typeApplicationTerm(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.TypeApplicationTerm) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("body")(term)(fieldMap)(cx))(((field_body: Core.Term) => LibEithers.bind(ExtractCore.requireField("type")(type)(fieldMap)(cx))(((field_type: Core.Type) => ({ tag: "right", value: ({
    body: field_body,
    type: field_type
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function typeLambda(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.TypeLambda) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("parameter")(name)(fieldMap)(cx))(((field_parameter: Core.Name) => LibEithers.bind(ExtractCore.requireField("body")(term)(fieldMap)(cx))(((field_body: Core.Term) => ({ tag: "right", value: ({
    parameter: field_parameter,
    body: field_body
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function typeScheme(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.TypeScheme) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("variables")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(name)(v1)(v2))))(fieldMap)(cx))(((field_variables: ReadonlyArray<Core.Name>) => LibEithers.bind(ExtractCore.requireField("type")(type)(fieldMap)(cx))(((field_type: Core.Type) => LibEithers.bind(ExtractCore.requireField("constraints")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(((v12: Graph.Graph) => ((v22: Core.Term) => ExtractCore.decodeMap(name)(typeVariableMetadata)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_constraints: ReadonlyMap<Core.Name, Core.TypeVariableMetadata> | null) => ({ tag: "right", value: ({
    variables: field_variables,
    type: field_type,
    constraints: field_constraints
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function typeVariableMetadata(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.TypeVariableMetadata) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("classes")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeSet(name)(v1)(v2))))(fieldMap)(cx))(((field_classes: ReadonlySet<Core.Name>) => ({ tag: "right", value: ({
    classes: field_classes
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function wrappedTerm(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.WrappedTerm) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("typeName")(name)(fieldMap)(cx))(((field_typeName: Core.Name) => LibEithers.bind(ExtractCore.requireField("body")(term)(fieldMap)(cx))(((field_body: Core.Term) => ({ tag: "right", value: ({
    typeName: field_typeName,
    body: field_body
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}
