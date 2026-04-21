// Note: this is an automatically generated file. Do not edit.

/**
 * Extraction and validation for hydra.core types
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowCore from "../show/core.js";
import * as ShowErrors from "../show/errors.js";
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function bigfloat(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | number) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(floatLiteral(l))(((f: Core.FloatValue) => bigfloatValue(f))))));
}

export function bigfloatValue(v: Core.FloatValue): Errors.Error | number {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "bigfloat": return ((f: number) => ({ tag: "right", value: f }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "bigfloat",
    actual: ShowCore.float_(v)
  }) }) }) })(_m);
  }
})();
}

export function bigint(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | bigint) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(integerLiteral(l))(((i: Core.IntegerValue) => bigintValue(i))))));
}

export function bigintValue(v: Core.IntegerValue): Errors.Error | bigint {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "bigint": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "bigint",
    actual: ShowCore.integer(v)
  }) }) }) })(_m);
  }
})();
}

export function binary(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | Uint8Array) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => binaryLiteral(l))));
}

export function binaryLiteral(v: Core.Literal): Errors.Error | Uint8Array {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "binary": return ((b: Uint8Array) => ({ tag: "right", value: b }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "binary",
    actual: ShowCore.literal(v)
  }) }) }) })(_m);
  }
})();
}

export function boolean_(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | boolean) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => booleanLiteral(l))));
}

export function booleanLiteral(v: Core.Literal): Errors.Error | boolean {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "boolean": return ((b: boolean) => ({ tag: "right", value: b }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "boolean",
    actual: ShowCore.literal(v)
  }) }) }) })(_m);
  }
})();
}

export function caseField(name: Core.Name): ((x: string) => ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Field))) {
  return ((n: string) => ((graph: Graph.Graph) => ((term: Core.Term) => (() => {
  const fieldName = n;
  return LibEithers.bind(cases(name)(graph)(term))(((cs: Core.CaseStatement) => (() => {
  const matching = LibLists.filter(((f: Core.Field) => LibEquality.equal(((_x) => _x)(((_x) => _x.name)(f)))(((_x) => _x)(fieldName))))(((_x) => _x.cases)(cs));
  return LibLogic.ifElse(LibLists.null_(matching))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "matching case",
    actual: "no matching case"
  }) }) }) }))(({ tag: "right", value: LibLists.head(matching) }));
})()));
})())));
}

export function cases(name: Core.Name): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.CaseStatement)) {
  return ((graph: Graph.Graph) => ((term0: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term0))(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "cases": return ((cs: Core.CaseStatement) => LibLogic.ifElse(LibEquality.equal(((_x) => _x)(((_x) => _x.typeName)(cs)))(((_x) => _x)(name)))(({ tag: "right", value: cs }))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: LibStrings.cat2("case statement for type ")(((_x) => _x)(name)),
    actual: ShowCore.term(term)
  }) }) }) })))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "case statement",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})()))));
}

export function decodeEither<t0, t1>(leftDecoder: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t1))) => ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0 | t1))) {
  return ((rightDecoder: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t1))) => ((g: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(stripWithDecodingError(g)(term))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((lv: Core.Term) => LibEithers.map(((x: t0) => ({ tag: "left", value: x })))(leftDecoder(g)(lv))))(((rv: Core.Term) => LibEithers.map(((x: t1) => ({ tag: "right", value: x })))(rightDecoder(g)(rv))))(e))((_m as any).value);
    default: return ({ tag: "left", value: "expected either value" })(_m);
  }
})())))));
}

export function decodeList<t0>(elemDecoder: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | ReadonlyArray<t0>)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(stripWithDecodingError(g)(term))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "list": return ((els: ReadonlyArray<Core.Term>) => LibEithers.mapList(((v1: Core.Term) => elemDecoder(g)(v1)))(els))((_m as any).value);
    default: return ({ tag: "left", value: "expected list" })(_m);
  }
})()))));
}

export function decodeMap<t0, t1>(keyDecoder: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t1))) => ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | ReadonlyMap<t0, t1>))) {
  return ((valDecoder: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t1))) => ((g: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(stripWithDecodingError(g)(term))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibEithers.map(LibMaps.fromList)(LibEithers.mapList(((kv: readonly [Core.Term, Core.Term]) => LibEithers.bind(keyDecoder(g)(LibPairs.first(kv)))(((k: t0) => LibEithers.map(((v: t1) => [k, v]))(valDecoder(g)(LibPairs.second(kv)))))))(LibMaps.toList(m))))((_m as any).value);
    default: return ({ tag: "left", value: "expected map" })(_m);
  }
})())))));
}

export function decodeMaybe<t0>(elemDecoder: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0 | null)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(stripWithDecodingError(g)(term))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "maybe": return ((opt: Core.Term | null) => LibEithers.mapMaybe(((v1: Core.Term) => elemDecoder(g)(v1)))(opt))((_m as any).value);
    default: return ({ tag: "left", value: "expected optional value" })(_m);
  }
})()))));
}

export function decodePair<t0, t1>(firstDecoder: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t1))) => ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | readonly [t0, t1]))) {
  return ((secondDecoder: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t1))) => ((g: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(stripWithDecodingError(g)(term))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => LibEithers.bind(firstDecoder(g)(LibPairs.first(p)))(((f: t0) => LibEithers.map(((s: t1) => [f, s]))(secondDecoder(g)(LibPairs.second(p))))))((_m as any).value);
    default: return ({ tag: "left", value: "expected pair" })(_m);
  }
})())))));
}

export function decodeSet<t0>(elemDecoder: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | ReadonlySet<t0>)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(stripWithDecodingError(g)(term))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "set": return ((s: ReadonlySet<Core.Term>) => LibEithers.map(LibSets.fromList)(LibEithers.mapList(((v1: Core.Term) => elemDecoder(g)(v1)))(LibSets.toList(s))))((_m as any).value);
    default: return ({ tag: "left", value: "expected set" })(_m);
  }
})()))));
}

export function decodeUnit(g: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | void) {
  return ((term: Core.Term) => LibEithers.bind(stripWithDecodingError(g)(term))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "unit": return ((_: void) => ({ tag: "right", value: undefined }))((_m as any).value);
    default: return ({ tag: "left", value: "expected a unit value" })(_m);
  }
})())));
}

export function decodeWrapped<t0>(bodyDecoder: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(stripWithDecodingError(g)(term))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wt: Core.WrappedTerm) => bodyDecoder(g)(((_x) => _x.body)(wt)))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped value" })(_m);
  }
})()))));
}

export function eitherTerm<t0, t1>(leftFun: ((x: Core.Term) => Errors.Error | t0)): ((x: ((x: Core.Term) => Errors.Error | t1)) => ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | t0 | t1))) {
  return ((rightFun: ((x: Core.Term) => Errors.Error | t1)) => ((graph: Graph.Graph) => ((term0: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term0))(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "either": return ((et: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => LibEithers.map(((x: t0) => ({ tag: "left", value: x })))(leftFun(l))))(((r: Core.Term) => LibEithers.map(((x: t1) => ({ tag: "right", value: x })))(rightFun(r))))(et))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "either value",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})())))));
}

export function eitherType(typ: Core.Type): Errors.Error | Core.EitherType {
  return (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "either": return ((et: Core.EitherType) => ({ tag: "right", value: et }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "either type",
    actual: ShowCore.type(typ)
  }) }) }) })(_m);
  }
})();
})();
}

export function field<t0>(fname: Core.Name): ((x: ((x: Core.Term) => Errors.Error | t0)) => ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Field>) => Errors.Error | t0))) {
  return ((mapping: ((x: Core.Term) => Errors.Error | t0)) => ((graph: Graph.Graph) => ((fields: ReadonlyArray<Core.Field>) => (() => {
  const matchingFields = LibLists.filter(((f: Core.Field) => LibEquality.equal(((_x) => _x)(((_x) => _x.name)(f)))(((_x) => _x)(fname))))(fields);
  return LibLogic.ifElse(LibLists.null_(matchingFields))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: LibStrings.cat2("field ")(((_x) => _x)(fname)),
    actual: "no matching field"
  }) }) }) }))(LibLogic.ifElse(LibEquality.equal(LibLists.length(matchingFields))(1))(LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(((_x) => _x.term)(LibLists.head(matchingFields))))(((stripped: Core.Term) => mapping(stripped))))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "single field",
    actual: LibStrings.cat2("multiple fields named ")(((_x) => _x)(fname))
  }) }) }) })));
})())));
}

export function float32(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | number) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(floatLiteral(l))(((f: Core.FloatValue) => float32Value(f))))));
}

export function float32Value(v: Core.FloatValue): Errors.Error | number {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "float32": return ((f: number) => ({ tag: "right", value: f }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "float32",
    actual: ShowCore.float_(v)
  }) }) }) })(_m);
  }
})();
}

export function float64(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | number) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(floatLiteral(l))(((f: Core.FloatValue) => float64Value(f))))));
}

export function float64Value(v: Core.FloatValue): Errors.Error | number {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "float64": return ((f: number) => ({ tag: "right", value: f }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "float64",
    actual: ShowCore.float_(v)
  }) }) }) })(_m);
  }
})();
}

export function floatLiteral(lit: Core.Literal): Errors.Error | Core.FloatValue {
  return (() => {
  const _m = lit;
  switch (_m.tag) {
    case "float": return ((v: Core.FloatValue) => ({ tag: "right", value: v }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "floating-point value",
    actual: ShowCore.literal(lit)
  }) }) }) })(_m);
  }
})();
}

export function floatValue(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | Core.FloatValue) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => floatLiteral(l))));
}

export function functionType(typ: Core.Type): Errors.Error | Core.FunctionType {
  return (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => ({ tag: "right", value: ft }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "function type",
    actual: ShowCore.type(typ)
  }) }) }) })(_m);
  }
})();
})();
}

export function injection(expected: Core.Name): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Field)) {
  return ((graph: Graph.Graph) => ((term0: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term0))(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "inject": return ((injection: Core.Injection) => LibLogic.ifElse(LibEquality.equal(((_x) => _x)(((_x) => _x.typeName)(injection)))(((_x) => _x)(expected)))(({ tag: "right", value: ((_x) => _x.field)(injection) }))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: LibStrings.cat2("injection of type ")(((_x) => _x)(expected)),
    actual: ((_x) => _x)(((_x) => _x.typeName)(injection))
  }) }) }) })))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "injection",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})()))));
}

export function int16(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | bigint) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(integerLiteral(l))(((i: Core.IntegerValue) => int16Value(i))))));
}

export function int16Value(v: Core.IntegerValue): Errors.Error | bigint {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "int16": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "int16",
    actual: ShowCore.integer(v)
  }) }) }) })(_m);
  }
})();
}

export function int32(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | number) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(integerLiteral(l))(((i: Core.IntegerValue) => int32Value(i))))));
}

export function int32Value(v: Core.IntegerValue): Errors.Error | number {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "int32": return ((i: number) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "int32",
    actual: ShowCore.integer(v)
  }) }) }) })(_m);
  }
})();
}

export function int64(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | bigint) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(integerLiteral(l))(((i: Core.IntegerValue) => int64Value(i))))));
}

export function int64Value(v: Core.IntegerValue): Errors.Error | bigint {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "int64": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "int64",
    actual: ShowCore.integer(v)
  }) }) }) })(_m);
  }
})();
}

export function int8(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | number) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(integerLiteral(l))(((i: Core.IntegerValue) => int8Value(i))))));
}

export function int8Value(v: Core.IntegerValue): Errors.Error | number {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "int8": return ((i: number) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "int8",
    actual: ShowCore.integer(v)
  }) }) }) })(_m);
  }
})();
}

export function integerLiteral(lit: Core.Literal): Errors.Error | Core.IntegerValue {
  return (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((v: Core.IntegerValue) => ({ tag: "right", value: v }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "integer value",
    actual: ShowCore.literal(lit)
  }) }) }) })(_m);
  }
})();
}

export function integerValue(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | Core.IntegerValue) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => integerLiteral(l))));
}

export function lambda(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | Core.Lambda) {
  return ((term0: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term0))(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => ({ tag: "right", value: l }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "lambda",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})())));
}

export function lambdaBody(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | Core.Term) {
  return ((term: Core.Term) => LibEithers.map(((_x) => _x.body))(lambda(graph)(term)));
}

export function let_(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | Core.Let) {
  return ((term0: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term0))(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "let": return ((lt: Core.Let) => ({ tag: "right", value: lt }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "let term",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})())));
}

export function letBinding(n: string): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((graph: Graph.Graph) => ((term: Core.Term) => (() => {
  const name = n;
  return LibEithers.bind(let_(graph)(term))(((letExpr: Core.Let) => (() => {
  const matchingBindings = LibLists.filter(((b: Core.Binding) => LibEquality.equal(((_x) => _x)(((_x) => _x.name)(b)))(((_x) => _x)(name))))(((_x) => _x.bindings)(letExpr));
  return LibLogic.ifElse(LibLists.null_(matchingBindings))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "noSuchBinding", value: ({
    name: name
  }) }) }) }))(LibLogic.ifElse(LibEquality.equal(LibLists.length(matchingBindings))(1))(({ tag: "right", value: ((_x) => _x.term)(LibLists.head(matchingBindings)) }))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "multipleBindings", value: ({
    name: name
  }) }) }) })));
})()));
})()));
}

export function list(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>) {
  return ((term: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "list": return ((l: ReadonlyArray<Core.Term>) => ({ tag: "right", value: l }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "list",
    actual: ShowCore.term(stripped)
  }) }) }) })(_m);
  }
})())));
}

export function listHead(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | Core.Term) {
  return ((term: Core.Term) => LibEithers.bind(list(graph)(term))(((l: ReadonlyArray<Core.Term>) => LibLogic.ifElse(LibLists.null_(l))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "non-empty list",
    actual: "empty list"
  }) }) }) }))(({ tag: "right", value: LibLists.head(l) })))));
}

export function listOf<t0>(f: ((x: Core.Term) => Errors.Error | t0)): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | ReadonlyArray<t0>)) {
  return ((graph: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(list(graph)(term))(((els: ReadonlyArray<Core.Term>) => LibEithers.mapList(f)(els)))));
}

export function listType(typ: Core.Type): Errors.Error | Core.Type {
  return (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "list": return ((t: Core.Type) => ({ tag: "right", value: t }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "list type",
    actual: ShowCore.type(typ)
  }) }) }) })(_m);
  }
})();
})();
}

export function literal(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | Core.Literal) {
  return ((term0: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term0))(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => ({ tag: "right", value: lit }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "literal",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})())));
}

export function map<t0, t1>(fk: ((x: Core.Term) => Errors.Error | t0)): ((x: ((x: Core.Term) => Errors.Error | t1)) => ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | ReadonlyMap<t0, t1>))) {
  return ((fv: ((x: Core.Term) => Errors.Error | t1)) => ((graph: Graph.Graph) => ((term0: Core.Term) => (() => {
  const pair = ((kvPair: readonly [Core.Term, Core.Term]) => (() => {
  const kterm = LibPairs.first(kvPair);
  return (() => {
  const vterm = LibPairs.second(kvPair);
  return LibEithers.bind(fk(kterm))(((kval: t0) => LibEithers.bind(fv(vterm))(((vval: t1) => ({ tag: "right", value: [kval, vval] })))));
})();
})());
  return LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term0))(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibEithers.map(LibMaps.fromList)(LibEithers.mapList(pair)(LibMaps.toList(m))))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "map",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})()));
})())));
}

export function mapType(typ: Core.Type): Errors.Error | Core.MapType {
  return (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "map": return ((mt: Core.MapType) => ({ tag: "right", value: mt }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "map type",
    actual: ShowCore.type(typ)
  }) }) }) })(_m);
  }
})();
})();
}

export function maybeTerm<t0>(f: ((x: Core.Term) => Errors.Error | t0)): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | t0 | null)) {
  return ((graph: Graph.Graph) => ((term0: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term0))(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.maybe(({ tag: "right", value: null }))(((t: Core.Term) => LibEithers.map(LibMaybes.pure)(f(t))))(mt))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "maybe value",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})()))));
}

export function maybeType(typ: Core.Type): Errors.Error | Core.Type {
  return (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "maybe": return ((t: Core.Type) => ({ tag: "right", value: t }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "maybe type",
    actual: ShowCore.type(typ)
  }) }) }) })(_m);
  }
})();
})();
}

export function nArgs<t0>(name: Core.Name): ((x: number) => ((x: ReadonlyArray<t0>) => Errors.Error | void)) {
  return ((n: number) => ((args: ReadonlyArray<t0>) => LibLogic.ifElse(LibEquality.equal(LibLists.length(args))(n))(({ tag: "right", value: undefined }))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: LibStrings.cat([LibLiterals.showInt32(n), " arguments to primitive ", LibLiterals.showString(((_x) => _x)(name))]),
    actual: LibLiterals.showInt32(LibLists.length(args))
  }) }) }) }))));
}

export function pair<t0, t1>(kf: ((x: Core.Term) => Errors.Error | t0)): ((x: ((x: Core.Term) => Errors.Error | t1)) => ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | readonly [t0, t1]))) {
  return ((vf: ((x: Core.Term) => Errors.Error | t1)) => ((graph: Graph.Graph) => ((term0: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term0))(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => LibEithers.bind(kf(LibPairs.first(p)))(((kVal: t0) => LibEithers.bind(vf(LibPairs.second(p)))(((vVal: t1) => ({ tag: "right", value: [kVal, vVal] }))))))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "pair",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})())))));
}

export function record(expected: Core.Name): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Field>)) {
  return ((graph: Graph.Graph) => ((term0: Core.Term) => LibEithers.bind(termRecord(graph)(term0))(((record: Core.Record) => LibLogic.ifElse(LibEquality.equal(((_x) => _x.typeName)(record))(expected))(({ tag: "right", value: ((_x) => _x.fields)(record) }))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: LibStrings.cat2("record of type ")(((_x) => _x)(expected)),
    actual: ((_x) => _x)(((_x) => _x.typeName)(record))
  }) }) }) }))))));
}

export function recordType<t0>(ename: t0): ((x: Core.Type) => Errors.Error | ReadonlyArray<Core.FieldType>) {
  return ((typ: Core.Type) => (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((fields: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: fields }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "record type",
    actual: ShowCore.type(typ)
  }) }) }) })(_m);
  }
})();
})());
}

export function requireField<t0, t1, t2>(fieldName: string): ((x: ((x: t0) => ((x: t1) => Errors.DecodingError | t2))) => ((x: ReadonlyMap<Core.Name, t1>) => ((x: t0) => Errors.DecodingError | t2))) {
  return ((decoder: ((x: t0) => ((x: t1) => Errors.DecodingError | t2))) => ((fieldMap: ReadonlyMap<Core.Name, t1>) => ((g: t0) => LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["missing field ", fieldName, " in record"]) }))(((fieldTerm: t1) => decoder(g)(fieldTerm)))(LibMaps.lookup(fieldName)(fieldMap)))));
}

export function set(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | ReadonlySet<Core.Term>) {
  return ((term: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "set": return ((s: ReadonlySet<Core.Term>) => ({ tag: "right", value: s }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "set",
    actual: ShowCore.term(stripped)
  }) }) }) })(_m);
  }
})())));
}

export function setOf<t0>(f: ((x: Core.Term) => Errors.Error | t0)): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | ReadonlySet<t0>)) {
  return ((graph: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(set(graph)(term))(((els: ReadonlySet<Core.Term>) => LibEithers.mapSet(f)(els)))));
}

export function setType(typ: Core.Type): Errors.Error | Core.Type {
  return (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "set": return ((t: Core.Type) => ({ tag: "right", value: t }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "set type",
    actual: ShowCore.type(typ)
  }) }) }) })(_m);
  }
})();
})();
}

export function string(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | string) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => stringLiteral(l))));
}

export function stringLiteral(v: Core.Literal): Errors.Error | string {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "right", value: s }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "string",
    actual: ShowCore.literal(v)
  }) }) }) })(_m);
  }
})();
}

export function stripWithDecodingError(g: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Core.Term) {
  return ((term: Core.Term) => LibEithers.bimap(((_e: Errors.Error) => ShowErrors.error(_e)))(((x: Core.Term) => x))(Lexical.stripAndDereferenceTermEither(g)(term)));
}

export function termRecord(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | Core.Record) {
  return ((term0: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term0))(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => ({ tag: "right", value: record }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "record",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})())));
}

export function toFieldMap(record: Core.Record): ReadonlyMap<Core.Name, Core.Term> {
  return LibMaps.fromList(LibLists.map(((f: Core.Field) => [((_x) => _x.name)(f), ((_x) => _x.term)(f)]))(((_x) => _x.fields)(record)));
}

export function uint16(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | number) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(integerLiteral(l))(((i: Core.IntegerValue) => uint16Value(i))))));
}

export function uint16Value(v: Core.IntegerValue): Errors.Error | number {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "uint16": return ((i: number) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "uint16",
    actual: ShowCore.integer(v)
  }) }) }) })(_m);
  }
})();
}

export function uint32(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | bigint) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(integerLiteral(l))(((i: Core.IntegerValue) => uint32Value(i))))));
}

export function uint32Value(v: Core.IntegerValue): Errors.Error | bigint {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "uint32": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "uint32",
    actual: ShowCore.integer(v)
  }) }) }) })(_m);
  }
})();
}

export function uint64(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | bigint) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(integerLiteral(l))(((i: Core.IntegerValue) => uint64Value(i))))));
}

export function uint64Value(v: Core.IntegerValue): Errors.Error | bigint {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "uint64": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "uint64",
    actual: ShowCore.integer(v)
  }) }) }) })(_m);
  }
})();
}

export function uint8(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | bigint) {
  return ((t: Core.Term) => LibEithers.bind(literal(graph)(t))(((l: Core.Literal) => LibEithers.bind(integerLiteral(l))(((i: Core.IntegerValue) => uint8Value(i))))));
}

export function uint8Value(v: Core.IntegerValue): Errors.Error | bigint {
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "uint8": return ((i: bigint) => ({ tag: "right", value: i }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "uint8",
    actual: ShowCore.integer(v)
  }) }) }) })(_m);
  }
})();
}

export function unionType<t0>(ename: t0): ((x: Core.Type) => Errors.Error | ReadonlyArray<Core.FieldType>) {
  return ((typ: Core.Type) => (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "union": return ((fields: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: fields }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "union type",
    actual: ShowCore.type(typ)
  }) }) }) })(_m);
  }
})();
})());
}

export function unit(term: Core.Term): Errors.Error | void {
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "unit": return ((_: void) => ({ tag: "right", value: undefined }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "unit",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})();
}

export function unitVariant(tname: Core.Name): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Name)) {
  return ((graph: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(injection(tname)(graph)(term))(((field: Core.Field) => LibEithers.bind(unit(((_x) => _x.term)(field)))(((ignored: void) => ({ tag: "right", value: ((_x) => _x.name)(field) })))))));
}

export function wrap(expected: Core.Name): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((graph: Graph.Graph) => ((term0: Core.Term) => LibEithers.bind(Lexical.stripAndDereferenceTerm(graph)(term0))(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibLogic.ifElse(LibEquality.equal(((_x) => _x)(((_x) => _x.typeName)(wrappedTerm)))(((_x) => _x)(expected)))(({ tag: "right", value: ((_x) => _x.body)(wrappedTerm) }))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: LibStrings.cat2("wrapper of type ")(((_x) => _x)(expected)),
    actual: ((_x) => _x)(((_x) => _x.typeName)(wrappedTerm))
  }) }) }) })))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: LibStrings.cat2(LibStrings.cat2("wrap(")(((_x) => _x)(expected)))(")"),
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})()))));
}

export function wrappedType<t0>(ename: t0): ((x: Core.Type) => Errors.Error | Core.Type) {
  return ((typ: Core.Type) => (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((innerType: Core.Type) => ({ tag: "right", value: innerType }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "wrapped type",
    actual: ShowCore.type(typ)
  }) }) }) })(_m);
  }
})();
})());
}
