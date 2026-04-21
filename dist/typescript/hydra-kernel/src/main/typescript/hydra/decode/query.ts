// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.query
 */



import * as Core from "../core.js";
import * as DecodeCore from "./core.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibStrings from "../lib/strings.js";
import * as Query from "../query.js";
import * as Rewriting from "../rewriting.js";
import * as Util from "../util.js";

export function comparisonConstraint(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.ComparisonConstraint) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["equal", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "equal", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["notEqual", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "notEqual", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["lessThan", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "lessThan", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["greaterThan", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "greaterThan", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["lessThanOrEqual", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "lessThanOrEqual", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["greaterThanOrEqual", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "greaterThanOrEqual", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Query.ComparisonConstraint)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function edge(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.Edge) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("type")(DecodeCore.name)(fieldMap)(cx))(((field_type: Core.Name) => LibEithers.bind(ExtractCore.requireField("out")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(DecodeCore.name)(v1)(v2))))(fieldMap)(cx))(((field_out: Core.Name | null) => LibEithers.bind(ExtractCore.requireField("in")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(DecodeCore.name)(v1)(v2))))(fieldMap)(cx))(((field_in: Core.Name | null) => ({ tag: "right", value: ({
    type: field_type,
    out: field_out,
    in: field_in
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function graphPattern(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.GraphPattern) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("graph")(DecodeCore.name)(fieldMap)(cx))(((field_graph: Core.Name) => LibEithers.bind(ExtractCore.requireField("patterns")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(pattern)(v1)(v2))))(fieldMap)(cx))(((field_patterns: ReadonlyArray<Query.Pattern>) => ({ tag: "right", value: ({
    graph: field_graph,
    patterns: field_patterns
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function node(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.Node) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["term", ((input: Core.Term) => LibEithers.map(((t: Core.Term) => ({ tag: "term", value: t })))(DecodeCore.term(cx)(input)))], ["variable", ((input: Core.Term) => LibEithers.map(((t: Query.Variable) => ({ tag: "variable", value: t })))(variable(cx)(input)))], ["wildcard", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "wildcard", value: t })))(ExtractCore.decodeUnit(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Query.Node)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function path(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.Path) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["step", ((input: Core.Term) => LibEithers.map(((t: Query.Step) => ({ tag: "step", value: t })))(step(cx)(input)))], ["regex", ((input: Core.Term) => LibEithers.map(((t: Query.RegexSequence) => ({ tag: "regex", value: t })))(regexSequence(cx)(input)))], ["inverse", ((input: Core.Term) => LibEithers.map(((t: Query.Path) => ({ tag: "inverse", value: t })))(path(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Query.Path)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function pathEquation(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.PathEquation) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("left")(path)(fieldMap)(cx))(((field_left: Query.Path) => LibEithers.bind(ExtractCore.requireField("right")(path)(fieldMap)(cx))(((field_right: Query.Path) => ({ tag: "right", value: ({
    left: field_left,
    right: field_right
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function pattern(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.Pattern) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["triple", ((input: Core.Term) => LibEithers.map(((t: Query.TriplePattern) => ({ tag: "triple", value: t })))(triplePattern(cx)(input)))], ["negation", ((input: Core.Term) => LibEithers.map(((t: Query.Pattern) => ({ tag: "negation", value: t })))(pattern(cx)(input)))], ["conjunction", ((input: Core.Term) => LibEithers.map(((t: ReadonlyArray<Query.Pattern>) => ({ tag: "conjunction", value: t })))(ExtractCore.decodeList(pattern)(cx)(input)))], ["disjunction", ((input: Core.Term) => LibEithers.map(((t: ReadonlyArray<Query.Pattern>) => ({ tag: "disjunction", value: t })))(ExtractCore.decodeList(pattern)(cx)(input)))], ["graph", ((input: Core.Term) => LibEithers.map(((t: Query.GraphPattern) => ({ tag: "graph", value: t })))(graphPattern(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Query.Pattern)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function patternImplication(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.PatternImplication) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("antecedent")(pattern)(fieldMap)(cx))(((field_antecedent: Query.Pattern) => LibEithers.bind(ExtractCore.requireField("consequent")(pattern)(fieldMap)(cx))(((field_consequent: Query.Pattern) => ({ tag: "right", value: ({
    antecedent: field_antecedent,
    consequent: field_consequent
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function query(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.Query) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("variables")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(variable)(v1)(v2))))(fieldMap)(cx))(((field_variables: ReadonlyArray<Query.Variable>) => LibEithers.bind(ExtractCore.requireField("patterns")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(pattern)(v1)(v2))))(fieldMap)(cx))(((field_patterns: ReadonlyArray<Query.Pattern>) => ({ tag: "right", value: ({
    variables: field_variables,
    patterns: field_patterns
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function range(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.Range) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("min")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_min: number) => LibEithers.bind(ExtractCore.requireField("max")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_max: number) => ({ tag: "right", value: ({
    min: field_min,
    max: field_max
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function regexQuantifier(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.RegexQuantifier) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["one", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "one", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["zeroOrOne", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "zeroOrOne", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["zeroOrMore", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "zeroOrMore", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["oneOrMore", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "oneOrMore", value: t })))(ExtractCore.decodeUnit(cx)(input)))], ["exactly", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "exactly", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["atLeast", ((input: Core.Term) => LibEithers.map(((t: number) => ({ tag: "atLeast", value: t })))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx)(input))))], ["range", ((input: Core.Term) => LibEithers.map(((t: Query.Range) => ({ tag: "range", value: t })))(range(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Query.RegexQuantifier)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function regexSequence(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.RegexSequence) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("path")(path)(fieldMap)(cx))(((field_path: Query.Path) => LibEithers.bind(ExtractCore.requireField("quantifier")(regexQuantifier)(fieldMap)(cx))(((field_quantifier: Query.RegexQuantifier) => ({ tag: "right", value: ({
    path: field_path,
    quantifier: field_quantifier
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function step(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.Step) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["edge", ((input: Core.Term) => LibEithers.map(((t: Query.Edge) => ({ tag: "edge", value: t })))(edge(cx)(input)))], ["project", ((input: Core.Term) => LibEithers.map(((t: Core.Projection) => ({ tag: "project", value: t })))(DecodeCore.projection(cx)(input)))], ["compare", ((input: Core.Term) => LibEithers.map(((t: Query.ComparisonConstraint) => ({ tag: "compare", value: t })))(comparisonConstraint(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Query.Step)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function triplePattern(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.TriplePattern) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("subject")(node)(fieldMap)(cx))(((field_subject: Query.Node) => LibEithers.bind(ExtractCore.requireField("predicate")(path)(fieldMap)(cx))(((field_predicate: Query.Path) => LibEithers.bind(ExtractCore.requireField("object")(node)(fieldMap)(cx))(((field_object: Query.Node) => ({ tag: "right", value: ({
    subject: field_subject,
    predicate: field_predicate,
    object: field_object
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function variable(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Query.Variable) {
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
