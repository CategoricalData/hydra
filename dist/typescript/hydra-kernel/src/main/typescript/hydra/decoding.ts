// Note: this is an automatically generated file. Do not edit.

/**
 * Functions for generating term decoders from type modules
 */



import * as Annotations from "./annotations.js";
import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Constants from "./constants.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as DecodeCore from "./decode/core.js";
import * as EncodeCore from "./encode/core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as ExtractCore from "./extract/core.js";
import * as Formatting from "./formatting.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as Lexical from "./lexical.js";
import * as LibEithers from "./lib/eithers.js";
import * as LibLists from "./lib/lists.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibSets from "./lib/sets.js";
import * as LibStrings from "./lib/strings.js";
import * as Names from "./names.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Predicates from "./predicates.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Rewriting from "./rewriting.js";
import * as ShowCore from "./show/core.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function collectForallVariables(typ: Core.Type): ReadonlyArray<Core.Name> {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => collectForallVariables(((_x) => _x.body)(at)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => LibLists.cons(((_x) => _x.parameter)(ft))(collectForallVariables(((_x) => _x.body)(ft))))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function collectOrdConstrainedVariables(typ: Core.Type): ReadonlyArray<Core.Name> {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => collectOrdConstrainedVariables(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => LibLists.concat2(collectOrdConstrainedVariables(((_x) => _x.function)(appType)))(collectOrdConstrainedVariables(((_x) => _x.argument)(appType))))((_m as any).value);
    case "either": return ((et: Core.EitherType) => LibLists.concat2(collectOrdConstrainedVariables(((_x) => _x.left)(et)))(collectOrdConstrainedVariables(((_x) => _x.right)(et))))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => collectOrdConstrainedVariables(((_x) => _x.body)(ft)))((_m as any).value);
    case "list": return ((elemType: Core.Type) => collectOrdConstrainedVariables(elemType))((_m as any).value);
    case "map": return ((mt: Core.MapType) => LibLists.concat([collectTypeVariablesFromType(((_x) => _x.keys)(mt)), collectOrdConstrainedVariables(((_x) => _x.keys)(mt)), collectOrdConstrainedVariables(((_x) => _x.values)(mt))]))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => collectOrdConstrainedVariables(elemType))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => LibLists.concat2(collectOrdConstrainedVariables(((_x) => _x.first)(pt)))(collectOrdConstrainedVariables(((_x) => _x.second)(pt))))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibLists.concat(LibLists.map(((ft: Core.FieldType) => collectOrdConstrainedVariables(((_x) => _x.type)(ft))))(rt)))((_m as any).value);
    case "set": return ((elemType: Core.Type) => LibLists.concat2(collectTypeVariablesFromType(elemType))(collectOrdConstrainedVariables(elemType)))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => LibLists.concat(LibLists.map(((ft: Core.FieldType) => collectOrdConstrainedVariables(((_x) => _x.type)(ft))))(rt)))((_m as any).value);
    case "wrap": return ((wt: Core.Type) => collectOrdConstrainedVariables(wt))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function collectTypeVariables(typ: Core.Type): ReadonlyArray<Core.Name> {
  return collectForallVariables(typ);
}

export function collectTypeVariablesFromType(typ: Core.Type): ReadonlyArray<Core.Name> {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => collectTypeVariablesFromType(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => LibLists.concat2(collectTypeVariablesFromType(((_x) => _x.function)(appType)))(collectTypeVariablesFromType(((_x) => _x.argument)(appType))))((_m as any).value);
    case "either": return ((et: Core.EitherType) => LibLists.concat2(collectTypeVariablesFromType(((_x) => _x.left)(et)))(collectTypeVariablesFromType(((_x) => _x.right)(et))))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => collectTypeVariablesFromType(((_x) => _x.body)(ft)))((_m as any).value);
    case "list": return ((elemType: Core.Type) => collectTypeVariablesFromType(elemType))((_m as any).value);
    case "map": return ((mt: Core.MapType) => LibLists.concat2(collectTypeVariablesFromType(((_x) => _x.keys)(mt)))(collectTypeVariablesFromType(((_x) => _x.values)(mt))))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => collectTypeVariablesFromType(elemType))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => LibLists.concat2(collectTypeVariablesFromType(((_x) => _x.first)(pt)))(collectTypeVariablesFromType(((_x) => _x.second)(pt))))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibLists.concat(LibLists.map(((ft: Core.FieldType) => collectTypeVariablesFromType(((_x) => _x.type)(ft))))(rt)))((_m as any).value);
    case "set": return ((elemType: Core.Type) => collectTypeVariablesFromType(elemType))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => LibLists.concat(LibLists.map(((ft: Core.FieldType) => collectTypeVariablesFromType(((_x) => _x.type)(ft))))(rt)))((_m as any).value);
    case "variable": return ((name: Core.Name) => [name])((_m as any).value);
    case "wrap": return ((wt: Core.Type) => collectTypeVariablesFromType(wt))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function decodeBinding<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Binding) => Errors.DecodingError | Core.Binding)) {
  return ((graph: Graph.Graph) => ((b: Core.Binding) => LibEithers.bind(DecodeCore.type(graph)(((_x) => _x.term)(b)))(((typ: Core.Type) => ({ tag: "right", value: ({
    name: decodeBindingName(((_x) => _x.name)(b)),
    term: decodeTypeNamed(((_x) => _x.name)(b))(typ),
    type: decoderTypeSchemeNamed(((_x) => _x.name)(b))(typ)
  }) })))));
}

export function decodeBindingName(n: Core.Name): Core.Name {
  return LibLogic.ifElse(LibLogic.not(LibLists.null_(LibLists.tail(LibStrings.splitOn(".")(((_x) => _x)(n))))))(LibStrings.intercalate(".")(LibLists.concat2(["hydra", "decode"])(LibLists.concat2(LibLists.tail(LibLists.init(LibStrings.splitOn(".")(((_x) => _x)(n)))))([Formatting.decapitalize(Names.localNameOf(n))]))))(Formatting.decapitalize(Names.localNameOf(n)));
}

export function decodeEitherType(et: Core.EitherType): Core.Term {
  return (() => {
  const leftDecoder = decodeType(((_x) => _x.left)(et));
  return (() => {
  const rightDecoder = decodeType(((_x) => _x.right)(et));
  return ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.decodeEither" }),
    argument: leftDecoder
  }) }),
    argument: rightDecoder
  }) });
})();
})();
}

export function decodeForallType(ft: Core.ForallType): Core.Term {
  return ({ tag: "lambda", value: ({
    parameter: decodeBindingName(((_x) => _x.parameter)(ft)),
    domain: null,
    body: decodeType(((_x) => _x.body)(ft))
  }) });
}

export function decodeListType(elemType: Core.Type): Core.Term {
  return (() => {
  const elemDecoder = decodeType(elemType);
  return ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.decodeList" }),
    argument: elemDecoder
  }) });
})();
}

export function decodeLiteralType(lt: Core.LiteralType): Core.Term {
  return (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected binary literal" }) })
  }) }) }) }),
    cases: [({
    name: "binary",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "b" }) }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "boolean": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected boolean literal" }) })
  }) }) }) }),
    cases: [({
    name: "boolean",
    term: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "b" }) }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "float": return ((ft: Core.FloatType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "bigfloat", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "float",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.FloatValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "bigfloat", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "bigfloat",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "f" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "float32": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "float32", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "float",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.FloatValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "float32", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "float32",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "f" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "float64": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "float64", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "float",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.FloatValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "float64", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "float64",
    term: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "f" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
  }
})())((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "bigint": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "bigint", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "integer",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.IntegerValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "bigint", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "bigint",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "i" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "int8": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "int8", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "integer",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.IntegerValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "int8", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "int8",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "i" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "int16": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "int16", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "integer",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.IntegerValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "int16", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "int16",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "i" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "int32": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "int32", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "integer",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.IntegerValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "int32", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "int32",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "i" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "int64": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "int64", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "integer",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.IntegerValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "int64", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "int64",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "i" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "uint8": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "uint8", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "integer",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.IntegerValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "uint8", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "uint8",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "i" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "uint16": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "uint16", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "integer",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.IntegerValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "uint16", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "uint16",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "i" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "uint32": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "uint32", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "integer",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.IntegerValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "uint32", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "uint32",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "i" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
    case "uint64": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "uint64", " literal"]) }) })
  }) }) }) }),
    cases: [({
    name: "integer",
    term: ({ tag: "cases", value: ({
    typeName: "hydra.core.IntegerValue",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: LibStrings.cat(["expected ", "uint64", " value"]) }) })
  }) }) }) }),
    cases: [({
    name: "uint64",
    term: ({ tag: "lambda", value: ({
    parameter: "i",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "i" }) }) })
  }) })
  })]
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
  }
})())((_m as any).value);
    case "string": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected literal" }) })
  }) }) }) }),
    cases: [({
    name: "literal",
    term: ({ tag: "lambda", value: ({
    parameter: "v",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Literal",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected string literal" }) })
  }) }) }) }),
    cases: [({
    name: "string",
    term: ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "right", value: ({ tag: "variable", value: "s" }) }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) }))((_m as any).value);
  }
})();
}

export function decodeMapType(mt: Core.MapType): Core.Term {
  return (() => {
  const keyDecoder = decodeType(((_x) => _x.keys)(mt));
  return (() => {
  const valDecoder = decodeType(((_x) => _x.values)(mt));
  return ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.decodeMap" }),
    argument: keyDecoder
  }) }),
    argument: valDecoder
  }) });
})();
})();
}

export function decodeMaybeType(elemType: Core.Type): Core.Term {
  return (() => {
  const elemDecoder = decodeType(elemType);
  return ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.decodeMaybe" }),
    argument: elemDecoder
  }) });
})();
}

export function decodeModule(cx: Context.Context): ((x: Graph.Graph) => ((x: Packaging.Module) => Errors.Error | Packaging.Module | null)) {
  return ((graph: Graph.Graph) => ((mod: Packaging.Module) => LibEithers.bind(filterTypeBindings(cx)(graph)(LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "type": return ((td: Packaging.TypeDefinition) => (() => {
  const schemaTerm = ({ tag: "variable", value: "hydra.core.Type" });
  return (() => {
  const dataTerm = Annotations.normalizeTermAnnotations(({ tag: "annotated", value: ({
    body: EncodeCore.type(((_x) => _x.type)(((_x) => _x.type)(td))),
    annotation: LibMaps.fromList([[Constants.key_type, schemaTerm]])
  }) }));
  return ({
    name: ((_x) => _x.name)(td),
    term: dataTerm,
    type: ({
    variables: [],
    type: ({ tag: "variable", value: "hydra.core.Type" }),
    constraints: null
  })
  });
})();
})())((_m as any).value);
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(mod)))))(((typeBindings: ReadonlyArray<Core.Binding>) => LibLogic.ifElse(LibLists.null_(typeBindings))(({ tag: "right", value: null }))(LibEithers.bind(LibEithers.mapList(((b: Core.Binding) => LibEithers.bimap(((_e: Errors.DecodingError) => ({ tag: "decoding", value: _e })))(((x: Core.Binding) => x))(decodeBinding(cx)(graph)(b))))(typeBindings))(((decodedBindings: ReadonlyArray<Core.Binding>) => (() => {
  const decodedTypeDeps = LibLists.map(decodeNamespace)(((_x) => _x.typeDependencies)(mod));
  return (() => {
  const decodedTermDeps = LibLists.map(decodeNamespace)(((_x) => _x.termDependencies)(mod));
  return (() => {
  const allDecodedDeps = LibLists.nub(LibLists.concat2(decodedTypeDeps)(decodedTermDeps));
  return ({ tag: "right", value: ({
    namespace: decodeNamespace(((_x) => _x.namespace)(mod)),
    definitions: LibLists.map(((b: Core.Binding) => ({ tag: "term", value: ({
    name: ((_x) => _x.name)(b),
    term: ((_x) => _x.term)(b),
    type: ((_x) => _x.type)(b)
  }) })))(decodedBindings),
    termDependencies: LibLists.concat2(["hydra.extract.core", "hydra.lexical", "hydra.rewriting"])(allDecodedDeps),
    typeDependencies: [((_x) => _x.namespace)(mod), "hydra.util"],
    description: LibStrings.cat(["Term decoders for ", ((_x) => _x)(((_x) => _x.namespace)(mod))])
  }) });
})();
})();
})())))))));
}

export function decodeNamespace(ns: Packaging.Namespace): Packaging.Namespace {
  return LibStrings.cat(["hydra.decode.", LibStrings.intercalate(".")(LibLists.tail(LibStrings.splitOn(".")(((_x) => _x)(ns))))]);
}

export function decodePairType(pt: Core.PairType): Core.Term {
  return (() => {
  const firstDecoder = decodeType(((_x) => _x.first)(pt));
  return (() => {
  const secondDecoder = decodeType(((_x) => _x.second)(pt));
  return ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.decodePair" }),
    argument: firstDecoder
  }) }),
    argument: secondDecoder
  }) });
})();
})();
}

export function decodeRecordType(rt: ReadonlyArray<Core.FieldType>): Core.Term {
  return decodeRecordTypeImpl("unknown")(rt);
}

export function decodeRecordTypeImpl(tname: Core.Name): ((x: ReadonlyArray<Core.FieldType>) => Core.Term) {
  return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const decodeFieldTerm = ((ft: Core.FieldType) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.requireField" }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(((_x) => _x.name)(ft)) }) })
  }) }),
    argument: decodeType(((_x) => _x.type)(ft))
  }) }),
    argument: ({ tag: "variable", value: "fieldMap" })
  }) }),
    argument: ({ tag: "variable", value: "cx" })
  }) }));
  return (() => {
  const localVarName = ((ft: Core.FieldType) => LibStrings.cat(["field_", ((_x) => _x)(((_x) => _x.name)(ft))]));
  return (() => {
  const toFieldLambda = ((ft: Core.FieldType) => ((body: Core.Term) => ({ tag: "lambda", value: ({
    parameter: localVarName(ft),
    domain: null,
    body: body
  }) })));
  return (() => {
  const decodeBody = LibLists.foldl(((acc: Core.Term) => ((ft: Core.FieldType) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.bind" }),
    argument: decodeFieldTerm(ft)
  }) }),
    argument: toFieldLambda(ft)(acc)
  }) }))))(({ tag: "either", value: ({ tag: "right", value: ({ tag: "record", value: ({
    typeName: tname,
    fields: LibLists.map(((ft: Core.FieldType) => ({
    name: ((_x) => _x.name)(ft),
    term: ({ tag: "variable", value: localVarName(ft) })
  })))(rt)
  }) }) }) }))(LibLists.reverse(rt));
  return ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected record" }) })
  }) }) }) }),
    cases: [({
    name: "record",
    term: ({ tag: "lambda", value: ({
    parameter: "record",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "fieldMap",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.toFieldMap" }),
    argument: ({ tag: "variable", value: "record" })
  }) }),
    type: null
  })],
    body: decodeBody
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) });
})();
})();
})();
})());
}

export function decodeRecordTypeNamed(ename: Core.Name): ((x: ReadonlyArray<Core.FieldType>) => Core.Term) {
  return ((rt: ReadonlyArray<Core.FieldType>) => decodeRecordTypeImpl(ename)(rt));
}

export function decodeSetType(elemType: Core.Type): Core.Term {
  return (() => {
  const elemDecoder = decodeType(elemType);
  return ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.decodeSet" }),
    argument: elemDecoder
  }) });
})();
}

export function decodeType(typ: Core.Type): Core.Term {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => decodeType(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => ({ tag: "application", value: ({
    function: decodeType(((_x) => _x.function)(appType)),
    argument: decodeType(((_x) => _x.argument)(appType))
  }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => decodeEitherType(et))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => decodeForallType(ft))((_m as any).value);
    case "list": return ((elemType: Core.Type) => decodeListType(elemType))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => decodeLiteralType(lt))((_m as any).value);
    case "map": return ((mt: Core.MapType) => decodeMapType(mt))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => decodeMaybeType(elemType))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => decodePairType(pt))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => decodeRecordType(rt))((_m as any).value);
    case "set": return ((elemType: Core.Type) => decodeSetType(elemType))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => decodeUnionType(rt))((_m as any).value);
    case "unit": return ((_: void) => decodeUnitType)((_m as any).value);
    case "void": return ((_: void) => decodeUnitType)((_m as any).value);
    case "wrap": return ((wt: Core.Type) => decodeWrappedType(wt))((_m as any).value);
    case "variable": return ((typeName: Core.Name) => ({ tag: "variable", value: decodeBindingName(typeName) }))((_m as any).value);
    default: return ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "unsupported type variant" }) })
  }) }) }) })
  }) })
  }) })(_m);
  }
})();
}

export function decodeTypeNamed(ename: Core.Name): ((x: Core.Type) => Core.Term) {
  return ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => decodeTypeNamed(ename)(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => ({ tag: "application", value: ({
    function: decodeType(((_x) => _x.function)(appType)),
    argument: decodeType(((_x) => _x.argument)(appType))
  }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => decodeEitherType(et))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => ({ tag: "lambda", value: ({
    parameter: decodeBindingName(((_x) => _x.parameter)(ft)),
    domain: null,
    body: decodeTypeNamed(ename)(((_x) => _x.body)(ft))
  }) }))((_m as any).value);
    case "list": return ((elemType: Core.Type) => decodeListType(elemType))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => decodeLiteralType(lt))((_m as any).value);
    case "map": return ((mt: Core.MapType) => decodeMapType(mt))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => decodeMaybeType(elemType))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => decodePairType(pt))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => decodeRecordTypeNamed(ename)(rt))((_m as any).value);
    case "set": return ((elemType: Core.Type) => decodeSetType(elemType))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => decodeUnionTypeNamed(ename)(rt))((_m as any).value);
    case "unit": return ((_: void) => decodeUnitType)((_m as any).value);
    case "void": return ((_: void) => decodeUnitType)((_m as any).value);
    case "wrap": return ((wt: Core.Type) => decodeWrappedTypeNamed(ename)(wt))((_m as any).value);
    case "variable": return ((typeName: Core.Name) => ({ tag: "variable", value: decodeBindingName(typeName) }))((_m as any).value);
    default: return ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "unsupported type variant" }) })
  }) }) }) })
  }) })
  }) })(_m);
  }
})());
}

export function decodeUnionType(rt: ReadonlyArray<Core.FieldType>): Core.Term {
  return decodeUnionTypeNamed("unknown")(rt);
}

export function decodeUnionTypeNamed(ename: Core.Name): ((x: ReadonlyArray<Core.FieldType>) => Core.Term) {
  return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const toVariantPair = ((ft: Core.FieldType) => ({ tag: "pair", value: [({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(((_x) => _x.name)(ft)) }) })
  }) }), ({ tag: "lambda", value: ({
    parameter: "input",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.map" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: ename,
    field: ({
    name: ((_x) => _x.name)(ft),
    term: ({ tag: "variable", value: "t" })
  })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: decodeType(((_x) => _x.type)(ft)),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "input" })
  }) })
  }) })
  }) })] }));
  return ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected union" }) })
  }) }) }) }),
    cases: [({
    name: "inject",
    term: ({ tag: "lambda", value: ({
    parameter: "inj",
    domain: null,
    body: ({ tag: "let", value: ({
    bindings: [({
    name: "field",
    term: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: "hydra.core.Injection",
    field: "field"
  }) }),
    argument: ({ tag: "variable", value: "inj" })
  }) }),
    type: null
  }), ({
    name: "fname",
    term: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: "hydra.core.Field",
    field: "name"
  }) }),
    argument: ({ tag: "variable", value: "field" })
  }) }),
    type: null
  }), ({
    name: "fterm",
    term: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: "hydra.core.Field",
    field: "term"
  }) }),
    argument: ({ tag: "variable", value: "field" })
  }) }),
    type: null
  }), ({
    name: "variantMap",
    term: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.fromList" }),
    argument: ({ tag: "list", value: LibLists.map(toVariantPair)(rt) })
  }) }),
    type: null
  })],
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.maybe" }),
    argument: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.strings.cat" }),
    argument: ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "no such field " }) }), ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: "hydra.core.Name" }),
    argument: ({ tag: "variable", value: "fname" })
  }) }), ({ tag: "literal", value: ({ tag: "string", value: " in union" }) })] })
  }) })
  }) }) }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "f",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "f" }),
    argument: ({ tag: "variable", value: "fterm" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.lookup" }),
    argument: ({ tag: "variable", value: "fname" })
  }) }),
    argument: ({ tag: "variable", value: "variantMap" })
  }) })
  }) })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) });
})());
}

export const decodeUnitType: Core.Term = ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "t",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.decodeUnit" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "t" })
  }) })
  }) })
  }) });

export function decodeWrappedType(wt: Core.Type): Core.Term {
  return decodeWrappedTypeNamed("unknown")(wt);
}

export function decodeWrappedTypeNamed(ename: Core.Name): ((x: Core.Type) => Core.Term) {
  return ((wt: Core.Type) => (() => {
  const bodyDecoder = decodeType(wt);
  return ({ tag: "lambda", value: ({
    parameter: "cx",
    domain: null,
    body: ({ tag: "lambda", value: ({
    parameter: "raw",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.either" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "err",
    domain: null,
    body: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "variable", value: "err" }) }) })
  }) })
  }) }),
    argument: ({ tag: "lambda", value: ({
    parameter: "stripped",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "cases", value: ({
    typeName: "hydra.core.Term",
    default: ({ tag: "either", value: ({ tag: "left", value: ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: "expected wrapped type" }) })
  }) }) }) }),
    cases: [({
    name: "wrap",
    term: ({ tag: "lambda", value: ({
    parameter: "wrappedTerm",
    domain: null,
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.map" }),
    argument: ({ tag: "lambda", value: ({
    parameter: "b",
    domain: null,
    body: ({ tag: "wrap", value: ({
    typeName: ename,
    body: ({ tag: "variable", value: "b" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: bodyDecoder,
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: "hydra.core.WrappedTerm",
    field: "body"
  }) }),
    argument: ({ tag: "variable", value: "wrappedTerm" })
  }) })
  }) })
  }) })
  }) })
  })]
  }) }),
    argument: ({ tag: "variable", value: "stripped" })
  }) })
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.extract.core.stripWithDecodingError" }),
    argument: ({ tag: "variable", value: "cx" })
  }) }),
    argument: ({ tag: "variable", value: "raw" })
  }) })
  }) })
  }) })
  }) });
})());
}

export function decoderFullResultType(typ: Core.Type): Core.Type {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => decoderFullResultType(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => ({ tag: "application", value: ({
    function: decoderFullResultType(((_x) => _x.function)(appType)),
    argument: ((_x) => _x.argument)(appType)
  }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => ({ tag: "either", value: ({
    left: decoderFullResultType(((_x) => _x.left)(et)),
    right: decoderFullResultType(((_x) => _x.right)(et))
  }) }))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => ({ tag: "application", value: ({
    function: decoderFullResultType(((_x) => _x.body)(ft)),
    argument: ({ tag: "variable", value: ((_x) => _x.parameter)(ft) })
  }) }))((_m as any).value);
    case "list": return ((elemType: Core.Type) => ({ tag: "list", value: decoderFullResultType(elemType) }))((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => ({ tag: "variable", value: "hydra.core.Literal" }))((_m as any).value);
    case "map": return ((mt: Core.MapType) => ({ tag: "map", value: ({
    keys: decoderFullResultType(((_x) => _x.keys)(mt)),
    values: decoderFullResultType(((_x) => _x.values)(mt))
  }) }))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => ({ tag: "maybe", value: decoderFullResultType(elemType) }))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => ({ tag: "pair", value: ({
    first: decoderFullResultType(((_x) => _x.first)(pt)),
    second: decoderFullResultType(((_x) => _x.second)(pt))
  }) }))((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "variable", value: "hydra.core.Term" }))((_m as any).value);
    case "set": return ((elemType: Core.Type) => ({ tag: "set", value: decoderFullResultType(elemType) }))((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "variable", value: "hydra.core.Term" }))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "unit" }))((_m as any).value);
    case "variable": return ((name: Core.Name) => ({ tag: "variable", value: name }))((_m as any).value);
    case "void": return ((_: void) => ({ tag: "void" }))((_m as any).value);
    case "wrap": return ((_: Core.Type) => ({ tag: "variable", value: "hydra.core.Term" }))((_m as any).value);
    default: return ({ tag: "variable", value: "hydra.core.Term" })(_m);
  }
})();
}

export function decoderFullResultTypeNamed(ename: Core.Name): ((x: Core.Type) => Core.Type) {
  return ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => decoderFullResultTypeNamed(ename)(((_x) => _x.body)(at)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => ({ tag: "application", value: ({
    function: decoderFullResultTypeNamed(ename)(((_x) => _x.body)(ft)),
    argument: ({ tag: "variable", value: ((_x) => _x.parameter)(ft) })
  }) }))((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "variable", value: ename }))((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "variable", value: ename }))((_m as any).value);
    case "wrap": return ((_: Core.Type) => ({ tag: "variable", value: ename }))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => ({ tag: "application", value: ({
    function: decoderFullResultType(((_x) => _x.function)(appType)),
    argument: ((_x) => _x.argument)(appType)
  }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => ({ tag: "either", value: ({
    left: decoderFullResultType(((_x) => _x.left)(et)),
    right: decoderFullResultType(((_x) => _x.right)(et))
  }) }))((_m as any).value);
    case "list": return ((elemType: Core.Type) => ({ tag: "list", value: decoderFullResultType(elemType) }))((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => ({ tag: "variable", value: "hydra.core.Literal" }))((_m as any).value);
    case "map": return ((mt: Core.MapType) => ({ tag: "map", value: ({
    keys: decoderFullResultType(((_x) => _x.keys)(mt)),
    values: decoderFullResultType(((_x) => _x.values)(mt))
  }) }))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => ({ tag: "maybe", value: decoderFullResultType(elemType) }))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => ({ tag: "pair", value: ({
    first: decoderFullResultType(((_x) => _x.first)(pt)),
    second: decoderFullResultType(((_x) => _x.second)(pt))
  }) }))((_m as any).value);
    case "set": return ((elemType: Core.Type) => ({ tag: "set", value: decoderFullResultType(elemType) }))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "unit" }))((_m as any).value);
    case "variable": return ((name: Core.Name) => ({ tag: "variable", value: name }))((_m as any).value);
    case "void": return ((_: void) => ({ tag: "void" }))((_m as any).value);
    default: return ({ tag: "variable", value: "hydra.core.Term" })(_m);
  }
})());
}

export function decoderResultType(typ: Core.Type): Core.Name {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => decoderResultType(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => decoderResultType(((_x) => _x.function)(appType)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => decoderResultType(((_x) => _x.body)(ft)))((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => "hydra.core.Literal")((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => "hydra.core.Term")((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => "hydra.core.Term")((_m as any).value);
    case "wrap": return ((_: Core.Type) => "hydra.core.Term")((_m as any).value);
    default: return "hydra.core.Term"(_m);
  }
})();
}

export function decoderType(typ: Core.Type): Core.Type {
  return (() => {
  const resultType = decoderFullResultType(typ);
  return (() => {
  const baseType = ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.graph.Graph" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.core.Term" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "hydra.errors.DecodingError" }),
    right: resultType
  }) })
  }) })
  }) });
  return prependForallDecoders(baseType)(typ);
})();
})();
}

export function decoderTypeNamed(ename: Core.Name): ((x: Core.Type) => Core.Type) {
  return ((typ: Core.Type) => (() => {
  const resultType = decoderFullResultTypeNamed(ename)(typ);
  return (() => {
  const baseType = ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.graph.Graph" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.core.Term" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "hydra.errors.DecodingError" }),
    right: resultType
  }) })
  }) })
  }) });
  return prependForallDecoders(baseType)(typ);
})();
})());
}

export function decoderTypeScheme(typ: Core.Type): Core.TypeScheme {
  return (() => {
  const typeVars = collectTypeVariables(typ);
  return (() => {
  const allOrdVars = collectOrdConstrainedVariables(typ);
  return (() => {
  const ordVars = LibLists.filter(((v: Core.Name) => LibLists.elem(v)(typeVars)))(allOrdVars);
  return (() => {
  const constraints = LibLogic.ifElse(LibLists.null_(ordVars))(null)(LibMaps.fromList(LibLists.map(((v: Core.Name) => [v, ({
    classes: LibSets.singleton("ordering")
  })]))(ordVars)));
  return ({
    variables: typeVars,
    type: decoderType(typ),
    constraints: constraints
  });
})();
})();
})();
})();
}

export function decoderTypeSchemeNamed(ename: Core.Name): ((x: Core.Type) => Core.TypeScheme) {
  return ((typ: Core.Type) => (() => {
  const typeVars = collectTypeVariables(typ);
  return (() => {
  const allOrdVars = collectOrdConstrainedVariables(typ);
  return (() => {
  const ordVars = LibLists.filter(((v: Core.Name) => LibLists.elem(v)(typeVars)))(allOrdVars);
  return (() => {
  const constraints = LibLogic.ifElse(LibLists.null_(ordVars))(null)(LibMaps.fromList(LibLists.map(((v: Core.Name) => [v, ({
    classes: LibSets.singleton("ordering")
  })]))(ordVars)));
  return ({
    variables: typeVars,
    type: decoderTypeNamed(ename)(typ),
    constraints: constraints
  });
})();
})();
})();
})());
}

export function filterTypeBindings(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Binding>) => Errors.Error | ReadonlyArray<Core.Binding>)) {
  return ((graph: Graph.Graph) => ((bindings: ReadonlyArray<Core.Binding>) => LibEithers.map(LibMaybes.cat)(LibEithers.mapList(((v1: Core.Binding) => isDecodableBinding(cx)(graph)(v1)))(LibLists.filter(Annotations.isNativeType)(bindings)))));
}

export function isDecodableBinding(cx: Context.Context): ((x: Graph.Graph) => ((x: Core.Binding) => Errors.Error | Core.Binding | null)) {
  return ((graph: Graph.Graph) => ((b: Core.Binding) => LibEithers.bind(Predicates.isSerializableByName(cx)(graph)(((_x) => _x.name)(b)))(((serializable: boolean) => ({ tag: "right", value: LibLogic.ifElse(serializable)(b)(null) })))));
}

export function prependForallDecoders(baseType: Core.Type): ((x: Core.Type) => Core.Type) {
  return ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => prependForallDecoders(baseType)(((_x) => _x.body)(at)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.graph.Graph" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.core.Term" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "hydra.errors.DecodingError" }),
    right: ({ tag: "variable", value: ((_x) => _x.parameter)(ft) })
  }) })
  }) })
  }) }),
    codomain: prependForallDecoders(baseType)(((_x) => _x.body)(ft))
  }) }))((_m as any).value);
    default: return baseType(_m);
  }
})());
}
