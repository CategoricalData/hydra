// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.errors
 */



import * as Core from "../core.js";
import * as DecodeContext from "./context.js";
import * as DecodeCore from "./core.js";
import * as DecodeErrorChecking from "./error/checking.js";
import * as DecodeErrorCore from "./error/core.js";
import * as DecodePaths from "./paths.js";
import * as DecodeTyping from "./typing.js";
import * as DecodeVariants from "./variants.js";
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

export function decodingError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.DecodingError) {
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

export function emptyListError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | void) {
  return ((t: Core.Term) => ExtractCore.decodeUnit(cx)(t));
}

export function error(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.Error) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["checking", ((input: Core.Term) => LibEithers.map(((t: hydra.error.checking.CheckingError) => ({ tag: "checking", value: t })))(DecodeErrorChecking.checkingError(cx)(input)))], ["decoding", ((input: Core.Term) => LibEithers.map(((t: Errors.DecodingError) => ({ tag: "decoding", value: t })))(decodingError(cx)(input)))], ["duplicateBinding", ((input: Core.Term) => LibEithers.map(((t: hydra.error.core.DuplicateBindingError) => ({ tag: "duplicateBinding", value: t })))(DecodeErrorCore.duplicateBindingError(cx)(input)))], ["duplicateField", ((input: Core.Term) => LibEithers.map(((t: hydra.error.core.DuplicateFieldError) => ({ tag: "duplicateField", value: t })))(DecodeErrorCore.duplicateFieldError(cx)(input)))], ["extraction", ((input: Core.Term) => LibEithers.map(((t: Errors.ExtractionError) => ({ tag: "extraction", value: t })))(extractionError(cx)(input)))], ["inference", ((input: Core.Term) => LibEithers.map(((t: Errors.InferenceError) => ({ tag: "inference", value: t })))(inferenceError(cx)(input)))], ["other", ((input: Core.Term) => LibEithers.map(((t: Errors.OtherError) => ({ tag: "other", value: t })))(otherError(cx)(input)))], ["resolution", ((input: Core.Term) => LibEithers.map(((t: Errors.ResolutionError) => ({ tag: "resolution", value: t })))(resolutionError(cx)(input)))], ["undefinedField", ((input: Core.Term) => LibEithers.map(((t: hydra.error.core.UndefinedFieldError) => ({ tag: "undefinedField", value: t })))(DecodeErrorCore.undefinedFieldError(cx)(input)))], ["undefinedTermVariable", ((input: Core.Term) => LibEithers.map(((t: hydra.error.core.UndefinedTermVariableError) => ({ tag: "undefinedTermVariable", value: t })))(DecodeErrorCore.undefinedTermVariableError(cx)(input)))], ["untypedTermVariable", ((input: Core.Term) => LibEithers.map(((t: hydra.error.core.UntypedTermVariableError) => ({ tag: "untypedTermVariable", value: t })))(DecodeErrorCore.untypedTermVariableError(cx)(input)))], ["unexpectedTermVariant", ((input: Core.Term) => LibEithers.map(((t: hydra.error.core.UnexpectedTermVariantError) => ({ tag: "unexpectedTermVariant", value: t })))(DecodeErrorCore.unexpectedTermVariantError(cx)(input)))], ["unexpectedTypeVariant", ((input: Core.Term) => LibEithers.map(((t: hydra.error.core.UnexpectedTypeVariantError) => ({ tag: "unexpectedTypeVariant", value: t })))(DecodeErrorCore.unexpectedTypeVariantError(cx)(input)))], ["unification", ((input: Core.Term) => LibEithers.map(((t: Errors.UnificationError) => ({ tag: "unification", value: t })))(unificationError(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Errors.Error)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function extractionError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.ExtractionError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["emptyList", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "emptyList", value: t })))(emptyListError(cx)(input)))], ["multipleBindings", ((input: Core.Term) => LibEithers.map(((t: Errors.MultipleBindingsError) => ({ tag: "multipleBindings", value: t })))(multipleBindingsError(cx)(input)))], ["multipleFields", ((input: Core.Term) => LibEithers.map(((t: Errors.MultipleFieldsError) => ({ tag: "multipleFields", value: t })))(multipleFieldsError(cx)(input)))], ["noMatchingField", ((input: Core.Term) => LibEithers.map(((t: Errors.NoMatchingFieldError) => ({ tag: "noMatchingField", value: t })))(noMatchingFieldError(cx)(input)))], ["noSuchBinding", ((input: Core.Term) => LibEithers.map(((t: Errors.NoSuchBindingError) => ({ tag: "noSuchBinding", value: t })))(noSuchBindingError(cx)(input)))], ["notEnoughCases", ((input: Core.Term) => LibEithers.map(((t: void) => ({ tag: "notEnoughCases", value: t })))(notEnoughCasesError(cx)(input)))], ["unexpectedShape", ((input: Core.Term) => LibEithers.map(((t: Errors.UnexpectedShapeError) => ({ tag: "unexpectedShape", value: t })))(unexpectedShapeError(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Errors.ExtractionError)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function inferenceError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.InferenceError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["checking", ((input: Core.Term) => LibEithers.map(((t: hydra.error.checking.CheckingError) => ({ tag: "checking", value: t })))(DecodeErrorChecking.checkingError(cx)(input)))], ["other", ((input: Core.Term) => LibEithers.map(((t: Errors.OtherInferenceError) => ({ tag: "other", value: t })))(otherInferenceError(cx)(input)))], ["unification", ((input: Core.Term) => LibEithers.map(((t: Errors.UnificationInferenceError) => ({ tag: "unification", value: t })))(unificationInferenceError(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Errors.InferenceError)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function multipleBindingsError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.MultipleBindingsError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    name: field_name
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function multipleFieldsError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.MultipleFieldsError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("fieldName")(DecodeCore.name)(fieldMap)(cx))(((field_fieldName: Core.Name) => ({ tag: "right", value: ({
    fieldName: field_fieldName
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function noMatchingFieldError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.NoMatchingFieldError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("fieldName")(DecodeCore.name)(fieldMap)(cx))(((field_fieldName: Core.Name) => ({ tag: "right", value: ({
    fieldName: field_fieldName
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function noSuchBindingError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.NoSuchBindingError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    name: field_name
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function noSuchPrimitiveError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.NoSuchPrimitiveError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    name: field_name
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function notEnoughCasesError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | void) {
  return ((t: Core.Term) => ExtractCore.decodeUnit(cx)(t));
}

export function otherError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.OtherError) {
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

export function otherInferenceError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.OtherInferenceError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("path")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_path: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("message")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_message: string) => ({ tag: "right", value: ({
    path: field_path,
    message: field_message
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function otherResolutionError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.OtherResolutionError) {
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

export function resolutionError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.ResolutionError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["noSuchBinding", ((input: Core.Term) => LibEithers.map(((t: Errors.NoSuchBindingError) => ({ tag: "noSuchBinding", value: t })))(noSuchBindingError(cx)(input)))], ["noSuchPrimitive", ((input: Core.Term) => LibEithers.map(((t: Errors.NoSuchPrimitiveError) => ({ tag: "noSuchPrimitive", value: t })))(noSuchPrimitiveError(cx)(input)))], ["noMatchingField", ((input: Core.Term) => LibEithers.map(((t: Errors.NoMatchingFieldError) => ({ tag: "noMatchingField", value: t })))(noMatchingFieldError(cx)(input)))], ["other", ((input: Core.Term) => LibEithers.map(((t: Errors.OtherResolutionError) => ({ tag: "other", value: t })))(otherResolutionError(cx)(input)))], ["unexpectedShape", ((input: Core.Term) => LibEithers.map(((t: Errors.UnexpectedShapeError) => ({ tag: "unexpectedShape", value: t })))(unexpectedShapeError(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | Errors.ResolutionError)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function unexpectedShapeError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.UnexpectedShapeError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("expected")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_expected: string) => LibEithers.bind(ExtractCore.requireField("actual")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_actual: string) => ({ tag: "right", value: ({
    expected: field_expected,
    actual: field_actual
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function unificationError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.UnificationError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("leftType")(DecodeCore.type)(fieldMap)(cx))(((field_leftType: Core.Type) => LibEithers.bind(ExtractCore.requireField("rightType")(DecodeCore.type)(fieldMap)(cx))(((field_rightType: Core.Type) => LibEithers.bind(ExtractCore.requireField("message")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_message: string) => ({ tag: "right", value: ({
    leftType: field_leftType,
    rightType: field_rightType,
    message: field_message
  }) })))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function unificationInferenceError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Errors.UnificationInferenceError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("path")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_path: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("cause")(unificationError)(fieldMap)(cx))(((field_cause: Errors.UnificationError) => ({ tag: "right", value: ({
    path: field_path,
    cause: field_cause
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}
