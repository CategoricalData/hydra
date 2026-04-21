// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.error.checking
 */



import * as Core from "../../core.js";
import * as DecodeCore from "../core.js";
import * as DecodePaths from "../paths.js";
import * as DecodeTyping from "../typing.js";
import * as DecodeVariants from "../variants.js";
import * as ErrorChecking from "../../error/checking.js";
import * as Errors from "../../errors.js";
import * as ExtractCore from "../../extract/core.js";
import * as Graph from "../../graph.js";
import * as Lexical from "../../lexical.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibMaps from "../../lib/maps.js";
import * as LibMaybes from "../../lib/maybes.js";
import * as LibStrings from "../../lib/strings.js";
import * as Rewriting from "../../rewriting.js";
import * as Util from "../../util.js";

export function checkingError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.CheckingError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["incorrectUnification", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.IncorrectUnificationError) => ({ tag: "incorrectUnification", value: t })))(incorrectUnificationError(cx)(input)))], ["notAForallType", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.NotAForallTypeError) => ({ tag: "notAForallType", value: t })))(notAForallTypeError(cx)(input)))], ["notAFunctionType", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.NotAFunctionTypeError) => ({ tag: "notAFunctionType", value: t })))(notAFunctionTypeError(cx)(input)))], ["other", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.OtherCheckingError) => ({ tag: "other", value: t })))(otherCheckingError(cx)(input)))], ["typeArityMismatch", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.TypeArityMismatchError) => ({ tag: "typeArityMismatch", value: t })))(typeArityMismatchError(cx)(input)))], ["typeMismatch", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.TypeMismatchError) => ({ tag: "typeMismatch", value: t })))(typeMismatchError(cx)(input)))], ["unboundTypeVariables", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.UnboundTypeVariablesError) => ({ tag: "unboundTypeVariables", value: t })))(unboundTypeVariablesError(cx)(input)))], ["undefinedTermVariable", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.UndefinedTermVariableCheckingError) => ({ tag: "undefinedTermVariable", value: t })))(undefinedTermVariableCheckingError(cx)(input)))], ["unequalTypes", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.UnequalTypesError) => ({ tag: "unequalTypes", value: t })))(unequalTypesError(cx)(input)))], ["unsupportedTermVariant", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.UnsupportedTermVariantError) => ({ tag: "unsupportedTermVariant", value: t })))(unsupportedTermVariantError(cx)(input)))], ["untypedLambda", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.UntypedLambdaError) => ({ tag: "untypedLambda", value: t })))(untypedLambdaError(cx)(input)))], ["untypedLetBinding", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.UntypedLetBindingError) => ({ tag: "untypedLetBinding", value: t })))(untypedLetBindingError(cx)(input)))], ["untypedTermVariable", ((input: Core.Term) => LibEithers.map(((t: ErrorChecking.UntypedTermVariableCheckingError) => ({ tag: "untypedTermVariable", value: t })))(untypedTermVariableCheckingError(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | ErrorChecking.CheckingError)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function incorrectUnificationError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.IncorrectUnificationError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("substitution")(DecodeTyping.typeSubst)(fieldMap)(cx))(((field_substitution: hydra.typing.TypeSubst) => ({ tag: "right", value: ({
    substitution: field_substitution
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function notAForallTypeError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.NotAForallTypeError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("type")(DecodeCore.type)(fieldMap)(cx))(((field_type: Core.Type) => LibEithers.bind(ExtractCore.requireField("typeArguments")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(DecodeCore.type)(v1)(v2))))(fieldMap)(cx))(((field_typeArguments: ReadonlyArray<Core.Type>) => ({ tag: "right", value: ({
    type: field_type,
    typeArguments: field_typeArguments
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function notAFunctionTypeError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.NotAFunctionTypeError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("type")(DecodeCore.type)(fieldMap)(cx))(((field_type: Core.Type) => ({ tag: "right", value: ({
    type: field_type
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function otherCheckingError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.OtherCheckingError) {
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

export function typeArityMismatchError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.TypeArityMismatchError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("type")(DecodeCore.type)(fieldMap)(cx))(((field_type: Core.Type) => LibEithers.bind(ExtractCore.requireField("expectedArity")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_expectedArity: number) => LibEithers.bind(ExtractCore.requireField("actualArity")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_actualArity: number) => LibEithers.bind(ExtractCore.requireField("typeArguments")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(DecodeCore.type)(v1)(v2))))(fieldMap)(cx))(((field_typeArguments: ReadonlyArray<Core.Type>) => ({ tag: "right", value: ({
    type: field_type,
    expectedArity: field_expectedArity,
    actualArity: field_actualArity,
    typeArguments: field_typeArguments
  }) })))))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function typeMismatchError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.TypeMismatchError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("expectedType")(DecodeCore.type)(fieldMap)(cx))(((field_expectedType: Core.Type) => LibEithers.bind(ExtractCore.requireField("actualType")(DecodeCore.type)(fieldMap)(cx))(((field_actualType: Core.Type) => ({ tag: "right", value: ({
    expectedType: field_expectedType,
    actualType: field_actualType
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function unboundTypeVariablesError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.UnboundTypeVariablesError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("variables")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeSet(DecodeCore.name)(v1)(v2))))(fieldMap)(cx))(((field_variables: ReadonlySet<Core.Name>) => LibEithers.bind(ExtractCore.requireField("type")(DecodeCore.type)(fieldMap)(cx))(((field_type: Core.Type) => ({ tag: "right", value: ({
    variables: field_variables,
    type: field_type
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function undefinedTermVariableCheckingError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.UndefinedTermVariableCheckingError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("path")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_path: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    path: field_path,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function unequalTypesError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.UnequalTypesError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("types")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(DecodeCore.type)(v1)(v2))))(fieldMap)(cx))(((field_types: ReadonlyArray<Core.Type>) => LibEithers.bind(ExtractCore.requireField("description")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_description: string) => ({ tag: "right", value: ({
    types: field_types,
    description: field_description
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function unsupportedTermVariantError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.UnsupportedTermVariantError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("termVariant")(DecodeVariants.termVariant)(fieldMap)(cx))(((field_termVariant: hydra.variants.TermVariant) => ({ tag: "right", value: ({
    termVariant: field_termVariant
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function untypedLambdaError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.UntypedLambdaError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return ({ tag: "right", value: ({

  }) });
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function untypedLetBindingError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.UntypedLetBindingError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("binding")(DecodeCore.binding)(fieldMap)(cx))(((field_binding: Core.Binding) => ({ tag: "right", value: ({
    binding: field_binding
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function untypedTermVariableCheckingError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorChecking.UntypedTermVariableCheckingError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("path")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_path: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    path: field_path,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}
