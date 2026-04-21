// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.error.core
 */



import * as Core from "../../core.js";
import * as DecodeCore from "../core.js";
import * as DecodePaths from "../paths.js";
import * as DecodeVariants from "../variants.js";
import * as ErrorCore from "../../error/core.js";
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

export function constantConditionError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.ConstantConditionError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("value")(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(fieldMap)(cx))(((field_value: boolean) => ({ tag: "right", value: ({
    location: field_location,
    value: field_value
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function duplicateBindingError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.DuplicateBindingError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function duplicateFieldError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.DuplicateFieldError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function duplicateRecordTypeFieldNamesError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.DuplicateRecordTypeFieldNamesError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function duplicateUnionTypeFieldNamesError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.DuplicateUnionTypeFieldNamesError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function emptyCaseStatementError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.EmptyCaseStatementError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("typeName")(DecodeCore.name)(fieldMap)(cx))(((field_typeName: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    typeName: field_typeName
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function emptyLetBindingsError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.EmptyLetBindingsError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => ({ tag: "right", value: ({
    location: field_location
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function emptyRecordTypeError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.EmptyRecordTypeError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => ({ tag: "right", value: ({
    location: field_location
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function emptyTermAnnotationError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.EmptyTermAnnotationError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => ({ tag: "right", value: ({
    location: field_location
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function emptyTypeAnnotationError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.EmptyTypeAnnotationError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => ({ tag: "right", value: ({
    location: field_location
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function emptyTypeNameInTermError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.EmptyTypeNameInTermError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => ({ tag: "right", value: ({
    location: field_location
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function emptyUnionTypeError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.EmptyUnionTypeError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => ({ tag: "right", value: ({
    location: field_location
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function invalidForallParameterNameError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.InvalidForallParameterNameError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function invalidLambdaParameterNameError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.InvalidLambdaParameterNameError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function invalidLetBindingNameError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.InvalidLetBindingNameError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function invalidTermError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.InvalidTermError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["constantCondition", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.ConstantConditionError) => ({ tag: "constantCondition", value: t })))(constantConditionError(cx)(input)))], ["duplicateBinding", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.DuplicateBindingError) => ({ tag: "duplicateBinding", value: t })))(duplicateBindingError(cx)(input)))], ["duplicateField", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.DuplicateFieldError) => ({ tag: "duplicateField", value: t })))(duplicateFieldError(cx)(input)))], ["emptyCaseStatement", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.EmptyCaseStatementError) => ({ tag: "emptyCaseStatement", value: t })))(emptyCaseStatementError(cx)(input)))], ["emptyLetBindings", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.EmptyLetBindingsError) => ({ tag: "emptyLetBindings", value: t })))(emptyLetBindingsError(cx)(input)))], ["emptyTermAnnotation", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.EmptyTermAnnotationError) => ({ tag: "emptyTermAnnotation", value: t })))(emptyTermAnnotationError(cx)(input)))], ["emptyTypeNameInTerm", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.EmptyTypeNameInTermError) => ({ tag: "emptyTypeNameInTerm", value: t })))(emptyTypeNameInTermError(cx)(input)))], ["invalidLambdaParameterName", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.InvalidLambdaParameterNameError) => ({ tag: "invalidLambdaParameterName", value: t })))(invalidLambdaParameterNameError(cx)(input)))], ["invalidLetBindingName", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.InvalidLetBindingNameError) => ({ tag: "invalidLetBindingName", value: t })))(invalidLetBindingNameError(cx)(input)))], ["invalidTypeLambdaParameterName", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.InvalidTypeLambdaParameterNameError) => ({ tag: "invalidTypeLambdaParameterName", value: t })))(invalidTypeLambdaParameterNameError(cx)(input)))], ["nestedTermAnnotation", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.NestedTermAnnotationError) => ({ tag: "nestedTermAnnotation", value: t })))(nestedTermAnnotationError(cx)(input)))], ["redundantWrapUnwrap", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.RedundantWrapUnwrapError) => ({ tag: "redundantWrapUnwrap", value: t })))(redundantWrapUnwrapError(cx)(input)))], ["selfApplication", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.SelfApplicationError) => ({ tag: "selfApplication", value: t })))(selfApplicationError(cx)(input)))], ["termVariableShadowing", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.TermVariableShadowingError) => ({ tag: "termVariableShadowing", value: t })))(termVariableShadowingError(cx)(input)))], ["typeVariableShadowingInTypeLambda", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.TypeVariableShadowingInTypeLambdaError) => ({ tag: "typeVariableShadowingInTypeLambda", value: t })))(typeVariableShadowingInTypeLambdaError(cx)(input)))], ["undefinedTermVariable", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.UndefinedTermVariableError) => ({ tag: "undefinedTermVariable", value: t })))(undefinedTermVariableError(cx)(input)))], ["undefinedTypeVariableInBindingType", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.UndefinedTypeVariableInBindingTypeError) => ({ tag: "undefinedTypeVariableInBindingType", value: t })))(undefinedTypeVariableInBindingTypeError(cx)(input)))], ["undefinedTypeVariableInLambdaDomain", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.UndefinedTypeVariableInLambdaDomainError) => ({ tag: "undefinedTypeVariableInLambdaDomain", value: t })))(undefinedTypeVariableInLambdaDomainError(cx)(input)))], ["undefinedTypeVariableInTypeApplication", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.UndefinedTypeVariableInTypeApplicationError) => ({ tag: "undefinedTypeVariableInTypeApplication", value: t })))(undefinedTypeVariableInTypeApplicationError(cx)(input)))], ["unknownPrimitiveName", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.UnknownPrimitiveNameError) => ({ tag: "unknownPrimitiveName", value: t })))(unknownPrimitiveNameError(cx)(input)))], ["unnecessaryIdentityApplication", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.UnnecessaryIdentityApplicationError) => ({ tag: "unnecessaryIdentityApplication", value: t })))(unnecessaryIdentityApplicationError(cx)(input)))], ["untypedTermVariable", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.UntypedTermVariableError) => ({ tag: "untypedTermVariable", value: t })))(untypedTermVariableError(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | ErrorCore.InvalidTermError)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function invalidTypeError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.InvalidTypeError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => (() => {
  const field = ((_x) => _x.field)(inj);
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  const variantMap = LibMaps.fromList([["duplicateRecordTypeFieldNames", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.DuplicateRecordTypeFieldNamesError) => ({ tag: "duplicateRecordTypeFieldNames", value: t })))(duplicateRecordTypeFieldNamesError(cx)(input)))], ["duplicateUnionTypeFieldNames", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.DuplicateUnionTypeFieldNamesError) => ({ tag: "duplicateUnionTypeFieldNames", value: t })))(duplicateUnionTypeFieldNamesError(cx)(input)))], ["emptyRecordType", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.EmptyRecordTypeError) => ({ tag: "emptyRecordType", value: t })))(emptyRecordTypeError(cx)(input)))], ["emptyTypeAnnotation", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.EmptyTypeAnnotationError) => ({ tag: "emptyTypeAnnotation", value: t })))(emptyTypeAnnotationError(cx)(input)))], ["emptyUnionType", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.EmptyUnionTypeError) => ({ tag: "emptyUnionType", value: t })))(emptyUnionTypeError(cx)(input)))], ["invalidForallParameterName", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.InvalidForallParameterNameError) => ({ tag: "invalidForallParameterName", value: t })))(invalidForallParameterNameError(cx)(input)))], ["invalidTypeSchemeVariableName", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.InvalidTypeSchemeVariableNameError) => ({ tag: "invalidTypeSchemeVariableName", value: t })))(invalidTypeSchemeVariableNameError(cx)(input)))], ["nestedTypeAnnotation", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.NestedTypeAnnotationError) => ({ tag: "nestedTypeAnnotation", value: t })))(nestedTypeAnnotationError(cx)(input)))], ["nonComparableMapKeyType", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.NonComparableMapKeyTypeError) => ({ tag: "nonComparableMapKeyType", value: t })))(nonComparableMapKeyTypeError(cx)(input)))], ["nonComparableSetElementType", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.NonComparableSetElementTypeError) => ({ tag: "nonComparableSetElementType", value: t })))(nonComparableSetElementTypeError(cx)(input)))], ["singleVariantUnion", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.SingleVariantUnionError) => ({ tag: "singleVariantUnion", value: t })))(singleVariantUnionError(cx)(input)))], ["typeVariableShadowingInForall", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.TypeVariableShadowingInForallError) => ({ tag: "typeVariableShadowingInForall", value: t })))(typeVariableShadowingInForallError(cx)(input)))], ["undefinedTypeVariable", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.UndefinedTypeVariableError) => ({ tag: "undefinedTypeVariable", value: t })))(undefinedTypeVariableError(cx)(input)))], ["voidInNonBottomPosition", ((input: Core.Term) => LibEithers.map(((t: ErrorCore.VoidInNonBottomPositionError) => ({ tag: "voidInNonBottomPosition", value: t })))(voidInNonBottomPositionError(cx)(input)))]]);
  return LibMaybes.maybe(({ tag: "left", value: LibStrings.cat(["no such field ", ((_x) => _x)(fname), " in union"]) }))(((f: ((x: Core.Term) => Errors.DecodingError | ErrorCore.InvalidTypeError)) => f(fterm)))(LibMaps.lookup(fname)(variantMap));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected union" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function invalidTypeLambdaParameterNameError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.InvalidTypeLambdaParameterNameError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function invalidTypeSchemeVariableNameError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.InvalidTypeSchemeVariableNameError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function nestedTermAnnotationError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.NestedTermAnnotationError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => ({ tag: "right", value: ({
    location: field_location
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function nestedTypeAnnotationError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.NestedTypeAnnotationError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => ({ tag: "right", value: ({
    location: field_location
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function nonComparableMapKeyTypeError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.NonComparableMapKeyTypeError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("keyType")(DecodeCore.type)(fieldMap)(cx))(((field_keyType: Core.Type) => ({ tag: "right", value: ({
    location: field_location,
    keyType: field_keyType
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function nonComparableSetElementTypeError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.NonComparableSetElementTypeError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("elementType")(DecodeCore.type)(fieldMap)(cx))(((field_elementType: Core.Type) => ({ tag: "right", value: ({
    location: field_location,
    elementType: field_elementType
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function redundantWrapUnwrapError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.RedundantWrapUnwrapError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("typeName")(DecodeCore.name)(fieldMap)(cx))(((field_typeName: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    typeName: field_typeName
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function selfApplicationError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.SelfApplicationError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function singleVariantUnionError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.SingleVariantUnionError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("fieldName")(DecodeCore.name)(fieldMap)(cx))(((field_fieldName: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    fieldName: field_fieldName
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function termVariableShadowingError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.TermVariableShadowingError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function typeVariableShadowingInForallError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.TypeVariableShadowingInForallError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function typeVariableShadowingInTypeLambdaError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.TypeVariableShadowingInTypeLambdaError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function undefinedFieldError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.UndefinedFieldError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("fieldName")(DecodeCore.name)(fieldMap)(cx))(((field_fieldName: Core.Name) => LibEithers.bind(ExtractCore.requireField("typeName")(DecodeCore.name)(fieldMap)(cx))(((field_typeName: Core.Name) => ({ tag: "right", value: ({
    fieldName: field_fieldName,
    typeName: field_typeName
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function undefinedTermVariableError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.UndefinedTermVariableError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function undefinedTypeVariableError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.UndefinedTypeVariableError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function undefinedTypeVariableInBindingTypeError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.UndefinedTypeVariableInBindingTypeError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function undefinedTypeVariableInLambdaDomainError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.UndefinedTypeVariableInLambdaDomainError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function undefinedTypeVariableInTypeApplicationError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.UndefinedTypeVariableInTypeApplicationError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function unexpectedTermVariantError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.UnexpectedTermVariantError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("expectedVariant")(DecodeVariants.termVariant)(fieldMap)(cx))(((field_expectedVariant: hydra.variants.TermVariant) => LibEithers.bind(ExtractCore.requireField("actualTerm")(DecodeCore.term)(fieldMap)(cx))(((field_actualTerm: Core.Term) => ({ tag: "right", value: ({
    expectedVariant: field_expectedVariant,
    actualTerm: field_actualTerm
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function unexpectedTypeVariantError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.UnexpectedTypeVariantError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("expectedVariant")(DecodeVariants.typeVariant)(fieldMap)(cx))(((field_expectedVariant: hydra.variants.TypeVariant) => LibEithers.bind(ExtractCore.requireField("actualType")(DecodeCore.type)(fieldMap)(cx))(((field_actualType: Core.Type) => ({ tag: "right", value: ({
    expectedVariant: field_expectedVariant,
    actualType: field_actualType
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function unknownPrimitiveNameError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.UnknownPrimitiveNameError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function unnecessaryIdentityApplicationError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.UnnecessaryIdentityApplicationError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => ({ tag: "right", value: ({
    location: field_location
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function untypedTermVariableError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.UntypedTermVariableError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => LibEithers.bind(ExtractCore.requireField("name")(DecodeCore.name)(fieldMap)(cx))(((field_name: Core.Name) => ({ tag: "right", value: ({
    location: field_location,
    name: field_name
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function voidInNonBottomPositionError(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | ErrorCore.VoidInNonBottomPositionError) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("location")(DecodePaths.subtermPath)(fieldMap)(cx))(((field_location: hydra.paths.SubtermPath) => ({ tag: "right", value: ({
    location: field_location
  }) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}
