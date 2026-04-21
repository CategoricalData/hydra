// Note: this is an automatically generated file. Do not edit.

/**
 * String representations of hydra.error.core types
 */



import * as Ast from "../../ast.js";
import * as Classes from "../../classes.js";
import * as Coders from "../../coders.js";
import * as Context from "../../context.js";
import * as Core from "../../core.js";
import * as ErrorChecking from "../../error/checking.js";
import * as ErrorCore from "../../error/core.js";
import * as ErrorPackaging from "../../error/packaging.js";
import * as Errors from "../../errors.js";
import * as Graph from "../../graph.js";
import * as JsonModel from "../../json/model.js";
import * as LibLiterals from "../../lib/literals.js";
import * as LibStrings from "../../lib/strings.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as ShowCore from "../core.js";
import * as ShowVariants from "../variants.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export function constantConditionError(e: ErrorCore.ConstantConditionError): string {
  return LibStrings.cat(["constant condition: ifElse with literal ", LibLiterals.showBoolean(((_x) => _x.value)(e))]);
}

export function duplicateBindingError(e: ErrorCore.DuplicateBindingError): string {
  return LibStrings.cat(["duplicate binding: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function duplicateFieldError(e: ErrorCore.DuplicateFieldError): string {
  return LibStrings.cat(["duplicate field: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function duplicateRecordTypeFieldNamesError(e: ErrorCore.DuplicateRecordTypeFieldNamesError): string {
  return LibStrings.cat(["duplicate field in record type: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function duplicateUnionTypeFieldNamesError(e: ErrorCore.DuplicateUnionTypeFieldNamesError): string {
  return LibStrings.cat(["duplicate field in union type: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function emptyCaseStatementError(e: ErrorCore.EmptyCaseStatementError): string {
  return LibStrings.cat(["empty case statement for type: ", ((_x) => _x)(((_x) => _x.typeName)(e))]);
}

export function emptyLetBindingsError<t0>(e: t0): string {
  return "let expression with no bindings";
}

export function emptyRecordTypeError<t0>(e: t0): string {
  return "record type with no fields (use TypeUnit instead)";
}

export function emptyTermAnnotationError<t0>(e: t0): string {
  return "term annotation with empty annotation map";
}

export function emptyTypeAnnotationError<t0>(e: t0): string {
  return "type annotation with empty annotation map";
}

export function emptyTypeNameInTermError<t0>(e: t0): string {
  return "term with empty type name";
}

export function emptyUnionTypeError<t0>(e: t0): string {
  return "union type with no alternatives (use TypeVoid instead)";
}

export function invalidForallParameterNameError(e: ErrorCore.InvalidForallParameterNameError): string {
  return LibStrings.cat(["invalid forall parameter name: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function invalidLambdaParameterNameError(e: ErrorCore.InvalidLambdaParameterNameError): string {
  return LibStrings.cat(["invalid lambda parameter name: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function invalidLetBindingNameError(e: ErrorCore.InvalidLetBindingNameError): string {
  return LibStrings.cat(["invalid let binding name: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function invalidTermError(e: ErrorCore.InvalidTermError): string {
  return LibStrings.cat2("invalid term: ")((() => {
  const _m = e;
  switch (_m.tag) {
    case "constantCondition": return ((v1: ErrorCore.ConstantConditionError) => constantConditionError(v1))((_m as any).value);
    case "duplicateBinding": return ((v1: ErrorCore.DuplicateBindingError) => duplicateBindingError(v1))((_m as any).value);
    case "duplicateField": return ((v1: ErrorCore.DuplicateFieldError) => duplicateFieldError(v1))((_m as any).value);
    case "emptyCaseStatement": return ((v1: ErrorCore.EmptyCaseStatementError) => emptyCaseStatementError(v1))((_m as any).value);
    case "emptyLetBindings": return ((v1: ErrorCore.EmptyLetBindingsError) => emptyLetBindingsError(v1))((_m as any).value);
    case "emptyTermAnnotation": return ((v1: ErrorCore.EmptyTermAnnotationError) => emptyTermAnnotationError(v1))((_m as any).value);
    case "emptyTypeNameInTerm": return ((v1: ErrorCore.EmptyTypeNameInTermError) => emptyTypeNameInTermError(v1))((_m as any).value);
    case "invalidLambdaParameterName": return ((v1: ErrorCore.InvalidLambdaParameterNameError) => invalidLambdaParameterNameError(v1))((_m as any).value);
    case "invalidLetBindingName": return ((v1: ErrorCore.InvalidLetBindingNameError) => invalidLetBindingNameError(v1))((_m as any).value);
    case "invalidTypeLambdaParameterName": return ((v1: ErrorCore.InvalidTypeLambdaParameterNameError) => invalidTypeLambdaParameterNameError(v1))((_m as any).value);
    case "nestedTermAnnotation": return ((v1: ErrorCore.NestedTermAnnotationError) => nestedTermAnnotationError(v1))((_m as any).value);
    case "redundantWrapUnwrap": return ((v1: ErrorCore.RedundantWrapUnwrapError) => redundantWrapUnwrapError(v1))((_m as any).value);
    case "selfApplication": return ((v1: ErrorCore.SelfApplicationError) => selfApplicationError(v1))((_m as any).value);
    case "termVariableShadowing": return ((v1: ErrorCore.TermVariableShadowingError) => termVariableShadowingError(v1))((_m as any).value);
    case "typeVariableShadowingInTypeLambda": return ((v1: ErrorCore.TypeVariableShadowingInTypeLambdaError) => typeVariableShadowingInTypeLambdaError(v1))((_m as any).value);
    case "undefinedTermVariable": return ((v1: ErrorCore.UndefinedTermVariableError) => undefinedTermVariableError(v1))((_m as any).value);
    case "undefinedTypeVariableInBindingType": return ((v1: ErrorCore.UndefinedTypeVariableInBindingTypeError) => undefinedTypeVariableInBindingTypeError(v1))((_m as any).value);
    case "undefinedTypeVariableInLambdaDomain": return ((v1: ErrorCore.UndefinedTypeVariableInLambdaDomainError) => undefinedTypeVariableInLambdaDomainError(v1))((_m as any).value);
    case "undefinedTypeVariableInTypeApplication": return ((v1: ErrorCore.UndefinedTypeVariableInTypeApplicationError) => undefinedTypeVariableInTypeApplicationError(v1))((_m as any).value);
    case "unknownPrimitiveName": return ((v1: ErrorCore.UnknownPrimitiveNameError) => unknownPrimitiveNameError(v1))((_m as any).value);
    case "unnecessaryIdentityApplication": return ((v1: ErrorCore.UnnecessaryIdentityApplicationError) => unnecessaryIdentityApplicationError(v1))((_m as any).value);
    case "untypedTermVariable": return ((v1: ErrorCore.UntypedTermVariableError) => untypedTermVariableError(v1))((_m as any).value);
  }
})());
}

export function invalidTypeError(e: ErrorCore.InvalidTypeError): string {
  return LibStrings.cat2("invalid type: ")((() => {
  const _m = e;
  switch (_m.tag) {
    case "duplicateRecordTypeFieldNames": return ((v1: ErrorCore.DuplicateRecordTypeFieldNamesError) => duplicateRecordTypeFieldNamesError(v1))((_m as any).value);
    case "duplicateUnionTypeFieldNames": return ((v1: ErrorCore.DuplicateUnionTypeFieldNamesError) => duplicateUnionTypeFieldNamesError(v1))((_m as any).value);
    case "emptyRecordType": return ((v1: ErrorCore.EmptyRecordTypeError) => emptyRecordTypeError(v1))((_m as any).value);
    case "emptyTypeAnnotation": return ((v1: ErrorCore.EmptyTypeAnnotationError) => emptyTypeAnnotationError(v1))((_m as any).value);
    case "emptyUnionType": return ((v1: ErrorCore.EmptyUnionTypeError) => emptyUnionTypeError(v1))((_m as any).value);
    case "invalidForallParameterName": return ((v1: ErrorCore.InvalidForallParameterNameError) => invalidForallParameterNameError(v1))((_m as any).value);
    case "invalidTypeSchemeVariableName": return ((v1: ErrorCore.InvalidTypeSchemeVariableNameError) => invalidTypeSchemeVariableNameError(v1))((_m as any).value);
    case "nestedTypeAnnotation": return ((v1: ErrorCore.NestedTypeAnnotationError) => nestedTypeAnnotationError(v1))((_m as any).value);
    case "nonComparableMapKeyType": return ((v1: ErrorCore.NonComparableMapKeyTypeError) => nonComparableMapKeyTypeError(v1))((_m as any).value);
    case "nonComparableSetElementType": return ((v1: ErrorCore.NonComparableSetElementTypeError) => nonComparableSetElementTypeError(v1))((_m as any).value);
    case "singleVariantUnion": return ((v1: ErrorCore.SingleVariantUnionError) => singleVariantUnionError(v1))((_m as any).value);
    case "typeVariableShadowingInForall": return ((v1: ErrorCore.TypeVariableShadowingInForallError) => typeVariableShadowingInForallError(v1))((_m as any).value);
    case "undefinedTypeVariable": return ((v1: ErrorCore.UndefinedTypeVariableError) => undefinedTypeVariableError(v1))((_m as any).value);
    case "voidInNonBottomPosition": return ((v1: ErrorCore.VoidInNonBottomPositionError) => voidInNonBottomPositionError(v1))((_m as any).value);
  }
})());
}

export function invalidTypeLambdaParameterNameError(e: ErrorCore.InvalidTypeLambdaParameterNameError): string {
  return LibStrings.cat(["invalid type lambda parameter name: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function invalidTypeSchemeVariableNameError(e: ErrorCore.InvalidTypeSchemeVariableNameError): string {
  return LibStrings.cat(["invalid type scheme variable name: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function nestedTermAnnotationError<t0>(e: t0): string {
  return "nested term annotations should be merged";
}

export function nestedTypeAnnotationError<t0>(e: t0): string {
  return "nested type annotations should be merged";
}

export function nonComparableMapKeyTypeError(e: ErrorCore.NonComparableMapKeyTypeError): string {
  return LibStrings.cat(["map key type contains a function type: ", ShowCore.type(((_x) => _x.keyType)(e))]);
}

export function nonComparableSetElementTypeError(e: ErrorCore.NonComparableSetElementTypeError): string {
  return LibStrings.cat(["set element type contains a function type: ", ShowCore.type(((_x) => _x.elementType)(e))]);
}

export function redundantWrapUnwrapError(e: ErrorCore.RedundantWrapUnwrapError): string {
  return LibStrings.cat(["redundant wrap/unwrap for type: ", ((_x) => _x)(((_x) => _x.typeName)(e))]);
}

export function selfApplicationError(e: ErrorCore.SelfApplicationError): string {
  return LibStrings.cat(["self-application of variable: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function singleVariantUnionError(e: ErrorCore.SingleVariantUnionError): string {
  return LibStrings.cat(["union type with single variant: ", ((_x) => _x)(((_x) => _x.fieldName)(e))]);
}

export function termVariableShadowingError(e: ErrorCore.TermVariableShadowingError): string {
  return LibStrings.cat(["variable shadowing: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function typeVariableShadowingInForallError(e: ErrorCore.TypeVariableShadowingInForallError): string {
  return LibStrings.cat(["type variable shadowing in forall: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function typeVariableShadowingInTypeLambdaError(e: ErrorCore.TypeVariableShadowingInTypeLambdaError): string {
  return LibStrings.cat(["type variable shadowing in type lambda: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function undefinedFieldError(e: ErrorCore.UndefinedFieldError): string {
  return (() => {
  const fname = ((_x) => _x.fieldName)(e);
  return (() => {
  const tname = ((_x) => _x.typeName)(e);
  return LibStrings.cat(["no such field \"", ((_x) => _x)(fname), "\" in type \"", ((_x) => _x)(tname), "\""]);
})();
})();
}

export function undefinedTermVariableError(e: ErrorCore.UndefinedTermVariableError): string {
  return LibStrings.cat(["undefined term variable: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function undefinedTypeVariableError(e: ErrorCore.UndefinedTypeVariableError): string {
  return LibStrings.cat(["undefined type variable: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function undefinedTypeVariableInBindingTypeError(e: ErrorCore.UndefinedTypeVariableInBindingTypeError): string {
  return LibStrings.cat(["undefined type variable in binding type: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function undefinedTypeVariableInLambdaDomainError(e: ErrorCore.UndefinedTypeVariableInLambdaDomainError): string {
  return LibStrings.cat(["undefined type variable in lambda domain: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function undefinedTypeVariableInTypeApplicationError(e: ErrorCore.UndefinedTypeVariableInTypeApplicationError): string {
  return LibStrings.cat(["undefined type variable in type application: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function unexpectedTermVariantError(e: ErrorCore.UnexpectedTermVariantError): string {
  return (() => {
  const expected = ((_x) => _x.expectedVariant)(e);
  return (() => {
  const actual = ((_x) => _x.actualTerm)(e);
  return LibStrings.cat(["expected ", ShowVariants.termVariant(expected), " term but found ", ShowCore.term(actual)]);
})();
})();
}

export function unexpectedTypeVariantError(e: ErrorCore.UnexpectedTypeVariantError): string {
  return (() => {
  const expected = ((_x) => _x.expectedVariant)(e);
  return (() => {
  const actual = ((_x) => _x.actualType)(e);
  return LibStrings.cat(["expected ", ShowVariants.typeVariant(expected), " type but found ", ShowCore.type(actual)]);
})();
})();
}

export function unknownPrimitiveNameError(e: ErrorCore.UnknownPrimitiveNameError): string {
  return LibStrings.cat(["unknown primitive: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function unnecessaryIdentityApplicationError<t0>(e: t0): string {
  return "unnecessary application of identity lambda";
}

export function untypedTermVariableError(e: ErrorCore.UntypedTermVariableError): string {
  return LibStrings.cat(["untyped term variable: ", ((_x) => _x)(((_x) => _x.name)(e))]);
}

export function voidInNonBottomPositionError<t0>(e: t0): string {
  return "TypeVoid in a position where no value can be constructed";
}
