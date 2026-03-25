package hydra.show.error.core

import hydra.core.*

import hydra.error.core.*

import hydra.lib.literals

import hydra.lib.strings

def constantConditionError(e: hydra.error.core.ConstantConditionError): scala.Predef.String =
  hydra.lib.strings.cat(Seq("constant condition: ifElse with literal ", hydra.lib.literals.showBoolean(e.value)))

def duplicateBindingError(e: hydra.error.core.DuplicateBindingError): scala.Predef.String = hydra.lib.strings.cat(Seq("duplicate binding: ", (e.name)))

def duplicateFieldError(e: hydra.error.core.DuplicateFieldError): scala.Predef.String = hydra.lib.strings.cat(Seq("duplicate field: ", (e.name)))

def duplicateRecordTypeFieldNamesError(e: hydra.error.core.DuplicateRecordTypeFieldNamesError): scala.Predef.String = hydra.lib.strings.cat(Seq("duplicate field in record type: ", (e.name)))

def duplicateUnionTypeFieldNamesError(e: hydra.error.core.DuplicateUnionTypeFieldNamesError): scala.Predef.String = hydra.lib.strings.cat(Seq("duplicate field in union type: ", (e.name)))

def emptyCaseStatementError(e: hydra.error.core.EmptyCaseStatementError): scala.Predef.String = hydra.lib.strings.cat(Seq("empty case statement for type: ", (e.typeName)))

def emptyLetBindingsError[T0](e: T0): scala.Predef.String = "let expression with no bindings"

def emptyRecordTypeError[T0](e: T0): scala.Predef.String = "record type with no fields (use TypeUnit instead)"

def emptyTermAnnotationError[T0](e: T0): scala.Predef.String = "term annotation with empty annotation map"

def emptyTypeAnnotationError[T0](e: T0): scala.Predef.String = "type annotation with empty annotation map"

def emptyTypeNameInTermError[T0](e: T0): scala.Predef.String = "term with empty type name"

def emptyUnionTypeError[T0](e: T0): scala.Predef.String = "union type with no alternatives (use TypeVoid instead)"

def invalidForallParameterNameError(e: hydra.error.core.InvalidForallParameterNameError): scala.Predef.String = hydra.lib.strings.cat(Seq("invalid forall parameter name: ", (e.name)))

def invalidLambdaParameterNameError(e: hydra.error.core.InvalidLambdaParameterNameError): scala.Predef.String = hydra.lib.strings.cat(Seq("invalid lambda parameter name: ", (e.name)))

def invalidLetBindingNameError(e: hydra.error.core.InvalidLetBindingNameError): scala.Predef.String = hydra.lib.strings.cat(Seq("invalid let binding name: ", (e.name)))

def invalidTermError(e: hydra.error.core.InvalidTermError): scala.Predef.String =
  hydra.lib.strings.cat2("invalid term: ")(e match
  case hydra.error.core.InvalidTermError.constantCondition(v_InvalidTermError_constantCondition_v1) => hydra.show.error.core.constantConditionError(v_InvalidTermError_constantCondition_v1)
  case hydra.error.core.InvalidTermError.duplicateBinding(v_InvalidTermError_duplicateBinding_v1) => hydra.show.error.core.duplicateBindingError(v_InvalidTermError_duplicateBinding_v1)
  case hydra.error.core.InvalidTermError.duplicateField(v_InvalidTermError_duplicateField_v1) => hydra.show.error.core.duplicateFieldError(v_InvalidTermError_duplicateField_v1)
  case hydra.error.core.InvalidTermError.emptyCaseStatement(v_InvalidTermError_emptyCaseStatement_v1) => hydra.show.error.core.emptyCaseStatementError(v_InvalidTermError_emptyCaseStatement_v1)
  case hydra.error.core.InvalidTermError.emptyLetBindings(v_InvalidTermError_emptyLetBindings_v1) => hydra.show.error.core.emptyLetBindingsError(v_InvalidTermError_emptyLetBindings_v1)
  case hydra.error.core.InvalidTermError.emptyTermAnnotation(v_InvalidTermError_emptyTermAnnotation_v1) => hydra.show.error.core.emptyTermAnnotationError(v_InvalidTermError_emptyTermAnnotation_v1)
  case hydra.error.core.InvalidTermError.emptyTypeNameInTerm(v_InvalidTermError_emptyTypeNameInTerm_v1) => hydra.show.error.core.emptyTypeNameInTermError(v_InvalidTermError_emptyTypeNameInTerm_v1)
  case hydra.error.core.InvalidTermError.invalidLambdaParameterName(v_InvalidTermError_invalidLambdaParameterName_v1) => hydra.show.error.core.invalidLambdaParameterNameError(v_InvalidTermError_invalidLambdaParameterName_v1)
  case hydra.error.core.InvalidTermError.invalidLetBindingName(v_InvalidTermError_invalidLetBindingName_v1) => hydra.show.error.core.invalidLetBindingNameError(v_InvalidTermError_invalidLetBindingName_v1)
  case hydra.error.core.InvalidTermError.invalidTypeLambdaParameterName(v_InvalidTermError_invalidTypeLambdaParameterName_v1) => hydra.show.error.core.invalidTypeLambdaParameterNameError(v_InvalidTermError_invalidTypeLambdaParameterName_v1)
  case hydra.error.core.InvalidTermError.nestedTermAnnotation(v_InvalidTermError_nestedTermAnnotation_v1) => hydra.show.error.core.nestedTermAnnotationError(v_InvalidTermError_nestedTermAnnotation_v1)
  case hydra.error.core.InvalidTermError.redundantWrapUnwrap(v_InvalidTermError_redundantWrapUnwrap_v1) => hydra.show.error.core.redundantWrapUnwrapError(v_InvalidTermError_redundantWrapUnwrap_v1)
  case hydra.error.core.InvalidTermError.selfApplication(v_InvalidTermError_selfApplication_v1) => hydra.show.error.core.selfApplicationError(v_InvalidTermError_selfApplication_v1)
  case hydra.error.core.InvalidTermError.termVariableShadowing(v_InvalidTermError_termVariableShadowing_v1) => hydra.show.error.core.termVariableShadowingError(v_InvalidTermError_termVariableShadowing_v1)
  case hydra.error.core.InvalidTermError.typeVariableShadowingInTypeLambda(v_InvalidTermError_typeVariableShadowingInTypeLambda_v1) => hydra.show.error.core.typeVariableShadowingInTypeLambdaError(v_InvalidTermError_typeVariableShadowingInTypeLambda_v1)
  case hydra.error.core.InvalidTermError.undefinedTermVariable(v_InvalidTermError_undefinedTermVariable_v1) => hydra.show.error.core.undefinedTermVariableError(v_InvalidTermError_undefinedTermVariable_v1)
  case hydra.error.core.InvalidTermError.undefinedTypeVariableInBindingType(v_InvalidTermError_undefinedTypeVariableInBindingType_v1) => hydra.show.error.core.undefinedTypeVariableInBindingTypeError(v_InvalidTermError_undefinedTypeVariableInBindingType_v1)
  case hydra.error.core.InvalidTermError.undefinedTypeVariableInLambdaDomain(v_InvalidTermError_undefinedTypeVariableInLambdaDomain_v1) => hydra.show.error.core.undefinedTypeVariableInLambdaDomainError(v_InvalidTermError_undefinedTypeVariableInLambdaDomain_v1)
  case hydra.error.core.InvalidTermError.undefinedTypeVariableInTypeApplication(v_InvalidTermError_undefinedTypeVariableInTypeApplication_v1) => hydra.show.error.core.undefinedTypeVariableInTypeApplicationError(v_InvalidTermError_undefinedTypeVariableInTypeApplication_v1)
  case hydra.error.core.InvalidTermError.unknownPrimitiveName(v_InvalidTermError_unknownPrimitiveName_v1) => hydra.show.error.core.unknownPrimitiveNameError(v_InvalidTermError_unknownPrimitiveName_v1)
  case hydra.error.core.InvalidTermError.unnecessaryIdentityApplication(v_InvalidTermError_unnecessaryIdentityApplication_v1) => hydra.show.error.core.unnecessaryIdentityApplicationError(v_InvalidTermError_unnecessaryIdentityApplication_v1)
  case hydra.error.core.InvalidTermError.untypedTermVariable(v_InvalidTermError_untypedTermVariable_v1) => hydra.show.error.core.untypedTermVariableError(v_InvalidTermError_untypedTermVariable_v1))

def invalidTypeError(e: hydra.error.core.InvalidTypeError): scala.Predef.String =
  hydra.lib.strings.cat2("invalid type: ")(e match
  case hydra.error.core.InvalidTypeError.duplicateRecordTypeFieldNames(v_InvalidTypeError_duplicateRecordTypeFieldNames_v1) => hydra.show.error.core.duplicateRecordTypeFieldNamesError(v_InvalidTypeError_duplicateRecordTypeFieldNames_v1)
  case hydra.error.core.InvalidTypeError.duplicateUnionTypeFieldNames(v_InvalidTypeError_duplicateUnionTypeFieldNames_v1) => hydra.show.error.core.duplicateUnionTypeFieldNamesError(v_InvalidTypeError_duplicateUnionTypeFieldNames_v1)
  case hydra.error.core.InvalidTypeError.emptyRecordType(v_InvalidTypeError_emptyRecordType_v1) => hydra.show.error.core.emptyRecordTypeError(v_InvalidTypeError_emptyRecordType_v1)
  case hydra.error.core.InvalidTypeError.emptyTypeAnnotation(v_InvalidTypeError_emptyTypeAnnotation_v1) => hydra.show.error.core.emptyTypeAnnotationError(v_InvalidTypeError_emptyTypeAnnotation_v1)
  case hydra.error.core.InvalidTypeError.emptyUnionType(v_InvalidTypeError_emptyUnionType_v1) => hydra.show.error.core.emptyUnionTypeError(v_InvalidTypeError_emptyUnionType_v1)
  case hydra.error.core.InvalidTypeError.invalidForallParameterName(v_InvalidTypeError_invalidForallParameterName_v1) => hydra.show.error.core.invalidForallParameterNameError(v_InvalidTypeError_invalidForallParameterName_v1)
  case hydra.error.core.InvalidTypeError.invalidTypeSchemeVariableName(v_InvalidTypeError_invalidTypeSchemeVariableName_v1) => hydra.show.error.core.invalidTypeSchemeVariableNameError(v_InvalidTypeError_invalidTypeSchemeVariableName_v1)
  case hydra.error.core.InvalidTypeError.nestedTypeAnnotation(v_InvalidTypeError_nestedTypeAnnotation_v1) => hydra.show.error.core.nestedTypeAnnotationError(v_InvalidTypeError_nestedTypeAnnotation_v1)
  case hydra.error.core.InvalidTypeError.nonComparableMapKeyType(v_InvalidTypeError_nonComparableMapKeyType_v1) => hydra.show.error.core.nonComparableMapKeyTypeError(v_InvalidTypeError_nonComparableMapKeyType_v1)
  case hydra.error.core.InvalidTypeError.nonComparableSetElementType(v_InvalidTypeError_nonComparableSetElementType_v1) => hydra.show.error.core.nonComparableSetElementTypeError(v_InvalidTypeError_nonComparableSetElementType_v1)
  case hydra.error.core.InvalidTypeError.singleVariantUnion(v_InvalidTypeError_singleVariantUnion_v1) => hydra.show.error.core.singleVariantUnionError(v_InvalidTypeError_singleVariantUnion_v1)
  case hydra.error.core.InvalidTypeError.typeVariableShadowingInForall(v_InvalidTypeError_typeVariableShadowingInForall_v1) => hydra.show.error.core.typeVariableShadowingInForallError(v_InvalidTypeError_typeVariableShadowingInForall_v1)
  case hydra.error.core.InvalidTypeError.undefinedTypeVariable(v_InvalidTypeError_undefinedTypeVariable_v1) => hydra.show.error.core.undefinedTypeVariableError(v_InvalidTypeError_undefinedTypeVariable_v1)
  case hydra.error.core.InvalidTypeError.voidInNonBottomPosition(v_InvalidTypeError_voidInNonBottomPosition_v1) => hydra.show.error.core.voidInNonBottomPositionError(v_InvalidTypeError_voidInNonBottomPosition_v1))

def invalidTypeLambdaParameterNameError(e: hydra.error.core.InvalidTypeLambdaParameterNameError): scala.Predef.String = hydra.lib.strings.cat(Seq("invalid type lambda parameter name: ", (e.name)))

def invalidTypeSchemeVariableNameError(e: hydra.error.core.InvalidTypeSchemeVariableNameError): scala.Predef.String = hydra.lib.strings.cat(Seq("invalid type scheme variable name: ", (e.name)))

def nestedTermAnnotationError[T0](e: T0): scala.Predef.String = "nested term annotations should be merged"

def nestedTypeAnnotationError[T0](e: T0): scala.Predef.String = "nested type annotations should be merged"

def nonComparableMapKeyTypeError(e: hydra.error.core.NonComparableMapKeyTypeError): scala.Predef.String =
  hydra.lib.strings.cat(Seq("map key type contains a function type: ", hydra.show.core.`type`(e.keyType)))

def nonComparableSetElementTypeError(e: hydra.error.core.NonComparableSetElementTypeError): scala.Predef.String =
  hydra.lib.strings.cat(Seq("set element type contains a function type: ", hydra.show.core.`type`(e.elementType)))

def redundantWrapUnwrapError(e: hydra.error.core.RedundantWrapUnwrapError): scala.Predef.String = hydra.lib.strings.cat(Seq("redundant wrap/unwrap for type: ", (e.typeName)))

def selfApplicationError(e: hydra.error.core.SelfApplicationError): scala.Predef.String = hydra.lib.strings.cat(Seq("self-application of variable: ", (e.name)))

def singleVariantUnionError(e: hydra.error.core.SingleVariantUnionError): scala.Predef.String = hydra.lib.strings.cat(Seq("union type with single variant: ", (e.fieldName)))

def termVariableShadowingError(e: hydra.error.core.TermVariableShadowingError): scala.Predef.String = hydra.lib.strings.cat(Seq("variable shadowing: ", (e.name)))

def typeVariableShadowingInForallError(e: hydra.error.core.TypeVariableShadowingInForallError): scala.Predef.String = hydra.lib.strings.cat(Seq("type variable shadowing in forall: ", (e.name)))

def typeVariableShadowingInTypeLambdaError(e: hydra.error.core.TypeVariableShadowingInTypeLambdaError): scala.Predef.String = hydra.lib.strings.cat(Seq("type variable shadowing in type lambda: ", (e.name)))

def undefinedFieldError(e: hydra.error.core.UndefinedFieldError): scala.Predef.String =
  {
  lazy val fname: hydra.core.Name = (e.fieldName)
  lazy val tname: hydra.core.Name = (e.typeName)
  hydra.lib.strings.cat(Seq("no such field \"", fname, "\" in type \"", tname, "\""))
}

def undefinedTermVariableError(e: hydra.error.core.UndefinedTermVariableError): scala.Predef.String = hydra.lib.strings.cat(Seq("undefined term variable: ", (e.name)))

def undefinedTypeVariableError(e: hydra.error.core.UndefinedTypeVariableError): scala.Predef.String = hydra.lib.strings.cat(Seq("undefined type variable: ", (e.name)))

def undefinedTypeVariableInBindingTypeError(e: hydra.error.core.UndefinedTypeVariableInBindingTypeError): scala.Predef.String = hydra.lib.strings.cat(Seq("undefined type variable in binding type: ", (e.name)))

def undefinedTypeVariableInLambdaDomainError(e: hydra.error.core.UndefinedTypeVariableInLambdaDomainError): scala.Predef.String = hydra.lib.strings.cat(Seq("undefined type variable in lambda domain: ", (e.name)))

def undefinedTypeVariableInTypeApplicationError(e: hydra.error.core.UndefinedTypeVariableInTypeApplicationError): scala.Predef.String =
  hydra.lib.strings.cat(Seq("undefined type variable in type application: ", (e.name)))

def unexpectedTermVariantError(e: hydra.error.core.UnexpectedTermVariantError): scala.Predef.String =
  {
  lazy val expected: hydra.variants.TermVariant = (e.expectedVariant)
  lazy val actual: hydra.core.Term = (e.actualTerm)
  hydra.lib.strings.cat(Seq("expected ", hydra.show.meta.termVariant(expected), " term but found ", hydra.show.core.term(actual)))
}

def unexpectedTypeVariantError(e: hydra.error.core.UnexpectedTypeVariantError): scala.Predef.String =
  {
  lazy val expected: hydra.variants.TypeVariant = (e.expectedVariant)
  lazy val actual: hydra.core.Type = (e.actualType)
  hydra.lib.strings.cat(Seq("expected ", hydra.show.meta.typeVariant(expected), " type but found ", hydra.show.core.`type`(actual)))
}

def unknownPrimitiveNameError(e: hydra.error.core.UnknownPrimitiveNameError): scala.Predef.String = hydra.lib.strings.cat(Seq("unknown primitive: ", (e.name)))

def unnecessaryIdentityApplicationError[T0](e: T0): scala.Predef.String = "unnecessary application of identity lambda"

def untypedTermVariableError(e: hydra.error.core.UntypedTermVariableError): scala.Predef.String = hydra.lib.strings.cat(Seq("untyped term variable: ", (e.name)))

def voidInNonBottomPositionError[T0](e: T0): scala.Predef.String = "TypeVoid in a position where no value can be constructed"
