-- Note: this is an automatically generated file. Do not edit.

-- | String representations of hydra.error.core types

module Hydra.Show.Error.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Show.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Show a constant condition error as a string
constantConditionError :: ErrorCore.ConstantConditionError -> String
constantConditionError e =
    Strings.cat [
      "constant condition: ifElse with literal ",
      (Literals.showBoolean (ErrorCore.constantConditionErrorValue e))]

-- | Show a duplicate binding error as a string
duplicateBindingError :: ErrorCore.DuplicateBindingError -> String
duplicateBindingError e =
    Strings.cat [
      "duplicate binding: ",
      (Core.unName (ErrorCore.duplicateBindingErrorName e))]

-- | Show a duplicate field error as a string
duplicateFieldError :: ErrorCore.DuplicateFieldError -> String
duplicateFieldError e =
    Strings.cat [
      "duplicate field: ",
      (Core.unName (ErrorCore.duplicateFieldErrorName e))]

-- | Show a duplicate record type field names error as a string
duplicateRecordTypeFieldNamesError :: ErrorCore.DuplicateRecordTypeFieldNamesError -> String
duplicateRecordTypeFieldNamesError e =
    Strings.cat [
      "duplicate field in record type: ",
      (Core.unName (ErrorCore.duplicateRecordTypeFieldNamesErrorName e))]

-- | Show a duplicate union type field names error as a string
duplicateUnionTypeFieldNamesError :: ErrorCore.DuplicateUnionTypeFieldNamesError -> String
duplicateUnionTypeFieldNamesError e =
    Strings.cat [
      "duplicate field in union type: ",
      (Core.unName (ErrorCore.duplicateUnionTypeFieldNamesErrorName e))]

-- | Show an empty case statement error as a string
emptyCaseStatementError :: ErrorCore.EmptyCaseStatementError -> String
emptyCaseStatementError e =
    Strings.cat [
      "empty case statement for type: ",
      (Core.unName (ErrorCore.emptyCaseStatementErrorTypeName e))]

-- | Show an empty let bindings error as a string
emptyLetBindingsError :: t0 -> String
emptyLetBindingsError e = "let expression with no bindings"

-- | Show an empty record type error as a string
emptyRecordTypeError :: t0 -> String
emptyRecordTypeError e = "record type with no fields (use TypeUnit instead)"

-- | Show an empty term annotation error as a string
emptyTermAnnotationError :: t0 -> String
emptyTermAnnotationError e = "term annotation with empty annotation map"

-- | Show an empty type annotation error as a string
emptyTypeAnnotationError :: t0 -> String
emptyTypeAnnotationError e = "type annotation with empty annotation map"

-- | Show an empty type name in term error as a string
emptyTypeNameInTermError :: t0 -> String
emptyTypeNameInTermError e = "term with empty type name"

-- | Show an empty union type error as a string
emptyUnionTypeError :: t0 -> String
emptyUnionTypeError e = "union type with no alternatives (use TypeVoid instead)"

-- | Show an invalid forall parameter name error as a string
invalidForallParameterNameError :: ErrorCore.InvalidForallParameterNameError -> String
invalidForallParameterNameError e =
    Strings.cat [
      "invalid forall parameter name: ",
      (Core.unName (ErrorCore.invalidForallParameterNameErrorName e))]

-- | Show an invalid lambda parameter name error as a string
invalidLambdaParameterNameError :: ErrorCore.InvalidLambdaParameterNameError -> String
invalidLambdaParameterNameError e =
    Strings.cat [
      "invalid lambda parameter name: ",
      (Core.unName (ErrorCore.invalidLambdaParameterNameErrorName e))]

-- | Show an invalid let binding name error as a string
invalidLetBindingNameError :: ErrorCore.InvalidLetBindingNameError -> String
invalidLetBindingNameError e =
    Strings.cat [
      "invalid let binding name: ",
      (Core.unName (ErrorCore.invalidLetBindingNameErrorName e))]

-- | Show an invalid term error as a string
invalidTermError :: ErrorCore.InvalidTermError -> String
invalidTermError e =
    Strings.cat2 "invalid term: " (case e of
      ErrorCore.InvalidTermErrorConstantCondition v0 -> constantConditionError v0
      ErrorCore.InvalidTermErrorDuplicateBinding v0 -> duplicateBindingError v0
      ErrorCore.InvalidTermErrorDuplicateField v0 -> duplicateFieldError v0
      ErrorCore.InvalidTermErrorEmptyCaseStatement v0 -> emptyCaseStatementError v0
      ErrorCore.InvalidTermErrorEmptyLetBindings v0 -> emptyLetBindingsError v0
      ErrorCore.InvalidTermErrorEmptyTermAnnotation v0 -> emptyTermAnnotationError v0
      ErrorCore.InvalidTermErrorEmptyTypeNameInTerm v0 -> emptyTypeNameInTermError v0
      ErrorCore.InvalidTermErrorInvalidLambdaParameterName v0 -> invalidLambdaParameterNameError v0
      ErrorCore.InvalidTermErrorInvalidLetBindingName v0 -> invalidLetBindingNameError v0
      ErrorCore.InvalidTermErrorInvalidTypeLambdaParameterName v0 -> invalidTypeLambdaParameterNameError v0
      ErrorCore.InvalidTermErrorNestedTermAnnotation v0 -> nestedTermAnnotationError v0
      ErrorCore.InvalidTermErrorRedundantWrapUnwrap v0 -> redundantWrapUnwrapError v0
      ErrorCore.InvalidTermErrorSelfApplication v0 -> selfApplicationError v0
      ErrorCore.InvalidTermErrorTermVariableShadowing v0 -> termVariableShadowingError v0
      ErrorCore.InvalidTermErrorTypeVariableShadowingInTypeLambda v0 -> typeVariableShadowingInTypeLambdaError v0
      ErrorCore.InvalidTermErrorUndefinedTermVariable v0 -> undefinedTermVariableError v0
      ErrorCore.InvalidTermErrorUndefinedTypeVariableInBindingType v0 -> undefinedTypeVariableInBindingTypeError v0
      ErrorCore.InvalidTermErrorUndefinedTypeVariableInLambdaDomain v0 -> undefinedTypeVariableInLambdaDomainError v0
      ErrorCore.InvalidTermErrorUndefinedTypeVariableInTypeApplication v0 -> undefinedTypeVariableInTypeApplicationError v0
      ErrorCore.InvalidTermErrorUnknownPrimitiveName v0 -> unknownPrimitiveNameError v0
      ErrorCore.InvalidTermErrorUnnecessaryIdentityApplication v0 -> unnecessaryIdentityApplicationError v0
      ErrorCore.InvalidTermErrorUntypedTermVariable v0 -> untypedTermVariableError v0)

-- | Show an invalid type error as a string
invalidTypeError :: ErrorCore.InvalidTypeError -> String
invalidTypeError e =
    Strings.cat2 "invalid type: " (case e of
      ErrorCore.InvalidTypeErrorDuplicateRecordTypeFieldNames v0 -> duplicateRecordTypeFieldNamesError v0
      ErrorCore.InvalidTypeErrorDuplicateUnionTypeFieldNames v0 -> duplicateUnionTypeFieldNamesError v0
      ErrorCore.InvalidTypeErrorEmptyRecordType v0 -> emptyRecordTypeError v0
      ErrorCore.InvalidTypeErrorEmptyTypeAnnotation v0 -> emptyTypeAnnotationError v0
      ErrorCore.InvalidTypeErrorEmptyUnionType v0 -> emptyUnionTypeError v0
      ErrorCore.InvalidTypeErrorInvalidForallParameterName v0 -> invalidForallParameterNameError v0
      ErrorCore.InvalidTypeErrorInvalidTypeSchemeVariableName v0 -> invalidTypeSchemeVariableNameError v0
      ErrorCore.InvalidTypeErrorNestedTypeAnnotation v0 -> nestedTypeAnnotationError v0
      ErrorCore.InvalidTypeErrorNonComparableMapKeyType v0 -> nonComparableMapKeyTypeError v0
      ErrorCore.InvalidTypeErrorNonComparableSetElementType v0 -> nonComparableSetElementTypeError v0
      ErrorCore.InvalidTypeErrorSingleVariantUnion v0 -> singleVariantUnionError v0
      ErrorCore.InvalidTypeErrorTypeVariableShadowingInForall v0 -> typeVariableShadowingInForallError v0
      ErrorCore.InvalidTypeErrorUndefinedTypeVariable v0 -> undefinedTypeVariableError v0
      ErrorCore.InvalidTypeErrorVoidInNonBottomPosition v0 -> voidInNonBottomPositionError v0)

-- | Show an invalid type lambda parameter name error as a string
invalidTypeLambdaParameterNameError :: ErrorCore.InvalidTypeLambdaParameterNameError -> String
invalidTypeLambdaParameterNameError e =
    Strings.cat [
      "invalid type lambda parameter name: ",
      (Core.unName (ErrorCore.invalidTypeLambdaParameterNameErrorName e))]

-- | Show an invalid type scheme variable name error as a string
invalidTypeSchemeVariableNameError :: ErrorCore.InvalidTypeSchemeVariableNameError -> String
invalidTypeSchemeVariableNameError e =
    Strings.cat [
      "invalid type scheme variable name: ",
      (Core.unName (ErrorCore.invalidTypeSchemeVariableNameErrorName e))]

-- | Show a nested term annotation error as a string
nestedTermAnnotationError :: t0 -> String
nestedTermAnnotationError e = "nested term annotations should be merged"

-- | Show a nested type annotation error as a string
nestedTypeAnnotationError :: t0 -> String
nestedTypeAnnotationError e = "nested type annotations should be merged"

-- | Show a non-comparable map key type error as a string
nonComparableMapKeyTypeError :: ErrorCore.NonComparableMapKeyTypeError -> String
nonComparableMapKeyTypeError e =
    Strings.cat [
      "map key type contains a function type: ",
      (ShowCore.type_ (ErrorCore.nonComparableMapKeyTypeErrorKeyType e))]

-- | Show a non-comparable set element type error as a string
nonComparableSetElementTypeError :: ErrorCore.NonComparableSetElementTypeError -> String
nonComparableSetElementTypeError e =
    Strings.cat [
      "set element type contains a function type: ",
      (ShowCore.type_ (ErrorCore.nonComparableSetElementTypeErrorElementType e))]

-- | Show a redundant wrap/unwrap error as a string
redundantWrapUnwrapError :: ErrorCore.RedundantWrapUnwrapError -> String
redundantWrapUnwrapError e =
    Strings.cat [
      "redundant wrap/unwrap for type: ",
      (Core.unName (ErrorCore.redundantWrapUnwrapErrorTypeName e))]

-- | Show a self-application error as a string
selfApplicationError :: ErrorCore.SelfApplicationError -> String
selfApplicationError e =
    Strings.cat [
      "self-application of variable: ",
      (Core.unName (ErrorCore.selfApplicationErrorName e))]

-- | Show a single variant union error as a string
singleVariantUnionError :: ErrorCore.SingleVariantUnionError -> String
singleVariantUnionError e =
    Strings.cat [
      "union type with single variant: ",
      (Core.unName (ErrorCore.singleVariantUnionErrorFieldName e))]

-- | Show a term variable shadowing error as a string
termVariableShadowingError :: ErrorCore.TermVariableShadowingError -> String
termVariableShadowingError e =
    Strings.cat [
      "variable shadowing: ",
      (Core.unName (ErrorCore.termVariableShadowingErrorName e))]

-- | Show a type variable shadowing in forall error as a string
typeVariableShadowingInForallError :: ErrorCore.TypeVariableShadowingInForallError -> String
typeVariableShadowingInForallError e =
    Strings.cat [
      "type variable shadowing in forall: ",
      (Core.unName (ErrorCore.typeVariableShadowingInForallErrorName e))]

-- | Show a type variable shadowing in type lambda error as a string
typeVariableShadowingInTypeLambdaError :: ErrorCore.TypeVariableShadowingInTypeLambdaError -> String
typeVariableShadowingInTypeLambdaError e =
    Strings.cat [
      "type variable shadowing in type lambda: ",
      (Core.unName (ErrorCore.typeVariableShadowingInTypeLambdaErrorName e))]

-- | Show an undefined field error as a string
undefinedFieldError :: ErrorCore.UndefinedFieldError -> String
undefinedFieldError e =

      let fname = ErrorCore.undefinedFieldErrorFieldName e
          tname = ErrorCore.undefinedFieldErrorTypeName e
      in (Strings.cat [
        "no such field \"",
        (Core.unName fname),
        "\" in type \"",
        (Core.unName tname),
        "\""])

-- | Show an undefined term variable error as a string
undefinedTermVariableError :: ErrorCore.UndefinedTermVariableError -> String
undefinedTermVariableError e =
    Strings.cat [
      "undefined term variable: ",
      (Core.unName (ErrorCore.undefinedTermVariableErrorName e))]

-- | Show an undefined type variable error as a string
undefinedTypeVariableError :: ErrorCore.UndefinedTypeVariableError -> String
undefinedTypeVariableError e =
    Strings.cat [
      "undefined type variable: ",
      (Core.unName (ErrorCore.undefinedTypeVariableErrorName e))]

-- | Show an undefined type variable in binding type error as a string
undefinedTypeVariableInBindingTypeError :: ErrorCore.UndefinedTypeVariableInBindingTypeError -> String
undefinedTypeVariableInBindingTypeError e =
    Strings.cat [
      "undefined type variable in binding type: ",
      (Core.unName (ErrorCore.undefinedTypeVariableInBindingTypeErrorName e))]

-- | Show an undefined type variable in lambda domain error as a string
undefinedTypeVariableInLambdaDomainError :: ErrorCore.UndefinedTypeVariableInLambdaDomainError -> String
undefinedTypeVariableInLambdaDomainError e =
    Strings.cat [
      "undefined type variable in lambda domain: ",
      (Core.unName (ErrorCore.undefinedTypeVariableInLambdaDomainErrorName e))]

-- | Show an undefined type variable in type application error as a string
undefinedTypeVariableInTypeApplicationError :: ErrorCore.UndefinedTypeVariableInTypeApplicationError -> String
undefinedTypeVariableInTypeApplicationError e =
    Strings.cat [
      "undefined type variable in type application: ",
      (Core.unName (ErrorCore.undefinedTypeVariableInTypeApplicationErrorName e))]

-- | Show an unexpected term variant error as a string
unexpectedTermVariantError :: ErrorCore.UnexpectedTermVariantError -> String
unexpectedTermVariantError e =

      let expected = ErrorCore.unexpectedTermVariantErrorExpectedVariant e
          actual = ErrorCore.unexpectedTermVariantErrorActualTerm e
      in (Strings.cat [
        "expected ",
        (Variants.termVariant expected),
        " term but found ",
        (ShowCore.term actual)])

-- | Show an unexpected type variant error as a string
unexpectedTypeVariantError :: ErrorCore.UnexpectedTypeVariantError -> String
unexpectedTypeVariantError e =

      let expected = ErrorCore.unexpectedTypeVariantErrorExpectedVariant e
          actual = ErrorCore.unexpectedTypeVariantErrorActualType e
      in (Strings.cat [
        "expected ",
        (Variants.typeVariant expected),
        " type but found ",
        (ShowCore.type_ actual)])

-- | Show an unknown primitive name error as a string
unknownPrimitiveNameError :: ErrorCore.UnknownPrimitiveNameError -> String
unknownPrimitiveNameError e =
    Strings.cat [
      "unknown primitive: ",
      (Core.unName (ErrorCore.unknownPrimitiveNameErrorName e))]

-- | Show an unnecessary identity application error as a string
unnecessaryIdentityApplicationError :: t0 -> String
unnecessaryIdentityApplicationError e = "unnecessary application of identity lambda"

-- | Show an untyped term variable error as a string
untypedTermVariableError :: ErrorCore.UntypedTermVariableError -> String
untypedTermVariableError e =
    Strings.cat [
      "untyped term variable: ",
      (Core.unName (ErrorCore.untypedTermVariableErrorName e))]

-- | Show a void in non-bottom position error as a string
voidInNonBottomPositionError :: t0 -> String
voidInNonBottomPositionError e = "TypeVoid in a position where no value can be constructed"
