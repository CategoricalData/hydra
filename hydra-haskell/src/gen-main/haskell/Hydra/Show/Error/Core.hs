-- Note: this is an automatically generated file. Do not edit.

-- | String representations of hydra.error.core types

module Hydra.Show.Error.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as Core_
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Show.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Show a constant condition error as a string
constantConditionError :: Core_.ConstantConditionError -> String
constantConditionError e =
    Strings.cat [
      "constant condition: ifElse with literal ",
      (Literals.showBoolean (Core_.constantConditionErrorValue e))]

-- | Show a duplicate binding error as a string
duplicateBindingError :: Core_.DuplicateBindingError -> String
duplicateBindingError e =
    Strings.cat [
      "duplicate binding: ",
      (Core.unName (Core_.duplicateBindingErrorName e))]

-- | Show a duplicate field error as a string
duplicateFieldError :: Core_.DuplicateFieldError -> String
duplicateFieldError e =
    Strings.cat [
      "duplicate field: ",
      (Core.unName (Core_.duplicateFieldErrorName e))]

-- | Show a duplicate record type field names error as a string
duplicateRecordTypeFieldNamesError :: Core_.DuplicateRecordTypeFieldNamesError -> String
duplicateRecordTypeFieldNamesError e =
    Strings.cat [
      "duplicate field in record type: ",
      (Core.unName (Core_.duplicateRecordTypeFieldNamesErrorName e))]

-- | Show a duplicate union type field names error as a string
duplicateUnionTypeFieldNamesError :: Core_.DuplicateUnionTypeFieldNamesError -> String
duplicateUnionTypeFieldNamesError e =
    Strings.cat [
      "duplicate field in union type: ",
      (Core.unName (Core_.duplicateUnionTypeFieldNamesErrorName e))]

-- | Show an empty case statement error as a string
emptyCaseStatementError :: Core_.EmptyCaseStatementError -> String
emptyCaseStatementError e =
    Strings.cat [
      "empty case statement for type: ",
      (Core.unName (Core_.emptyCaseStatementErrorTypeName e))]

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
invalidForallParameterNameError :: Core_.InvalidForallParameterNameError -> String
invalidForallParameterNameError e =
    Strings.cat [
      "invalid forall parameter name: ",
      (Core.unName (Core_.invalidForallParameterNameErrorName e))]

-- | Show an invalid lambda parameter name error as a string
invalidLambdaParameterNameError :: Core_.InvalidLambdaParameterNameError -> String
invalidLambdaParameterNameError e =
    Strings.cat [
      "invalid lambda parameter name: ",
      (Core.unName (Core_.invalidLambdaParameterNameErrorName e))]

-- | Show an invalid let binding name error as a string
invalidLetBindingNameError :: Core_.InvalidLetBindingNameError -> String
invalidLetBindingNameError e =
    Strings.cat [
      "invalid let binding name: ",
      (Core.unName (Core_.invalidLetBindingNameErrorName e))]

-- | Show an invalid term error as a string
invalidTermError :: Core_.InvalidTermError -> String
invalidTermError e =
    Strings.cat2 "invalid term: " (case e of
      Core_.InvalidTermErrorConstantCondition v0 -> constantConditionError v0
      Core_.InvalidTermErrorDuplicateBinding v0 -> duplicateBindingError v0
      Core_.InvalidTermErrorDuplicateField v0 -> duplicateFieldError v0
      Core_.InvalidTermErrorEmptyCaseStatement v0 -> emptyCaseStatementError v0
      Core_.InvalidTermErrorEmptyLetBindings v0 -> emptyLetBindingsError v0
      Core_.InvalidTermErrorEmptyTermAnnotation v0 -> emptyTermAnnotationError v0
      Core_.InvalidTermErrorEmptyTypeNameInTerm v0 -> emptyTypeNameInTermError v0
      Core_.InvalidTermErrorInvalidLambdaParameterName v0 -> invalidLambdaParameterNameError v0
      Core_.InvalidTermErrorInvalidLetBindingName v0 -> invalidLetBindingNameError v0
      Core_.InvalidTermErrorInvalidTypeLambdaParameterName v0 -> invalidTypeLambdaParameterNameError v0
      Core_.InvalidTermErrorNestedTermAnnotation v0 -> nestedTermAnnotationError v0
      Core_.InvalidTermErrorRedundantWrapUnwrap v0 -> redundantWrapUnwrapError v0
      Core_.InvalidTermErrorSelfApplication v0 -> selfApplicationError v0
      Core_.InvalidTermErrorTermVariableShadowing v0 -> termVariableShadowingError v0
      Core_.InvalidTermErrorTypeVariableShadowingInTypeLambda v0 -> typeVariableShadowingInTypeLambdaError v0
      Core_.InvalidTermErrorUndefinedTermVariable v0 -> undefinedTermVariableError v0
      Core_.InvalidTermErrorUndefinedTypeVariableInBindingType v0 -> undefinedTypeVariableInBindingTypeError v0
      Core_.InvalidTermErrorUndefinedTypeVariableInLambdaDomain v0 -> undefinedTypeVariableInLambdaDomainError v0
      Core_.InvalidTermErrorUndefinedTypeVariableInTypeApplication v0 -> undefinedTypeVariableInTypeApplicationError v0
      Core_.InvalidTermErrorUnknownPrimitiveName v0 -> unknownPrimitiveNameError v0
      Core_.InvalidTermErrorUnnecessaryIdentityApplication v0 -> unnecessaryIdentityApplicationError v0
      Core_.InvalidTermErrorUntypedTermVariable v0 -> untypedTermVariableError v0)

-- | Show an invalid type error as a string
invalidTypeError :: Core_.InvalidTypeError -> String
invalidTypeError e =
    Strings.cat2 "invalid type: " (case e of
      Core_.InvalidTypeErrorDuplicateRecordTypeFieldNames v0 -> duplicateRecordTypeFieldNamesError v0
      Core_.InvalidTypeErrorDuplicateUnionTypeFieldNames v0 -> duplicateUnionTypeFieldNamesError v0
      Core_.InvalidTypeErrorEmptyRecordType v0 -> emptyRecordTypeError v0
      Core_.InvalidTypeErrorEmptyTypeAnnotation v0 -> emptyTypeAnnotationError v0
      Core_.InvalidTypeErrorEmptyUnionType v0 -> emptyUnionTypeError v0
      Core_.InvalidTypeErrorInvalidForallParameterName v0 -> invalidForallParameterNameError v0
      Core_.InvalidTypeErrorInvalidTypeSchemeVariableName v0 -> invalidTypeSchemeVariableNameError v0
      Core_.InvalidTypeErrorNestedTypeAnnotation v0 -> nestedTypeAnnotationError v0
      Core_.InvalidTypeErrorNonComparableMapKeyType v0 -> nonComparableMapKeyTypeError v0
      Core_.InvalidTypeErrorNonComparableSetElementType v0 -> nonComparableSetElementTypeError v0
      Core_.InvalidTypeErrorSingleVariantUnion v0 -> singleVariantUnionError v0
      Core_.InvalidTypeErrorTypeVariableShadowingInForall v0 -> typeVariableShadowingInForallError v0
      Core_.InvalidTypeErrorUndefinedTypeVariable v0 -> undefinedTypeVariableError v0
      Core_.InvalidTypeErrorVoidInNonBottomPosition v0 -> voidInNonBottomPositionError v0)

-- | Show an invalid type lambda parameter name error as a string
invalidTypeLambdaParameterNameError :: Core_.InvalidTypeLambdaParameterNameError -> String
invalidTypeLambdaParameterNameError e =
    Strings.cat [
      "invalid type lambda parameter name: ",
      (Core.unName (Core_.invalidTypeLambdaParameterNameErrorName e))]

-- | Show an invalid type scheme variable name error as a string
invalidTypeSchemeVariableNameError :: Core_.InvalidTypeSchemeVariableNameError -> String
invalidTypeSchemeVariableNameError e =
    Strings.cat [
      "invalid type scheme variable name: ",
      (Core.unName (Core_.invalidTypeSchemeVariableNameErrorName e))]

-- | Show a nested term annotation error as a string
nestedTermAnnotationError :: t0 -> String
nestedTermAnnotationError e = "nested term annotations should be merged"

-- | Show a nested type annotation error as a string
nestedTypeAnnotationError :: t0 -> String
nestedTypeAnnotationError e = "nested type annotations should be merged"

-- | Show a non-comparable map key type error as a string
nonComparableMapKeyTypeError :: Core_.NonComparableMapKeyTypeError -> String
nonComparableMapKeyTypeError e =
    Strings.cat [
      "map key type contains a function type: ",
      (Core__.type_ (Core_.nonComparableMapKeyTypeErrorKeyType e))]

-- | Show a non-comparable set element type error as a string
nonComparableSetElementTypeError :: Core_.NonComparableSetElementTypeError -> String
nonComparableSetElementTypeError e =
    Strings.cat [
      "set element type contains a function type: ",
      (Core__.type_ (Core_.nonComparableSetElementTypeErrorElementType e))]

-- | Show a redundant wrap/unwrap error as a string
redundantWrapUnwrapError :: Core_.RedundantWrapUnwrapError -> String
redundantWrapUnwrapError e =
    Strings.cat [
      "redundant wrap/unwrap for type: ",
      (Core.unName (Core_.redundantWrapUnwrapErrorTypeName e))]

-- | Show a self-application error as a string
selfApplicationError :: Core_.SelfApplicationError -> String
selfApplicationError e =
    Strings.cat [
      "self-application of variable: ",
      (Core.unName (Core_.selfApplicationErrorName e))]

-- | Show a single variant union error as a string
singleVariantUnionError :: Core_.SingleVariantUnionError -> String
singleVariantUnionError e =
    Strings.cat [
      "union type with single variant: ",
      (Core.unName (Core_.singleVariantUnionErrorFieldName e))]

-- | Show a term variable shadowing error as a string
termVariableShadowingError :: Core_.TermVariableShadowingError -> String
termVariableShadowingError e =
    Strings.cat [
      "variable shadowing: ",
      (Core.unName (Core_.termVariableShadowingErrorName e))]

-- | Show a type variable shadowing in forall error as a string
typeVariableShadowingInForallError :: Core_.TypeVariableShadowingInForallError -> String
typeVariableShadowingInForallError e =
    Strings.cat [
      "type variable shadowing in forall: ",
      (Core.unName (Core_.typeVariableShadowingInForallErrorName e))]

-- | Show a type variable shadowing in type lambda error as a string
typeVariableShadowingInTypeLambdaError :: Core_.TypeVariableShadowingInTypeLambdaError -> String
typeVariableShadowingInTypeLambdaError e =
    Strings.cat [
      "type variable shadowing in type lambda: ",
      (Core.unName (Core_.typeVariableShadowingInTypeLambdaErrorName e))]

-- | Show an undefined field error as a string
undefinedFieldError :: Core_.UndefinedFieldError -> String
undefinedFieldError e =

      let fname = Core_.undefinedFieldErrorFieldName e
          tname = Core_.undefinedFieldErrorTypeName e
      in (Strings.cat [
        "no such field \"",
        (Core.unName fname),
        "\" in type \"",
        (Core.unName tname),
        "\""])

-- | Show an undefined term variable error as a string
undefinedTermVariableError :: Core_.UndefinedTermVariableError -> String
undefinedTermVariableError e =
    Strings.cat [
      "undefined term variable: ",
      (Core.unName (Core_.undefinedTermVariableErrorName e))]

-- | Show an undefined type variable error as a string
undefinedTypeVariableError :: Core_.UndefinedTypeVariableError -> String
undefinedTypeVariableError e =
    Strings.cat [
      "undefined type variable: ",
      (Core.unName (Core_.undefinedTypeVariableErrorName e))]

-- | Show an undefined type variable in binding type error as a string
undefinedTypeVariableInBindingTypeError :: Core_.UndefinedTypeVariableInBindingTypeError -> String
undefinedTypeVariableInBindingTypeError e =
    Strings.cat [
      "undefined type variable in binding type: ",
      (Core.unName (Core_.undefinedTypeVariableInBindingTypeErrorName e))]

-- | Show an undefined type variable in lambda domain error as a string
undefinedTypeVariableInLambdaDomainError :: Core_.UndefinedTypeVariableInLambdaDomainError -> String
undefinedTypeVariableInLambdaDomainError e =
    Strings.cat [
      "undefined type variable in lambda domain: ",
      (Core.unName (Core_.undefinedTypeVariableInLambdaDomainErrorName e))]

-- | Show an undefined type variable in type application error as a string
undefinedTypeVariableInTypeApplicationError :: Core_.UndefinedTypeVariableInTypeApplicationError -> String
undefinedTypeVariableInTypeApplicationError e =
    Strings.cat [
      "undefined type variable in type application: ",
      (Core.unName (Core_.undefinedTypeVariableInTypeApplicationErrorName e))]

-- | Show an unexpected term variant error as a string
unexpectedTermVariantError :: Core_.UnexpectedTermVariantError -> String
unexpectedTermVariantError e =

      let expected = Core_.unexpectedTermVariantErrorExpectedVariant e
          actual = Core_.unexpectedTermVariantErrorActualTerm e
      in (Strings.cat [
        "expected ",
        (Variants.termVariant expected),
        " term but found ",
        (Core__.term actual)])

-- | Show an unexpected type variant error as a string
unexpectedTypeVariantError :: Core_.UnexpectedTypeVariantError -> String
unexpectedTypeVariantError e =

      let expected = Core_.unexpectedTypeVariantErrorExpectedVariant e
          actual = Core_.unexpectedTypeVariantErrorActualType e
      in (Strings.cat [
        "expected ",
        (Variants.typeVariant expected),
        " type but found ",
        (Core__.type_ actual)])

-- | Show an unknown primitive name error as a string
unknownPrimitiveNameError :: Core_.UnknownPrimitiveNameError -> String
unknownPrimitiveNameError e =
    Strings.cat [
      "unknown primitive: ",
      (Core.unName (Core_.unknownPrimitiveNameErrorName e))]

-- | Show an unnecessary identity application error as a string
unnecessaryIdentityApplicationError :: t0 -> String
unnecessaryIdentityApplicationError e = "unnecessary application of identity lambda"

-- | Show an untyped term variable error as a string
untypedTermVariableError :: Core_.UntypedTermVariableError -> String
untypedTermVariableError e =
    Strings.cat [
      "untyped term variable: ",
      (Core.unName (Core_.untypedTermVariableErrorName e))]

-- | Show a void in non-bottom position error as a string
voidInNonBottomPositionError :: t0 -> String
voidInNonBottomPositionError e = "TypeVoid in a position where no value can be constructed"
