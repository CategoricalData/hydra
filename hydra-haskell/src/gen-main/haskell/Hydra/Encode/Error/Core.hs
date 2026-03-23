-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.error.core

module Hydra.Encode.Error.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Accessors as Accessors
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Encode.Variants as Variants
import qualified Hydra.Error.Core as Core__
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

duplicateBindingError :: Core__.DuplicateBindingError -> Core.Term
duplicateBindingError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.duplicateBindingErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.duplicateBindingErrorName x))}]})

duplicateFieldError :: Core__.DuplicateFieldError -> Core.Term
duplicateFieldError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.duplicateFieldErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.duplicateFieldErrorName x))}]})

undefinedFieldError :: Core__.UndefinedFieldError -> Core.Term
undefinedFieldError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Core_.name (Core__.undefinedFieldErrorFieldName x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core_.name (Core__.undefinedFieldErrorTypeName x))}]})

unexpectedTermVariantError :: Core__.UnexpectedTermVariantError -> Core.Term
unexpectedTermVariantError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Variants.termVariant (Core__.unexpectedTermVariantErrorExpectedVariant x))},
        Core.Field {
          Core.fieldName = (Core.Name "actualTerm"),
          Core.fieldTerm = (Core_.term (Core__.unexpectedTermVariantErrorActualTerm x))}]})

unexpectedTypeVariantError :: Core__.UnexpectedTypeVariantError -> Core.Term
unexpectedTypeVariantError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Variants.typeVariant (Core__.unexpectedTypeVariantErrorExpectedVariant x))},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Core_.type_ (Core__.unexpectedTypeVariantErrorActualType x))}]})

constantConditionError :: Core__.ConstantConditionError -> Core.Term
constantConditionError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.constantConditionErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralBoolean x)) (Core__.constantConditionErrorValue x))}]})

emptyCaseStatementError :: Core__.EmptyCaseStatementError -> Core.Term
emptyCaseStatementError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.emptyCaseStatementErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core_.name (Core__.emptyCaseStatementErrorTypeName x))}]})

emptyLetBindingsError :: Core__.EmptyLetBindingsError -> Core.Term
emptyLetBindingsError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.emptyLetBindingsErrorLocation x))}]})

emptyTermAnnotationError :: Core__.EmptyTermAnnotationError -> Core.Term
emptyTermAnnotationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.emptyTermAnnotationErrorLocation x))}]})

emptyTypeNameInTermError :: Core__.EmptyTypeNameInTermError -> Core.Term
emptyTypeNameInTermError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.emptyTypeNameInTermErrorLocation x))}]})

invalidLambdaParameterNameError :: Core__.InvalidLambdaParameterNameError -> Core.Term
invalidLambdaParameterNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.invalidLambdaParameterNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.invalidLambdaParameterNameErrorName x))}]})

invalidLetBindingNameError :: Core__.InvalidLetBindingNameError -> Core.Term
invalidLetBindingNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.invalidLetBindingNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.invalidLetBindingNameErrorName x))}]})

invalidTypeLambdaParameterNameError :: Core__.InvalidTypeLambdaParameterNameError -> Core.Term
invalidTypeLambdaParameterNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.invalidTypeLambdaParameterNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.invalidTypeLambdaParameterNameErrorName x))}]})

nestedTermAnnotationError :: Core__.NestedTermAnnotationError -> Core.Term
nestedTermAnnotationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.nestedTermAnnotationErrorLocation x))}]})

redundantWrapUnwrapError :: Core__.RedundantWrapUnwrapError -> Core.Term
redundantWrapUnwrapError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.redundantWrapUnwrapErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core_.name (Core__.redundantWrapUnwrapErrorTypeName x))}]})

selfApplicationError :: Core__.SelfApplicationError -> Core.Term
selfApplicationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.selfApplicationErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.selfApplicationErrorName x))}]})

termVariableShadowingError :: Core__.TermVariableShadowingError -> Core.Term
termVariableShadowingError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.termVariableShadowingErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.termVariableShadowingErrorName x))}]})

typeVariableShadowingInTypeLambdaError :: Core__.TypeVariableShadowingInTypeLambdaError -> Core.Term
typeVariableShadowingInTypeLambdaError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.typeVariableShadowingInTypeLambdaErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.typeVariableShadowingInTypeLambdaErrorName x))}]})

undefinedTermVariableError :: Core__.UndefinedTermVariableError -> Core.Term
undefinedTermVariableError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.undefinedTermVariableErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.undefinedTermVariableErrorName x))}]})

undefinedTypeVariableInBindingTypeError :: Core__.UndefinedTypeVariableInBindingTypeError -> Core.Term
undefinedTypeVariableInBindingTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.undefinedTypeVariableInBindingTypeErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.undefinedTypeVariableInBindingTypeErrorName x))}]})

undefinedTypeVariableInLambdaDomainError :: Core__.UndefinedTypeVariableInLambdaDomainError -> Core.Term
undefinedTypeVariableInLambdaDomainError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.undefinedTypeVariableInLambdaDomainErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.undefinedTypeVariableInLambdaDomainErrorName x))}]})

undefinedTypeVariableInTypeApplicationError :: Core__.UndefinedTypeVariableInTypeApplicationError -> Core.Term
undefinedTypeVariableInTypeApplicationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.undefinedTypeVariableInTypeApplicationErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.undefinedTypeVariableInTypeApplicationErrorName x))}]})

unknownPrimitiveNameError :: Core__.UnknownPrimitiveNameError -> Core.Term
unknownPrimitiveNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.unknownPrimitiveNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.unknownPrimitiveNameErrorName x))}]})

unnecessaryIdentityApplicationError :: Core__.UnnecessaryIdentityApplicationError -> Core.Term
unnecessaryIdentityApplicationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.unnecessaryIdentityApplicationErrorLocation x))}]})

untypedTermVariableError :: Core__.UntypedTermVariableError -> Core.Term
untypedTermVariableError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.untypedTermVariableErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.untypedTermVariableErrorName x))}]})

invalidTermError :: Core__.InvalidTermError -> Core.Term
invalidTermError x =
    case x of
      Core__.InvalidTermErrorConstantCondition v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "constantCondition"),
          Core.fieldTerm = (constantConditionError v0)}})
      Core__.InvalidTermErrorDuplicateBinding v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateBinding"),
          Core.fieldTerm = (duplicateBindingError v0)}})
      Core__.InvalidTermErrorDuplicateField v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateField"),
          Core.fieldTerm = (duplicateFieldError v0)}})
      Core__.InvalidTermErrorEmptyCaseStatement v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyCaseStatement"),
          Core.fieldTerm = (emptyCaseStatementError v0)}})
      Core__.InvalidTermErrorEmptyLetBindings v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyLetBindings"),
          Core.fieldTerm = (emptyLetBindingsError v0)}})
      Core__.InvalidTermErrorEmptyTermAnnotation v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyTermAnnotation"),
          Core.fieldTerm = (emptyTermAnnotationError v0)}})
      Core__.InvalidTermErrorEmptyTypeNameInTerm v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyTypeNameInTerm"),
          Core.fieldTerm = (emptyTypeNameInTermError v0)}})
      Core__.InvalidTermErrorInvalidLambdaParameterName v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidLambdaParameterName"),
          Core.fieldTerm = (invalidLambdaParameterNameError v0)}})
      Core__.InvalidTermErrorInvalidLetBindingName v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidLetBindingName"),
          Core.fieldTerm = (invalidLetBindingNameError v0)}})
      Core__.InvalidTermErrorInvalidTypeLambdaParameterName v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidTypeLambdaParameterName"),
          Core.fieldTerm = (invalidTypeLambdaParameterNameError v0)}})
      Core__.InvalidTermErrorNestedTermAnnotation v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "nestedTermAnnotation"),
          Core.fieldTerm = (nestedTermAnnotationError v0)}})
      Core__.InvalidTermErrorRedundantWrapUnwrap v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "redundantWrapUnwrap"),
          Core.fieldTerm = (redundantWrapUnwrapError v0)}})
      Core__.InvalidTermErrorSelfApplication v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "selfApplication"),
          Core.fieldTerm = (selfApplicationError v0)}})
      Core__.InvalidTermErrorTermVariableShadowing v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "termVariableShadowing"),
          Core.fieldTerm = (termVariableShadowingError v0)}})
      Core__.InvalidTermErrorTypeVariableShadowingInTypeLambda v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typeVariableShadowingInTypeLambda"),
          Core.fieldTerm = (typeVariableShadowingInTypeLambdaError v0)}})
      Core__.InvalidTermErrorUndefinedTermVariable v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTermVariable"),
          Core.fieldTerm = (undefinedTermVariableError v0)}})
      Core__.InvalidTermErrorUndefinedTypeVariableInBindingType v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTypeVariableInBindingType"),
          Core.fieldTerm = (undefinedTypeVariableInBindingTypeError v0)}})
      Core__.InvalidTermErrorUndefinedTypeVariableInLambdaDomain v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTypeVariableInLambdaDomain"),
          Core.fieldTerm = (undefinedTypeVariableInLambdaDomainError v0)}})
      Core__.InvalidTermErrorUndefinedTypeVariableInTypeApplication v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTypeVariableInTypeApplication"),
          Core.fieldTerm = (undefinedTypeVariableInTypeApplicationError v0)}})
      Core__.InvalidTermErrorUnknownPrimitiveName v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unknownPrimitiveName"),
          Core.fieldTerm = (unknownPrimitiveNameError v0)}})
      Core__.InvalidTermErrorUnnecessaryIdentityApplication v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unnecessaryIdentityApplication"),
          Core.fieldTerm = (unnecessaryIdentityApplicationError v0)}})
      Core__.InvalidTermErrorUntypedTermVariable v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "untypedTermVariable"),
          Core.fieldTerm = (untypedTermVariableError v0)}})

duplicateRecordTypeFieldNamesError :: Core__.DuplicateRecordTypeFieldNamesError -> Core.Term
duplicateRecordTypeFieldNamesError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.duplicateRecordTypeFieldNamesErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.duplicateRecordTypeFieldNamesErrorName x))}]})

duplicateUnionTypeFieldNamesError :: Core__.DuplicateUnionTypeFieldNamesError -> Core.Term
duplicateUnionTypeFieldNamesError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.duplicateUnionTypeFieldNamesErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.duplicateUnionTypeFieldNamesErrorName x))}]})

emptyRecordTypeError :: Core__.EmptyRecordTypeError -> Core.Term
emptyRecordTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.emptyRecordTypeErrorLocation x))}]})

emptyTypeAnnotationError :: Core__.EmptyTypeAnnotationError -> Core.Term
emptyTypeAnnotationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.emptyTypeAnnotationErrorLocation x))}]})

emptyUnionTypeError :: Core__.EmptyUnionTypeError -> Core.Term
emptyUnionTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.emptyUnionTypeErrorLocation x))}]})

invalidForallParameterNameError :: Core__.InvalidForallParameterNameError -> Core.Term
invalidForallParameterNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.invalidForallParameterNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.invalidForallParameterNameErrorName x))}]})

invalidTypeSchemeVariableNameError :: Core__.InvalidTypeSchemeVariableNameError -> Core.Term
invalidTypeSchemeVariableNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.invalidTypeSchemeVariableNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.invalidTypeSchemeVariableNameErrorName x))}]})

nestedTypeAnnotationError :: Core__.NestedTypeAnnotationError -> Core.Term
nestedTypeAnnotationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.nestedTypeAnnotationErrorLocation x))}]})

nonComparableMapKeyTypeError :: Core__.NonComparableMapKeyTypeError -> Core.Term
nonComparableMapKeyTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.nonComparableMapKeyTypeErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Core_.type_ (Core__.nonComparableMapKeyTypeErrorKeyType x))}]})

nonComparableSetElementTypeError :: Core__.NonComparableSetElementTypeError -> Core.Term
nonComparableSetElementTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.nonComparableSetElementTypeErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Core_.type_ (Core__.nonComparableSetElementTypeErrorElementType x))}]})

singleVariantUnionError :: Core__.SingleVariantUnionError -> Core.Term
singleVariantUnionError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.singleVariantUnionErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Core_.name (Core__.singleVariantUnionErrorFieldName x))}]})

typeVariableShadowingInForallError :: Core__.TypeVariableShadowingInForallError -> Core.Term
typeVariableShadowingInForallError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.typeVariableShadowingInForallErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.typeVariableShadowingInForallErrorName x))}]})

undefinedTypeVariableError :: Core__.UndefinedTypeVariableError -> Core.Term
undefinedTypeVariableError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.undefinedTypeVariableErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.undefinedTypeVariableErrorName x))}]})

voidInNonBottomPositionError :: Core__.VoidInNonBottomPositionError -> Core.Term
voidInNonBottomPositionError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.voidInNonBottomPositionErrorLocation x))}]})

invalidTypeError :: Core__.InvalidTypeError -> Core.Term
invalidTypeError x =
    case x of
      Core__.InvalidTypeErrorDuplicateRecordTypeFieldNames v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateRecordTypeFieldNames"),
          Core.fieldTerm = (duplicateRecordTypeFieldNamesError v0)}})
      Core__.InvalidTypeErrorDuplicateUnionTypeFieldNames v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateUnionTypeFieldNames"),
          Core.fieldTerm = (duplicateUnionTypeFieldNamesError v0)}})
      Core__.InvalidTypeErrorEmptyRecordType v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyRecordType"),
          Core.fieldTerm = (emptyRecordTypeError v0)}})
      Core__.InvalidTypeErrorEmptyTypeAnnotation v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyTypeAnnotation"),
          Core.fieldTerm = (emptyTypeAnnotationError v0)}})
      Core__.InvalidTypeErrorEmptyUnionType v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyUnionType"),
          Core.fieldTerm = (emptyUnionTypeError v0)}})
      Core__.InvalidTypeErrorInvalidForallParameterName v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidForallParameterName"),
          Core.fieldTerm = (invalidForallParameterNameError v0)}})
      Core__.InvalidTypeErrorInvalidTypeSchemeVariableName v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidTypeSchemeVariableName"),
          Core.fieldTerm = (invalidTypeSchemeVariableNameError v0)}})
      Core__.InvalidTypeErrorNestedTypeAnnotation v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "nestedTypeAnnotation"),
          Core.fieldTerm = (nestedTypeAnnotationError v0)}})
      Core__.InvalidTypeErrorNonComparableMapKeyType v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "nonComparableMapKeyType"),
          Core.fieldTerm = (nonComparableMapKeyTypeError v0)}})
      Core__.InvalidTypeErrorNonComparableSetElementType v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "nonComparableSetElementType"),
          Core.fieldTerm = (nonComparableSetElementTypeError v0)}})
      Core__.InvalidTypeErrorSingleVariantUnion v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "singleVariantUnion"),
          Core.fieldTerm = (singleVariantUnionError v0)}})
      Core__.InvalidTypeErrorTypeVariableShadowingInForall v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typeVariableShadowingInForall"),
          Core.fieldTerm = (typeVariableShadowingInForallError v0)}})
      Core__.InvalidTypeErrorUndefinedTypeVariable v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTypeVariable"),
          Core.fieldTerm = (undefinedTypeVariableError v0)}})
      Core__.InvalidTypeErrorVoidInNonBottomPosition v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "voidInNonBottomPosition"),
          Core.fieldTerm = (voidInNonBottomPositionError v0)}})
