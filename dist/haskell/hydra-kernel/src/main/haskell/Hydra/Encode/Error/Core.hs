-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.error.core

module Hydra.Encode.Error.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Paths as Paths
import qualified Hydra.Encode.Variants as Variants
import qualified Hydra.Error.Core as ErrorCore
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

constantConditionError :: ErrorCore.ConstantConditionError -> Core.Term
constantConditionError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.constantConditionErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBoolean x2)) (ErrorCore.constantConditionErrorValue x))}]})

duplicateBindingError :: ErrorCore.DuplicateBindingError -> Core.Term
duplicateBindingError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.duplicateBindingErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.duplicateBindingErrorName x))}]})

duplicateFieldError :: ErrorCore.DuplicateFieldError -> Core.Term
duplicateFieldError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.duplicateFieldErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.duplicateFieldErrorName x))}]})

duplicateRecordTypeFieldNamesError :: ErrorCore.DuplicateRecordTypeFieldNamesError -> Core.Term
duplicateRecordTypeFieldNamesError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.duplicateRecordTypeFieldNamesErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.duplicateRecordTypeFieldNamesErrorName x))}]})

duplicateUnionTypeFieldNamesError :: ErrorCore.DuplicateUnionTypeFieldNamesError -> Core.Term
duplicateUnionTypeFieldNamesError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.duplicateUnionTypeFieldNamesErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.duplicateUnionTypeFieldNamesErrorName x))}]})

emptyCaseStatementError :: ErrorCore.EmptyCaseStatementError -> Core.Term
emptyCaseStatementError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.emptyCaseStatementErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.emptyCaseStatementErrorTypeName x))}]})

emptyLetBindingsError :: ErrorCore.EmptyLetBindingsError -> Core.Term
emptyLetBindingsError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.emptyLetBindingsErrorLocation x))}]})

emptyRecordTypeError :: ErrorCore.EmptyRecordTypeError -> Core.Term
emptyRecordTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.emptyRecordTypeErrorLocation x))}]})

emptyTermAnnotationError :: ErrorCore.EmptyTermAnnotationError -> Core.Term
emptyTermAnnotationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.emptyTermAnnotationErrorLocation x))}]})

emptyTypeAnnotationError :: ErrorCore.EmptyTypeAnnotationError -> Core.Term
emptyTypeAnnotationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.emptyTypeAnnotationErrorLocation x))}]})

emptyTypeNameInTermError :: ErrorCore.EmptyTypeNameInTermError -> Core.Term
emptyTypeNameInTermError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.emptyTypeNameInTermErrorLocation x))}]})

emptyUnionTypeError :: ErrorCore.EmptyUnionTypeError -> Core.Term
emptyUnionTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.emptyUnionTypeErrorLocation x))}]})

invalidForallParameterNameError :: ErrorCore.InvalidForallParameterNameError -> Core.Term
invalidForallParameterNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.invalidForallParameterNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.invalidForallParameterNameErrorName x))}]})

invalidLambdaParameterNameError :: ErrorCore.InvalidLambdaParameterNameError -> Core.Term
invalidLambdaParameterNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.invalidLambdaParameterNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.invalidLambdaParameterNameErrorName x))}]})

invalidLetBindingNameError :: ErrorCore.InvalidLetBindingNameError -> Core.Term
invalidLetBindingNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.invalidLetBindingNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.invalidLetBindingNameErrorName x))}]})

invalidTermError :: ErrorCore.InvalidTermError -> Core.Term
invalidTermError x =
    case x of
      ErrorCore.InvalidTermErrorConstantCondition v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "constantCondition"),
          Core.fieldTerm = (constantConditionError v0)}})
      ErrorCore.InvalidTermErrorDuplicateBinding v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateBinding"),
          Core.fieldTerm = (duplicateBindingError v0)}})
      ErrorCore.InvalidTermErrorDuplicateField v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateField"),
          Core.fieldTerm = (duplicateFieldError v0)}})
      ErrorCore.InvalidTermErrorEmptyCaseStatement v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyCaseStatement"),
          Core.fieldTerm = (emptyCaseStatementError v0)}})
      ErrorCore.InvalidTermErrorEmptyLetBindings v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyLetBindings"),
          Core.fieldTerm = (emptyLetBindingsError v0)}})
      ErrorCore.InvalidTermErrorEmptyTermAnnotation v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyTermAnnotation"),
          Core.fieldTerm = (emptyTermAnnotationError v0)}})
      ErrorCore.InvalidTermErrorEmptyTypeNameInTerm v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyTypeNameInTerm"),
          Core.fieldTerm = (emptyTypeNameInTermError v0)}})
      ErrorCore.InvalidTermErrorInvalidLambdaParameterName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidLambdaParameterName"),
          Core.fieldTerm = (invalidLambdaParameterNameError v0)}})
      ErrorCore.InvalidTermErrorInvalidLetBindingName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidLetBindingName"),
          Core.fieldTerm = (invalidLetBindingNameError v0)}})
      ErrorCore.InvalidTermErrorInvalidTypeLambdaParameterName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidTypeLambdaParameterName"),
          Core.fieldTerm = (invalidTypeLambdaParameterNameError v0)}})
      ErrorCore.InvalidTermErrorNestedTermAnnotation v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "nestedTermAnnotation"),
          Core.fieldTerm = (nestedTermAnnotationError v0)}})
      ErrorCore.InvalidTermErrorRedundantWrapUnwrap v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "redundantWrapUnwrap"),
          Core.fieldTerm = (redundantWrapUnwrapError v0)}})
      ErrorCore.InvalidTermErrorSelfApplication v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "selfApplication"),
          Core.fieldTerm = (selfApplicationError v0)}})
      ErrorCore.InvalidTermErrorTermVariableShadowing v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "termVariableShadowing"),
          Core.fieldTerm = (termVariableShadowingError v0)}})
      ErrorCore.InvalidTermErrorTypeVariableShadowingInTypeLambda v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typeVariableShadowingInTypeLambda"),
          Core.fieldTerm = (typeVariableShadowingInTypeLambdaError v0)}})
      ErrorCore.InvalidTermErrorUndefinedTermVariable v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTermVariable"),
          Core.fieldTerm = (undefinedTermVariableError v0)}})
      ErrorCore.InvalidTermErrorUndefinedTypeVariableInBindingType v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTypeVariableInBindingType"),
          Core.fieldTerm = (undefinedTypeVariableInBindingTypeError v0)}})
      ErrorCore.InvalidTermErrorUndefinedTypeVariableInLambdaDomain v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTypeVariableInLambdaDomain"),
          Core.fieldTerm = (undefinedTypeVariableInLambdaDomainError v0)}})
      ErrorCore.InvalidTermErrorUndefinedTypeVariableInTypeApplication v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTypeVariableInTypeApplication"),
          Core.fieldTerm = (undefinedTypeVariableInTypeApplicationError v0)}})
      ErrorCore.InvalidTermErrorUnknownPrimitiveName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unknownPrimitiveName"),
          Core.fieldTerm = (unknownPrimitiveNameError v0)}})
      ErrorCore.InvalidTermErrorUnnecessaryIdentityApplication v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unnecessaryIdentityApplication"),
          Core.fieldTerm = (unnecessaryIdentityApplicationError v0)}})
      ErrorCore.InvalidTermErrorUntypedTermVariable v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "untypedTermVariable"),
          Core.fieldTerm = (untypedTermVariableError v0)}})

invalidTypeError :: ErrorCore.InvalidTypeError -> Core.Term
invalidTypeError x =
    case x of
      ErrorCore.InvalidTypeErrorDuplicateRecordTypeFieldNames v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateRecordTypeFieldNames"),
          Core.fieldTerm = (duplicateRecordTypeFieldNamesError v0)}})
      ErrorCore.InvalidTypeErrorDuplicateUnionTypeFieldNames v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateUnionTypeFieldNames"),
          Core.fieldTerm = (duplicateUnionTypeFieldNamesError v0)}})
      ErrorCore.InvalidTypeErrorEmptyRecordType v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyRecordType"),
          Core.fieldTerm = (emptyRecordTypeError v0)}})
      ErrorCore.InvalidTypeErrorEmptyTypeAnnotation v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyTypeAnnotation"),
          Core.fieldTerm = (emptyTypeAnnotationError v0)}})
      ErrorCore.InvalidTypeErrorEmptyUnionType v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyUnionType"),
          Core.fieldTerm = (emptyUnionTypeError v0)}})
      ErrorCore.InvalidTypeErrorInvalidForallParameterName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidForallParameterName"),
          Core.fieldTerm = (invalidForallParameterNameError v0)}})
      ErrorCore.InvalidTypeErrorInvalidTypeSchemeVariableName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidTypeSchemeVariableName"),
          Core.fieldTerm = (invalidTypeSchemeVariableNameError v0)}})
      ErrorCore.InvalidTypeErrorNestedTypeAnnotation v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "nestedTypeAnnotation"),
          Core.fieldTerm = (nestedTypeAnnotationError v0)}})
      ErrorCore.InvalidTypeErrorNonComparableMapKeyType v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "nonComparableMapKeyType"),
          Core.fieldTerm = (nonComparableMapKeyTypeError v0)}})
      ErrorCore.InvalidTypeErrorNonComparableSetElementType v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "nonComparableSetElementType"),
          Core.fieldTerm = (nonComparableSetElementTypeError v0)}})
      ErrorCore.InvalidTypeErrorSingleVariantUnion v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "singleVariantUnion"),
          Core.fieldTerm = (singleVariantUnionError v0)}})
      ErrorCore.InvalidTypeErrorTypeVariableShadowingInForall v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typeVariableShadowingInForall"),
          Core.fieldTerm = (typeVariableShadowingInForallError v0)}})
      ErrorCore.InvalidTypeErrorUndefinedTypeVariable v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTypeVariable"),
          Core.fieldTerm = (undefinedTypeVariableError v0)}})
      ErrorCore.InvalidTypeErrorVoidInNonBottomPosition v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "voidInNonBottomPosition"),
          Core.fieldTerm = (voidInNonBottomPositionError v0)}})

invalidTypeLambdaParameterNameError :: ErrorCore.InvalidTypeLambdaParameterNameError -> Core.Term
invalidTypeLambdaParameterNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.invalidTypeLambdaParameterNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.invalidTypeLambdaParameterNameErrorName x))}]})

invalidTypeSchemeVariableNameError :: ErrorCore.InvalidTypeSchemeVariableNameError -> Core.Term
invalidTypeSchemeVariableNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.invalidTypeSchemeVariableNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.invalidTypeSchemeVariableNameErrorName x))}]})

nestedTermAnnotationError :: ErrorCore.NestedTermAnnotationError -> Core.Term
nestedTermAnnotationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.nestedTermAnnotationErrorLocation x))}]})

nestedTypeAnnotationError :: ErrorCore.NestedTypeAnnotationError -> Core.Term
nestedTypeAnnotationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.nestedTypeAnnotationErrorLocation x))}]})

nonComparableMapKeyTypeError :: ErrorCore.NonComparableMapKeyTypeError -> Core.Term
nonComparableMapKeyTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.nonComparableMapKeyTypeErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (EncodeCore.type_ (ErrorCore.nonComparableMapKeyTypeErrorKeyType x))}]})

nonComparableSetElementTypeError :: ErrorCore.NonComparableSetElementTypeError -> Core.Term
nonComparableSetElementTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.nonComparableSetElementTypeErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (EncodeCore.type_ (ErrorCore.nonComparableSetElementTypeErrorElementType x))}]})

redundantWrapUnwrapError :: ErrorCore.RedundantWrapUnwrapError -> Core.Term
redundantWrapUnwrapError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.redundantWrapUnwrapErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.redundantWrapUnwrapErrorTypeName x))}]})

selfApplicationError :: ErrorCore.SelfApplicationError -> Core.Term
selfApplicationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.selfApplicationErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.selfApplicationErrorName x))}]})

singleVariantUnionError :: ErrorCore.SingleVariantUnionError -> Core.Term
singleVariantUnionError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.singleVariantUnionErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.singleVariantUnionErrorFieldName x))}]})

termVariableShadowingError :: ErrorCore.TermVariableShadowingError -> Core.Term
termVariableShadowingError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.termVariableShadowingErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.termVariableShadowingErrorName x))}]})

typeVariableShadowingInForallError :: ErrorCore.TypeVariableShadowingInForallError -> Core.Term
typeVariableShadowingInForallError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.typeVariableShadowingInForallErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.typeVariableShadowingInForallErrorName x))}]})

typeVariableShadowingInTypeLambdaError :: ErrorCore.TypeVariableShadowingInTypeLambdaError -> Core.Term
typeVariableShadowingInTypeLambdaError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.typeVariableShadowingInTypeLambdaErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.typeVariableShadowingInTypeLambdaErrorName x))}]})

undefinedFieldError :: ErrorCore.UndefinedFieldError -> Core.Term
undefinedFieldError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.undefinedFieldErrorFieldName x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.undefinedFieldErrorTypeName x))}]})

undefinedTermVariableError :: ErrorCore.UndefinedTermVariableError -> Core.Term
undefinedTermVariableError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.undefinedTermVariableErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.undefinedTermVariableErrorName x))}]})

undefinedTypeVariableError :: ErrorCore.UndefinedTypeVariableError -> Core.Term
undefinedTypeVariableError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.undefinedTypeVariableErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.undefinedTypeVariableErrorName x))}]})

undefinedTypeVariableInBindingTypeError :: ErrorCore.UndefinedTypeVariableInBindingTypeError -> Core.Term
undefinedTypeVariableInBindingTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.undefinedTypeVariableInBindingTypeErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.undefinedTypeVariableInBindingTypeErrorName x))}]})

undefinedTypeVariableInLambdaDomainError :: ErrorCore.UndefinedTypeVariableInLambdaDomainError -> Core.Term
undefinedTypeVariableInLambdaDomainError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.undefinedTypeVariableInLambdaDomainErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.undefinedTypeVariableInLambdaDomainErrorName x))}]})

undefinedTypeVariableInTypeApplicationError :: ErrorCore.UndefinedTypeVariableInTypeApplicationError -> Core.Term
undefinedTypeVariableInTypeApplicationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.undefinedTypeVariableInTypeApplicationErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.undefinedTypeVariableInTypeApplicationErrorName x))}]})

unexpectedTermVariantError :: ErrorCore.UnexpectedTermVariantError -> Core.Term
unexpectedTermVariantError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Variants.termVariant (ErrorCore.unexpectedTermVariantErrorExpectedVariant x))},
        Core.Field {
          Core.fieldName = (Core.Name "actualTerm"),
          Core.fieldTerm = (EncodeCore.term (ErrorCore.unexpectedTermVariantErrorActualTerm x))}]})

unexpectedTypeVariantError :: ErrorCore.UnexpectedTypeVariantError -> Core.Term
unexpectedTypeVariantError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Variants.typeVariant (ErrorCore.unexpectedTypeVariantErrorExpectedVariant x))},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (EncodeCore.type_ (ErrorCore.unexpectedTypeVariantErrorActualType x))}]})

unknownPrimitiveNameError :: ErrorCore.UnknownPrimitiveNameError -> Core.Term
unknownPrimitiveNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.unknownPrimitiveNameErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.unknownPrimitiveNameErrorName x))}]})

unnecessaryIdentityApplicationError :: ErrorCore.UnnecessaryIdentityApplicationError -> Core.Term
unnecessaryIdentityApplicationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.unnecessaryIdentityApplicationErrorLocation x))}]})

untypedTermVariableError :: ErrorCore.UntypedTermVariableError -> Core.Term
untypedTermVariableError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.untypedTermVariableErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorCore.untypedTermVariableErrorName x))}]})

voidInNonBottomPositionError :: ErrorCore.VoidInNonBottomPositionError -> Core.Term
voidInNonBottomPositionError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Paths.subtermPath (ErrorCore.voidInNonBottomPositionErrorLocation x))}]})
