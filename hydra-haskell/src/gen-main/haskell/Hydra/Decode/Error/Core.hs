-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.error.core

module Hydra.Decode.Error.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Decode.Paths as Paths
import qualified Hydra.Decode.Variants as Variants
import qualified Hydra.Error.Core as Core__
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

constantConditionError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.ConstantConditionError
constantConditionError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "value" (\cx2 -> \raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx2 raw2)) fieldMap cx) (\field_value -> Right (Core__.ConstantConditionError {
          Core__.constantConditionErrorLocation = field_location,
          Core__.constantConditionErrorValue = field_value}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

duplicateBindingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.DuplicateBindingError
duplicateBindingError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.DuplicateBindingError {
          Core__.duplicateBindingErrorLocation = field_location,
          Core__.duplicateBindingErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

duplicateFieldError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.DuplicateFieldError
duplicateFieldError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.DuplicateFieldError {
          Core__.duplicateFieldErrorLocation = field_location,
          Core__.duplicateFieldErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

duplicateRecordTypeFieldNamesError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.DuplicateRecordTypeFieldNamesError {
          Core__.duplicateRecordTypeFieldNamesErrorLocation = field_location,
          Core__.duplicateRecordTypeFieldNamesErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

duplicateUnionTypeFieldNamesError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.DuplicateUnionTypeFieldNamesError {
          Core__.duplicateUnionTypeFieldNamesErrorLocation = field_location,
          Core__.duplicateUnionTypeFieldNamesErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

emptyCaseStatementError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.EmptyCaseStatementError
emptyCaseStatementError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "typeName" Core_.name fieldMap cx) (\field_typeName -> Right (Core__.EmptyCaseStatementError {
          Core__.emptyCaseStatementErrorLocation = field_location,
          Core__.emptyCaseStatementErrorTypeName = field_typeName}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

emptyLetBindingsError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.EmptyLetBindingsError
emptyLetBindingsError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (Core__.EmptyLetBindingsError {
          Core__.emptyLetBindingsErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

emptyRecordTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.EmptyRecordTypeError
emptyRecordTypeError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (Core__.EmptyRecordTypeError {
          Core__.emptyRecordTypeErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

emptyTermAnnotationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.EmptyTermAnnotationError
emptyTermAnnotationError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (Core__.EmptyTermAnnotationError {
          Core__.emptyTermAnnotationErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

emptyTypeAnnotationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.EmptyTypeAnnotationError
emptyTypeAnnotationError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (Core__.EmptyTypeAnnotationError {
          Core__.emptyTypeAnnotationErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

emptyTypeNameInTermError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.EmptyTypeNameInTermError
emptyTypeNameInTermError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (Core__.EmptyTypeNameInTermError {
          Core__.emptyTypeNameInTermErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

emptyUnionTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.EmptyUnionTypeError
emptyUnionTypeError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (Core__.EmptyUnionTypeError {
          Core__.emptyUnionTypeErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

invalidForallParameterNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.InvalidForallParameterNameError
invalidForallParameterNameError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.InvalidForallParameterNameError {
          Core__.invalidForallParameterNameErrorLocation = field_location,
          Core__.invalidForallParameterNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

invalidLambdaParameterNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.InvalidLambdaParameterNameError
invalidLambdaParameterNameError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.InvalidLambdaParameterNameError {
          Core__.invalidLambdaParameterNameErrorLocation = field_location,
          Core__.invalidLambdaParameterNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

invalidLetBindingNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.InvalidLetBindingNameError
invalidLetBindingNameError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.InvalidLetBindingNameError {
          Core__.invalidLetBindingNameErrorLocation = field_location,
          Core__.invalidLetBindingNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

invalidTermError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.InvalidTermError
invalidTermError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "constantCondition", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorConstantCondition t) (constantConditionError cx input))),
                      (Core.Name "duplicateBinding", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorDuplicateBinding t) (duplicateBindingError cx input))),
                      (Core.Name "duplicateField", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorDuplicateField t) (duplicateFieldError cx input))),
                      (Core.Name "emptyCaseStatement", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorEmptyCaseStatement t) (emptyCaseStatementError cx input))),
                      (Core.Name "emptyLetBindings", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorEmptyLetBindings t) (emptyLetBindingsError cx input))),
                      (Core.Name "emptyTermAnnotation", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorEmptyTermAnnotation t) (emptyTermAnnotationError cx input))),
                      (Core.Name "emptyTypeNameInTerm", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorEmptyTypeNameInTerm t) (emptyTypeNameInTermError cx input))),
                      (Core.Name "invalidLambdaParameterName", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorInvalidLambdaParameterName t) (invalidLambdaParameterNameError cx input))),
                      (Core.Name "invalidLetBindingName", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorInvalidLetBindingName t) (invalidLetBindingNameError cx input))),
                      (Core.Name "invalidTypeLambdaParameterName", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorInvalidTypeLambdaParameterName t) (invalidTypeLambdaParameterNameError cx input))),
                      (Core.Name "nestedTermAnnotation", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorNestedTermAnnotation t) (nestedTermAnnotationError cx input))),
                      (Core.Name "redundantWrapUnwrap", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorRedundantWrapUnwrap t) (redundantWrapUnwrapError cx input))),
                      (Core.Name "selfApplication", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorSelfApplication t) (selfApplicationError cx input))),
                      (Core.Name "termVariableShadowing", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorTermVariableShadowing t) (termVariableShadowingError cx input))),
                      (Core.Name "typeVariableShadowingInTypeLambda", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorTypeVariableShadowingInTypeLambda t) (typeVariableShadowingInTypeLambdaError cx input))),
                      (Core.Name "undefinedTermVariable", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorUndefinedTermVariable t) (undefinedTermVariableError cx input))),
                      (Core.Name "undefinedTypeVariableInBindingType", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorUndefinedTypeVariableInBindingType t) (undefinedTypeVariableInBindingTypeError cx input))),
                      (Core.Name "undefinedTypeVariableInLambdaDomain", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorUndefinedTypeVariableInLambdaDomain t) (undefinedTypeVariableInLambdaDomainError cx input))),
                      (Core.Name "undefinedTypeVariableInTypeApplication", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorUndefinedTypeVariableInTypeApplication t) (undefinedTypeVariableInTypeApplicationError cx input))),
                      (Core.Name "unknownPrimitiveName", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorUnknownPrimitiveName t) (unknownPrimitiveNameError cx input))),
                      (Core.Name "unnecessaryIdentityApplication", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorUnnecessaryIdentityApplication t) (unnecessaryIdentityApplicationError cx input))),
                      (Core.Name "untypedTermVariable", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorUntypedTermVariable t) (untypedTermVariableError cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

invalidTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.InvalidTypeError
invalidTypeError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "duplicateRecordTypeFieldNames", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorDuplicateRecordTypeFieldNames t) (duplicateRecordTypeFieldNamesError cx input))),
                      (Core.Name "duplicateUnionTypeFieldNames", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorDuplicateUnionTypeFieldNames t) (duplicateUnionTypeFieldNamesError cx input))),
                      (Core.Name "emptyRecordType", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorEmptyRecordType t) (emptyRecordTypeError cx input))),
                      (Core.Name "emptyTypeAnnotation", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorEmptyTypeAnnotation t) (emptyTypeAnnotationError cx input))),
                      (Core.Name "emptyUnionType", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorEmptyUnionType t) (emptyUnionTypeError cx input))),
                      (Core.Name "invalidForallParameterName", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorInvalidForallParameterName t) (invalidForallParameterNameError cx input))),
                      (Core.Name "invalidTypeSchemeVariableName", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorInvalidTypeSchemeVariableName t) (invalidTypeSchemeVariableNameError cx input))),
                      (Core.Name "nestedTypeAnnotation", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorNestedTypeAnnotation t) (nestedTypeAnnotationError cx input))),
                      (Core.Name "nonComparableMapKeyType", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorNonComparableMapKeyType t) (nonComparableMapKeyTypeError cx input))),
                      (Core.Name "nonComparableSetElementType", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorNonComparableSetElementType t) (nonComparableSetElementTypeError cx input))),
                      (Core.Name "singleVariantUnion", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorSingleVariantUnion t) (singleVariantUnionError cx input))),
                      (Core.Name "typeVariableShadowingInForall", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorTypeVariableShadowingInForall t) (typeVariableShadowingInForallError cx input))),
                      (Core.Name "undefinedTypeVariable", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorUndefinedTypeVariable t) (undefinedTypeVariableError cx input))),
                      (Core.Name "voidInNonBottomPosition", (\input -> Eithers.map (\t -> Core__.InvalidTypeErrorVoidInNonBottomPosition t) (voidInNonBottomPositionError cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

invalidTypeLambdaParameterNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.InvalidTypeLambdaParameterNameError {
          Core__.invalidTypeLambdaParameterNameErrorLocation = field_location,
          Core__.invalidTypeLambdaParameterNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

invalidTypeSchemeVariableNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.InvalidTypeSchemeVariableNameError {
          Core__.invalidTypeSchemeVariableNameErrorLocation = field_location,
          Core__.invalidTypeSchemeVariableNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

nestedTermAnnotationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.NestedTermAnnotationError
nestedTermAnnotationError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (Core__.NestedTermAnnotationError {
          Core__.nestedTermAnnotationErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

nestedTypeAnnotationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.NestedTypeAnnotationError
nestedTypeAnnotationError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (Core__.NestedTypeAnnotationError {
          Core__.nestedTypeAnnotationErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

nonComparableMapKeyTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.NonComparableMapKeyTypeError
nonComparableMapKeyTypeError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "keyType" Core_.type_ fieldMap cx) (\field_keyType -> Right (Core__.NonComparableMapKeyTypeError {
          Core__.nonComparableMapKeyTypeErrorLocation = field_location,
          Core__.nonComparableMapKeyTypeErrorKeyType = field_keyType}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

nonComparableSetElementTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.NonComparableSetElementTypeError
nonComparableSetElementTypeError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "elementType" Core_.type_ fieldMap cx) (\field_elementType -> Right (Core__.NonComparableSetElementTypeError {
          Core__.nonComparableSetElementTypeErrorLocation = field_location,
          Core__.nonComparableSetElementTypeErrorElementType = field_elementType}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

redundantWrapUnwrapError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.RedundantWrapUnwrapError
redundantWrapUnwrapError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "typeName" Core_.name fieldMap cx) (\field_typeName -> Right (Core__.RedundantWrapUnwrapError {
          Core__.redundantWrapUnwrapErrorLocation = field_location,
          Core__.redundantWrapUnwrapErrorTypeName = field_typeName}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

selfApplicationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.SelfApplicationError
selfApplicationError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.SelfApplicationError {
          Core__.selfApplicationErrorLocation = field_location,
          Core__.selfApplicationErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

singleVariantUnionError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.SingleVariantUnionError
singleVariantUnionError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "fieldName" Core_.name fieldMap cx) (\field_fieldName -> Right (Core__.SingleVariantUnionError {
          Core__.singleVariantUnionErrorLocation = field_location,
          Core__.singleVariantUnionErrorFieldName = field_fieldName}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

termVariableShadowingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.TermVariableShadowingError
termVariableShadowingError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.TermVariableShadowingError {
          Core__.termVariableShadowingErrorLocation = field_location,
          Core__.termVariableShadowingErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

typeVariableShadowingInForallError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.TypeVariableShadowingInForallError
typeVariableShadowingInForallError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.TypeVariableShadowingInForallError {
          Core__.typeVariableShadowingInForallErrorLocation = field_location,
          Core__.typeVariableShadowingInForallErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

typeVariableShadowingInTypeLambdaError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.TypeVariableShadowingInTypeLambdaError {
          Core__.typeVariableShadowingInTypeLambdaErrorLocation = field_location,
          Core__.typeVariableShadowingInTypeLambdaErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

undefinedFieldError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UndefinedFieldError
undefinedFieldError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "fieldName" Core_.name fieldMap cx) (\field_fieldName -> Eithers.bind (Helpers.requireField "typeName" Core_.name fieldMap cx) (\field_typeName -> Right (Core__.UndefinedFieldError {
          Core__.undefinedFieldErrorFieldName = field_fieldName,
          Core__.undefinedFieldErrorTypeName = field_typeName}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

undefinedTermVariableError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UndefinedTermVariableError
undefinedTermVariableError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.UndefinedTermVariableError {
          Core__.undefinedTermVariableErrorLocation = field_location,
          Core__.undefinedTermVariableErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

undefinedTypeVariableError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UndefinedTypeVariableError
undefinedTypeVariableError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.UndefinedTypeVariableError {
          Core__.undefinedTypeVariableErrorLocation = field_location,
          Core__.undefinedTypeVariableErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

undefinedTypeVariableInBindingTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.UndefinedTypeVariableInBindingTypeError {
          Core__.undefinedTypeVariableInBindingTypeErrorLocation = field_location,
          Core__.undefinedTypeVariableInBindingTypeErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

undefinedTypeVariableInLambdaDomainError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.UndefinedTypeVariableInLambdaDomainError {
          Core__.undefinedTypeVariableInLambdaDomainErrorLocation = field_location,
          Core__.undefinedTypeVariableInLambdaDomainErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

undefinedTypeVariableInTypeApplicationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.UndefinedTypeVariableInTypeApplicationError {
          Core__.undefinedTypeVariableInTypeApplicationErrorLocation = field_location,
          Core__.undefinedTypeVariableInTypeApplicationErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

unexpectedTermVariantError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UnexpectedTermVariantError
unexpectedTermVariantError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "expectedVariant" Variants.termVariant fieldMap cx) (\field_expectedVariant -> Eithers.bind (Helpers.requireField "actualTerm" Core_.term fieldMap cx) (\field_actualTerm -> Right (Core__.UnexpectedTermVariantError {
          Core__.unexpectedTermVariantErrorExpectedVariant = field_expectedVariant,
          Core__.unexpectedTermVariantErrorActualTerm = field_actualTerm}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

unexpectedTypeVariantError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UnexpectedTypeVariantError
unexpectedTypeVariantError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "expectedVariant" Variants.typeVariant fieldMap cx) (\field_expectedVariant -> Eithers.bind (Helpers.requireField "actualType" Core_.type_ fieldMap cx) (\field_actualType -> Right (Core__.UnexpectedTypeVariantError {
          Core__.unexpectedTypeVariantErrorExpectedVariant = field_expectedVariant,
          Core__.unexpectedTypeVariantErrorActualType = field_actualType}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

unknownPrimitiveNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UnknownPrimitiveNameError
unknownPrimitiveNameError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.UnknownPrimitiveNameError {
          Core__.unknownPrimitiveNameErrorLocation = field_location,
          Core__.unknownPrimitiveNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

unnecessaryIdentityApplicationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (Core__.UnnecessaryIdentityApplicationError {
          Core__.unnecessaryIdentityApplicationErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

untypedTermVariableError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UntypedTermVariableError
untypedTermVariableError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.UntypedTermVariableError {
          Core__.untypedTermVariableErrorLocation = field_location,
          Core__.untypedTermVariableErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

voidInNonBottomPositionError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.VoidInNonBottomPositionError
voidInNonBottomPositionError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (Core__.VoidInNonBottomPositionError {
          Core__.voidInNonBottomPositionErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)
