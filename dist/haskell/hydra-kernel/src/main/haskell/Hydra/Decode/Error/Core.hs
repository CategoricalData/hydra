-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.error.core

module Hydra.Decode.Error.Core where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.Paths as Paths
import qualified Hydra.Decode.Variants as Variants
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
constantConditionError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.ConstantConditionError
constantConditionError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "value" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_value -> Right (ErrorCore.ConstantConditionError {
          ErrorCore.constantConditionErrorLocation = field_location,
          ErrorCore.constantConditionErrorValue = field_value}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
duplicateBindingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.DuplicateBindingError
duplicateBindingError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.DuplicateBindingError {
          ErrorCore.duplicateBindingErrorLocation = field_location,
          ErrorCore.duplicateBindingErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
duplicateFieldError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.DuplicateFieldError
duplicateFieldError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.DuplicateFieldError {
          ErrorCore.duplicateFieldErrorLocation = field_location,
          ErrorCore.duplicateFieldErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
duplicateRecordTypeFieldNamesError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.DuplicateRecordTypeFieldNamesError {
          ErrorCore.duplicateRecordTypeFieldNamesErrorLocation = field_location,
          ErrorCore.duplicateRecordTypeFieldNamesErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
duplicateUnionTypeFieldNamesError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.DuplicateUnionTypeFieldNamesError {
          ErrorCore.duplicateUnionTypeFieldNamesErrorLocation = field_location,
          ErrorCore.duplicateUnionTypeFieldNamesErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
emptyCaseStatementError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.EmptyCaseStatementError
emptyCaseStatementError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "typeName" DecodeCore.name fieldMap cx) (\field_typeName -> Right (ErrorCore.EmptyCaseStatementError {
          ErrorCore.emptyCaseStatementErrorLocation = field_location,
          ErrorCore.emptyCaseStatementErrorTypeName = field_typeName}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
emptyLetBindingsError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.EmptyLetBindingsError
emptyLetBindingsError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (ErrorCore.EmptyLetBindingsError {
          ErrorCore.emptyLetBindingsErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
emptyRecordTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.EmptyRecordTypeError
emptyRecordTypeError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (ErrorCore.EmptyRecordTypeError {
          ErrorCore.emptyRecordTypeErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
emptyTermAnnotationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.EmptyTermAnnotationError
emptyTermAnnotationError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (ErrorCore.EmptyTermAnnotationError {
          ErrorCore.emptyTermAnnotationErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
emptyTypeAnnotationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.EmptyTypeAnnotationError
emptyTypeAnnotationError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (ErrorCore.EmptyTypeAnnotationError {
          ErrorCore.emptyTypeAnnotationErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
emptyTypeNameInTermError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.EmptyTypeNameInTermError
emptyTypeNameInTermError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (ErrorCore.EmptyTypeNameInTermError {
          ErrorCore.emptyTypeNameInTermErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
emptyUnionTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.EmptyUnionTypeError
emptyUnionTypeError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (ErrorCore.EmptyUnionTypeError {
          ErrorCore.emptyUnionTypeErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
invalidForallParameterNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.InvalidForallParameterNameError
invalidForallParameterNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.InvalidForallParameterNameError {
          ErrorCore.invalidForallParameterNameErrorLocation = field_location,
          ErrorCore.invalidForallParameterNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
invalidLambdaParameterNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.InvalidLambdaParameterNameError
invalidLambdaParameterNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.InvalidLambdaParameterNameError {
          ErrorCore.invalidLambdaParameterNameErrorLocation = field_location,
          ErrorCore.invalidLambdaParameterNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
invalidLetBindingNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.InvalidLetBindingNameError
invalidLetBindingNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.InvalidLetBindingNameError {
          ErrorCore.invalidLetBindingNameErrorLocation = field_location,
          ErrorCore.invalidLetBindingNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
invalidTermError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.InvalidTermError
invalidTermError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "constantCondition", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorConstantCondition t) (constantConditionError cx input))),
                      (Core.Name "duplicateBinding", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorDuplicateBinding t) (duplicateBindingError cx input))),
                      (Core.Name "duplicateField", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorDuplicateField t) (duplicateFieldError cx input))),
                      (Core.Name "emptyCaseStatement", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorEmptyCaseStatement t) (emptyCaseStatementError cx input))),
                      (Core.Name "emptyLetBindings", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorEmptyLetBindings t) (emptyLetBindingsError cx input))),
                      (Core.Name "emptyTermAnnotation", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorEmptyTermAnnotation t) (emptyTermAnnotationError cx input))),
                      (Core.Name "emptyTypeNameInTerm", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorEmptyTypeNameInTerm t) (emptyTypeNameInTermError cx input))),
                      (Core.Name "invalidLambdaParameterName", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorInvalidLambdaParameterName t) (invalidLambdaParameterNameError cx input))),
                      (Core.Name "invalidLetBindingName", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorInvalidLetBindingName t) (invalidLetBindingNameError cx input))),
                      (Core.Name "invalidTypeLambdaParameterName", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorInvalidTypeLambdaParameterName t) (invalidTypeLambdaParameterNameError cx input))),
                      (Core.Name "nestedTermAnnotation", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorNestedTermAnnotation t) (nestedTermAnnotationError cx input))),
                      (Core.Name "redundantWrapUnwrap", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorRedundantWrapUnwrap t) (redundantWrapUnwrapError cx input))),
                      (Core.Name "selfApplication", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorSelfApplication t) (selfApplicationError cx input))),
                      (Core.Name "termVariableShadowing", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorTermVariableShadowing t) (termVariableShadowingError cx input))),
                      (Core.Name "typeVariableShadowingInTypeLambda", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorTypeVariableShadowingInTypeLambda t) (typeVariableShadowingInTypeLambdaError cx input))),
                      (Core.Name "undefinedTermVariable", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorUndefinedTermVariable t) (undefinedTermVariableError cx input))),
                      (Core.Name "undefinedTypeVariableInBindingType", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorUndefinedTypeVariableInBindingType t) (undefinedTypeVariableInBindingTypeError cx input))),
                      (Core.Name "undefinedTypeVariableInLambdaDomain", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorUndefinedTypeVariableInLambdaDomain t) (undefinedTypeVariableInLambdaDomainError cx input))),
                      (Core.Name "undefinedTypeVariableInTypeApplication", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorUndefinedTypeVariableInTypeApplication t) (undefinedTypeVariableInTypeApplicationError cx input))),
                      (Core.Name "unknownPrimitiveName", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorUnknownPrimitiveName t) (unknownPrimitiveNameError cx input))),
                      (Core.Name "unnecessaryIdentityApplication", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorUnnecessaryIdentityApplication t) (unnecessaryIdentityApplicationError cx input))),
                      (Core.Name "untypedTermVariable", (\input -> Eithers.map (\t -> ErrorCore.InvalidTermErrorUntypedTermVariable t) (untypedTermVariableError cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
invalidTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.InvalidTypeError
invalidTypeError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "duplicateRecordTypeFieldNames", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorDuplicateRecordTypeFieldNames t) (duplicateRecordTypeFieldNamesError cx input))),
                      (Core.Name "duplicateUnionTypeFieldNames", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorDuplicateUnionTypeFieldNames t) (duplicateUnionTypeFieldNamesError cx input))),
                      (Core.Name "emptyRecordType", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorEmptyRecordType t) (emptyRecordTypeError cx input))),
                      (Core.Name "emptyTypeAnnotation", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorEmptyTypeAnnotation t) (emptyTypeAnnotationError cx input))),
                      (Core.Name "emptyUnionType", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorEmptyUnionType t) (emptyUnionTypeError cx input))),
                      (Core.Name "invalidForallParameterName", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorInvalidForallParameterName t) (invalidForallParameterNameError cx input))),
                      (Core.Name "invalidTypeSchemeVariableName", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorInvalidTypeSchemeVariableName t) (invalidTypeSchemeVariableNameError cx input))),
                      (Core.Name "nestedTypeAnnotation", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorNestedTypeAnnotation t) (nestedTypeAnnotationError cx input))),
                      (Core.Name "nonComparableMapKeyType", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorNonComparableMapKeyType t) (nonComparableMapKeyTypeError cx input))),
                      (Core.Name "nonComparableSetElementType", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorNonComparableSetElementType t) (nonComparableSetElementTypeError cx input))),
                      (Core.Name "singleVariantUnion", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorSingleVariantUnion t) (singleVariantUnionError cx input))),
                      (Core.Name "typeVariableShadowingInForall", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorTypeVariableShadowingInForall t) (typeVariableShadowingInForallError cx input))),
                      (Core.Name "undefinedTypeVariable", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorUndefinedTypeVariable t) (undefinedTypeVariableError cx input))),
                      (Core.Name "voidInNonBottomPosition", (\input -> Eithers.map (\t -> ErrorCore.InvalidTypeErrorVoidInNonBottomPosition t) (voidInNonBottomPositionError cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
invalidTypeLambdaParameterNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.InvalidTypeLambdaParameterNameError {
          ErrorCore.invalidTypeLambdaParameterNameErrorLocation = field_location,
          ErrorCore.invalidTypeLambdaParameterNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
invalidTypeSchemeVariableNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.InvalidTypeSchemeVariableNameError {
          ErrorCore.invalidTypeSchemeVariableNameErrorLocation = field_location,
          ErrorCore.invalidTypeSchemeVariableNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
nestedTermAnnotationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.NestedTermAnnotationError
nestedTermAnnotationError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (ErrorCore.NestedTermAnnotationError {
          ErrorCore.nestedTermAnnotationErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
nestedTypeAnnotationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.NestedTypeAnnotationError
nestedTypeAnnotationError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (ErrorCore.NestedTypeAnnotationError {
          ErrorCore.nestedTypeAnnotationErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
nonComparableMapKeyTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.NonComparableMapKeyTypeError
nonComparableMapKeyTypeError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "keyType" DecodeCore.type_ fieldMap cx) (\field_keyType -> Right (ErrorCore.NonComparableMapKeyTypeError {
          ErrorCore.nonComparableMapKeyTypeErrorLocation = field_location,
          ErrorCore.nonComparableMapKeyTypeErrorKeyType = field_keyType}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
nonComparableSetElementTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.NonComparableSetElementTypeError
nonComparableSetElementTypeError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "elementType" DecodeCore.type_ fieldMap cx) (\field_elementType -> Right (ErrorCore.NonComparableSetElementTypeError {
          ErrorCore.nonComparableSetElementTypeErrorLocation = field_location,
          ErrorCore.nonComparableSetElementTypeErrorElementType = field_elementType}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
redundantWrapUnwrapError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.RedundantWrapUnwrapError
redundantWrapUnwrapError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "typeName" DecodeCore.name fieldMap cx) (\field_typeName -> Right (ErrorCore.RedundantWrapUnwrapError {
          ErrorCore.redundantWrapUnwrapErrorLocation = field_location,
          ErrorCore.redundantWrapUnwrapErrorTypeName = field_typeName}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
selfApplicationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.SelfApplicationError
selfApplicationError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.SelfApplicationError {
          ErrorCore.selfApplicationErrorLocation = field_location,
          ErrorCore.selfApplicationErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
singleVariantUnionError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.SingleVariantUnionError
singleVariantUnionError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "fieldName" DecodeCore.name fieldMap cx) (\field_fieldName -> Right (ErrorCore.SingleVariantUnionError {
          ErrorCore.singleVariantUnionErrorLocation = field_location,
          ErrorCore.singleVariantUnionErrorFieldName = field_fieldName}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
termVariableShadowingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.TermVariableShadowingError
termVariableShadowingError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.TermVariableShadowingError {
          ErrorCore.termVariableShadowingErrorLocation = field_location,
          ErrorCore.termVariableShadowingErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
typeVariableShadowingInForallError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.TypeVariableShadowingInForallError
typeVariableShadowingInForallError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.TypeVariableShadowingInForallError {
          ErrorCore.typeVariableShadowingInForallErrorLocation = field_location,
          ErrorCore.typeVariableShadowingInForallErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
typeVariableShadowingInTypeLambdaError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.TypeVariableShadowingInTypeLambdaError {
          ErrorCore.typeVariableShadowingInTypeLambdaErrorLocation = field_location,
          ErrorCore.typeVariableShadowingInTypeLambdaErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
undefinedFieldError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.UndefinedFieldError
undefinedFieldError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "fieldName" DecodeCore.name fieldMap cx) (\field_fieldName -> Eithers.bind (ExtractCore.requireField "typeName" DecodeCore.name fieldMap cx) (\field_typeName -> Right (ErrorCore.UndefinedFieldError {
          ErrorCore.undefinedFieldErrorFieldName = field_fieldName,
          ErrorCore.undefinedFieldErrorTypeName = field_typeName}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
undefinedTermVariableError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.UndefinedTermVariableError
undefinedTermVariableError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.UndefinedTermVariableError {
          ErrorCore.undefinedTermVariableErrorLocation = field_location,
          ErrorCore.undefinedTermVariableErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
undefinedTypeVariableError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.UndefinedTypeVariableError
undefinedTypeVariableError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.UndefinedTypeVariableError {
          ErrorCore.undefinedTypeVariableErrorLocation = field_location,
          ErrorCore.undefinedTypeVariableErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
undefinedTypeVariableInBindingTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.UndefinedTypeVariableInBindingTypeError {
          ErrorCore.undefinedTypeVariableInBindingTypeErrorLocation = field_location,
          ErrorCore.undefinedTypeVariableInBindingTypeErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
undefinedTypeVariableInLambdaDomainError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.UndefinedTypeVariableInLambdaDomainError {
          ErrorCore.undefinedTypeVariableInLambdaDomainErrorLocation = field_location,
          ErrorCore.undefinedTypeVariableInLambdaDomainErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
undefinedTypeVariableInTypeApplicationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.UndefinedTypeVariableInTypeApplicationError {
          ErrorCore.undefinedTypeVariableInTypeApplicationErrorLocation = field_location,
          ErrorCore.undefinedTypeVariableInTypeApplicationErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
unexpectedTermVariantError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.UnexpectedTermVariantError
unexpectedTermVariantError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "expectedVariant" Variants.termVariant fieldMap cx) (\field_expectedVariant -> Eithers.bind (ExtractCore.requireField "actualTerm" DecodeCore.term fieldMap cx) (\field_actualTerm -> Right (ErrorCore.UnexpectedTermVariantError {
          ErrorCore.unexpectedTermVariantErrorExpectedVariant = field_expectedVariant,
          ErrorCore.unexpectedTermVariantErrorActualTerm = field_actualTerm}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
unexpectedTypeVariantError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.UnexpectedTypeVariantError
unexpectedTypeVariantError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "expectedVariant" Variants.typeVariant fieldMap cx) (\field_expectedVariant -> Eithers.bind (ExtractCore.requireField "actualType" DecodeCore.type_ fieldMap cx) (\field_actualType -> Right (ErrorCore.UnexpectedTypeVariantError {
          ErrorCore.unexpectedTypeVariantErrorExpectedVariant = field_expectedVariant,
          ErrorCore.unexpectedTypeVariantErrorActualType = field_actualType}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
unknownPrimitiveNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.UnknownPrimitiveNameError
unknownPrimitiveNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.UnknownPrimitiveNameError {
          ErrorCore.unknownPrimitiveNameErrorLocation = field_location,
          ErrorCore.unknownPrimitiveNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
unnecessaryIdentityApplicationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (ErrorCore.UnnecessaryIdentityApplicationError {
          ErrorCore.unnecessaryIdentityApplicationErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
untypedTermVariableError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.UntypedTermVariableError
untypedTermVariableError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorCore.UntypedTermVariableError {
          ErrorCore.untypedTermVariableErrorLocation = field_location,
          ErrorCore.untypedTermVariableErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
voidInNonBottomPositionError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorCore.VoidInNonBottomPositionError
voidInNonBottomPositionError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "location" Paths.subtermPath fieldMap cx) (\field_location -> Right (ErrorCore.VoidInNonBottomPositionError {
          ErrorCore.voidInNonBottomPositionErrorLocation = field_location})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
