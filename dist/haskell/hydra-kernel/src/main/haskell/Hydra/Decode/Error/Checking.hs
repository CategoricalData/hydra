-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.error.checking

module Hydra.Decode.Error.Checking where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.Paths as Paths
import qualified Hydra.Decode.Typing as Typing
import qualified Hydra.Decode.Variants as Variants
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

checkingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.CheckingError
checkingError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "incorrectUnification", (\input -> Eithers.map (\t -> Checking.CheckingErrorIncorrectUnification t) (incorrectUnificationError cx input))),
                      (Core.Name "notAForallType", (\input -> Eithers.map (\t -> Checking.CheckingErrorNotAForallType t) (notAForallTypeError cx input))),
                      (Core.Name "notAFunctionType", (\input -> Eithers.map (\t -> Checking.CheckingErrorNotAFunctionType t) (notAFunctionTypeError cx input))),
                      (Core.Name "other", (\input -> Eithers.map (\t -> Checking.CheckingErrorOther t) (otherCheckingError cx input))),
                      (Core.Name "typeArityMismatch", (\input -> Eithers.map (\t -> Checking.CheckingErrorTypeArityMismatch t) (typeArityMismatchError cx input))),
                      (Core.Name "typeMismatch", (\input -> Eithers.map (\t -> Checking.CheckingErrorTypeMismatch t) (typeMismatchError cx input))),
                      (Core.Name "unboundTypeVariables", (\input -> Eithers.map (\t -> Checking.CheckingErrorUnboundTypeVariables t) (unboundTypeVariablesError cx input))),
                      (Core.Name "undefinedTermVariable", (\input -> Eithers.map (\t -> Checking.CheckingErrorUndefinedTermVariable t) (undefinedTermVariableCheckingError cx input))),
                      (Core.Name "unequalTypes", (\input -> Eithers.map (\t -> Checking.CheckingErrorUnequalTypes t) (unequalTypesError cx input))),
                      (Core.Name "unsupportedTermVariant", (\input -> Eithers.map (\t -> Checking.CheckingErrorUnsupportedTermVariant t) (unsupportedTermVariantError cx input))),
                      (Core.Name "untypedLambda", (\input -> Eithers.map (\t -> Checking.CheckingErrorUntypedLambda t) (untypedLambdaError cx input))),
                      (Core.Name "untypedLetBinding", (\input -> Eithers.map (\t -> Checking.CheckingErrorUntypedLetBinding t) (untypedLetBindingError cx input))),
                      (Core.Name "untypedTermVariable", (\input -> Eithers.map (\t -> Checking.CheckingErrorUntypedTermVariable t) (untypedTermVariableCheckingError cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

incorrectUnificationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.IncorrectUnificationError
incorrectUnificationError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "substitution" Typing.typeSubst fieldMap cx) (\field_substitution -> Right (Checking.IncorrectUnificationError {
          Checking.incorrectUnificationErrorSubstitution = field_substitution})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

notAForallTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.NotAForallTypeError
notAForallTypeError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "type" DecodeCore.type_ fieldMap cx) (\field_type -> Eithers.bind (ExtractCore.requireField "typeArguments" (ExtractCore.decodeList DecodeCore.type_) fieldMap cx) (\field_typeArguments -> Right (Checking.NotAForallTypeError {
          Checking.notAForallTypeErrorType = field_type,
          Checking.notAForallTypeErrorTypeArguments = field_typeArguments}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

notAFunctionTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.NotAFunctionTypeError
notAFunctionTypeError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "type" DecodeCore.type_ fieldMap cx) (\field_type -> Right (Checking.NotAFunctionTypeError {
          Checking.notAFunctionTypeErrorType = field_type})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

otherCheckingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.OtherCheckingError
otherCheckingError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "path" Paths.subtermPath fieldMap cx) (\field_path -> Eithers.bind (ExtractCore.requireField "message" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_message -> Right (Checking.OtherCheckingError {
          Checking.otherCheckingErrorPath = field_path,
          Checking.otherCheckingErrorMessage = field_message}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

typeArityMismatchError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.TypeArityMismatchError
typeArityMismatchError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "type" DecodeCore.type_ fieldMap cx) (\field_type -> Eithers.bind (ExtractCore.requireField "expectedArity" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_expectedArity -> Eithers.bind (ExtractCore.requireField "actualArity" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_actualArity -> Eithers.bind (ExtractCore.requireField "typeArguments" (ExtractCore.decodeList DecodeCore.type_) fieldMap cx) (\field_typeArguments -> Right (Checking.TypeArityMismatchError {
          Checking.typeArityMismatchErrorType = field_type,
          Checking.typeArityMismatchErrorExpectedArity = field_expectedArity,
          Checking.typeArityMismatchErrorActualArity = field_actualArity,
          Checking.typeArityMismatchErrorTypeArguments = field_typeArguments}))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

typeMismatchError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.TypeMismatchError
typeMismatchError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "expectedType" DecodeCore.type_ fieldMap cx) (\field_expectedType -> Eithers.bind (ExtractCore.requireField "actualType" DecodeCore.type_ fieldMap cx) (\field_actualType -> Right (Checking.TypeMismatchError {
          Checking.typeMismatchErrorExpectedType = field_expectedType,
          Checking.typeMismatchErrorActualType = field_actualType}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

unboundTypeVariablesError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UnboundTypeVariablesError
unboundTypeVariablesError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "variables" (ExtractCore.decodeSet DecodeCore.name) fieldMap cx) (\field_variables -> Eithers.bind (ExtractCore.requireField "type" DecodeCore.type_ fieldMap cx) (\field_type -> Right (Checking.UnboundTypeVariablesError {
          Checking.unboundTypeVariablesErrorVariables = field_variables,
          Checking.unboundTypeVariablesErrorType = field_type}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

undefinedTermVariableCheckingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "path" Paths.subtermPath fieldMap cx) (\field_path -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (Checking.UndefinedTermVariableCheckingError {
          Checking.undefinedTermVariableCheckingErrorPath = field_path,
          Checking.undefinedTermVariableCheckingErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

unequalTypesError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UnequalTypesError
unequalTypesError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "types" (ExtractCore.decodeList DecodeCore.type_) fieldMap cx) (\field_types -> Eithers.bind (ExtractCore.requireField "description" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_description -> Right (Checking.UnequalTypesError {
          Checking.unequalTypesErrorTypes = field_types,
          Checking.unequalTypesErrorDescription = field_description}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

unsupportedTermVariantError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UnsupportedTermVariantError
unsupportedTermVariantError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "termVariant" Variants.termVariant fieldMap cx) (\field_termVariant -> Right (Checking.UnsupportedTermVariantError {
          Checking.unsupportedTermVariantErrorTermVariant = field_termVariant})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

untypedLambdaError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UntypedLambdaError
untypedLambdaError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Right (Checking.UntypedLambdaError {
        }))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

untypedLetBindingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UntypedLetBindingError
untypedLetBindingError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "binding" DecodeCore.binding fieldMap cx) (\field_binding -> Right (Checking.UntypedLetBindingError {
          Checking.untypedLetBindingErrorBinding = field_binding})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

untypedTermVariableCheckingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "path" Paths.subtermPath fieldMap cx) (\field_path -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (Checking.UntypedTermVariableCheckingError {
          Checking.untypedTermVariableCheckingErrorPath = field_path,
          Checking.untypedTermVariableCheckingErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
