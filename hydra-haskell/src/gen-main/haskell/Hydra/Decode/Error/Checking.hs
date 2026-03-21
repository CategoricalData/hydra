-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.error.checking

module Hydra.Decode.Error.Checking where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Decode.Typing as Typing
import qualified Hydra.Decode.Variants as Variants
import qualified Hydra.Error.Checking as Checking
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

checkingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.CheckingError
checkingError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "incorrectUnification", (\input -> Eithers.map (\t -> Checking.CheckingErrorIncorrectUnification t) (incorrectUnificationError cx input))),
                      (Core.Name "notAForallType", (\input -> Eithers.map (\t -> Checking.CheckingErrorNotAForallType t) (notAForallTypeError cx input))),
                      (Core.Name "notAFunctionType", (\input -> Eithers.map (\t -> Checking.CheckingErrorNotAFunctionType t) (notAFunctionTypeError cx input))),
                      (Core.Name "typeArityMismatch", (\input -> Eithers.map (\t -> Checking.CheckingErrorTypeArityMismatch t) (typeArityMismatchError cx input))),
                      (Core.Name "typeMismatch", (\input -> Eithers.map (\t -> Checking.CheckingErrorTypeMismatch t) (typeMismatchError cx input))),
                      (Core.Name "unboundTypeVariables", (\input -> Eithers.map (\t -> Checking.CheckingErrorUnboundTypeVariables t) (unboundTypeVariablesError cx input))),
                      (Core.Name "unequalTypes", (\input -> Eithers.map (\t -> Checking.CheckingErrorUnequalTypes t) (unequalTypesError cx input))),
                      (Core.Name "unsupportedTermVariant", (\input -> Eithers.map (\t -> Checking.CheckingErrorUnsupportedTermVariant t) (unsupportedTermVariantError cx input))),
                      (Core.Name "untypedLambda", (\input -> Eithers.map (\t -> Checking.CheckingErrorUntypedLambda t) (untypedLambdaError cx input))),
                      (Core.Name "untypedLetBinding", (\input -> Eithers.map (\t -> Checking.CheckingErrorUntypedLetBinding t) (untypedLetBindingError cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

incorrectUnificationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.IncorrectUnificationError
incorrectUnificationError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "substitution" Typing.typeSubst fieldMap cx) (\field_substitution -> Right (Checking.IncorrectUnificationError {
          Checking.incorrectUnificationErrorSubstitution = field_substitution})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

notAForallTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.NotAForallTypeError
notAForallTypeError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Eithers.bind (Helpers.requireField "typeArguments" (Helpers.decodeList Core_.type_) fieldMap cx) (\field_typeArguments -> Right (Checking.NotAForallTypeError {
          Checking.notAForallTypeErrorType = field_type,
          Checking.notAForallTypeErrorTypeArguments = field_typeArguments}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

notAFunctionTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.NotAFunctionTypeError
notAFunctionTypeError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Right (Checking.NotAFunctionTypeError {
          Checking.notAFunctionTypeErrorType = field_type})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

typeArityMismatchError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.TypeArityMismatchError
typeArityMismatchError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Eithers.bind (Helpers.requireField "expectedArity" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_expectedArity -> Eithers.bind (Helpers.requireField "actualArity" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_actualArity -> Eithers.bind (Helpers.requireField "typeArguments" (Helpers.decodeList Core_.type_) fieldMap cx) (\field_typeArguments -> Right (Checking.TypeArityMismatchError {
          Checking.typeArityMismatchErrorType = field_type,
          Checking.typeArityMismatchErrorExpectedArity = field_expectedArity,
          Checking.typeArityMismatchErrorActualArity = field_actualArity,
          Checking.typeArityMismatchErrorTypeArguments = field_typeArguments}))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

typeMismatchError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.TypeMismatchError
typeMismatchError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "expectedType" Core_.type_ fieldMap cx) (\field_expectedType -> Eithers.bind (Helpers.requireField "actualType" Core_.type_ fieldMap cx) (\field_actualType -> Right (Checking.TypeMismatchError {
          Checking.typeMismatchErrorExpectedType = field_expectedType,
          Checking.typeMismatchErrorActualType = field_actualType}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

unboundTypeVariablesError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UnboundTypeVariablesError
unboundTypeVariablesError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "variables" (Helpers.decodeSet Core_.name) fieldMap cx) (\field_variables -> Eithers.bind (Helpers.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Right (Checking.UnboundTypeVariablesError {
          Checking.unboundTypeVariablesErrorVariables = field_variables,
          Checking.unboundTypeVariablesErrorType = field_type}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

unequalTypesError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UnequalTypesError
unequalTypesError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "types" (Helpers.decodeList Core_.type_) fieldMap cx) (\field_types -> Eithers.bind (Helpers.requireField "description" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_description -> Right (Checking.UnequalTypesError {
          Checking.unequalTypesErrorTypes = field_types,
          Checking.unequalTypesErrorDescription = field_description}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

unsupportedTermVariantError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UnsupportedTermVariantError
unsupportedTermVariantError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "termVariant" Variants.termVariant fieldMap cx) (\field_termVariant -> Right (Checking.UnsupportedTermVariantError {
          Checking.unsupportedTermVariantErrorTermVariant = field_termVariant})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

untypedLambdaError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UntypedLambdaError
untypedLambdaError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Right (Checking.UntypedLambdaError {
        }))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

untypedLetBindingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Checking.UntypedLetBindingError
untypedLetBindingError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "binding" Core_.binding fieldMap cx) (\field_binding -> Right (Checking.UntypedLetBindingError {
          Checking.untypedLetBindingErrorBinding = field_binding})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)
