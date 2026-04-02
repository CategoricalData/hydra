-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.errors

module Hydra.Decode.Errors where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Decode.Error.Checking as Checking
import qualified Hydra.Decode.Error.Core as Core__
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core___
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

decodingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.DecodingError
decodingError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Errors.DecodingError b) ((\raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

error :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.Error
error cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "checking", (\input -> Eithers.map (\t -> Errors.ErrorChecking t) (Checking.checkingError cx input))),
                      (Core.Name "decoding", (\input -> Eithers.map (\t -> Errors.ErrorDecoding t) (decodingError cx input))),
                      (Core.Name "duplicateBinding", (\input -> Eithers.map (\t -> Errors.ErrorDuplicateBinding t) (Core__.duplicateBindingError cx input))),
                      (Core.Name "duplicateField", (\input -> Eithers.map (\t -> Errors.ErrorDuplicateField t) (Core__.duplicateFieldError cx input))),
                      (Core.Name "other", (\input -> Eithers.map (\t -> Errors.ErrorOther t) (otherError cx input))),
                      (Core.Name "undefinedField", (\input -> Eithers.map (\t -> Errors.ErrorUndefinedField t) (Core__.undefinedFieldError cx input))),
                      (Core.Name "undefinedTermVariable", (\input -> Eithers.map (\t -> Errors.ErrorUndefinedTermVariable t) (Core__.undefinedTermVariableError cx input))),
                      (Core.Name "untypedTermVariable", (\input -> Eithers.map (\t -> Errors.ErrorUntypedTermVariable t) (Core__.untypedTermVariableError cx input))),
                      (Core.Name "unexpectedTermVariant", (\input -> Eithers.map (\t -> Errors.ErrorUnexpectedTermVariant t) (Core__.unexpectedTermVariantError cx input))),
                      (Core.Name "unexpectedTypeVariant", (\input -> Eithers.map (\t -> Errors.ErrorUnexpectedTypeVariant t) (Core__.unexpectedTypeVariantError cx input))),
                      (Core.Name "unification", (\input -> Eithers.map (\t -> Errors.ErrorUnification t) (unificationError cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

otherError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.OtherError
otherError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Errors.OtherError b) ((\raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

unificationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.UnificationError
unificationError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core___.toFieldMap v0
        in (Eithers.bind (Core___.requireField "leftType" Core_.type_ fieldMap cx) (\field_leftType -> Eithers.bind (Core___.requireField "rightType" Core_.type_ fieldMap cx) (\field_rightType -> Eithers.bind (Core___.requireField "message" (\cx2 -> \raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx2 raw2)) fieldMap cx) (\field_message -> Right (Errors.UnificationError {
          Errors.unificationErrorLeftType = field_leftType,
          Errors.unificationErrorRightType = field_rightType,
          Errors.unificationErrorMessage = field_message})))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)
