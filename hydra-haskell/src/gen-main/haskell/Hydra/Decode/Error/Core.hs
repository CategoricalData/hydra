-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.error.core

module Hydra.Decode.Error.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Accessors as Accessors
import qualified Hydra.Decode.Core as Core_
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

duplicateBindingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.DuplicateBindingError
duplicateBindingError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Accessors.accessorPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.DuplicateBindingError {
          Core__.duplicateBindingErrorLocation = field_location,
          Core__.duplicateBindingErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

duplicateFieldError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.DuplicateFieldError
duplicateFieldError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "location" Accessors.accessorPath fieldMap cx) (\field_location -> Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.DuplicateFieldError {
          Core__.duplicateFieldErrorLocation = field_location,
          Core__.duplicateFieldErrorName = field_name}))))
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
                      (Core.Name "duplicateBinding", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorDuplicateBinding t) (duplicateBindingError cx input))),
                      (Core.Name "duplicateField", (\input -> Eithers.map (\t -> Core__.InvalidTermErrorDuplicateField t) (duplicateFieldError cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

undefinedFieldError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UndefinedFieldError
undefinedFieldError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "fieldName" Core_.name fieldMap cx) (\field_fieldName -> Eithers.bind (Helpers.requireField "typeName" Core_.name fieldMap cx) (\field_typeName -> Right (Core__.UndefinedFieldError {
          Core__.undefinedFieldErrorFieldName = field_fieldName,
          Core__.undefinedFieldErrorTypeName = field_typeName}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

undefinedTermError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UndefinedTermError
undefinedTermError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.UndefinedTermError {
          Core__.undefinedTermErrorName = field_name})))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

undefinedTypeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core__.UndefinedTypeError
undefinedTypeError cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Right (Core__.UndefinedTypeError {
          Core__.undefinedTypeErrorName = field_name})))
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
