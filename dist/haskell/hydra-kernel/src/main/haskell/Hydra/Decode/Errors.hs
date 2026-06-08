-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.errors

module Hydra.Decode.Errors where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.Error.Checking as Checking
import qualified Hydra.Decode.Error.Core as ErrorCore
import qualified Hydra.Decode.Paths as Paths
import qualified Hydra.Decode.Typing as Typing
import qualified Hydra.Decode.Variants as Variants
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.errors.DecodingError
decodingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.DecodingError
decodingError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Errors.DecodingError b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.EmptyListError
emptyListError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ()
emptyListError cx t = ExtractCore.decodeUnit cx t
-- | Decoder for hydra.errors.Error
error :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.Error
error cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "checking", (\input -> Eithers.map (\t -> Errors.ErrorChecking t) (Checking.checkingError cx input))),
                      (Core.Name "decoding", (\input -> Eithers.map (\t -> Errors.ErrorDecoding t) (decodingError cx input))),
                      (
                        Core.Name "duplicateBinding",
                        (\input -> Eithers.map (\t -> Errors.ErrorDuplicateBinding t) (ErrorCore.duplicateBindingError cx input))),
                      (
                        Core.Name "duplicateField",
                        (\input -> Eithers.map (\t -> Errors.ErrorDuplicateField t) (ErrorCore.duplicateFieldError cx input))),
                      (Core.Name "extraction", (\input -> Eithers.map (\t -> Errors.ErrorExtraction t) (extractionError cx input))),
                      (Core.Name "inference", (\input -> Eithers.map (\t -> Errors.ErrorInference t) (inferenceError cx input))),
                      (
                        Core.Name "invalidLiteral",
                        (\input -> Eithers.map (\t -> Errors.ErrorInvalidLiteral t) (ErrorCore.invalidLiteralError cx input))),
                      (Core.Name "other", (\input -> Eithers.map (\t -> Errors.ErrorOther t) (otherError cx input))),
                      (Core.Name "resolution", (\input -> Eithers.map (\t -> Errors.ErrorResolution t) (resolutionError cx input))),
                      (
                        Core.Name "undefinedField",
                        (\input -> Eithers.map (\t -> Errors.ErrorUndefinedField t) (ErrorCore.undefinedFieldError cx input))),
                      (
                        Core.Name "undefinedTermVariable",
                        (\input -> Eithers.map (\t -> Errors.ErrorUndefinedTermVariable t) (ErrorCore.undefinedTermVariableError cx input))),
                      (
                        Core.Name "untypedTermVariable",
                        (\input -> Eithers.map (\t -> Errors.ErrorUntypedTermVariable t) (ErrorCore.untypedTermVariableError cx input))),
                      (
                        Core.Name "unexpectedTermVariant",
                        (\input -> Eithers.map (\t -> Errors.ErrorUnexpectedTermVariant t) (ErrorCore.unexpectedTermVariantError cx input))),
                      (
                        Core.Name "unexpectedTypeVariant",
                        (\input -> Eithers.map (\t -> Errors.ErrorUnexpectedTypeVariant t) (ErrorCore.unexpectedTypeVariantError cx input))),
                      (Core.Name "unification", (\input -> Eithers.map (\t -> Errors.ErrorUnification t) (unificationError cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.ExtractionError
extractionError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.ExtractionError
extractionError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "emptyList", (\input -> Eithers.map (\t -> Errors.ExtractionErrorEmptyList t) (emptyListError cx input))),
                      (
                        Core.Name "multipleBindings",
                        (\input -> Eithers.map (\t -> Errors.ExtractionErrorMultipleBindings t) (multipleBindingsError cx input))),
                      (
                        Core.Name "multipleFields",
                        (\input -> Eithers.map (\t -> Errors.ExtractionErrorMultipleFields t) (multipleFieldsError cx input))),
                      (
                        Core.Name "noMatchingField",
                        (\input -> Eithers.map (\t -> Errors.ExtractionErrorNoMatchingField t) (noMatchingFieldError cx input))),
                      (
                        Core.Name "noSuchBinding",
                        (\input -> Eithers.map (\t -> Errors.ExtractionErrorNoSuchBinding t) (noSuchBindingError cx input))),
                      (
                        Core.Name "notEnoughCases",
                        (\input -> Eithers.map (\t -> Errors.ExtractionErrorNotEnoughCases t) (notEnoughCasesError cx input))),
                      (
                        Core.Name "unexpectedShape",
                        (\input -> Eithers.map (\t -> Errors.ExtractionErrorUnexpectedShape t) (unexpectedShapeError cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.InferenceError
inferenceError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.InferenceError
inferenceError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "checking", (\input -> Eithers.map (\t -> Errors.InferenceErrorChecking t) (Checking.checkingError cx input))),
                      (Core.Name "other", (\input -> Eithers.map (\t -> Errors.InferenceErrorOther t) (otherInferenceError cx input))),
                      (
                        Core.Name "unification",
                        (\input -> Eithers.map (\t -> Errors.InferenceErrorUnification t) (unificationInferenceError cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.MultipleBindingsError
multipleBindingsError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.MultipleBindingsError
multipleBindingsError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (Errors.MultipleBindingsError {
          Errors.multipleBindingsErrorName = field_name})))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.errors.MultipleBindingsError")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.MultipleFieldsError
multipleFieldsError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.MultipleFieldsError
multipleFieldsError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "fieldName" DecodeCore.name fieldMap cx) (\field_fieldName -> Right (Errors.MultipleFieldsError {
          Errors.multipleFieldsErrorFieldName = field_fieldName})))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.errors.MultipleFieldsError")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.NoMatchingFieldError
noMatchingFieldError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.NoMatchingFieldError
noMatchingFieldError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "fieldName" DecodeCore.name fieldMap cx) (\field_fieldName -> Right (Errors.NoMatchingFieldError {
          Errors.noMatchingFieldErrorFieldName = field_fieldName})))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.errors.NoMatchingFieldError")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.NoSuchBindingError
noSuchBindingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.NoSuchBindingError
noSuchBindingError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (Errors.NoSuchBindingError {
          Errors.noSuchBindingErrorName = field_name})))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.errors.NoSuchBindingError")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.NoSuchPrimitiveError
noSuchPrimitiveError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.NoSuchPrimitiveError
noSuchPrimitiveError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (Errors.NoSuchPrimitiveError {
          Errors.noSuchPrimitiveErrorName = field_name})))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.errors.NoSuchPrimitiveError")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.NotEnoughCasesError
notEnoughCasesError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ()
notEnoughCasesError cx t = ExtractCore.decodeUnit cx t
-- | Decoder for hydra.errors.OtherError
otherError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.OtherError
otherError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Errors.OtherError b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.OtherInferenceError
otherInferenceError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.OtherInferenceError
otherInferenceError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "path" Paths.subtermPath fieldMap cx) (\field_path -> Eithers.bind (ExtractCore.requireField "message" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_message -> Right (Errors.OtherInferenceError {
          Errors.otherInferenceErrorPath = field_path,
          Errors.otherInferenceErrorMessage = field_message}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.errors.OtherInferenceError")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.OtherResolutionError
otherResolutionError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.OtherResolutionError
otherResolutionError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Errors.OtherResolutionError b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.ResolutionError
resolutionError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.ResolutionError
resolutionError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (
                        Core.Name "noSuchBinding",
                        (\input -> Eithers.map (\t -> Errors.ResolutionErrorNoSuchBinding t) (noSuchBindingError cx input))),
                      (
                        Core.Name "noSuchPrimitive",
                        (\input -> Eithers.map (\t -> Errors.ResolutionErrorNoSuchPrimitive t) (noSuchPrimitiveError cx input))),
                      (
                        Core.Name "noMatchingField",
                        (\input -> Eithers.map (\t -> Errors.ResolutionErrorNoMatchingField t) (noMatchingFieldError cx input))),
                      (Core.Name "other", (\input -> Eithers.map (\t -> Errors.ResolutionErrorOther t) (otherResolutionError cx input))),
                      (
                        Core.Name "unexpectedShape",
                        (\input -> Eithers.map (\t -> Errors.ResolutionErrorUnexpectedShape t) (unexpectedShapeError cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.UnexpectedShapeError
unexpectedShapeError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.UnexpectedShapeError
unexpectedShapeError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "expected" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_expected -> Eithers.bind (ExtractCore.requireField "actual" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_actual -> Right (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = field_expected,
          Errors.unexpectedShapeErrorActual = field_actual}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.errors.UnexpectedShapeError")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.UnificationError
unificationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.UnificationError
unificationError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "leftType" DecodeCore.type_ fieldMap cx) (\field_leftType -> Eithers.bind (ExtractCore.requireField "rightType" DecodeCore.type_ fieldMap cx) (\field_rightType -> Eithers.bind (ExtractCore.requireField "message" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_message -> Right (Errors.UnificationError {
          Errors.unificationErrorLeftType = field_leftType,
          Errors.unificationErrorRightType = field_rightType,
          Errors.unificationErrorMessage = field_message})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.errors.UnificationError")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.errors.UnificationInferenceError
unificationInferenceError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Errors.UnificationInferenceError
unificationInferenceError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "path" Paths.subtermPath fieldMap cx) (\field_path -> Eithers.bind (ExtractCore.requireField "cause" unificationError fieldMap cx) (\field_cause -> Right (Errors.UnificationInferenceError {
          Errors.unificationInferenceErrorPath = field_path,
          Errors.unificationInferenceErrorCause = field_cause}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.errors.UnificationInferenceError")) (ExtractCore.stripWithDecodingError cx raw)
