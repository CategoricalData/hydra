-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.validation

module Hydra.Decode.Validation where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.Error.Core as ErrorCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.validation.ValidationProfile
validationProfile :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Validation.ValidationProfile
validationProfile cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "errorRules" (ExtractCore.decodeSet DecodeCore.name) fieldMap cx) (\field_errorRules -> Eithers.bind (ExtractCore.requireField "warningRules" (ExtractCore.decodeSet DecodeCore.name) fieldMap cx) (\field_warningRules -> Eithers.bind (ExtractCore.requireField "maxErrors" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_maxErrors -> Eithers.bind (ExtractCore.requireField "maxWarnings" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_maxWarnings -> Right (Validation.ValidationProfile {
          Validation.validationProfileErrorRules = field_errorRules,
          Validation.validationProfileWarningRules = field_warningRules,
          Validation.validationProfileMaxErrors = field_maxErrors,
          Validation.validationProfileMaxWarnings = field_maxWarnings}))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.validation.ValidationProfile")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.validation.ValidationResult
validationResult :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Validation.ValidationResult t0)
validationResult e cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "errors" (ExtractCore.decodeList e) fieldMap cx) (\field_errors -> Eithers.bind (ExtractCore.requireField "warnings" (ExtractCore.decodeList e) fieldMap cx) (\field_warnings -> Right (Validation.ValidationResult {
          Validation.validationResultErrors = field_errors,
          Validation.validationResultWarnings = field_warnings}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.validation.ValidationResult")) (ExtractCore.stripWithDecodingError cx raw)
