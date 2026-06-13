-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.validation

module Hydra.Encode.Validation where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Error.Core as ErrorCore
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Validation as Validation
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.validation.ValidationProfile
validationProfile :: Validation.ValidationProfile -> Core.Term
validationProfile x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationProfile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errorRules"),
          Core.fieldTerm = ((\s -> Core.TermSet (Sets.map EncodeCore.name s)) (Validation.validationProfileErrorRules x))},
        Core.Field {
          Core.fieldName = (Core.Name "warningRules"),
          Core.fieldTerm = ((\s -> Core.TermSet (Sets.map EncodeCore.name s)) (Validation.validationProfileWarningRules x))},
        Core.Field {
          Core.fieldName = (Core.Name "maxErrors"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Validation.validationProfileMaxErrors x))},
        Core.Field {
          Core.fieldName = (Core.Name "maxWarnings"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Validation.validationProfileMaxWarnings x))}]})
-- | Encoder for hydra.validation.ValidationResult
validationResult :: (t0 -> Core.Term) -> Validation.ValidationResult t0 -> Core.Term
validationResult e x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errors"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map e xs)) (Validation.validationResultErrors x))},
        Core.Field {
          Core.fieldName = (Core.Name "warnings"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map e xs)) (Validation.validationResultWarnings x))}]})
