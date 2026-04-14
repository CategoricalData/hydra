-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.util

module Hydra.Decode.Util where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

caseConvention :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Util.CaseConvention
caseConvention cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "camel", (\input -> Eithers.map (\t -> Util.CaseConventionCamel) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "pascal", (\input -> Eithers.map (\t -> Util.CaseConventionPascal) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "lowerSnake", (\input -> Eithers.map (\t -> Util.CaseConventionLowerSnake) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "upperSnake", (\input -> Eithers.map (\t -> Util.CaseConventionUpperSnake) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

comparison :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Util.Comparison
comparison cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "lessThan", (\input -> Eithers.map (\t -> Util.ComparisonLessThan) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "equalTo", (\input -> Eithers.map (\t -> Util.ComparisonEqualTo) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "greaterThan", (\input -> Eithers.map (\t -> Util.ComparisonGreaterThan) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

precision :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Util.Precision
precision cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "arbitrary", (\input -> Eithers.map (\t -> Util.PrecisionArbitrary) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "bits", (\input -> Eithers.map (\t -> Util.PrecisionBits t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralInteger v2 -> case v2 of
                            Core.IntegerValueInt32 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected int32 value")
                          _ -> Left (Errors.DecodingError "expected int32 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input))))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
