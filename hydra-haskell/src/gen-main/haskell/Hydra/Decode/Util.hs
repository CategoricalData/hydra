-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.util

module Hydra.Decode.Util where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

caseConvention :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Util.CaseConvention
caseConvention cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "camel", (\input -> Eithers.map (\t -> Util.CaseConventionCamel) (Helpers.decodeUnit cx input))),
                      (Core.Name "pascal", (\input -> Eithers.map (\t -> Util.CaseConventionPascal) (Helpers.decodeUnit cx input))),
                      (Core.Name "lowerSnake", (\input -> Eithers.map (\t -> Util.CaseConventionLowerSnake) (Helpers.decodeUnit cx input))),
                      (Core.Name "upperSnake", (\input -> Eithers.map (\t -> Util.CaseConventionUpperSnake) (Helpers.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

comparison :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Util.Comparison
comparison cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "lessThan", (\input -> Eithers.map (\t -> Util.ComparisonLessThan) (Helpers.decodeUnit cx input))),
                      (Core.Name "equalTo", (\input -> Eithers.map (\t -> Util.ComparisonEqualTo) (Helpers.decodeUnit cx input))),
                      (Core.Name "greaterThan", (\input -> Eithers.map (\t -> Util.ComparisonGreaterThan) (Helpers.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

precision :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Util.Precision
precision cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "arbitrary", (\input -> Eithers.map (\t -> Util.PrecisionArbitrary) (Helpers.decodeUnit cx input))),
                      (Core.Name "bits", (\input -> Eithers.map (\t -> Util.PrecisionBits t) (Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralInteger v2 -> case v2 of
                            Core.IntegerValueInt32 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected int32 value")
                          _ -> Left (Errors.DecodingError "expected int32 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx input))))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)
