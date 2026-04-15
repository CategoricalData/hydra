-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.json.model

module Hydra.Decode.Json.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

value :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Value
value cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "array", (\input -> Eithers.map (\t -> Model.ValueArray t) (ExtractCore.decodeList value cx input))),
                      (Core.Name "boolean", (\input -> Eithers.map (\t -> Model.ValueBoolean t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralBoolean v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected boolean literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "null", (\input -> Eithers.map (\t -> Model.ValueNull) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "number", (\input -> Eithers.map (\t -> Model.ValueNumber t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralDecimal v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected decimal literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "object", (\input -> Eithers.map (\t -> Model.ValueObject t) (ExtractCore.decodeMap (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralString v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected string literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) value cx input))),
                      (Core.Name "string", (\input -> Eithers.map (\t -> Model.ValueString t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralString v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected string literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input))))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
