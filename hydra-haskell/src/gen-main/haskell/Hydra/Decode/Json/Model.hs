-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.json.model

module Hydra.Decode.Json.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

value :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Value
value cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "array", (\input -> Eithers.map (\t -> Model.ValueArray t) (Core_.decodeList value cx input))),
                      (Core.Name "boolean", (\input -> Eithers.map (\t -> Model.ValueBoolean t) (Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralBoolean v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected boolean literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx input)))),
                      (Core.Name "null", (\input -> Eithers.map (\t -> Model.ValueNull) (Core_.decodeUnit cx input))),
                      (Core.Name "number", (\input -> Eithers.map (\t -> Model.ValueNumber t) (Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralFloat v2 -> case v2 of
                            Core.FloatValueBigfloat v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected bigfloat value")
                          _ -> Left (Errors.DecodingError "expected bigfloat literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx input)))),
                      (Core.Name "object", (\input -> Eithers.map (\t -> Model.ValueObject t) (Core_.decodeMap (\cx2 -> \raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralString v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected string literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx2 raw2)) value cx input))),
                      (Core.Name "string", (\input -> Eithers.map (\t -> Model.ValueString t) (Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralString v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected string literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx input))))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)
