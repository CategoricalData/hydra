-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.json

module Hydra.Decode.Json where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Json as Json
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

value :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Json.Value)
value cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "array", (\input -> Eithers.map (\t -> Json.ValueArray t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermList v2 -> (Eithers.mapList (value cx) v2)
                  _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "boolean", (\input -> Eithers.map (\t -> Json.ValueBoolean t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralBoolean v3 -> (Right v3)
                    _ -> (Left (Util.DecodingError "expected boolean literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "null", (\input -> Eithers.map (\t -> Json.ValueNull) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "number", (\input -> Eithers.map (\t -> Json.ValueNumber t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralFloat v3 -> ((\x -> case x of
                      Core.FloatValueBigfloat v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected bigfloat value"))) v3)
                    _ -> (Left (Util.DecodingError "expected bigfloat literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "object", (\input -> Eithers.map (\t -> Json.ValueObject t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermMap v2 ->  
                    let pairs = (Maps.toList v2) 
                        decodePair = (\kv ->  
                                let rawKey = (Pairs.first kv) 
                                    rawVal = (Pairs.second kv)
                                in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (value cx rawVal)) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                                  Core.TermLiteral v3 -> ((\x -> case x of
                                    Core.LiteralString v4 -> (Right v4)
                                    _ -> (Left (Util.DecodingError "expected string literal"))) v3)
                                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx rawKey))))
                    in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
                  _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "string", (\input -> Eithers.map (\t -> Json.ValueString t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralString v3 -> (Right v3)
                    _ -> (Left (Util.DecodingError "expected string literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.json.Value"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
