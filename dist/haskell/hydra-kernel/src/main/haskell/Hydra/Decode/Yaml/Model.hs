-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.yaml.model

module Hydra.Decode.Yaml.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import qualified Hydra.Yaml.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Decoder for hydra.yaml.model.Node
node :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Node
node cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "mapping", (\input -> Eithers.map (\t -> Model.NodeMapping t) (ExtractCore.decodeMap node node cx input))),
                      (Core.Name "scalar", (\input -> Eithers.map (\t -> Model.NodeScalar t) (scalar cx input))),
                      (Core.Name "sequence", (\input -> Eithers.map (\t -> Model.NodeSequence t) (ExtractCore.decodeList node cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.yaml.model.Scalar
scalar :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Scalar
scalar cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (
                        Core.Name "bool",
                        (\input -> Eithers.map (\t -> Model.ScalarBool t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralBoolean v2 -> Right v2
                            _ -> Left (Errors.DecodingError "expected boolean literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "decimal",
                        (\input -> Eithers.map (\t -> Model.ScalarDecimal t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralDecimal v2 -> Right v2
                            _ -> Left (Errors.DecodingError "expected decimal literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "float",
                        (\input -> Eithers.map (\t -> Model.ScalarFloat t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralFloat v2 -> case v2 of
                              Core.FloatValueFloat64 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected float64 value")
                            _ -> Left (Errors.DecodingError "expected float64 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "int",
                        (\input -> Eithers.map (\t -> Model.ScalarInt t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueBigint v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected bigint value")
                            _ -> Left (Errors.DecodingError "expected bigint literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "null", (\input -> Eithers.map (\t -> Model.ScalarNull) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "str",
                        (\input -> Eithers.map (\t -> Model.ScalarStr t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralString v2 -> Right v2
                            _ -> Left (Errors.DecodingError "expected string literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input))))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
