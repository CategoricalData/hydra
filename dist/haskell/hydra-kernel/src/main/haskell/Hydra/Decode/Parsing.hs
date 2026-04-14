-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.parsing

module Hydra.Decode.Parsing where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Parsing as Parsing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

parseError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Parsing.ParseError
parseError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "message" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Core_.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_message -> Eithers.bind (Core_.requireField "remainder" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Core_.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_remainder -> Right (Parsing.ParseError {
          Parsing.parseErrorMessage = field_message,
          Parsing.parseErrorRemainder = field_remainder}))))
      _ -> Left (Errors.DecodingError "expected record")) (Core_.stripWithDecodingError cx raw)

parseResult :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Parsing.ParseResult t0)
parseResult a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "success", (\input -> Eithers.map (\t -> Parsing.ParseResultSuccess t) (parseSuccess a cx input))),
                      (Core.Name "failure", (\input -> Eithers.map (\t -> Parsing.ParseResultFailure t) (parseError cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Core_.stripWithDecodingError cx raw)

parseSuccess :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Parsing.ParseSuccess t0)
parseSuccess a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "value" a fieldMap cx) (\field_value -> Eithers.bind (Core_.requireField "remainder" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Core_.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_remainder -> Right (Parsing.ParseSuccess {
          Parsing.parseSuccessValue = field_value,
          Parsing.parseSuccessRemainder = field_remainder}))))
      _ -> Left (Errors.DecodingError "expected record")) (Core_.stripWithDecodingError cx raw)
