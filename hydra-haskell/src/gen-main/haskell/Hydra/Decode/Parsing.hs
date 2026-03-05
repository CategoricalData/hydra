-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.parsing

module Hydra.Decode.Parsing where

import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Parsing as Parsing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

parseError :: (Graph.Graph -> Core.Term -> Either Error.DecodingError Parsing.ParseError)
parseError cx raw = (Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v0 ->  
    let fieldMap = (Helpers.toFieldMap v0)
    in (Eithers.bind (Helpers.requireField "message" (\cx -> \raw -> Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v1 -> ((\x -> case x of
        Core.LiteralString v2 -> (Right v2)
        _ -> (Left (Error.DecodingError "expected string literal"))) v1)
      _ -> (Left (Error.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_message -> Eithers.bind (Helpers.requireField "remainder" (\cx -> \raw -> Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v1 -> ((\x -> case x of
        Core.LiteralString v2 -> (Right v2)
        _ -> (Left (Error.DecodingError "expected string literal"))) v1)
      _ -> (Left (Error.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_remainder -> Right (Parsing.ParseError {
      Parsing.parseErrorMessage = field_message,
      Parsing.parseErrorRemainder = field_remainder}))))
  _ -> (Left (Error.DecodingError "expected record of type hydra.parsing.ParseError"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

parseResult :: ((Graph.Graph -> Core.Term -> Either Error.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Error.DecodingError (Parsing.ParseResult t0))
parseResult a cx raw = (Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v0 ->  
    let tname = (Core.injectionTypeName v0) 
        field = (Core.injectionField v0)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "success", (\input -> Eithers.map (\t -> Parsing.ParseResultSuccess t) (parseSuccess a cx input))),
                (Core.Name "failure", (\input -> Eithers.map (\t -> Parsing.ParseResultFailure t) (parseError cx input)))])
    in (Maybes.maybe (Left (Error.DecodingError (Strings.cat [
      "no such field ",
      (Core.unName fname),
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Error.DecodingError "expected union of type hydra.parsing.ParseResult"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

parseSuccess :: ((Graph.Graph -> Core.Term -> Either Error.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Error.DecodingError (Parsing.ParseSuccess t0))
parseSuccess a cx raw = (Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v0 ->  
    let fieldMap = (Helpers.toFieldMap v0)
    in (Eithers.bind (Helpers.requireField "value" a fieldMap cx) (\field_value -> Eithers.bind (Helpers.requireField "remainder" (\cx -> \raw -> Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v1 -> ((\x -> case x of
        Core.LiteralString v2 -> (Right v2)
        _ -> (Left (Error.DecodingError "expected string literal"))) v1)
      _ -> (Left (Error.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_remainder -> Right (Parsing.ParseSuccess {
      Parsing.parseSuccessValue = field_value,
      Parsing.parseSuccessRemainder = field_remainder}))))
  _ -> (Left (Error.DecodingError "expected record of type hydra.parsing.ParseSuccess"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
