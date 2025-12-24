-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.parsing

module Hydra.Decode.Parsing where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

parseError :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Parsing.ParseError)
parseError cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\message -> Eithers.either (\err -> Left err) (\remainder -> Right (Parsing.ParseError {
      Parsing.parseErrorMessage = message,
      Parsing.parseErrorRemainder = remainder})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "remainder",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "remainder") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "message",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "message") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.parsing.ParseError"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

parseResult :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Parsing.ParseResult t0))
parseResult a cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "success", (\input -> Eithers.map (\t -> Parsing.ParseResultSuccess t) (parseSuccess a cx input))),
                (Core.Name "failure", (\input -> Eithers.map (\t -> Parsing.ParseResultFailure t) (parseError cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.parsing.ParseResult"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

parseSuccess :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Parsing.ParseSuccess t0))
parseSuccess a cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\value -> Eithers.either (\err -> Left err) (\remainder -> Right (Parsing.ParseSuccess {
      Parsing.parseSuccessValue = value,
      Parsing.parseSuccessRemainder = remainder})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "remainder",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "remainder") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "value",
      " in record"]))) (\fieldTerm -> a cx fieldTerm) (Maps.lookup (Core.Name "value") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.parsing.ParseSuccess"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
