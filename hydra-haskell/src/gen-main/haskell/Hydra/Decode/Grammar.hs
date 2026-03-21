-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.grammar

module Hydra.Decode.Grammar where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Grammar as Grammar
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

constant :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Grammar.Constant
constant cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Grammar.Constant b) ((\raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

grammar :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Grammar.Grammar
grammar cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Grammar.Grammar b) (Helpers.decodeList production cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

label :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Grammar.Label
label cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Grammar.Label b) ((\raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

labeledPattern :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Grammar.LabeledPattern
labeledPattern cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "label" label fieldMap cx) (\field_label -> Eithers.bind (Helpers.requireField "pattern" pattern fieldMap cx) (\field_pattern -> Right (Grammar.LabeledPattern {
          Grammar.labeledPatternLabel = field_label,
          Grammar.labeledPatternPattern = field_pattern}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

pattern :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Grammar.Pattern
pattern cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "alternatives", (\input -> Eithers.map (\t -> Grammar.PatternAlternatives t) (Helpers.decodeList pattern cx input))),
                      (Core.Name "constant", (\input -> Eithers.map (\t -> Grammar.PatternConstant t) (constant cx input))),
                      (Core.Name "ignored", (\input -> Eithers.map (\t -> Grammar.PatternIgnored t) (pattern cx input))),
                      (Core.Name "labeled", (\input -> Eithers.map (\t -> Grammar.PatternLabeled t) (labeledPattern cx input))),
                      (Core.Name "nil", (\input -> Eithers.map (\t -> Grammar.PatternNil) (Helpers.decodeUnit cx input))),
                      (Core.Name "nonterminal", (\input -> Eithers.map (\t -> Grammar.PatternNonterminal t) (symbol cx input))),
                      (Core.Name "option", (\input -> Eithers.map (\t -> Grammar.PatternOption t) (pattern cx input))),
                      (Core.Name "plus", (\input -> Eithers.map (\t -> Grammar.PatternPlus t) (pattern cx input))),
                      (Core.Name "regex", (\input -> Eithers.map (\t -> Grammar.PatternRegex t) (regex cx input))),
                      (Core.Name "sequence", (\input -> Eithers.map (\t -> Grammar.PatternSequence t) (Helpers.decodeList pattern cx input))),
                      (Core.Name "star", (\input -> Eithers.map (\t -> Grammar.PatternStar t) (pattern cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

production :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Grammar.Production
production cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "symbol" symbol fieldMap cx) (\field_symbol -> Eithers.bind (Helpers.requireField "pattern" pattern fieldMap cx) (\field_pattern -> Right (Grammar.Production {
          Grammar.productionSymbol = field_symbol,
          Grammar.productionPattern = field_pattern}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

regex :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Grammar.Regex
regex cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Grammar.Regex b) ((\raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

symbol :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Grammar.Symbol
symbol cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Grammar.Symbol b) ((\raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)
