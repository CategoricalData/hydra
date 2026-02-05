-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.query

module Hydra.Decode.Query where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Query as Query
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

comparisonConstraint :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.ComparisonConstraint)
comparisonConstraint cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "equal", (\input -> Eithers.map (\t -> Query.ComparisonConstraintEqual) (Helpers.decodeUnit cx input))),
                (Core.Name "notEqual", (\input -> Eithers.map (\t -> Query.ComparisonConstraintNotEqual) (Helpers.decodeUnit cx input))),
                (Core.Name "lessThan", (\input -> Eithers.map (\t -> Query.ComparisonConstraintLessThan) (Helpers.decodeUnit cx input))),
                (Core.Name "greaterThan", (\input -> Eithers.map (\t -> Query.ComparisonConstraintGreaterThan) (Helpers.decodeUnit cx input))),
                (Core.Name "lessThanOrEqual", (\input -> Eithers.map (\t -> Query.ComparisonConstraintLessThanOrEqual) (Helpers.decodeUnit cx input))),
                (Core.Name "greaterThanOrEqual", (\input -> Eithers.map (\t -> Query.ComparisonConstraintGreaterThanOrEqual) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      (Core.unName fname),
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.query.ComparisonConstraint"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

edge :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Edge)
edge cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "type" Core_.name fieldMap cx) (\field_type -> Eithers.bind (Helpers.requireField "out" (Helpers.decodeMaybe Core_.name) fieldMap cx) (\field_out -> Eithers.bind (Helpers.requireField "in" (Helpers.decodeMaybe Core_.name) fieldMap cx) (\field_in -> Right (Query.Edge {
      Query.edgeType = field_type,
      Query.edgeOut = field_out,
      Query.edgeIn = field_in})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.query.Edge"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

graphPattern :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.GraphPattern)
graphPattern cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "graph" Core_.name fieldMap cx) (\field_graph -> Eithers.bind (Helpers.requireField "patterns" (Helpers.decodeList pattern) fieldMap cx) (\field_patterns -> Right (Query.GraphPattern {
      Query.graphPatternGraph = field_graph,
      Query.graphPatternPatterns = field_patterns}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.query.GraphPattern"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

node :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Node)
node cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "term", (\input -> Eithers.map (\t -> Query.NodeTerm t) (Core_.term cx input))),
                (Core.Name "variable", (\input -> Eithers.map (\t -> Query.NodeVariable t) (variable cx input))),
                (Core.Name "wildcard", (\input -> Eithers.map (\t -> Query.NodeWildcard) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      (Core.unName fname),
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.query.Node"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

path :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Path)
path cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "step", (\input -> Eithers.map (\t -> Query.PathStep t) (step cx input))),
                (Core.Name "regex", (\input -> Eithers.map (\t -> Query.PathRegex t) (regexSequence cx input))),
                (Core.Name "inverse", (\input -> Eithers.map (\t -> Query.PathInverse t) (path cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      (Core.unName fname),
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.query.Path"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

pattern :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Pattern)
pattern cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "triple", (\input -> Eithers.map (\t -> Query.PatternTriple t) (triplePattern cx input))),
                (Core.Name "negation", (\input -> Eithers.map (\t -> Query.PatternNegation t) (pattern cx input))),
                (Core.Name "conjunction", (\input -> Eithers.map (\t -> Query.PatternConjunction t) (Helpers.decodeList pattern cx input))),
                (Core.Name "disjunction", (\input -> Eithers.map (\t -> Query.PatternDisjunction t) (Helpers.decodeList pattern cx input))),
                (Core.Name "graph", (\input -> Eithers.map (\t -> Query.PatternGraph t) (graphPattern cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      (Core.unName fname),
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.query.Pattern"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

query :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Query)
query cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "variables" (Helpers.decodeList variable) fieldMap cx) (\field_variables -> Eithers.bind (Helpers.requireField "patterns" (Helpers.decodeList pattern) fieldMap cx) (\field_patterns -> Right (Query.Query {
      Query.queryVariables = field_variables,
      Query.queryPatterns = field_patterns}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.query.Query"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

range :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Range)
range cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "min" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_min -> Eithers.bind (Helpers.requireField "max" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_max -> Right (Query.Range {
      Query.rangeMin = field_min,
      Query.rangeMax = field_max}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.query.Range"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

regexQuantifier :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.RegexQuantifier)
regexQuantifier cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "one", (\input -> Eithers.map (\t -> Query.RegexQuantifierOne) (Helpers.decodeUnit cx input))),
                (Core.Name "zeroOrOne", (\input -> Eithers.map (\t -> Query.RegexQuantifierZeroOrOne) (Helpers.decodeUnit cx input))),
                (Core.Name "zeroOrMore", (\input -> Eithers.map (\t -> Query.RegexQuantifierZeroOrMore) (Helpers.decodeUnit cx input))),
                (Core.Name "oneOrMore", (\input -> Eithers.map (\t -> Query.RegexQuantifierOneOrMore) (Helpers.decodeUnit cx input))),
                (Core.Name "exactly", (\input -> Eithers.map (\t -> Query.RegexQuantifierExactly t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueInt32 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "atLeast", (\input -> Eithers.map (\t -> Query.RegexQuantifierAtLeast t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueInt32 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "range", (\input -> Eithers.map (\t -> Query.RegexQuantifierRange t) (range cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      (Core.unName fname),
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.query.RegexQuantifier"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

regexSequence :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.RegexSequence)
regexSequence cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "path" path fieldMap cx) (\field_path -> Eithers.bind (Helpers.requireField "quantifier" regexQuantifier fieldMap cx) (\field_quantifier -> Right (Query.RegexSequence {
      Query.regexSequencePath = field_path,
      Query.regexSequenceQuantifier = field_quantifier}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.query.RegexSequence"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

step :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Step)
step cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "edge", (\input -> Eithers.map (\t -> Query.StepEdge t) (edge cx input))),
                (Core.Name "project", (\input -> Eithers.map (\t -> Query.StepProject t) (Core_.projection cx input))),
                (Core.Name "compare", (\input -> Eithers.map (\t -> Query.StepCompare t) (comparisonConstraint cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      (Core.unName fname),
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.query.Step"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

triplePattern :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.TriplePattern)
triplePattern cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "subject" node fieldMap cx) (\field_subject -> Eithers.bind (Helpers.requireField "predicate" path fieldMap cx) (\field_predicate -> Eithers.bind (Helpers.requireField "object" node fieldMap cx) (\field_object -> Right (Query.TriplePattern {
      Query.triplePatternSubject = field_subject,
      Query.triplePatternPredicate = field_predicate,
      Query.triplePatternObject = field_object})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.query.TriplePattern"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

variable :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Variable)
variable cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Query.Variable b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralString v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected string literal"))) v2)
    _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.query.Variable"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
