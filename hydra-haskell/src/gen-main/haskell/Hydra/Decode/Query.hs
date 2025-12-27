-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.query

module Hydra.Decode.Query where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Query as Query
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
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
                (Core.Name "equal", (\input -> Eithers.map (\t -> Query.ComparisonConstraintEqual) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "notEqual", (\input -> Eithers.map (\t -> Query.ComparisonConstraintNotEqual) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "lessThan", (\input -> Eithers.map (\t -> Query.ComparisonConstraintLessThan) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "greaterThan", (\input -> Eithers.map (\t -> Query.ComparisonConstraintGreaterThan) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "lessThanOrEqual", (\input -> Eithers.map (\t -> Query.ComparisonConstraintLessThanOrEqual) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "greaterThanOrEqual", (\input -> Eithers.map (\t -> Query.ComparisonConstraintGreaterThanOrEqual) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.query.ComparisonConstraint"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

edge :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Edge)
edge cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_type -> Eithers.either (\err -> Left err) (\field_out -> Eithers.either (\err -> Left err) (\field_in -> Right (Query.Edge {
      Query.edgeType = field_type,
      Query.edgeOut = field_out,
      Query.edgeIn = field_in})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "in",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (Core_.name cx) v2)
      _ -> (Left (Util.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "in") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "out",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (Core_.name cx) v2)
      _ -> (Left (Util.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "out") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "type",
      " in record"]))) (\fieldTerm -> Core_.name cx fieldTerm) (Maps.lookup (Core.Name "type") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.query.Edge"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

graphPattern :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.GraphPattern)
graphPattern cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_graph -> Eithers.either (\err -> Left err) (\field_patterns -> Right (Query.GraphPattern {
      Query.graphPatternGraph = field_graph,
      Query.graphPatternPatterns = field_patterns})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "patterns",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (pattern cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "patterns") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "graph",
      " in record"]))) (\fieldTerm -> Core_.name cx fieldTerm) (Maps.lookup (Core.Name "graph") fieldMap)))
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
                (Core.Name "wildcard", (\input -> Eithers.map (\t -> Query.NodeWildcard) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
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
      Core.unName fname,
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
                (Core.Name "conjunction", (\input -> Eithers.map (\t -> Query.PatternConjunction t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermList v2 -> (Eithers.mapList (pattern cx) v2)
                  _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "disjunction", (\input -> Eithers.map (\t -> Query.PatternDisjunction t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermList v2 -> (Eithers.mapList (pattern cx) v2)
                  _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "graph", (\input -> Eithers.map (\t -> Query.PatternGraph t) (graphPattern cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.query.Pattern"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

query :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Query)
query cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_variables -> Eithers.either (\err -> Left err) (\field_patterns -> Right (Query.Query {
      Query.queryVariables = field_variables,
      Query.queryPatterns = field_patterns})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "patterns",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (pattern cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "patterns") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "variables",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (variable cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "variables") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.query.Query"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

range :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Range)
range cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_min -> Eithers.either (\err -> Left err) (\field_max -> Right (Query.Range {
      Query.rangeMin = field_min,
      Query.rangeMax = field_max})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "max",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "max") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "min",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "min") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.query.Range"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

regexQuantifier :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.RegexQuantifier)
regexQuantifier cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "one", (\input -> Eithers.map (\t -> Query.RegexQuantifierOne) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "zeroOrOne", (\input -> Eithers.map (\t -> Query.RegexQuantifierZeroOrOne) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "zeroOrMore", (\input -> Eithers.map (\t -> Query.RegexQuantifierZeroOrMore) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "oneOrMore", (\input -> Eithers.map (\t -> Query.RegexQuantifierOneOrMore) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
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
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.query.RegexQuantifier"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

regexSequence :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.RegexSequence)
regexSequence cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_path -> Eithers.either (\err -> Left err) (\field_quantifier -> Right (Query.RegexSequence {
      Query.regexSequencePath = field_path,
      Query.regexSequenceQuantifier = field_quantifier})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "quantifier",
      " in record"]))) (\fieldTerm -> regexQuantifier cx fieldTerm) (Maps.lookup (Core.Name "quantifier") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "path",
      " in record"]))) (\fieldTerm -> path cx fieldTerm) (Maps.lookup (Core.Name "path") fieldMap)))
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
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.query.Step"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

triplePattern :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.TriplePattern)
triplePattern cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_subject -> Eithers.either (\err -> Left err) (\field_predicate -> Eithers.either (\err -> Left err) (\field_object -> Right (Query.TriplePattern {
      Query.triplePatternSubject = field_subject,
      Query.triplePatternPredicate = field_predicate,
      Query.triplePatternObject = field_object})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "object",
      " in record"]))) (\fieldTerm -> node cx fieldTerm) (Maps.lookup (Core.Name "object") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "predicate",
      " in record"]))) (\fieldTerm -> path cx fieldTerm) (Maps.lookup (Core.Name "predicate") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "subject",
      " in record"]))) (\fieldTerm -> node cx fieldTerm) (Maps.lookup (Core.Name "subject") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.query.TriplePattern"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

variable :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Query.Variable)
variable cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Query.Variable b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralString v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected string literal"))) v2)
    _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.query.Variable"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
