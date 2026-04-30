-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.query

module Hydra.Decode.Query where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Query as Query
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
comparisonConstraint :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.ComparisonConstraint
comparisonConstraint cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "equal", (\input -> Eithers.map (\t -> Query.ComparisonConstraintEqual) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "notEqual",
                        (\input -> Eithers.map (\t -> Query.ComparisonConstraintNotEqual) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "lessThan",
                        (\input -> Eithers.map (\t -> Query.ComparisonConstraintLessThan) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "greaterThan",
                        (\input -> Eithers.map (\t -> Query.ComparisonConstraintGreaterThan) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "lessThanOrEqual",
                        (\input -> Eithers.map (\t -> Query.ComparisonConstraintLessThanOrEqual) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "greaterThanOrEqual",
                        (\input -> Eithers.map (\t -> Query.ComparisonConstraintGreaterThanOrEqual) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
edge :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.Edge
edge cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "type" DecodeCore.name fieldMap cx) (\field_type -> Eithers.bind (ExtractCore.requireField "out" (ExtractCore.decodeMaybe DecodeCore.name) fieldMap cx) (\field_out -> Eithers.bind (ExtractCore.requireField "in" (ExtractCore.decodeMaybe DecodeCore.name) fieldMap cx) (\field_in -> Right (Query.Edge {
          Query.edgeType = field_type,
          Query.edgeOut = field_out,
          Query.edgeIn = field_in})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
graphPattern :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.GraphPattern
graphPattern cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "graph" DecodeCore.name fieldMap cx) (\field_graph -> Eithers.bind (ExtractCore.requireField "patterns" (ExtractCore.decodeList pattern) fieldMap cx) (\field_patterns -> Right (Query.GraphPattern {
          Query.graphPatternGraph = field_graph,
          Query.graphPatternPatterns = field_patterns}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
node :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.Node
node cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "term", (\input -> Eithers.map (\t -> Query.NodeTerm t) (DecodeCore.term cx input))),
                      (Core.Name "variable", (\input -> Eithers.map (\t -> Query.NodeVariable t) (variable cx input))),
                      (Core.Name "wildcard", (\input -> Eithers.map (\t -> Query.NodeWildcard) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
path :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.Path
path cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "step", (\input -> Eithers.map (\t -> Query.PathStep t) (step cx input))),
                      (Core.Name "regex", (\input -> Eithers.map (\t -> Query.PathRegex t) (regexSequence cx input))),
                      (Core.Name "inverse", (\input -> Eithers.map (\t -> Query.PathInverse t) (path cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
pathEquation :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.PathEquation
pathEquation cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "left" path fieldMap cx) (\field_left -> Eithers.bind (ExtractCore.requireField "right" path fieldMap cx) (\field_right -> Right (Query.PathEquation {
          Query.pathEquationLeft = field_left,
          Query.pathEquationRight = field_right}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
pattern :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.Pattern
pattern cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "triple", (\input -> Eithers.map (\t -> Query.PatternTriple t) (triplePattern cx input))),
                      (Core.Name "negation", (\input -> Eithers.map (\t -> Query.PatternNegation t) (pattern cx input))),
                      (
                        Core.Name "conjunction",
                        (\input -> Eithers.map (\t -> Query.PatternConjunction t) (ExtractCore.decodeList pattern cx input))),
                      (
                        Core.Name "disjunction",
                        (\input -> Eithers.map (\t -> Query.PatternDisjunction t) (ExtractCore.decodeList pattern cx input))),
                      (Core.Name "graph", (\input -> Eithers.map (\t -> Query.PatternGraph t) (graphPattern cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
patternImplication :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.PatternImplication
patternImplication cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "antecedent" pattern fieldMap cx) (\field_antecedent -> Eithers.bind (ExtractCore.requireField "consequent" pattern fieldMap cx) (\field_consequent -> Right (Query.PatternImplication {
          Query.patternImplicationAntecedent = field_antecedent,
          Query.patternImplicationConsequent = field_consequent}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
query :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.Query
query cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "variables" (ExtractCore.decodeList variable) fieldMap cx) (\field_variables -> Eithers.bind (ExtractCore.requireField "patterns" (ExtractCore.decodeList pattern) fieldMap cx) (\field_patterns -> Right (Query.Query {
          Query.queryVariables = field_variables,
          Query.queryPatterns = field_patterns}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
range :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.Range
range cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "min" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_min -> Eithers.bind (ExtractCore.requireField "max" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_max -> Right (Query.Range {
          Query.rangeMin = field_min,
          Query.rangeMax = field_max}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
regexQuantifier :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.RegexQuantifier
regexQuantifier cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "one", (\input -> Eithers.map (\t -> Query.RegexQuantifierOne) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "zeroOrOne", (\input -> Eithers.map (\t -> Query.RegexQuantifierZeroOrOne) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "zeroOrMore",
                        (\input -> Eithers.map (\t -> Query.RegexQuantifierZeroOrMore) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "oneOrMore", (\input -> Eithers.map (\t -> Query.RegexQuantifierOneOrMore) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "exactly",
                        (\input -> Eithers.map (\t -> Query.RegexQuantifierExactly t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueInt32 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected int32 value")
                            _ -> Left (Errors.DecodingError "expected int32 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "atLeast",
                        (\input -> Eithers.map (\t -> Query.RegexQuantifierAtLeast t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueInt32 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected int32 value")
                            _ -> Left (Errors.DecodingError "expected int32 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "range", (\input -> Eithers.map (\t -> Query.RegexQuantifierRange t) (range cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
regexSequence :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.RegexSequence
regexSequence cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "path" path fieldMap cx) (\field_path -> Eithers.bind (ExtractCore.requireField "quantifier" regexQuantifier fieldMap cx) (\field_quantifier -> Right (Query.RegexSequence {
          Query.regexSequencePath = field_path,
          Query.regexSequenceQuantifier = field_quantifier}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
step :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.Step
step cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "edge", (\input -> Eithers.map (\t -> Query.StepEdge t) (edge cx input))),
                      (Core.Name "project", (\input -> Eithers.map (\t -> Query.StepProject t) (DecodeCore.projection cx input))),
                      (Core.Name "compare", (\input -> Eithers.map (\t -> Query.StepCompare t) (comparisonConstraint cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
triplePattern :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.TriplePattern
triplePattern cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "subject" node fieldMap cx) (\field_subject -> Eithers.bind (ExtractCore.requireField "predicate" path fieldMap cx) (\field_predicate -> Eithers.bind (ExtractCore.requireField "object" node fieldMap cx) (\field_object -> Right (Query.TriplePattern {
          Query.triplePatternSubject = field_subject,
          Query.triplePatternPredicate = field_predicate,
          Query.triplePatternObject = field_object})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
variable :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Query.Variable
variable cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Query.Variable b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
