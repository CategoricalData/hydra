-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.query

module Hydra.Encode.Query where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Query as Query
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
comparisonConstraint :: Query.ComparisonConstraint -> Core.Term
comparisonConstraint x =
    case x of
      Query.ComparisonConstraintEqual -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "equal"),
          Core.fieldTerm = Core.TermUnit}})
      Query.ComparisonConstraintNotEqual -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "notEqual"),
          Core.fieldTerm = Core.TermUnit}})
      Query.ComparisonConstraintLessThan -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "lessThan"),
          Core.fieldTerm = Core.TermUnit}})
      Query.ComparisonConstraintGreaterThan -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "greaterThan"),
          Core.fieldTerm = Core.TermUnit}})
      Query.ComparisonConstraintLessThanOrEqual -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "lessThanOrEqual"),
          Core.fieldTerm = Core.TermUnit}})
      Query.ComparisonConstraintGreaterThanOrEqual -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "greaterThanOrEqual"),
          Core.fieldTerm = Core.TermUnit}})
edge :: Query.Edge -> Core.Term
edge x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (EncodeCore.name (Query.edgeType x))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map EncodeCore.name opt)) (Query.edgeOut x))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map EncodeCore.name opt)) (Query.edgeIn x))}]})
graphPattern :: Query.GraphPattern -> Core.Term
graphPattern x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.GraphPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (EncodeCore.name (Query.graphPatternGraph x))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map pattern xs)) (Query.graphPatternPatterns x))}]})
node :: Query.Node -> Core.Term
node x =
    case x of
      Query.NodeTerm v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Node"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (EncodeCore.term v0)}})
      Query.NodeVariable v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Node"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (variable v0)}})
      Query.NodeWildcard -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Node"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "wildcard"),
          Core.fieldTerm = Core.TermUnit}})
path :: Query.Path -> Core.Term
path x =
    case x of
      Query.PathStep v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Path"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "step"),
          Core.fieldTerm = (step v0)}})
      Query.PathRegex v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Path"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (regexSequence v0)}})
      Query.PathInverse v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Path"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "inverse"),
          Core.fieldTerm = (path v0)}})
pathEquation :: Query.PathEquation -> Core.Term
pathEquation x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PathEquation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (path (Query.pathEquationLeft x))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (path (Query.pathEquationRight x))}]})
pattern :: Query.Pattern -> Core.Term
pattern x =
    case x of
      Query.PatternTriple v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "triple"),
          Core.fieldTerm = (triplePattern v0)}})
      Query.PatternNegation v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "negation"),
          Core.fieldTerm = (pattern v0)}})
      Query.PatternConjunction v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "conjunction"),
          Core.fieldTerm = (Core.TermList (Lists.map pattern v0))}})
      Query.PatternDisjunction v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "disjunction"),
          Core.fieldTerm = (Core.TermList (Lists.map pattern v0))}})
      Query.PatternGraph v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (graphPattern v0)}})
patternImplication :: Query.PatternImplication -> Core.Term
patternImplication x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PatternImplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "antecedent"),
          Core.fieldTerm = (pattern (Query.patternImplicationAntecedent x))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (pattern (Query.patternImplicationConsequent x))}]})
query :: Query.Query -> Core.Term
query x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Query"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map variable xs)) (Query.queryVariables x))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map pattern xs)) (Query.queryPatterns x))}]})
range :: Query.Range -> Core.Term
range x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Range"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Query.rangeMin x))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Query.rangeMax x))}]})
regexQuantifier :: Query.RegexQuantifier -> Core.Term
regexQuantifier x =
    case x of
      Query.RegexQuantifierOne -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "one"),
          Core.fieldTerm = Core.TermUnit}})
      Query.RegexQuantifierZeroOrOne -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "zeroOrOne"),
          Core.fieldTerm = Core.TermUnit}})
      Query.RegexQuantifierZeroOrMore -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "zeroOrMore"),
          Core.fieldTerm = Core.TermUnit}})
      Query.RegexQuantifierOneOrMore -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "oneOrMore"),
          Core.fieldTerm = Core.TermUnit}})
      Query.RegexQuantifierExactly v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "exactly"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v0)))}})
      Query.RegexQuantifierAtLeast v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "atLeast"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v0)))}})
      Query.RegexQuantifierRange v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (range v0)}})
regexSequence :: Query.RegexSequence -> Core.Term
regexSequence x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.RegexSequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (path (Query.regexSequencePath x))},
        Core.Field {
          Core.fieldName = (Core.Name "quantifier"),
          Core.fieldTerm = (regexQuantifier (Query.regexSequenceQuantifier x))}]})
step :: Query.Step -> Core.Term
step x =
    case x of
      Query.StepEdge v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Step"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (edge v0)}})
      Query.StepProject v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Step"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "project"),
          Core.fieldTerm = (EncodeCore.projection v0)}})
      Query.StepCompare v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.query.Step"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "compare"),
          Core.fieldTerm = (comparisonConstraint v0)}})
triplePattern :: Query.TriplePattern -> Core.Term
triplePattern x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (node (Query.triplePatternSubject x))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (path (Query.triplePatternPredicate x))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (node (Query.triplePatternObject x))}]})
variable :: Query.Variable -> Core.Term
variable x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.query.Variable"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Query.unVariable x))})
