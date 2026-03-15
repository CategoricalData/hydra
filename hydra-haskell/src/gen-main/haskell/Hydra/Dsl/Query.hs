-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.query

module Hydra.Dsl.Query where

import qualified Hydra.Core as Core
import qualified Hydra.Query as Query
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

comparisonConstraintEqual :: Query.ComparisonConstraint
comparisonConstraintEqual = Query.ComparisonConstraintEqual

comparisonConstraintNotEqual :: Query.ComparisonConstraint
comparisonConstraintNotEqual = Query.ComparisonConstraintNotEqual

comparisonConstraintLessThan :: Query.ComparisonConstraint
comparisonConstraintLessThan = Query.ComparisonConstraintLessThan

comparisonConstraintGreaterThan :: Query.ComparisonConstraint
comparisonConstraintGreaterThan = Query.ComparisonConstraintGreaterThan

comparisonConstraintLessThanOrEqual :: Query.ComparisonConstraint
comparisonConstraintLessThanOrEqual = Query.ComparisonConstraintLessThanOrEqual

comparisonConstraintGreaterThanOrEqual :: Query.ComparisonConstraint
comparisonConstraintGreaterThanOrEqual = Query.ComparisonConstraintGreaterThanOrEqual

edge :: (Core.Name -> Maybe Core.Name -> Maybe Core.Name -> Query.Edge)
edge type_ out in_ = Query.Edge {
  Query.edgeType = type_,
  Query.edgeOut = out,
  Query.edgeIn = in_}

edgeType :: (Query.Edge -> Core.Name)
edgeType = Query.edgeType

edgeOut :: (Query.Edge -> Maybe Core.Name)
edgeOut = Query.edgeOut

edgeIn :: (Query.Edge -> Maybe Core.Name)
edgeIn = Query.edgeIn

edgeWithType :: (Query.Edge -> Core.Name -> Query.Edge)
edgeWithType original newVal = Query.Edge {
  Query.edgeType = newVal,
  Query.edgeOut = (Query.edgeOut original),
  Query.edgeIn = (Query.edgeIn original)}

edgeWithOut :: (Query.Edge -> Maybe Core.Name -> Query.Edge)
edgeWithOut original newVal = Query.Edge {
  Query.edgeType = (Query.edgeType original),
  Query.edgeOut = newVal,
  Query.edgeIn = (Query.edgeIn original)}

edgeWithIn :: (Query.Edge -> Maybe Core.Name -> Query.Edge)
edgeWithIn original newVal = Query.Edge {
  Query.edgeType = (Query.edgeType original),
  Query.edgeOut = (Query.edgeOut original),
  Query.edgeIn = newVal}

graphPattern :: (Core.Name -> [Query.Pattern] -> Query.GraphPattern)
graphPattern graph patterns = Query.GraphPattern {
  Query.graphPatternGraph = graph,
  Query.graphPatternPatterns = patterns}

graphPatternGraph :: (Query.GraphPattern -> Core.Name)
graphPatternGraph = Query.graphPatternGraph

graphPatternPatterns :: (Query.GraphPattern -> [Query.Pattern])
graphPatternPatterns = Query.graphPatternPatterns

graphPatternWithGraph :: (Query.GraphPattern -> Core.Name -> Query.GraphPattern)
graphPatternWithGraph original newVal = Query.GraphPattern {
  Query.graphPatternGraph = newVal,
  Query.graphPatternPatterns = (Query.graphPatternPatterns original)}

graphPatternWithPatterns :: (Query.GraphPattern -> [Query.Pattern] -> Query.GraphPattern)
graphPatternWithPatterns original newVal = Query.GraphPattern {
  Query.graphPatternGraph = (Query.graphPatternGraph original),
  Query.graphPatternPatterns = newVal}

nodeTerm :: (Core.Term -> Query.Node)
nodeTerm x = (Query.NodeTerm x)

nodeVariable :: (Query.Variable -> Query.Node)
nodeVariable x = (Query.NodeVariable x)

nodeWildcard :: Query.Node
nodeWildcard = Query.NodeWildcard

pathStep :: (Query.Step -> Query.Path)
pathStep x = (Query.PathStep x)

pathRegex :: (Query.RegexSequence -> Query.Path)
pathRegex x = (Query.PathRegex x)

pathInverse :: (Query.Path -> Query.Path)
pathInverse x = (Query.PathInverse x)

pathEquation :: (Query.Path -> Query.Path -> Query.PathEquation)
pathEquation left right = Query.PathEquation {
  Query.pathEquationLeft = left,
  Query.pathEquationRight = right}

pathEquationLeft :: (Query.PathEquation -> Query.Path)
pathEquationLeft = Query.pathEquationLeft

pathEquationRight :: (Query.PathEquation -> Query.Path)
pathEquationRight = Query.pathEquationRight

pathEquationWithLeft :: (Query.PathEquation -> Query.Path -> Query.PathEquation)
pathEquationWithLeft original newVal = Query.PathEquation {
  Query.pathEquationLeft = newVal,
  Query.pathEquationRight = (Query.pathEquationRight original)}

pathEquationWithRight :: (Query.PathEquation -> Query.Path -> Query.PathEquation)
pathEquationWithRight original newVal = Query.PathEquation {
  Query.pathEquationLeft = (Query.pathEquationLeft original),
  Query.pathEquationRight = newVal}

patternTriple :: (Query.TriplePattern -> Query.Pattern)
patternTriple x = (Query.PatternTriple x)

patternNegation :: (Query.Pattern -> Query.Pattern)
patternNegation x = (Query.PatternNegation x)

patternConjunction :: ([Query.Pattern] -> Query.Pattern)
patternConjunction x = (Query.PatternConjunction x)

patternDisjunction :: ([Query.Pattern] -> Query.Pattern)
patternDisjunction x = (Query.PatternDisjunction x)

patternGraph :: (Query.GraphPattern -> Query.Pattern)
patternGraph x = (Query.PatternGraph x)

patternImplication :: (Query.Pattern -> Query.Pattern -> Query.PatternImplication)
patternImplication antecedent consequent = Query.PatternImplication {
  Query.patternImplicationAntecedent = antecedent,
  Query.patternImplicationConsequent = consequent}

patternImplicationAntecedent :: (Query.PatternImplication -> Query.Pattern)
patternImplicationAntecedent = Query.patternImplicationAntecedent

patternImplicationConsequent :: (Query.PatternImplication -> Query.Pattern)
patternImplicationConsequent = Query.patternImplicationConsequent

patternImplicationWithAntecedent :: (Query.PatternImplication -> Query.Pattern -> Query.PatternImplication)
patternImplicationWithAntecedent original newVal = Query.PatternImplication {
  Query.patternImplicationAntecedent = newVal,
  Query.patternImplicationConsequent = (Query.patternImplicationConsequent original)}

patternImplicationWithConsequent :: (Query.PatternImplication -> Query.Pattern -> Query.PatternImplication)
patternImplicationWithConsequent original newVal = Query.PatternImplication {
  Query.patternImplicationAntecedent = (Query.patternImplicationAntecedent original),
  Query.patternImplicationConsequent = newVal}

query :: ([Query.Variable] -> [Query.Pattern] -> Query.Query)
query variables patterns = Query.Query {
  Query.queryVariables = variables,
  Query.queryPatterns = patterns}

queryVariables :: (Query.Query -> [Query.Variable])
queryVariables = Query.queryVariables

queryPatterns :: (Query.Query -> [Query.Pattern])
queryPatterns = Query.queryPatterns

queryWithVariables :: (Query.Query -> [Query.Variable] -> Query.Query)
queryWithVariables original newVal = Query.Query {
  Query.queryVariables = newVal,
  Query.queryPatterns = (Query.queryPatterns original)}

queryWithPatterns :: (Query.Query -> [Query.Pattern] -> Query.Query)
queryWithPatterns original newVal = Query.Query {
  Query.queryVariables = (Query.queryVariables original),
  Query.queryPatterns = newVal}

range :: (Int -> Int -> Query.Range)
range min max = Query.Range {
  Query.rangeMin = min,
  Query.rangeMax = max}

rangeMin :: (Query.Range -> Int)
rangeMin = Query.rangeMin

rangeMax :: (Query.Range -> Int)
rangeMax = Query.rangeMax

rangeWithMin :: (Query.Range -> Int -> Query.Range)
rangeWithMin original newVal = Query.Range {
  Query.rangeMin = newVal,
  Query.rangeMax = (Query.rangeMax original)}

rangeWithMax :: (Query.Range -> Int -> Query.Range)
rangeWithMax original newVal = Query.Range {
  Query.rangeMin = (Query.rangeMin original),
  Query.rangeMax = newVal}

regexQuantifierOne :: Query.RegexQuantifier
regexQuantifierOne = Query.RegexQuantifierOne

regexQuantifierZeroOrOne :: Query.RegexQuantifier
regexQuantifierZeroOrOne = Query.RegexQuantifierZeroOrOne

regexQuantifierZeroOrMore :: Query.RegexQuantifier
regexQuantifierZeroOrMore = Query.RegexQuantifierZeroOrMore

regexQuantifierOneOrMore :: Query.RegexQuantifier
regexQuantifierOneOrMore = Query.RegexQuantifierOneOrMore

regexQuantifierExactly :: (Int -> Query.RegexQuantifier)
regexQuantifierExactly x = (Query.RegexQuantifierExactly x)

regexQuantifierAtLeast :: (Int -> Query.RegexQuantifier)
regexQuantifierAtLeast x = (Query.RegexQuantifierAtLeast x)

regexQuantifierRange :: (Query.Range -> Query.RegexQuantifier)
regexQuantifierRange x = (Query.RegexQuantifierRange x)

regexSequence :: (Query.Path -> Query.RegexQuantifier -> Query.RegexSequence)
regexSequence path quantifier = Query.RegexSequence {
  Query.regexSequencePath = path,
  Query.regexSequenceQuantifier = quantifier}

regexSequencePath :: (Query.RegexSequence -> Query.Path)
regexSequencePath = Query.regexSequencePath

regexSequenceQuantifier :: (Query.RegexSequence -> Query.RegexQuantifier)
regexSequenceQuantifier = Query.regexSequenceQuantifier

regexSequenceWithPath :: (Query.RegexSequence -> Query.Path -> Query.RegexSequence)
regexSequenceWithPath original newVal = Query.RegexSequence {
  Query.regexSequencePath = newVal,
  Query.regexSequenceQuantifier = (Query.regexSequenceQuantifier original)}

regexSequenceWithQuantifier :: (Query.RegexSequence -> Query.RegexQuantifier -> Query.RegexSequence)
regexSequenceWithQuantifier original newVal = Query.RegexSequence {
  Query.regexSequencePath = (Query.regexSequencePath original),
  Query.regexSequenceQuantifier = newVal}

stepEdge :: (Query.Edge -> Query.Step)
stepEdge x = (Query.StepEdge x)

stepProject :: (Core.Projection -> Query.Step)
stepProject x = (Query.StepProject x)

stepCompare :: (Query.ComparisonConstraint -> Query.Step)
stepCompare x = (Query.StepCompare x)

triplePattern :: (Query.Node -> Query.Path -> Query.Node -> Query.TriplePattern)
triplePattern subject predicate object = Query.TriplePattern {
  Query.triplePatternSubject = subject,
  Query.triplePatternPredicate = predicate,
  Query.triplePatternObject = object}

triplePatternSubject :: (Query.TriplePattern -> Query.Node)
triplePatternSubject = Query.triplePatternSubject

triplePatternPredicate :: (Query.TriplePattern -> Query.Path)
triplePatternPredicate = Query.triplePatternPredicate

triplePatternObject :: (Query.TriplePattern -> Query.Node)
triplePatternObject = Query.triplePatternObject

triplePatternWithSubject :: (Query.TriplePattern -> Query.Node -> Query.TriplePattern)
triplePatternWithSubject original newVal = Query.TriplePattern {
  Query.triplePatternSubject = newVal,
  Query.triplePatternPredicate = (Query.triplePatternPredicate original),
  Query.triplePatternObject = (Query.triplePatternObject original)}

triplePatternWithPredicate :: (Query.TriplePattern -> Query.Path -> Query.TriplePattern)
triplePatternWithPredicate original newVal = Query.TriplePattern {
  Query.triplePatternSubject = (Query.triplePatternSubject original),
  Query.triplePatternPredicate = newVal,
  Query.triplePatternObject = (Query.triplePatternObject original)}

triplePatternWithObject :: (Query.TriplePattern -> Query.Node -> Query.TriplePattern)
triplePatternWithObject original newVal = Query.TriplePattern {
  Query.triplePatternSubject = (Query.triplePatternSubject original),
  Query.triplePatternPredicate = (Query.triplePatternPredicate original),
  Query.triplePatternObject = newVal}

variable :: (String -> Query.Variable)
variable x = (Query.Variable x)

unVariable :: (Query.Variable -> String)
unVariable = Query.unVariable
