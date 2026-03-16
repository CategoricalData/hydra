-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.query

module Hydra.Dsl.Query where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

comparisonConstraintEqual :: (Phantoms.TTerm Query.ComparisonConstraint)
comparisonConstraintEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "equal"),
    Core.fieldTerm = Core.TermUnit}})))

comparisonConstraintNotEqual :: (Phantoms.TTerm Query.ComparisonConstraint)
comparisonConstraintNotEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "notEqual"),
    Core.fieldTerm = Core.TermUnit}})))

comparisonConstraintLessThan :: (Phantoms.TTerm Query.ComparisonConstraint)
comparisonConstraintLessThan = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "lessThan"),
    Core.fieldTerm = Core.TermUnit}})))

comparisonConstraintGreaterThan :: (Phantoms.TTerm Query.ComparisonConstraint)
comparisonConstraintGreaterThan = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "greaterThan"),
    Core.fieldTerm = Core.TermUnit}})))

comparisonConstraintLessThanOrEqual :: (Phantoms.TTerm Query.ComparisonConstraint)
comparisonConstraintLessThanOrEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "lessThanOrEqual"),
    Core.fieldTerm = Core.TermUnit}})))

comparisonConstraintGreaterThanOrEqual :: (Phantoms.TTerm Query.ComparisonConstraint)
comparisonConstraintGreaterThanOrEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "greaterThanOrEqual"),
    Core.fieldTerm = Core.TermUnit}})))

edge :: (Phantoms.TTerm Core.Name -> Phantoms.TTerm (Maybe Core.Name) -> Phantoms.TTerm (Maybe Core.Name) -> Phantoms.TTerm Query.Edge)
edge type_ out in_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.Edge"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "out"),
      Core.fieldTerm = (Phantoms.unTTerm out)},
    Core.Field {
      Core.fieldName = (Core.Name "in"),
      Core.fieldTerm = (Phantoms.unTTerm in_)}]})))

edgeType :: (Phantoms.TTerm Query.Edge -> Phantoms.TTerm Core.Name)
edgeType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

edgeOut :: (Phantoms.TTerm Query.Edge -> Phantoms.TTerm (Maybe Core.Name))
edgeOut x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
    Core.projectionField = (Core.Name "out")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

edgeIn :: (Phantoms.TTerm Query.Edge -> Phantoms.TTerm (Maybe Core.Name))
edgeIn x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
    Core.projectionField = (Core.Name "in")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

edgeWithType :: (Phantoms.TTerm Query.Edge -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Query.Edge)
edgeWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.Edge"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "out"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
          Core.projectionField = (Core.Name "out")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "in"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
          Core.projectionField = (Core.Name "in")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

edgeWithOut :: (Phantoms.TTerm Query.Edge -> Phantoms.TTerm (Maybe Core.Name) -> Phantoms.TTerm Query.Edge)
edgeWithOut original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.Edge"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "out"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "in"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
          Core.projectionField = (Core.Name "in")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

edgeWithIn :: (Phantoms.TTerm Query.Edge -> Phantoms.TTerm (Maybe Core.Name) -> Phantoms.TTerm Query.Edge)
edgeWithIn original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.Edge"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "out"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
          Core.projectionField = (Core.Name "out")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "in"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

graphPattern :: (Phantoms.TTerm Core.Name -> Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.GraphPattern)
graphPattern graph patterns = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.GraphPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "graph"),
      Core.fieldTerm = (Phantoms.unTTerm graph)},
    Core.Field {
      Core.fieldName = (Core.Name "patterns"),
      Core.fieldTerm = (Phantoms.unTTerm patterns)}]})))

graphPatternGraph :: (Phantoms.TTerm Query.GraphPattern -> Phantoms.TTerm Core.Name)
graphPatternGraph x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
    Core.projectionField = (Core.Name "graph")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphPatternPatterns :: (Phantoms.TTerm Query.GraphPattern -> Phantoms.TTerm [Query.Pattern])
graphPatternPatterns x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
    Core.projectionField = (Core.Name "patterns")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphPatternWithGraph :: (Phantoms.TTerm Query.GraphPattern -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Query.GraphPattern)
graphPatternWithGraph original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.GraphPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "graph"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "patterns"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
          Core.projectionField = (Core.Name "patterns")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

graphPatternWithPatterns :: (Phantoms.TTerm Query.GraphPattern -> Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.GraphPattern)
graphPatternWithPatterns original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.GraphPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "graph"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
          Core.projectionField = (Core.Name "graph")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "patterns"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

nodeTerm :: (Phantoms.TTerm Core.Term -> Phantoms.TTerm Query.Node)
nodeTerm x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Node"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "term"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

nodeVariable :: (Phantoms.TTerm Query.Variable -> Phantoms.TTerm Query.Node)
nodeVariable x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Node"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "variable"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

nodeWildcard :: (Phantoms.TTerm Query.Node)
nodeWildcard = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Node"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "wildcard"),
    Core.fieldTerm = Core.TermUnit}})))

pathStep :: (Phantoms.TTerm Query.Step -> Phantoms.TTerm Query.Path)
pathStep x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Path"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "step"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

pathRegex :: (Phantoms.TTerm Query.RegexSequence -> Phantoms.TTerm Query.Path)
pathRegex x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Path"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "regex"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

pathInverse :: (Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.Path)
pathInverse x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Path"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "inverse"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

pathEquation :: (Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.PathEquation)
pathEquation left right = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.PathEquation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Phantoms.unTTerm left)},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Phantoms.unTTerm right)}]})))

pathEquationLeft :: (Phantoms.TTerm Query.PathEquation -> Phantoms.TTerm Query.Path)
pathEquationLeft x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
    Core.projectionField = (Core.Name "left")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pathEquationRight :: (Phantoms.TTerm Query.PathEquation -> Phantoms.TTerm Query.Path)
pathEquationRight x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
    Core.projectionField = (Core.Name "right")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pathEquationWithLeft :: (Phantoms.TTerm Query.PathEquation -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.PathEquation)
pathEquationWithLeft original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.PathEquation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
          Core.projectionField = (Core.Name "right")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pathEquationWithRight :: (Phantoms.TTerm Query.PathEquation -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.PathEquation)
pathEquationWithRight original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.PathEquation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
          Core.projectionField = (Core.Name "left")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

patternTriple :: (Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Pattern)
patternTriple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "triple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patternNegation :: (Phantoms.TTerm Query.Pattern -> Phantoms.TTerm Query.Pattern)
patternNegation x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "negation"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patternConjunction :: (Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.Pattern)
patternConjunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "conjunction"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patternDisjunction :: (Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.Pattern)
patternDisjunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "disjunction"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patternGraph :: (Phantoms.TTerm Query.GraphPattern -> Phantoms.TTerm Query.Pattern)
patternGraph x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "graph"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patternImplication :: (Phantoms.TTerm Query.Pattern -> Phantoms.TTerm Query.Pattern -> Phantoms.TTerm Query.PatternImplication)
patternImplication antecedent consequent = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.PatternImplication"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "antecedent"),
      Core.fieldTerm = (Phantoms.unTTerm antecedent)},
    Core.Field {
      Core.fieldName = (Core.Name "consequent"),
      Core.fieldTerm = (Phantoms.unTTerm consequent)}]})))

patternImplicationAntecedent :: (Phantoms.TTerm Query.PatternImplication -> Phantoms.TTerm Query.Pattern)
patternImplicationAntecedent x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
    Core.projectionField = (Core.Name "antecedent")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

patternImplicationConsequent :: (Phantoms.TTerm Query.PatternImplication -> Phantoms.TTerm Query.Pattern)
patternImplicationConsequent x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
    Core.projectionField = (Core.Name "consequent")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

patternImplicationWithAntecedent :: (Phantoms.TTerm Query.PatternImplication -> Phantoms.TTerm Query.Pattern -> Phantoms.TTerm Query.PatternImplication)
patternImplicationWithAntecedent original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.PatternImplication"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "antecedent"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "consequent"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
          Core.projectionField = (Core.Name "consequent")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

patternImplicationWithConsequent :: (Phantoms.TTerm Query.PatternImplication -> Phantoms.TTerm Query.Pattern -> Phantoms.TTerm Query.PatternImplication)
patternImplicationWithConsequent original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.PatternImplication"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "antecedent"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
          Core.projectionField = (Core.Name "antecedent")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "consequent"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

query :: (Phantoms.TTerm [Query.Variable] -> Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.Query)
query variables patterns = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.Query"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "variables"),
      Core.fieldTerm = (Phantoms.unTTerm variables)},
    Core.Field {
      Core.fieldName = (Core.Name "patterns"),
      Core.fieldTerm = (Phantoms.unTTerm patterns)}]})))

queryVariables :: (Phantoms.TTerm Query.Query -> Phantoms.TTerm [Query.Variable])
queryVariables x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.Query"),
    Core.projectionField = (Core.Name "variables")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

queryPatterns :: (Phantoms.TTerm Query.Query -> Phantoms.TTerm [Query.Pattern])
queryPatterns x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.Query"),
    Core.projectionField = (Core.Name "patterns")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

queryWithVariables :: (Phantoms.TTerm Query.Query -> Phantoms.TTerm [Query.Variable] -> Phantoms.TTerm Query.Query)
queryWithVariables original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.Query"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "variables"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "patterns"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.Query"),
          Core.projectionField = (Core.Name "patterns")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

queryWithPatterns :: (Phantoms.TTerm Query.Query -> Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.Query)
queryWithPatterns original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.Query"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "variables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.Query"),
          Core.projectionField = (Core.Name "variables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "patterns"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

range :: (Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm Query.Range)
range min max = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.Range"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "min"),
      Core.fieldTerm = (Phantoms.unTTerm min)},
    Core.Field {
      Core.fieldName = (Core.Name "max"),
      Core.fieldTerm = (Phantoms.unTTerm max)}]})))

rangeMin :: (Phantoms.TTerm Query.Range -> Phantoms.TTerm Int)
rangeMin x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.Range"),
    Core.projectionField = (Core.Name "min")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

rangeMax :: (Phantoms.TTerm Query.Range -> Phantoms.TTerm Int)
rangeMax x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.Range"),
    Core.projectionField = (Core.Name "max")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

rangeWithMin :: (Phantoms.TTerm Query.Range -> Phantoms.TTerm Int -> Phantoms.TTerm Query.Range)
rangeWithMin original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.Range"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "min"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "max"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.Range"),
          Core.projectionField = (Core.Name "max")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

rangeWithMax :: (Phantoms.TTerm Query.Range -> Phantoms.TTerm Int -> Phantoms.TTerm Query.Range)
rangeWithMax original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.Range"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "min"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.Range"),
          Core.projectionField = (Core.Name "min")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "max"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

regexQuantifierOne :: (Phantoms.TTerm Query.RegexQuantifier)
regexQuantifierOne = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "one"),
    Core.fieldTerm = Core.TermUnit}})))

regexQuantifierZeroOrOne :: (Phantoms.TTerm Query.RegexQuantifier)
regexQuantifierZeroOrOne = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "zeroOrOne"),
    Core.fieldTerm = Core.TermUnit}})))

regexQuantifierZeroOrMore :: (Phantoms.TTerm Query.RegexQuantifier)
regexQuantifierZeroOrMore = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "zeroOrMore"),
    Core.fieldTerm = Core.TermUnit}})))

regexQuantifierOneOrMore :: (Phantoms.TTerm Query.RegexQuantifier)
regexQuantifierOneOrMore = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "oneOrMore"),
    Core.fieldTerm = Core.TermUnit}})))

regexQuantifierExactly :: (Phantoms.TTerm Int -> Phantoms.TTerm Query.RegexQuantifier)
regexQuantifierExactly x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "exactly"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

regexQuantifierAtLeast :: (Phantoms.TTerm Int -> Phantoms.TTerm Query.RegexQuantifier)
regexQuantifierAtLeast x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "atLeast"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

regexQuantifierRange :: (Phantoms.TTerm Query.Range -> Phantoms.TTerm Query.RegexQuantifier)
regexQuantifierRange x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "range"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

regexSequence :: (Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.RegexQuantifier -> Phantoms.TTerm Query.RegexSequence)
regexSequence path quantifier = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.RegexSequence"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "path"),
      Core.fieldTerm = (Phantoms.unTTerm path)},
    Core.Field {
      Core.fieldName = (Core.Name "quantifier"),
      Core.fieldTerm = (Phantoms.unTTerm quantifier)}]})))

regexSequencePath :: (Phantoms.TTerm Query.RegexSequence -> Phantoms.TTerm Query.Path)
regexSequencePath x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
    Core.projectionField = (Core.Name "path")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

regexSequenceQuantifier :: (Phantoms.TTerm Query.RegexSequence -> Phantoms.TTerm Query.RegexQuantifier)
regexSequenceQuantifier x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
    Core.projectionField = (Core.Name "quantifier")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

regexSequenceWithPath :: (Phantoms.TTerm Query.RegexSequence -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.RegexSequence)
regexSequenceWithPath original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.RegexSequence"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "path"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "quantifier"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
          Core.projectionField = (Core.Name "quantifier")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

regexSequenceWithQuantifier :: (Phantoms.TTerm Query.RegexSequence -> Phantoms.TTerm Query.RegexQuantifier -> Phantoms.TTerm Query.RegexSequence)
regexSequenceWithQuantifier original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.RegexSequence"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "path"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
          Core.projectionField = (Core.Name "path")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "quantifier"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

stepEdge :: (Phantoms.TTerm Query.Edge -> Phantoms.TTerm Query.Step)
stepEdge x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Step"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "edge"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

stepProject :: (Phantoms.TTerm Core.Projection -> Phantoms.TTerm Query.Step)
stepProject x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Step"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "project"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

stepCompare :: (Phantoms.TTerm Query.ComparisonConstraint -> Phantoms.TTerm Query.Step)
stepCompare x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.query.Step"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "compare"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

triplePattern :: (Phantoms.TTerm Query.Node -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.Node -> Phantoms.TTerm Query.TriplePattern)
triplePattern subject predicate object = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "subject"),
      Core.fieldTerm = (Phantoms.unTTerm subject)},
    Core.Field {
      Core.fieldName = (Core.Name "predicate"),
      Core.fieldTerm = (Phantoms.unTTerm predicate)},
    Core.Field {
      Core.fieldName = (Core.Name "object"),
      Core.fieldTerm = (Phantoms.unTTerm object)}]})))

triplePatternSubject :: (Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Node)
triplePatternSubject x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
    Core.projectionField = (Core.Name "subject")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

triplePatternPredicate :: (Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Path)
triplePatternPredicate x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
    Core.projectionField = (Core.Name "predicate")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

triplePatternObject :: (Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Node)
triplePatternObject x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
    Core.projectionField = (Core.Name "object")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

triplePatternWithSubject :: (Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Node -> Phantoms.TTerm Query.TriplePattern)
triplePatternWithSubject original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "subject"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "predicate"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
          Core.projectionField = (Core.Name "predicate")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "object"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
          Core.projectionField = (Core.Name "object")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

triplePatternWithPredicate :: (Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.TriplePattern)
triplePatternWithPredicate original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "subject"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
          Core.projectionField = (Core.Name "subject")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "predicate"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "object"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
          Core.projectionField = (Core.Name "object")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

triplePatternWithObject :: (Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Node -> Phantoms.TTerm Query.TriplePattern)
triplePatternWithObject original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "subject"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
          Core.projectionField = (Core.Name "subject")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "predicate"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
          Core.projectionField = (Core.Name "predicate")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "object"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

variable :: (Phantoms.TTerm String -> Phantoms.TTerm Query.Variable)
variable x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.query.Variable"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unVariable :: (Phantoms.TTerm Query.Variable -> Phantoms.TTerm String)
unVariable x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.query.Variable")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))
