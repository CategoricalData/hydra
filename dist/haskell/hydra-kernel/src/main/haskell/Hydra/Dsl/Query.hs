-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.query

module Hydra.Dsl.Query where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the equal variant of hydra.query.ComparisonConstraint
comparisonConstraintEqual :: Phantoms.TTerm Query.ComparisonConstraint
comparisonConstraintEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterThan variant of hydra.query.ComparisonConstraint
comparisonConstraintGreaterThan :: Phantoms.TTerm Query.ComparisonConstraint
comparisonConstraintGreaterThan =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterThanOrEqual variant of hydra.query.ComparisonConstraint
comparisonConstraintGreaterThanOrEqual :: Phantoms.TTerm Query.ComparisonConstraint
comparisonConstraintGreaterThanOrEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessThan variant of hydra.query.ComparisonConstraint
comparisonConstraintLessThan :: Phantoms.TTerm Query.ComparisonConstraint
comparisonConstraintLessThan =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessThanOrEqual variant of hydra.query.ComparisonConstraint
comparisonConstraintLessThanOrEqual :: Phantoms.TTerm Query.ComparisonConstraint
comparisonConstraintLessThanOrEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the notEqual variant of hydra.query.ComparisonConstraint
comparisonConstraintNotEqual :: Phantoms.TTerm Query.ComparisonConstraint
comparisonConstraintNotEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.query.Edge
edge :: Phantoms.TTerm Core.Name -> Phantoms.TTerm (Maybe Core.Name) -> Phantoms.TTerm (Maybe Core.Name) -> Phantoms.TTerm Query.Edge
edge type_ out in_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
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
          Core.fieldTerm = (Phantoms.unTTerm in_)}]}))
-- | DSL accessor for the in field of hydra.query.Edge
edgeIn :: Phantoms.TTerm Query.Edge -> Phantoms.TTerm (Maybe Core.Name)
edgeIn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the out field of hydra.query.Edge
edgeOut :: Phantoms.TTerm Query.Edge -> Phantoms.TTerm (Maybe Core.Name)
edgeOut x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
        Core.projectionFieldName = (Core.Name "out")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.query.Edge
edgeType :: Phantoms.TTerm Query.Edge -> Phantoms.TTerm Core.Name
edgeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the in field of hydra.query.Edge
edgeWithIn :: Phantoms.TTerm Query.Edge -> Phantoms.TTerm (Maybe Core.Name) -> Phantoms.TTerm Query.Edge
edgeWithIn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the out field of hydra.query.Edge
edgeWithOut :: Phantoms.TTerm Query.Edge -> Phantoms.TTerm (Maybe Core.Name) -> Phantoms.TTerm Query.Edge
edgeWithOut original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.query.Edge
edgeWithType :: Phantoms.TTerm Query.Edge -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Query.Edge
edgeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.query.GraphPattern
graphPattern :: Phantoms.TTerm Core.Name -> Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.GraphPattern
graphPattern graph patterns =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.GraphPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm graph)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm patterns)}]}))
-- | DSL accessor for the graph field of hydra.query.GraphPattern
graphPatternGraph :: Phantoms.TTerm Query.GraphPattern -> Phantoms.TTerm Core.Name
graphPatternGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the patterns field of hydra.query.GraphPattern
graphPatternPatterns :: Phantoms.TTerm Query.GraphPattern -> Phantoms.TTerm [Query.Pattern]
graphPatternPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the graph field of hydra.query.GraphPattern
graphPatternWithGraph :: Phantoms.TTerm Query.GraphPattern -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Query.GraphPattern
graphPatternWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.GraphPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
              Core.projectionFieldName = (Core.Name "patterns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the patterns field of hydra.query.GraphPattern
graphPatternWithPatterns :: Phantoms.TTerm Query.GraphPattern -> Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.GraphPattern
graphPatternWithPatterns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.GraphPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the term variant of hydra.query.Node
nodeTerm :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Query.Node
nodeTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variable variant of hydra.query.Node
nodeVariable :: Phantoms.TTerm Query.Variable -> Phantoms.TTerm Query.Node
nodeVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.query.Node
nodeWildcard :: Phantoms.TTerm Query.Node
nodeWildcard =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.query.PathEquation
pathEquation :: Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.PathEquation
pathEquation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PathEquation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))
-- | DSL accessor for the left field of hydra.query.PathEquation
pathEquationLeft :: Phantoms.TTerm Query.PathEquation -> Phantoms.TTerm Query.Path
pathEquationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the right field of hydra.query.PathEquation
pathEquationRight :: Phantoms.TTerm Query.PathEquation -> Phantoms.TTerm Query.Path
pathEquationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the left field of hydra.query.PathEquation
pathEquationWithLeft :: Phantoms.TTerm Query.PathEquation -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.PathEquation
pathEquationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PathEquation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the right field of hydra.query.PathEquation
pathEquationWithRight :: Phantoms.TTerm Query.PathEquation -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.PathEquation
pathEquationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PathEquation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the inverse variant of hydra.query.Path
pathInverse :: Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.Path
pathInverse x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Path"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inverse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the regex variant of hydra.query.Path
pathRegex :: Phantoms.TTerm Query.RegexSequence -> Phantoms.TTerm Query.Path
pathRegex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Path"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the step variant of hydra.query.Path
pathStep :: Phantoms.TTerm Query.Step -> Phantoms.TTerm Query.Path
pathStep x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Path"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "step"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the conjunction variant of hydra.query.Pattern
patternConjunction :: Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.Pattern
patternConjunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conjunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the disjunction variant of hydra.query.Pattern
patternDisjunction :: Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.Pattern
patternDisjunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the graph variant of hydra.query.Pattern
patternGraph :: Phantoms.TTerm Query.GraphPattern -> Phantoms.TTerm Query.Pattern
patternGraph x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "graph"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.query.PatternImplication
patternImplication :: Phantoms.TTerm Query.Pattern -> Phantoms.TTerm Query.Pattern -> Phantoms.TTerm Query.PatternImplication
patternImplication antecedent consequent =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PatternImplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "antecedent"),
          Core.fieldTerm = (Phantoms.unTTerm antecedent)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Phantoms.unTTerm consequent)}]}))
-- | DSL accessor for the antecedent field of hydra.query.PatternImplication
patternImplicationAntecedent :: Phantoms.TTerm Query.PatternImplication -> Phantoms.TTerm Query.Pattern
patternImplicationAntecedent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
        Core.projectionFieldName = (Core.Name "antecedent")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the consequent field of hydra.query.PatternImplication
patternImplicationConsequent :: Phantoms.TTerm Query.PatternImplication -> Phantoms.TTerm Query.Pattern
patternImplicationConsequent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
        Core.projectionFieldName = (Core.Name "consequent")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the antecedent field of hydra.query.PatternImplication
patternImplicationWithAntecedent :: Phantoms.TTerm Query.PatternImplication -> Phantoms.TTerm Query.Pattern -> Phantoms.TTerm Query.PatternImplication
patternImplicationWithAntecedent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PatternImplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "antecedent"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
              Core.projectionFieldName = (Core.Name "consequent")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the consequent field of hydra.query.PatternImplication
patternImplicationWithConsequent :: Phantoms.TTerm Query.PatternImplication -> Phantoms.TTerm Query.Pattern -> Phantoms.TTerm Query.PatternImplication
patternImplicationWithConsequent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PatternImplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "antecedent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
              Core.projectionFieldName = (Core.Name "antecedent")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the negation variant of hydra.query.Pattern
patternNegation :: Phantoms.TTerm Query.Pattern -> Phantoms.TTerm Query.Pattern
patternNegation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the triple variant of hydra.query.Pattern
patternTriple :: Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Pattern
patternTriple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "triple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.query.Query
query :: Phantoms.TTerm [Query.Variable] -> Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.Query
query variables patterns =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Query"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm variables)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm patterns)}]}))
-- | DSL accessor for the patterns field of hydra.query.Query
queryPatterns :: Phantoms.TTerm Query.Query -> Phantoms.TTerm [Query.Pattern]
queryPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Query"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the variables field of hydra.query.Query
queryVariables :: Phantoms.TTerm Query.Query -> Phantoms.TTerm [Query.Variable]
queryVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Query"),
        Core.projectionFieldName = (Core.Name "variables")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the patterns field of hydra.query.Query
queryWithPatterns :: Phantoms.TTerm Query.Query -> Phantoms.TTerm [Query.Pattern] -> Phantoms.TTerm Query.Query
queryWithPatterns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Query"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Query"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the variables field of hydra.query.Query
queryWithVariables :: Phantoms.TTerm Query.Query -> Phantoms.TTerm [Query.Variable] -> Phantoms.TTerm Query.Query
queryWithVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Query"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Query"),
              Core.projectionFieldName = (Core.Name "patterns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.query.Range
range :: Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm Query.Range
range min max =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Range"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm min)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm max)}]}))
-- | DSL accessor for the max field of hydra.query.Range
rangeMax :: Phantoms.TTerm Query.Range -> Phantoms.TTerm Int
rangeMax x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Range"),
        Core.projectionFieldName = (Core.Name "max")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the min field of hydra.query.Range
rangeMin :: Phantoms.TTerm Query.Range -> Phantoms.TTerm Int
rangeMin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Range"),
        Core.projectionFieldName = (Core.Name "min")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the max field of hydra.query.Range
rangeWithMax :: Phantoms.TTerm Query.Range -> Phantoms.TTerm Int -> Phantoms.TTerm Query.Range
rangeWithMax original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Range"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Range"),
              Core.projectionFieldName = (Core.Name "min")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the min field of hydra.query.Range
rangeWithMin :: Phantoms.TTerm Query.Range -> Phantoms.TTerm Int -> Phantoms.TTerm Query.Range
rangeWithMin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Range"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Range"),
              Core.projectionFieldName = (Core.Name "max")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the atLeast variant of hydra.query.RegexQuantifier
regexQuantifierAtLeast :: Phantoms.TTerm Int -> Phantoms.TTerm Query.RegexQuantifier
regexQuantifierAtLeast x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atLeast"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the exactly variant of hydra.query.RegexQuantifier
regexQuantifierExactly :: Phantoms.TTerm Int -> Phantoms.TTerm Query.RegexQuantifier
regexQuantifierExactly x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exactly"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the one variant of hydra.query.RegexQuantifier
regexQuantifierOne :: Phantoms.TTerm Query.RegexQuantifier
regexQuantifierOne =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "one"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the oneOrMore variant of hydra.query.RegexQuantifier
regexQuantifierOneOrMore :: Phantoms.TTerm Query.RegexQuantifier
regexQuantifierOneOrMore =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "oneOrMore"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the range variant of hydra.query.RegexQuantifier
regexQuantifierRange :: Phantoms.TTerm Query.Range -> Phantoms.TTerm Query.RegexQuantifier
regexQuantifierRange x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "range"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the zeroOrMore variant of hydra.query.RegexQuantifier
regexQuantifierZeroOrMore :: Phantoms.TTerm Query.RegexQuantifier
regexQuantifierZeroOrMore =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "zeroOrMore"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the zeroOrOne variant of hydra.query.RegexQuantifier
regexQuantifierZeroOrOne :: Phantoms.TTerm Query.RegexQuantifier
regexQuantifierZeroOrOne =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "zeroOrOne"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.query.RegexSequence
regexSequence :: Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.RegexQuantifier -> Phantoms.TTerm Query.RegexSequence
regexSequence path quantifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.RegexSequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "quantifier"),
          Core.fieldTerm = (Phantoms.unTTerm quantifier)}]}))
-- | DSL accessor for the path field of hydra.query.RegexSequence
regexSequencePath :: Phantoms.TTerm Query.RegexSequence -> Phantoms.TTerm Query.Path
regexSequencePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the quantifier field of hydra.query.RegexSequence
regexSequenceQuantifier :: Phantoms.TTerm Query.RegexSequence -> Phantoms.TTerm Query.RegexQuantifier
regexSequenceQuantifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
        Core.projectionFieldName = (Core.Name "quantifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the path field of hydra.query.RegexSequence
regexSequenceWithPath :: Phantoms.TTerm Query.RegexSequence -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.RegexSequence
regexSequenceWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.RegexSequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "quantifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
              Core.projectionFieldName = (Core.Name "quantifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the quantifier field of hydra.query.RegexSequence
regexSequenceWithQuantifier :: Phantoms.TTerm Query.RegexSequence -> Phantoms.TTerm Query.RegexQuantifier -> Phantoms.TTerm Query.RegexSequence
regexSequenceWithQuantifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.RegexSequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "quantifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the compare variant of hydra.query.Step
stepCompare :: Phantoms.TTerm Query.ComparisonConstraint -> Phantoms.TTerm Query.Step
stepCompare x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Step"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compare"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the edge variant of hydra.query.Step
stepEdge :: Phantoms.TTerm Query.Edge -> Phantoms.TTerm Query.Step
stepEdge x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Step"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the project variant of hydra.query.Step
stepProject :: Phantoms.TTerm Core.Projection -> Phantoms.TTerm Query.Step
stepProject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Step"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.query.TriplePattern
triplePattern :: Phantoms.TTerm Query.Node -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.Node -> Phantoms.TTerm Query.TriplePattern
triplePattern subject predicate object =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
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
          Core.fieldTerm = (Phantoms.unTTerm object)}]}))
-- | DSL accessor for the object field of hydra.query.TriplePattern
triplePatternObject :: Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Node
triplePatternObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
        Core.projectionFieldName = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the predicate field of hydra.query.TriplePattern
triplePatternPredicate :: Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Path
triplePatternPredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
        Core.projectionFieldName = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the subject field of hydra.query.TriplePattern
triplePatternSubject :: Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Node
triplePatternSubject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
        Core.projectionFieldName = (Core.Name "subject")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the object field of hydra.query.TriplePattern
triplePatternWithObject :: Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Node -> Phantoms.TTerm Query.TriplePattern
triplePatternWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the predicate field of hydra.query.TriplePattern
triplePatternWithPredicate :: Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Path -> Phantoms.TTerm Query.TriplePattern
triplePatternWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the subject field of hydra.query.TriplePattern
triplePatternWithSubject :: Phantoms.TTerm Query.TriplePattern -> Phantoms.TTerm Query.Node -> Phantoms.TTerm Query.TriplePattern
triplePatternWithSubject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the body of hydra.query.Variable
unVariable :: Phantoms.TTerm Query.Variable -> Phantoms.TTerm String
unVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.query.Variable")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.query.Variable wrapper
variable :: Phantoms.TTerm String -> Phantoms.TTerm Query.Variable
variable x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.query.Variable"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
