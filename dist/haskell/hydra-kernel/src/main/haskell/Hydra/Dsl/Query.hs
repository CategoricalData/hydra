-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.query

module Hydra.Dsl.Query where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Query as Query
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the equal variant of hydra.query.ComparisonConstraint
comparisonConstraintEqual :: Typed.TypedTerm Query.ComparisonConstraint
comparisonConstraintEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterThan variant of hydra.query.ComparisonConstraint
comparisonConstraintGreaterThan :: Typed.TypedTerm Query.ComparisonConstraint
comparisonConstraintGreaterThan =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterThanOrEqual variant of hydra.query.ComparisonConstraint
comparisonConstraintGreaterThanOrEqual :: Typed.TypedTerm Query.ComparisonConstraint
comparisonConstraintGreaterThanOrEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessThan variant of hydra.query.ComparisonConstraint
comparisonConstraintLessThan :: Typed.TypedTerm Query.ComparisonConstraint
comparisonConstraintLessThan =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessThanOrEqual variant of hydra.query.ComparisonConstraint
comparisonConstraintLessThanOrEqual :: Typed.TypedTerm Query.ComparisonConstraint
comparisonConstraintLessThanOrEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the notEqual variant of hydra.query.ComparisonConstraint
comparisonConstraintNotEqual :: Typed.TypedTerm Query.ComparisonConstraint
comparisonConstraintNotEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.query.Edge
edge :: Typed.TypedTerm Core.Name -> Typed.TypedTerm (Maybe Core.Name) -> Typed.TypedTerm (Maybe Core.Name) -> Typed.TypedTerm Query.Edge
edge type_ out in_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Typed.unTypedTerm out)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm in_)}]}))
-- | DSL accessor for the in field of hydra.query.Edge
edgeIn :: Typed.TypedTerm Query.Edge -> Typed.TypedTerm (Maybe Core.Name)
edgeIn x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the out field of hydra.query.Edge
edgeOut :: Typed.TypedTerm Query.Edge -> Typed.TypedTerm (Maybe Core.Name)
edgeOut x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
        Core.projectionFieldName = (Core.Name "out")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.query.Edge
edgeType :: Typed.TypedTerm Query.Edge -> Typed.TypedTerm Core.Name
edgeType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the in field of hydra.query.Edge
edgeWithIn :: Typed.TypedTerm Query.Edge -> Typed.TypedTerm (Maybe Core.Name) -> Typed.TypedTerm Query.Edge
edgeWithIn original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the out field of hydra.query.Edge
edgeWithOut :: Typed.TypedTerm Query.Edge -> Typed.TypedTerm (Maybe Core.Name) -> Typed.TypedTerm Query.Edge
edgeWithOut original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.query.Edge
edgeWithType :: Typed.TypedTerm Query.Edge -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Query.Edge
edgeWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.query.GraphPattern
graphPattern :: Typed.TypedTerm Core.Name -> Typed.TypedTerm [Query.Pattern] -> Typed.TypedTerm Query.GraphPattern
graphPattern graph patterns =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.GraphPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm graph)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm patterns)}]}))
-- | DSL accessor for the graph field of hydra.query.GraphPattern
graphPatternGraph :: Typed.TypedTerm Query.GraphPattern -> Typed.TypedTerm Core.Name
graphPatternGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the patterns field of hydra.query.GraphPattern
graphPatternPatterns :: Typed.TypedTerm Query.GraphPattern -> Typed.TypedTerm [Query.Pattern]
graphPatternPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the graph field of hydra.query.GraphPattern
graphPatternWithGraph :: Typed.TypedTerm Query.GraphPattern -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Query.GraphPattern
graphPatternWithGraph original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.GraphPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
              Core.projectionFieldName = (Core.Name "patterns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the patterns field of hydra.query.GraphPattern
graphPatternWithPatterns :: Typed.TypedTerm Query.GraphPattern -> Typed.TypedTerm [Query.Pattern] -> Typed.TypedTerm Query.GraphPattern
graphPatternWithPatterns original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.GraphPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the term variant of hydra.query.Node
nodeTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Query.Node
nodeTerm x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.query.Node
nodeVariable :: Typed.TypedTerm Query.Variable -> Typed.TypedTerm Query.Node
nodeVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.query.Node
nodeWildcard :: Typed.TypedTerm Query.Node
nodeWildcard =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.query.PathEquation
pathEquation :: Typed.TypedTerm Query.Path -> Typed.TypedTerm Query.Path -> Typed.TypedTerm Query.PathEquation
pathEquation left right =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PathEquation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm right)}]}))
-- | DSL accessor for the left field of hydra.query.PathEquation
pathEquationLeft :: Typed.TypedTerm Query.PathEquation -> Typed.TypedTerm Query.Path
pathEquationLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the right field of hydra.query.PathEquation
pathEquationRight :: Typed.TypedTerm Query.PathEquation -> Typed.TypedTerm Query.Path
pathEquationRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the left field of hydra.query.PathEquation
pathEquationWithLeft :: Typed.TypedTerm Query.PathEquation -> Typed.TypedTerm Query.Path -> Typed.TypedTerm Query.PathEquation
pathEquationWithLeft original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PathEquation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the right field of hydra.query.PathEquation
pathEquationWithRight :: Typed.TypedTerm Query.PathEquation -> Typed.TypedTerm Query.Path -> Typed.TypedTerm Query.PathEquation
pathEquationWithRight original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PathEquation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.PathEquation"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the inverse variant of hydra.query.Path
pathInverse :: Typed.TypedTerm Query.Path -> Typed.TypedTerm Query.Path
pathInverse x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Path"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inverse"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the regex variant of hydra.query.Path
pathRegex :: Typed.TypedTerm Query.RegexSequence -> Typed.TypedTerm Query.Path
pathRegex x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Path"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the step variant of hydra.query.Path
pathStep :: Typed.TypedTerm Query.Step -> Typed.TypedTerm Query.Path
pathStep x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Path"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "step"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the conjunction variant of hydra.query.Pattern
patternConjunction :: Typed.TypedTerm [Query.Pattern] -> Typed.TypedTerm Query.Pattern
patternConjunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conjunction"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the disjunction variant of hydra.query.Pattern
patternDisjunction :: Typed.TypedTerm [Query.Pattern] -> Typed.TypedTerm Query.Pattern
patternDisjunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjunction"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the graph variant of hydra.query.Pattern
patternGraph :: Typed.TypedTerm Query.GraphPattern -> Typed.TypedTerm Query.Pattern
patternGraph x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "graph"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.query.PatternImplication
patternImplication :: Typed.TypedTerm Query.Pattern -> Typed.TypedTerm Query.Pattern -> Typed.TypedTerm Query.PatternImplication
patternImplication antecedent consequent =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PatternImplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "antecedent"),
          Core.fieldTerm = (Typed.unTypedTerm antecedent)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Typed.unTypedTerm consequent)}]}))
-- | DSL accessor for the antecedent field of hydra.query.PatternImplication
patternImplicationAntecedent :: Typed.TypedTerm Query.PatternImplication -> Typed.TypedTerm Query.Pattern
patternImplicationAntecedent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
        Core.projectionFieldName = (Core.Name "antecedent")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the consequent field of hydra.query.PatternImplication
patternImplicationConsequent :: Typed.TypedTerm Query.PatternImplication -> Typed.TypedTerm Query.Pattern
patternImplicationConsequent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
        Core.projectionFieldName = (Core.Name "consequent")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the antecedent field of hydra.query.PatternImplication
patternImplicationWithAntecedent :: Typed.TypedTerm Query.PatternImplication -> Typed.TypedTerm Query.Pattern -> Typed.TypedTerm Query.PatternImplication
patternImplicationWithAntecedent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PatternImplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "antecedent"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
              Core.projectionFieldName = (Core.Name "consequent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the consequent field of hydra.query.PatternImplication
patternImplicationWithConsequent :: Typed.TypedTerm Query.PatternImplication -> Typed.TypedTerm Query.Pattern -> Typed.TypedTerm Query.PatternImplication
patternImplicationWithConsequent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.PatternImplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "antecedent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.PatternImplication"),
              Core.projectionFieldName = (Core.Name "antecedent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the negation variant of hydra.query.Pattern
patternNegation :: Typed.TypedTerm Query.Pattern -> Typed.TypedTerm Query.Pattern
patternNegation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the triple variant of hydra.query.Pattern
patternTriple :: Typed.TypedTerm Query.TriplePattern -> Typed.TypedTerm Query.Pattern
patternTriple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "triple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.query.Query
query :: Typed.TypedTerm [Query.Variable] -> Typed.TypedTerm [Query.Pattern] -> Typed.TypedTerm Query.Query
query variables patterns =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Query"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Typed.unTypedTerm variables)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm patterns)}]}))
-- | DSL accessor for the patterns field of hydra.query.Query
queryPatterns :: Typed.TypedTerm Query.Query -> Typed.TypedTerm [Query.Pattern]
queryPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Query"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variables field of hydra.query.Query
queryVariables :: Typed.TypedTerm Query.Query -> Typed.TypedTerm [Query.Variable]
queryVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Query"),
        Core.projectionFieldName = (Core.Name "variables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the patterns field of hydra.query.Query
queryWithPatterns :: Typed.TypedTerm Query.Query -> Typed.TypedTerm [Query.Pattern] -> Typed.TypedTerm Query.Query
queryWithPatterns original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Query"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Query"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the variables field of hydra.query.Query
queryWithVariables :: Typed.TypedTerm Query.Query -> Typed.TypedTerm [Query.Variable] -> Typed.TypedTerm Query.Query
queryWithVariables original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Query"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Query"),
              Core.projectionFieldName = (Core.Name "patterns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.query.Range
range :: Typed.TypedTerm Int -> Typed.TypedTerm Int -> Typed.TypedTerm Query.Range
range min max =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Range"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Typed.unTypedTerm min)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Typed.unTypedTerm max)}]}))
-- | DSL accessor for the max field of hydra.query.Range
rangeMax :: Typed.TypedTerm Query.Range -> Typed.TypedTerm Int
rangeMax x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Range"),
        Core.projectionFieldName = (Core.Name "max")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the min field of hydra.query.Range
rangeMin :: Typed.TypedTerm Query.Range -> Typed.TypedTerm Int
rangeMin x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.Range"),
        Core.projectionFieldName = (Core.Name "min")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the max field of hydra.query.Range
rangeWithMax :: Typed.TypedTerm Query.Range -> Typed.TypedTerm Int -> Typed.TypedTerm Query.Range
rangeWithMax original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Range"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Range"),
              Core.projectionFieldName = (Core.Name "min")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the min field of hydra.query.Range
rangeWithMin :: Typed.TypedTerm Query.Range -> Typed.TypedTerm Int -> Typed.TypedTerm Query.Range
rangeWithMin original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.Range"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.Range"),
              Core.projectionFieldName = (Core.Name "max")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the atLeast variant of hydra.query.RegexQuantifier
regexQuantifierAtLeast :: Typed.TypedTerm Int -> Typed.TypedTerm Query.RegexQuantifier
regexQuantifierAtLeast x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atLeast"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the exactly variant of hydra.query.RegexQuantifier
regexQuantifierExactly :: Typed.TypedTerm Int -> Typed.TypedTerm Query.RegexQuantifier
regexQuantifierExactly x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exactly"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the one variant of hydra.query.RegexQuantifier
regexQuantifierOne :: Typed.TypedTerm Query.RegexQuantifier
regexQuantifierOne =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "one"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the oneOrMore variant of hydra.query.RegexQuantifier
regexQuantifierOneOrMore :: Typed.TypedTerm Query.RegexQuantifier
regexQuantifierOneOrMore =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "oneOrMore"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the range variant of hydra.query.RegexQuantifier
regexQuantifierRange :: Typed.TypedTerm Query.Range -> Typed.TypedTerm Query.RegexQuantifier
regexQuantifierRange x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "range"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the zeroOrMore variant of hydra.query.RegexQuantifier
regexQuantifierZeroOrMore :: Typed.TypedTerm Query.RegexQuantifier
regexQuantifierZeroOrMore =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "zeroOrMore"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the zeroOrOne variant of hydra.query.RegexQuantifier
regexQuantifierZeroOrOne :: Typed.TypedTerm Query.RegexQuantifier
regexQuantifierZeroOrOne =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.RegexQuantifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "zeroOrOne"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.query.RegexSequence
regexSequence :: Typed.TypedTerm Query.Path -> Typed.TypedTerm Query.RegexQuantifier -> Typed.TypedTerm Query.RegexSequence
regexSequence path quantifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.RegexSequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "quantifier"),
          Core.fieldTerm = (Typed.unTypedTerm quantifier)}]}))
-- | DSL accessor for the path field of hydra.query.RegexSequence
regexSequencePath :: Typed.TypedTerm Query.RegexSequence -> Typed.TypedTerm Query.Path
regexSequencePath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the quantifier field of hydra.query.RegexSequence
regexSequenceQuantifier :: Typed.TypedTerm Query.RegexSequence -> Typed.TypedTerm Query.RegexQuantifier
regexSequenceQuantifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
        Core.projectionFieldName = (Core.Name "quantifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the path field of hydra.query.RegexSequence
regexSequenceWithPath :: Typed.TypedTerm Query.RegexSequence -> Typed.TypedTerm Query.Path -> Typed.TypedTerm Query.RegexSequence
regexSequenceWithPath original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.RegexSequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "quantifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
              Core.projectionFieldName = (Core.Name "quantifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the quantifier field of hydra.query.RegexSequence
regexSequenceWithQuantifier :: Typed.TypedTerm Query.RegexSequence -> Typed.TypedTerm Query.RegexQuantifier -> Typed.TypedTerm Query.RegexSequence
regexSequenceWithQuantifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.RegexSequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "quantifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the compare variant of hydra.query.Step
stepCompare :: Typed.TypedTerm Query.ComparisonConstraint -> Typed.TypedTerm Query.Step
stepCompare x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Step"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compare"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the edge variant of hydra.query.Step
stepEdge :: Typed.TypedTerm Query.Edge -> Typed.TypedTerm Query.Step
stepEdge x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Step"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the project variant of hydra.query.Step
stepProject :: Typed.TypedTerm Core.Projection -> Typed.TypedTerm Query.Step
stepProject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.query.Step"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.query.TriplePattern
triplePattern :: Typed.TypedTerm Query.Node -> Typed.TypedTerm Query.Path -> Typed.TypedTerm Query.Node -> Typed.TypedTerm Query.TriplePattern
triplePattern subject predicate object =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Typed.unTypedTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Typed.unTypedTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Typed.unTypedTerm object)}]}))
-- | DSL accessor for the object field of hydra.query.TriplePattern
triplePatternObject :: Typed.TypedTerm Query.TriplePattern -> Typed.TypedTerm Query.Node
triplePatternObject x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
        Core.projectionFieldName = (Core.Name "object")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the predicate field of hydra.query.TriplePattern
triplePatternPredicate :: Typed.TypedTerm Query.TriplePattern -> Typed.TypedTerm Query.Path
triplePatternPredicate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
        Core.projectionFieldName = (Core.Name "predicate")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the subject field of hydra.query.TriplePattern
triplePatternSubject :: Typed.TypedTerm Query.TriplePattern -> Typed.TypedTerm Query.Node
triplePatternSubject x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
        Core.projectionFieldName = (Core.Name "subject")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the object field of hydra.query.TriplePattern
triplePatternWithObject :: Typed.TypedTerm Query.TriplePattern -> Typed.TypedTerm Query.Node -> Typed.TypedTerm Query.TriplePattern
triplePatternWithObject original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the predicate field of hydra.query.TriplePattern
triplePatternWithPredicate :: Typed.TypedTerm Query.TriplePattern -> Typed.TypedTerm Query.Path -> Typed.TypedTerm Query.TriplePattern
triplePatternWithPredicate original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the subject field of hydra.query.TriplePattern
triplePatternWithSubject :: Typed.TypedTerm Query.TriplePattern -> Typed.TypedTerm Query.Node -> Typed.TypedTerm Query.TriplePattern
triplePatternWithSubject original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.query.TriplePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the body of hydra.query.Variable
unVariable :: Typed.TypedTerm Query.Variable -> Typed.TypedTerm String
unVariable x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.query.Variable")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.query.Variable wrapper
variable :: Typed.TypedTerm String -> Typed.TypedTerm Query.Variable
variable x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.query.Variable"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
