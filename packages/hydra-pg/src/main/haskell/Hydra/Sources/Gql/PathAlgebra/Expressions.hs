module Hydra.Sources.Gql.PathAlgebra.Expressions where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y

-- Additional imports
import           Hydra.Sources.Kernel.Types.All


ns :: ModuleName
ns = ModuleName "com.gdblab.pathAlgebra.expressions"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> (kernelTypesModuleNames),
            moduleDescription = Just "Algebraic expression trees for the path algebra by Angles et al., extended for GQL support"}
  where
    definitions = [
      queryExpression, pathExpression, baseExpression, graphReference,
      selectionExpression, selectionCondition, simpleCondition, labelCondition,
      propertyCondition, propertyComparisonCondition, comparisonOperator,
      literalValue, lengthCondition, pathElement, andCondition, orCondition,
      notCondition, joinExpression, unionExpression, recursiveExpression,
      pathSemantics, solutionSpaceExpression, groupByExpression_, groupByCriterion,
      orderByExpression_, orderByCriterion, projectionExpression, projectionSpec,
      resultProjection, propertyExtraction, propertySource, nodePropertyRef,
      edgePropertyRef, pathPropertyRef]

andCondition :: TypeDefinition
andCondition = define "AndCondition" $
  T.record [
    "left">: expr "SelectionCondition",
    "right">: expr "SelectionCondition"]

baseExpression :: TypeDefinition
baseExpression = define "BaseExpression" $
  doc "Base path expressions that extract paths from graph" $
  T.union [
    "paths0">:
      doc "Paths0(G): all paths of length 0 (nodes)" $
      expr "GraphReference",
    "paths1">:
      doc "Paths1(G): all paths of length 1 (edges)" $
      expr "GraphReference",
    "pathsStar">:
      doc "Paths*(G): all paths in graph (infinite without restrictions)" $
      expr "GraphReference"]

comparisonOperator :: TypeDefinition
comparisonOperator = define "ComparisonOperator" $
  doc "Comparison operators for property conditions" $
  T.enum [
    "equal",
    "notEqual",
    "lessThan",
    "lessThanOrEqual",
    "greaterThan",
    "greaterThanOrEqual"]

edgePropertyRef :: TypeDefinition
edgePropertyRef = define "EdgePropertyRef" $
  doc "Reference to an edge property: edge.property" $
  T.record [
    "element">: expr "PathElement",
    "property">: T.string]

expr :: String -> Type
expr = typeref ns

graphReference :: TypeDefinition
graphReference = define "GraphReference" $
  doc "Reference to a property graph" $
  T.wrap T.string

groupByCriterion :: TypeDefinition
groupByCriterion = define "GroupByCriterion" $
  doc "Grouping criteria corresponding to paper's γ variants" $
  T.enum [
    "none",
    "source",
    "target",
    "length",
    "sourceTarget",
    "sourceLength",
    "targetLength",
    "sourceTargetLength"]

groupByExpression_ :: TypeDefinition
groupByExpression_ = define "GroupByExpression" $
  doc "Group-by operator: γ_criterion(expression)" $
  T.record [
    "criterion">: expr "GroupByCriterion",
    "expression">: expr "PathExpression"]

joinExpression :: TypeDefinition
joinExpression = define "JoinExpression" $
  doc "Join operator: expr1 ⊲⊳ expr2" $
  T.record [
    "left">: expr "PathExpression",
    "right">: expr "PathExpression"]

labelCondition :: TypeDefinition
labelCondition = define "LabelCondition" $
  doc "Conditions on node/edge labels: label(node(i)) = v" $
  T.record [
    "target">: expr "PathElement",
    "value">: T.string]

lengthCondition :: TypeDefinition
lengthCondition = define "LengthCondition" $
  doc "Condition on path length: len() = i" $
  T.record [
    "length">: T.int32]

literalValue :: TypeDefinition
literalValue = define "LiteralValue" $
  doc "Literal values for comparisons" $
  T.union [
    "string">: T.string,
    "integer">: T.int32,
    "float">: T.float64,
    "boolean">: T.boolean]

nodePropertyRef :: TypeDefinition
nodePropertyRef = define "NodePropertyRef" $
  doc "Reference to a node property: node.property" $
  T.record [
    "element">: expr "PathElement",
    "property">: T.string]

notCondition :: TypeDefinition
notCondition = define "NotCondition" $
  T.record [
    "condition">: expr "SelectionCondition"]

orCondition :: TypeDefinition
orCondition = define "OrCondition" $
  T.record [
    "left">: expr "SelectionCondition",
    "right">: expr "SelectionCondition"]

orderByCriterion :: TypeDefinition
orderByCriterion = define "OrderByCriterion" $
  doc "Ordering criteria corresponding to paper's τ variants" $
  T.enum [
    "partition",
    "group",
    "path",
    "partitionGroup",
    "partitionPath",
    "groupPath",
    "partitionGroupPath"]

orderByExpression_ :: TypeDefinition
orderByExpression_ = define "OrderByExpression" $
  doc "Order-by operator: τ_criterion(solutionSpace)" $
  T.record [
    "criterion">: expr "OrderByCriterion",
    "expression">: expr "SolutionSpaceExpression"]

pathElement :: TypeDefinition
pathElement = define "PathElement" $
  doc "References to elements within a path" $
  T.union [
    "node">: T.int32,
    "edge">: T.int32,
    "first">: T.unit,
    "last">: T.unit]

pathExpression :: TypeDefinition
pathExpression = define "PathExpression" $
  doc "A path algebra expression that evaluates to a set of paths" $
  T.union [
    "base">:
      doc "Base case: extract paths from graph" $
      expr "BaseExpression",
    "selection">:
      doc "Selection operator (σ): filter paths by condition" $
      expr "SelectionExpression",
    "join">:
      doc "Join operator (⊲⊳): concatenate compatible paths" $
      expr "JoinExpression",
    "union">:
      doc "Union operator (∪): combine path sets" $
      expr "UnionExpression",
    "recursive">:
      doc "Recursive operator (φ): compute transitive closure with semantics" $
      expr "RecursiveExpression",
    "groupBy">:
      doc "Group-by operator (γ): organize paths into solution space" $
      expr "GroupByExpression",
    "orderBy">:
      doc "Order-by operator (τ): sort solution space" $
      expr "OrderByExpression",
    "projection">:
      doc "Projection operator (π): extract paths from solution space" $
      expr "ProjectionExpression"]

pathPropertyRef :: TypeDefinition
pathPropertyRef = define "PathPropertyRef" $
  doc "Reference to path-level properties: length, etc." $
  T.enum [
    "length",
    "startNode",
    "endNode"]

pathSemantics :: TypeDefinition
pathSemantics = define "PathSemantics" $
  doc "Path semantics for recursive operations" $
  T.enum [
    "walk",
    "trail",
    "acyclic",
    "simple",
    "shortest"]

projectionExpression :: TypeDefinition
projectionExpression = define "ProjectionExpression" $
  doc "Projection operator: π_(#P,#G,#A)(solutionSpace)" $
  T.record [
    "partitions">: expr "ProjectionSpec",
    "groups">: expr "ProjectionSpec",
    "paths">: expr "ProjectionSpec",
    "expression">: expr "SolutionSpaceExpression"]

projectionSpec :: TypeDefinition
projectionSpec = define "ProjectionSpec" $
  doc "Projection specification: * or specific number" $
  T.union [
    "all">: T.unit,
    "limited">: T.int32]

propertyComparisonCondition :: TypeDefinition
propertyComparisonCondition = define "PropertyComparisonCondition" $
  doc "Property comparison conditions: node(i).prop > v, etc." $
  T.record [
    "target">: expr "PathElement",
    "property">: T.string,
    "operator">: expr "ComparisonOperator",
    "value">: expr "LiteralValue"]

propertyCondition :: TypeDefinition
propertyCondition = define "PropertyCondition" $
  doc "Property equality conditions: node(i).prop = v" $
  T.record [
    "target">: expr "PathElement",
    "property">: T.string,
    "value">: expr "LiteralValue"]

propertyExtraction :: TypeDefinition
propertyExtraction = define "PropertyExtraction" $
  doc "Extract properties from path elements" $
  T.record [
    "alias">: T.optional T.string,
    "source">: expr "PropertySource"]

propertySource :: TypeDefinition
propertySource = define "PropertySource" $
  doc "Source of a property value" $
  T.union [
    "nodeProperty">: expr "NodePropertyRef",
    "edgeProperty">: expr "EdgePropertyRef",
    "pathProperty">: expr "PathPropertyRef"]

queryExpression :: TypeDefinition
queryExpression = define "QueryExpression" $
  doc "Complete query with path algebra and result projection" $
  T.record [
    "pathExpression">: expr "PathExpression",
    "resultProjection">: T.optional $ expr "ResultProjection"]

recursiveExpression :: TypeDefinition
recursiveExpression = define "RecursiveExpression" $
  doc "Recursive operator with path semantics" $
  T.record [
    "semantics">: expr "PathSemantics",
    "expression">: expr "PathExpression"]

resultProjection :: TypeDefinition
resultProjection = define "ResultProjection" $
  doc "Extract specific values from paths for RETURN clause" $
  T.record [
    "projections">: T.list $ expr "PropertyExtraction"]

selectionCondition :: TypeDefinition
selectionCondition = define "SelectionCondition" $
  doc "Conditions for filtering paths" $
  T.union [
    "simple">: expr "SimpleCondition",
    "and">: expr "AndCondition",
    "or">: expr "OrCondition",
    "not">: expr "NotCondition"]

selectionExpression :: TypeDefinition
selectionExpression = define "SelectionExpression" $
  doc "Selection operator: σ_condition(expression)" $
  T.record [
    "condition">: expr "SelectionCondition",
    "expression">: expr "PathExpression"]

simpleCondition :: TypeDefinition
simpleCondition = define "SimpleCondition" $
  doc "Atomic selection conditions" $
  T.union [
    "labelEquals">: expr "LabelCondition",
    "propertyEquals">: expr "PropertyCondition",
    "propertyComparison">: expr "PropertyComparisonCondition",
    "lengthEquals">: expr "LengthCondition"]

solutionSpaceExpression :: TypeDefinition
solutionSpaceExpression = define "SolutionSpaceExpression" $
  doc "Expressions that work with solution spaces" $
  T.union [
    "groupBy">: expr "GroupByExpression",
    "orderBy">: expr "OrderByExpression"]

unionExpression :: TypeDefinition
unionExpression = define "UnionExpression" $
  doc "Union operator: expr1 ∪ expr2" $
  T.record [
    "left">: expr "PathExpression",
    "right">: expr "PathExpression"]
