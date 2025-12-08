module Hydra.Ext.Sources.Gql.PathAlgebra.Expressions where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import           Hydra.Sources.Kernel.Types.All


ns :: Namespace
ns = Namespace "com.gdblab.pathAlgebra.expressions"

define :: String -> Type -> Binding
define = defineType ns

expr :: String -> Type
expr = typeref ns

module_ :: Module
module_ = Module ns elements [] kernelTypesModules $
    Just "Algebraic expression trees for the path algebra by Angles et al., extended for GQL support"
  where
    elements = [
      queryExpression, pathExpression, baseExpression, graphReference,
      selectionExpression, selectionCondition, simpleCondition, labelCondition,
      propertyCondition, propertyComparisonCondition, comparisonOperator,
      literalValue, lengthCondition, pathElement, andCondition, orCondition,
      notCondition, joinExpression, unionExpression, recursiveExpression,
      pathSemantics, solutionSpaceExpression, groupByExpression_, groupByCriterion,
      orderByExpression_, orderByCriterion, projectionExpression, projectionSpec,
      resultProjection, propertyExtraction, propertySource, nodePropertyRef,
      edgePropertyRef, pathPropertyRef]

queryExpression :: Binding
queryExpression = define "QueryExpression" $
  doc "Complete query with path algebra and result projection" $
  T.record [
    "pathExpression">: expr "PathExpression",
    "resultProjection">: T.optional $ expr "ResultProjection"]

pathExpression :: Binding
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

baseExpression :: Binding
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

graphReference :: Binding
graphReference = define "GraphReference" $
  doc "Reference to a property graph" $
  T.wrap T.string

selectionExpression :: Binding
selectionExpression = define "SelectionExpression" $
  doc "Selection operator: σ_condition(expression)" $
  T.record [
    "condition">: expr "SelectionCondition",
    "expression">: expr "PathExpression"]

selectionCondition :: Binding
selectionCondition = define "SelectionCondition" $
  doc "Conditions for filtering paths" $
  T.union [
    "simple">: expr "SimpleCondition",
    "and">: expr "AndCondition",
    "or">: expr "OrCondition",
    "not">: expr "NotCondition"]

simpleCondition :: Binding
simpleCondition = define "SimpleCondition" $
  doc "Atomic selection conditions" $
  T.union [
    "labelEquals">: expr "LabelCondition",
    "propertyEquals">: expr "PropertyCondition",
    "propertyComparison">: expr "PropertyComparisonCondition",
    "lengthEquals">: expr "LengthCondition"]

labelCondition :: Binding
labelCondition = define "LabelCondition" $
  doc "Conditions on node/edge labels: label(node(i)) = v" $
  T.record [
    "target">: expr "PathElement",
    "value">: T.string]

propertyCondition :: Binding
propertyCondition = define "PropertyCondition" $
  doc "Property equality conditions: node(i).prop = v" $
  T.record [
    "target">: expr "PathElement",
    "property">: T.string,
    "value">: expr "LiteralValue"]

propertyComparisonCondition :: Binding
propertyComparisonCondition = define "PropertyComparisonCondition" $
  doc "Property comparison conditions: node(i).prop > v, etc." $
  T.record [
    "target">: expr "PathElement",
    "property">: T.string,
    "operator">: expr "ComparisonOperator",
    "value">: expr "LiteralValue"]

comparisonOperator :: Binding
comparisonOperator = define "ComparisonOperator" $
  doc "Comparison operators for property conditions" $
  T.enum [
    "equal",
    "notEqual",
    "lessThan",
    "lessThanOrEqual",
    "greaterThan",
    "greaterThanOrEqual"]

literalValue :: Binding
literalValue = define "LiteralValue" $
  doc "Literal values for comparisons" $
  T.union [
    "string">: T.string,
    "integer">: T.int32,
    "float">: T.float64,
    "boolean">: T.boolean]

lengthCondition :: Binding
lengthCondition = define "LengthCondition" $
  doc "Condition on path length: len() = i" $
  T.record [
    "length">: T.int32]

pathElement :: Binding
pathElement = define "PathElement" $
  doc "References to elements within a path" $
  T.union [
    "node">: T.int32,
    "edge">: T.int32,
    "first">: T.unit,
    "last">: T.unit]

andCondition :: Binding
andCondition = define "AndCondition" $
  T.record [
    "left">: expr "SelectionCondition",
    "right">: expr "SelectionCondition"]

orCondition :: Binding
orCondition = define "OrCondition" $
  T.record [
    "left">: expr "SelectionCondition",
    "right">: expr "SelectionCondition"]

notCondition :: Binding
notCondition = define "NotCondition" $
  T.record [
    "condition">: expr "SelectionCondition"]

joinExpression :: Binding
joinExpression = define "JoinExpression" $
  doc "Join operator: expr1 ⊲⊳ expr2" $
  T.record [
    "left">: expr "PathExpression",
    "right">: expr "PathExpression"]

unionExpression :: Binding
unionExpression = define "UnionExpression" $
  doc "Union operator: expr1 ∪ expr2" $
  T.record [
    "left">: expr "PathExpression",
    "right">: expr "PathExpression"]

recursiveExpression :: Binding
recursiveExpression = define "RecursiveExpression" $
  doc "Recursive operator with path semantics" $
  T.record [
    "semantics">: expr "PathSemantics",
    "expression">: expr "PathExpression"]

pathSemantics :: Binding
pathSemantics = define "PathSemantics" $
  doc "Path semantics for recursive operations" $
  T.enum [
    "walk",
    "trail",
    "acyclic",
    "simple",
    "shortest"]

solutionSpaceExpression :: Binding
solutionSpaceExpression = define "SolutionSpaceExpression" $
  doc "Expressions that work with solution spaces" $
  T.union [
    "groupBy">: expr "GroupByExpression",
    "orderBy">: expr "OrderByExpression"]

groupByExpression_ :: Binding
groupByExpression_ = define "GroupByExpression" $
  doc "Group-by operator: γ_criterion(expression)" $
  T.record [
    "criterion">: expr "GroupByCriterion",
    "expression">: expr "PathExpression"]

groupByCriterion :: Binding
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

orderByExpression_ :: Binding
orderByExpression_ = define "OrderByExpression" $
  doc "Order-by operator: τ_criterion(solutionSpace)" $
  T.record [
    "criterion">: expr "OrderByCriterion",
    "expression">: expr "SolutionSpaceExpression"]

orderByCriterion :: Binding
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

projectionExpression :: Binding
projectionExpression = define "ProjectionExpression" $
  doc "Projection operator: π_(#P,#G,#A)(solutionSpace)" $
  T.record [
    "partitions">: expr "ProjectionSpec",
    "groups">: expr "ProjectionSpec",
    "paths">: expr "ProjectionSpec",
    "expression">: expr "SolutionSpaceExpression"]

projectionSpec :: Binding
projectionSpec = define "ProjectionSpec" $
  doc "Projection specification: * or specific number" $
  T.union [
    "all">: T.unit,
    "limited">: T.int32]

resultProjection :: Binding
resultProjection = define "ResultProjection" $
  doc "Extract specific values from paths for RETURN clause" $
  T.record [
    "projections">: T.list $ expr "PropertyExtraction"]

propertyExtraction :: Binding
propertyExtraction = define "PropertyExtraction" $
  doc "Extract properties from path elements" $
  T.record [
    "alias">: T.optional T.string,
    "source">: expr "PropertySource"]

propertySource :: Binding
propertySource = define "PropertySource" $
  doc "Source of a property value" $
  T.union [
    "nodeProperty">: expr "NodePropertyRef",
    "edgeProperty">: expr "EdgePropertyRef",
    "pathProperty">: expr "PathPropertyRef"]

nodePropertyRef :: Binding
nodePropertyRef = define "NodePropertyRef" $
  doc "Reference to a node property: node.property" $
  T.record [
    "element">: expr "PathElement",
    "property">: T.string]

edgePropertyRef :: Binding
edgePropertyRef = define "EdgePropertyRef" $
  doc "Reference to an edge property: edge.property" $
  T.record [
    "element">: expr "PathElement",
    "property">: T.string]

pathPropertyRef :: Binding
pathPropertyRef = define "PathPropertyRef" $
  doc "Reference to path-level properties: length, etc." $
  T.enum [
    "length",
    "startNode",
    "endNode"]
