-- Note: this is an automatically generated file. Do not edit.

-- | Algebraic expression trees for the path algebra by Angles et al., extended for GQL support

module Com.Gdblab.PathAlgebra.Expressions where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Complete query with path algebra and result projection
data QueryExpression = 
  QueryExpression {
    queryExpressionPathExpression :: PathExpression,
    queryExpressionResultProjection :: (Maybe ResultProjection)}
  deriving (Eq, Ord, Read, Show)

_QueryExpression = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression")

_QueryExpression_pathExpression = (Core.Name "pathExpression")

_QueryExpression_resultProjection = (Core.Name "resultProjection")

-- | A path algebra expression that evaluates to a set of paths
data PathExpression = 
  -- | Base case: extract paths from graph
  PathExpressionBase BaseExpression |
  -- | Selection operator (σ): filter paths by condition
  PathExpressionSelection SelectionExpression |
  -- | Join operator (⊲⊳): concatenate compatible paths
  PathExpressionJoin JoinExpression |
  -- | Union operator (∪): combine path sets
  PathExpressionUnion UnionExpression |
  -- | Recursive operator (φ): compute transitive closure with semantics
  PathExpressionRecursive RecursiveExpression |
  -- | Group-by operator (γ): organize paths into solution space
  PathExpressionGroupBy GroupByExpression |
  -- | Order-by operator (τ): sort solution space
  PathExpressionOrderBy OrderByExpression |
  -- | Projection operator (π): extract paths from solution space
  PathExpressionProjection ProjectionExpression
  deriving (Eq, Ord, Read, Show)

_PathExpression = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression")

_PathExpression_base = (Core.Name "base")

_PathExpression_selection = (Core.Name "selection")

_PathExpression_join = (Core.Name "join")

_PathExpression_union = (Core.Name "union")

_PathExpression_recursive = (Core.Name "recursive")

_PathExpression_groupBy = (Core.Name "groupBy")

_PathExpression_orderBy = (Core.Name "orderBy")

_PathExpression_projection = (Core.Name "projection")

-- | Base path expressions that extract paths from graph
data BaseExpression = 
  -- | Paths0(G): all paths of length 0 (nodes)
  BaseExpressionPaths0 GraphReference |
  -- | Paths1(G): all paths of length 1 (edges)
  BaseExpressionPaths1 GraphReference |
  -- | Paths*(G): all paths in graph (infinite without restrictions)
  BaseExpressionPathsStar GraphReference
  deriving (Eq, Ord, Read, Show)

_BaseExpression = (Core.Name "com.gdblab.pathAlgebra.expressions.BaseExpression")

_BaseExpression_paths0 = (Core.Name "paths0")

_BaseExpression_paths1 = (Core.Name "paths1")

_BaseExpression_pathsStar = (Core.Name "pathsStar")

-- | Reference to a property graph
newtype GraphReference = 
  GraphReference {
    unGraphReference :: String}
  deriving (Eq, Ord, Read, Show)

_GraphReference = (Core.Name "com.gdblab.pathAlgebra.expressions.GraphReference")

-- | Selection operator: σ_condition(expression)
data SelectionExpression = 
  SelectionExpression {
    selectionExpressionCondition :: SelectionCondition,
    selectionExpressionExpression :: PathExpression}
  deriving (Eq, Ord, Read, Show)

_SelectionExpression = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression")

_SelectionExpression_condition = (Core.Name "condition")

_SelectionExpression_expression = (Core.Name "expression")

-- | Conditions for filtering paths
data SelectionCondition = 
  SelectionConditionSimple SimpleCondition |
  SelectionConditionAnd AndCondition |
  SelectionConditionOr OrCondition |
  SelectionConditionNot NotCondition
  deriving (Eq, Ord, Read, Show)

_SelectionCondition = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionCondition")

_SelectionCondition_simple = (Core.Name "simple")

_SelectionCondition_and = (Core.Name "and")

_SelectionCondition_or = (Core.Name "or")

_SelectionCondition_not = (Core.Name "not")

-- | Atomic selection conditions
data SimpleCondition = 
  SimpleConditionLabelEquals LabelCondition |
  SimpleConditionPropertyEquals PropertyCondition |
  SimpleConditionPropertyComparison PropertyComparisonCondition |
  SimpleConditionLengthEquals LengthCondition
  deriving (Eq, Ord, Read, Show)

_SimpleCondition = (Core.Name "com.gdblab.pathAlgebra.expressions.SimpleCondition")

_SimpleCondition_labelEquals = (Core.Name "labelEquals")

_SimpleCondition_propertyEquals = (Core.Name "propertyEquals")

_SimpleCondition_propertyComparison = (Core.Name "propertyComparison")

_SimpleCondition_lengthEquals = (Core.Name "lengthEquals")

-- | Conditions on node/edge labels: label(node(i)) = v
data LabelCondition = 
  LabelCondition {
    labelConditionTarget :: PathElement,
    labelConditionValue :: String}
  deriving (Eq, Ord, Read, Show)

_LabelCondition = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition")

_LabelCondition_target = (Core.Name "target")

_LabelCondition_value = (Core.Name "value")

-- | Property equality conditions: node(i).prop = v
data PropertyCondition = 
  PropertyCondition {
    propertyConditionTarget :: PathElement,
    propertyConditionProperty :: String,
    propertyConditionValue :: LiteralValue}
  deriving (Eq, Ord, Read, Show)

_PropertyCondition = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition")

_PropertyCondition_target = (Core.Name "target")

_PropertyCondition_property = (Core.Name "property")

_PropertyCondition_value = (Core.Name "value")

-- | Property comparison conditions: node(i).prop > v, etc.
data PropertyComparisonCondition = 
  PropertyComparisonCondition {
    propertyComparisonConditionTarget :: PathElement,
    propertyComparisonConditionProperty :: String,
    propertyComparisonConditionOperator :: ComparisonOperator,
    propertyComparisonConditionValue :: LiteralValue}
  deriving (Eq, Ord, Read, Show)

_PropertyComparisonCondition = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition")

_PropertyComparisonCondition_target = (Core.Name "target")

_PropertyComparisonCondition_property = (Core.Name "property")

_PropertyComparisonCondition_operator = (Core.Name "operator")

_PropertyComparisonCondition_value = (Core.Name "value")

-- | Comparison operators for property conditions
data ComparisonOperator = 
  ComparisonOperatorEqual  |
  ComparisonOperatorNotEqual  |
  ComparisonOperatorLessThan  |
  ComparisonOperatorLessThanOrEqual  |
  ComparisonOperatorGreaterThan  |
  ComparisonOperatorGreaterThanOrEqual 
  deriving (Eq, Ord, Read, Show)

_ComparisonOperator = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator")

_ComparisonOperator_equal = (Core.Name "equal")

_ComparisonOperator_notEqual = (Core.Name "notEqual")

_ComparisonOperator_lessThan = (Core.Name "lessThan")

_ComparisonOperator_lessThanOrEqual = (Core.Name "lessThanOrEqual")

_ComparisonOperator_greaterThan = (Core.Name "greaterThan")

_ComparisonOperator_greaterThanOrEqual = (Core.Name "greaterThanOrEqual")

-- | Literal values for comparisons
data LiteralValue = 
  LiteralValueString String |
  LiteralValueInteger Int |
  LiteralValueFloat Double |
  LiteralValueBoolean Bool
  deriving (Eq, Ord, Read, Show)

_LiteralValue = (Core.Name "com.gdblab.pathAlgebra.expressions.LiteralValue")

_LiteralValue_string = (Core.Name "string")

_LiteralValue_integer = (Core.Name "integer")

_LiteralValue_float = (Core.Name "float")

_LiteralValue_boolean = (Core.Name "boolean")

-- | Condition on path length: len() = i
data LengthCondition = 
  LengthCondition {
    lengthConditionLength :: Int}
  deriving (Eq, Ord, Read, Show)

_LengthCondition = (Core.Name "com.gdblab.pathAlgebra.expressions.LengthCondition")

_LengthCondition_length = (Core.Name "length")

-- | References to elements within a path
data PathElement = 
  PathElementNode Int |
  PathElementEdge Int |
  PathElementFirst  |
  PathElementLast 
  deriving (Eq, Ord, Read, Show)

_PathElement = (Core.Name "com.gdblab.pathAlgebra.expressions.PathElement")

_PathElement_node = (Core.Name "node")

_PathElement_edge = (Core.Name "edge")

_PathElement_first = (Core.Name "first")

_PathElement_last = (Core.Name "last")

data AndCondition = 
  AndCondition {
    andConditionLeft :: SelectionCondition,
    andConditionRight :: SelectionCondition}
  deriving (Eq, Ord, Read, Show)

_AndCondition = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition")

_AndCondition_left = (Core.Name "left")

_AndCondition_right = (Core.Name "right")

data OrCondition = 
  OrCondition {
    orConditionLeft :: SelectionCondition,
    orConditionRight :: SelectionCondition}
  deriving (Eq, Ord, Read, Show)

_OrCondition = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition")

_OrCondition_left = (Core.Name "left")

_OrCondition_right = (Core.Name "right")

data NotCondition = 
  NotCondition {
    notConditionCondition :: SelectionCondition}
  deriving (Eq, Ord, Read, Show)

_NotCondition = (Core.Name "com.gdblab.pathAlgebra.expressions.NotCondition")

_NotCondition_condition = (Core.Name "condition")

-- | Join operator: expr1 ⊲⊳ expr2
data JoinExpression = 
  JoinExpression {
    joinExpressionLeft :: PathExpression,
    joinExpressionRight :: PathExpression}
  deriving (Eq, Ord, Read, Show)

_JoinExpression = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression")

_JoinExpression_left = (Core.Name "left")

_JoinExpression_right = (Core.Name "right")

-- | Union operator: expr1 ∪ expr2
data UnionExpression = 
  UnionExpression {
    unionExpressionLeft :: PathExpression,
    unionExpressionRight :: PathExpression}
  deriving (Eq, Ord, Read, Show)

_UnionExpression = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression")

_UnionExpression_left = (Core.Name "left")

_UnionExpression_right = (Core.Name "right")

-- | Recursive operator with path semantics
data RecursiveExpression = 
  RecursiveExpression {
    recursiveExpressionSemantics :: PathSemantics,
    recursiveExpressionExpression :: PathExpression}
  deriving (Eq, Ord, Read, Show)

_RecursiveExpression = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression")

_RecursiveExpression_semantics = (Core.Name "semantics")

_RecursiveExpression_expression = (Core.Name "expression")

-- | Path semantics for recursive operations
data PathSemantics = 
  PathSemanticsWalk  |
  PathSemanticsTrail  |
  PathSemanticsAcyclic  |
  PathSemanticsSimple  |
  PathSemanticsShortest 
  deriving (Eq, Ord, Read, Show)

_PathSemantics = (Core.Name "com.gdblab.pathAlgebra.expressions.PathSemantics")

_PathSemantics_walk = (Core.Name "walk")

_PathSemantics_trail = (Core.Name "trail")

_PathSemantics_acyclic = (Core.Name "acyclic")

_PathSemantics_simple = (Core.Name "simple")

_PathSemantics_shortest = (Core.Name "shortest")

-- | Expressions that work with solution spaces
data SolutionSpaceExpression = 
  SolutionSpaceExpressionGroupBy GroupByExpression |
  SolutionSpaceExpressionOrderBy OrderByExpression
  deriving (Eq, Ord, Read, Show)

_SolutionSpaceExpression = (Core.Name "com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression")

_SolutionSpaceExpression_groupBy = (Core.Name "groupBy")

_SolutionSpaceExpression_orderBy = (Core.Name "orderBy")

-- | Group-by operator: γ_criterion(expression)
data GroupByExpression = 
  GroupByExpression {
    groupByExpressionCriterion :: GroupByCriterion,
    groupByExpressionExpression :: PathExpression}
  deriving (Eq, Ord, Read, Show)

_GroupByExpression = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression")

_GroupByExpression_criterion = (Core.Name "criterion")

_GroupByExpression_expression = (Core.Name "expression")

-- | Grouping criteria corresponding to paper's γ variants
data GroupByCriterion = 
  GroupByCriterionNone  |
  GroupByCriterionSource  |
  GroupByCriterionTarget  |
  GroupByCriterionLength  |
  GroupByCriterionSourceTarget  |
  GroupByCriterionSourceLength  |
  GroupByCriterionTargetLength  |
  GroupByCriterionSourceTargetLength 
  deriving (Eq, Ord, Read, Show)

_GroupByCriterion = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion")

_GroupByCriterion_none = (Core.Name "none")

_GroupByCriterion_source = (Core.Name "source")

_GroupByCriterion_target = (Core.Name "target")

_GroupByCriterion_length = (Core.Name "length")

_GroupByCriterion_sourceTarget = (Core.Name "sourceTarget")

_GroupByCriterion_sourceLength = (Core.Name "sourceLength")

_GroupByCriterion_targetLength = (Core.Name "targetLength")

_GroupByCriterion_sourceTargetLength = (Core.Name "sourceTargetLength")

-- | Order-by operator: τ_criterion(solutionSpace)
data OrderByExpression = 
  OrderByExpression {
    orderByExpressionCriterion :: OrderByCriterion,
    orderByExpressionExpression :: SolutionSpaceExpression}
  deriving (Eq, Ord, Read, Show)

_OrderByExpression = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression")

_OrderByExpression_criterion = (Core.Name "criterion")

_OrderByExpression_expression = (Core.Name "expression")

-- | Ordering criteria corresponding to paper's τ variants
data OrderByCriterion = 
  OrderByCriterionPartition  |
  OrderByCriterionGroup  |
  OrderByCriterionPath  |
  OrderByCriterionPartitionGroup  |
  OrderByCriterionPartitionPath  |
  OrderByCriterionGroupPath  |
  OrderByCriterionPartitionGroupPath 
  deriving (Eq, Ord, Read, Show)

_OrderByCriterion = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion")

_OrderByCriterion_partition = (Core.Name "partition")

_OrderByCriterion_group = (Core.Name "group")

_OrderByCriterion_path = (Core.Name "path")

_OrderByCriterion_partitionGroup = (Core.Name "partitionGroup")

_OrderByCriterion_partitionPath = (Core.Name "partitionPath")

_OrderByCriterion_groupPath = (Core.Name "groupPath")

_OrderByCriterion_partitionGroupPath = (Core.Name "partitionGroupPath")

-- | Projection operator: π_(#P,#G,#A)(solutionSpace)
data ProjectionExpression = 
  ProjectionExpression {
    projectionExpressionPartitions :: ProjectionSpec,
    projectionExpressionGroups :: ProjectionSpec,
    projectionExpressionPaths :: ProjectionSpec,
    projectionExpressionExpression :: SolutionSpaceExpression}
  deriving (Eq, Ord, Read, Show)

_ProjectionExpression = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression")

_ProjectionExpression_partitions = (Core.Name "partitions")

_ProjectionExpression_groups = (Core.Name "groups")

_ProjectionExpression_paths = (Core.Name "paths")

_ProjectionExpression_expression = (Core.Name "expression")

-- | Projection specification: * or specific number
data ProjectionSpec = 
  ProjectionSpecAll  |
  ProjectionSpecLimited Int
  deriving (Eq, Ord, Read, Show)

_ProjectionSpec = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionSpec")

_ProjectionSpec_all = (Core.Name "all")

_ProjectionSpec_limited = (Core.Name "limited")

-- | Extract specific values from paths for RETURN clause
data ResultProjection = 
  ResultProjection {
    resultProjectionProjections :: [PropertyExtraction]}
  deriving (Eq, Ord, Read, Show)

_ResultProjection = (Core.Name "com.gdblab.pathAlgebra.expressions.ResultProjection")

_ResultProjection_projections = (Core.Name "projections")

-- | Extract properties from path elements
data PropertyExtraction = 
  PropertyExtraction {
    propertyExtractionAlias :: (Maybe String),
    propertyExtractionSource :: PropertySource}
  deriving (Eq, Ord, Read, Show)

_PropertyExtraction = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction")

_PropertyExtraction_alias = (Core.Name "alias")

_PropertyExtraction_source = (Core.Name "source")

-- | Source of a property value
data PropertySource = 
  PropertySourceNodeProperty NodePropertyRef |
  PropertySourceEdgeProperty EdgePropertyRef |
  PropertySourcePathProperty PathPropertyRef
  deriving (Eq, Ord, Read, Show)

_PropertySource = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertySource")

_PropertySource_nodeProperty = (Core.Name "nodeProperty")

_PropertySource_edgeProperty = (Core.Name "edgeProperty")

_PropertySource_pathProperty = (Core.Name "pathProperty")

-- | Reference to a node property: node.property
data NodePropertyRef = 
  NodePropertyRef {
    nodePropertyRefElement :: PathElement,
    nodePropertyRefProperty :: String}
  deriving (Eq, Ord, Read, Show)

_NodePropertyRef = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef")

_NodePropertyRef_element = (Core.Name "element")

_NodePropertyRef_property = (Core.Name "property")

-- | Reference to an edge property: edge.property
data EdgePropertyRef = 
  EdgePropertyRef {
    edgePropertyRefElement :: PathElement,
    edgePropertyRefProperty :: String}
  deriving (Eq, Ord, Read, Show)

_EdgePropertyRef = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef")

_EdgePropertyRef_element = (Core.Name "element")

_EdgePropertyRef_property = (Core.Name "property")

-- | Reference to path-level properties: length, etc.
data PathPropertyRef = 
  PathPropertyRefLength  |
  PathPropertyRefStartNode  |
  PathPropertyRefEndNode 
  deriving (Eq, Ord, Read, Show)

_PathPropertyRef = (Core.Name "com.gdblab.pathAlgebra.expressions.PathPropertyRef")

_PathPropertyRef_length = (Core.Name "length")

_PathPropertyRef_startNode = (Core.Name "startNode")

_PathPropertyRef_endNode = (Core.Name "endNode")
