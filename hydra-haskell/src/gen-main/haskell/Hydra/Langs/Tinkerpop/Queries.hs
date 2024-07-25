-- | A common model for pattern-matching queries over property graphs

module Hydra.Langs.Tinkerpop.Queries where

import qualified Hydra.Core as Core
import qualified Hydra.Langs.Tinkerpop.PropertyGraph as PropertyGraph
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

data AggregationQuery = 
  AggregationQueryCount 
  deriving (Eq, Ord, Read, Show)

_AggregationQuery = (Core.Name "hydra/langs/tinkerpop/queries.AggregationQuery")

_AggregationQuery_count = (Core.FieldName "count")

newtype ApplicationQuery = 
  ApplicationQuery {
    unApplicationQuery :: [Query]}
  deriving (Eq, Ord, Read, Show)

_ApplicationQuery = (Core.Name "hydra/langs/tinkerpop/queries.ApplicationQuery")

data AssociativeExpression = 
  AssociativeExpression {
    associativeExpressionOperator :: BinaryOperator,
    associativeExpressionOperands :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_AssociativeExpression = (Core.Name "hydra/langs/tinkerpop/queries.AssociativeExpression")

_AssociativeExpression_operator = (Core.FieldName "operator")

_AssociativeExpression_operands = (Core.FieldName "operands")

data BinaryExpression = 
  BinaryExpression {
    binaryExpressionLeft :: Expression,
    binaryExpressionOperator :: BinaryOperator,
    binaryExpressionRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_BinaryExpression = (Core.Name "hydra/langs/tinkerpop/queries.BinaryExpression")

_BinaryExpression_left = (Core.FieldName "left")

_BinaryExpression_operator = (Core.FieldName "operator")

_BinaryExpression_right = (Core.FieldName "right")

data BinaryBooleanOperator = 
  BinaryBooleanOperatorAnd  |
  BinaryBooleanOperatorOr  |
  BinaryBooleanOperatorXor 
  deriving (Eq, Ord, Read, Show)

_BinaryBooleanOperator = (Core.Name "hydra/langs/tinkerpop/queries.BinaryBooleanOperator")

_BinaryBooleanOperator_and = (Core.FieldName "and")

_BinaryBooleanOperator_or = (Core.FieldName "or")

_BinaryBooleanOperator_xor = (Core.FieldName "xor")

data BinaryOperator = 
  BinaryOperatorBoolean BinaryBooleanOperator |
  BinaryOperatorComparison ComparisonOperator |
  BinaryOperatorPower 
  deriving (Eq, Ord, Read, Show)

_BinaryOperator = (Core.Name "hydra/langs/tinkerpop/queries.BinaryOperator")

_BinaryOperator_boolean = (Core.FieldName "boolean")

_BinaryOperator_comparison = (Core.FieldName "comparison")

_BinaryOperator_power = (Core.FieldName "power")

data Binding = 
  Binding {
    bindingKey :: Variable,
    bindingValue :: Query}
  deriving (Eq, Ord, Read, Show)

_Binding = (Core.Name "hydra/langs/tinkerpop/queries.Binding")

_Binding_key = (Core.FieldName "key")

_Binding_value = (Core.FieldName "value")

data ComparisonOperator = 
  ComparisonOperatorEq  |
  ComparisonOperatorNeq  |
  ComparisonOperatorLt  |
  ComparisonOperatorLte  |
  ComparisonOperatorGt  |
  ComparisonOperatorGte 
  deriving (Eq, Ord, Read, Show)

_ComparisonOperator = (Core.Name "hydra/langs/tinkerpop/queries.ComparisonOperator")

_ComparisonOperator_eq = (Core.FieldName "eq")

_ComparisonOperator_neq = (Core.FieldName "neq")

_ComparisonOperator_lt = (Core.FieldName "lt")

_ComparisonOperator_lte = (Core.FieldName "lte")

_ComparisonOperator_gt = (Core.FieldName "gt")

_ComparisonOperator_gte = (Core.FieldName "gte")

data EdgeProjectionPattern = 
  EdgeProjectionPattern {
    edgeProjectionPatternDirection :: PropertyGraph.Direction,
    edgeProjectionPatternLabel :: (Maybe PropertyGraph.EdgeLabel),
    edgeProjectionPatternProperties :: [PropertyPattern],
    edgeProjectionPatternVertex :: (Maybe VertexPattern)}
  deriving (Eq, Ord, Read, Show)

_EdgeProjectionPattern = (Core.Name "hydra/langs/tinkerpop/queries.EdgeProjectionPattern")

_EdgeProjectionPattern_direction = (Core.FieldName "direction")

_EdgeProjectionPattern_label = (Core.FieldName "label")

_EdgeProjectionPattern_properties = (Core.FieldName "properties")

_EdgeProjectionPattern_vertex = (Core.FieldName "vertex")

data Expression = 
  ExpressionAssociative AssociativeExpression |
  ExpressionBinary BinaryExpression |
  ExpressionProperty PropertyProjection |
  ExpressionUnary UnaryExpression |
  ExpressionVariable Variable |
  ExpressionVertex VertexPattern
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra/langs/tinkerpop/queries.Expression")

_Expression_associative = (Core.FieldName "associative")

_Expression_binary = (Core.FieldName "binary")

_Expression_property = (Core.FieldName "property")

_Expression_unary = (Core.FieldName "unary")

_Expression_variable = (Core.FieldName "variable")

_Expression_vertex = (Core.FieldName "vertex")

data LetQuery = 
  LetQuery {
    letQueryBindings :: [Binding],
    letQueryEnvironment :: Query}
  deriving (Eq, Ord, Read, Show)

_LetQuery = (Core.Name "hydra/langs/tinkerpop/queries.LetQuery")

_LetQuery_bindings = (Core.FieldName "bindings")

_LetQuery_environment = (Core.FieldName "environment")

data MatchQuery = 
  MatchQuery {
    matchQueryOptional :: Bool,
    matchQueryPattern :: [Projection],
    matchQueryWhere :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_MatchQuery = (Core.Name "hydra/langs/tinkerpop/queries.MatchQuery")

_MatchQuery_optional = (Core.FieldName "optional")

_MatchQuery_pattern = (Core.FieldName "pattern")

_MatchQuery_where = (Core.FieldName "where")

data Projection = 
  Projection {
    projectionValue :: Expression,
    projectionAs :: (Maybe Variable)}
  deriving (Eq, Ord, Read, Show)

_Projection = (Core.Name "hydra/langs/tinkerpop/queries.Projection")

_Projection_value = (Core.FieldName "value")

_Projection_as = (Core.FieldName "as")

data Projections = 
  Projections {
    projectionsAll :: Bool,
    projectionsExplicit :: [Projection]}
  deriving (Eq, Ord, Read, Show)

_Projections = (Core.Name "hydra/langs/tinkerpop/queries.Projections")

_Projections_all = (Core.FieldName "all")

_Projections_explicit = (Core.FieldName "explicit")

data PropertyPattern = 
  PropertyPattern {
    propertyPatternKey :: PropertyGraph.PropertyKey,
    propertyPatternValue :: PropertyValuePattern}
  deriving (Eq, Ord, Read, Show)

_PropertyPattern = (Core.Name "hydra/langs/tinkerpop/queries.PropertyPattern")

_PropertyPattern_key = (Core.FieldName "key")

_PropertyPattern_value = (Core.FieldName "value")

data PropertyProjection = 
  PropertyProjection {
    propertyProjectionBase :: Expression,
    propertyProjectionKey :: PropertyGraph.PropertyKey}
  deriving (Eq, Ord, Read, Show)

_PropertyProjection = (Core.Name "hydra/langs/tinkerpop/queries.PropertyProjection")

_PropertyProjection_base = (Core.FieldName "base")

_PropertyProjection_key = (Core.FieldName "key")

newtype PropertyValue = 
  PropertyValue {
    unPropertyValue :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyValue = (Core.Name "hydra/langs/tinkerpop/queries.PropertyValue")

data PropertyValuePattern = 
  PropertyValuePatternVariable PropertyGraph.PropertyKey |
  PropertyValuePatternValue String
  deriving (Eq, Ord, Read, Show)

_PropertyValuePattern = (Core.Name "hydra/langs/tinkerpop/queries.PropertyValuePattern")

_PropertyValuePattern_variable = (Core.FieldName "variable")

_PropertyValuePattern_value = (Core.FieldName "value")

data Query = 
  QueryApplication ApplicationQuery |
  QueryAggregate AggregationQuery |
  QueryLetQuery LetQuery |
  QueryMatch MatchQuery |
  QuerySelect SelectQuery |
  QueryValue String
  deriving (Eq, Ord, Read, Show)

_Query = (Core.Name "hydra/langs/tinkerpop/queries.Query")

_Query_application = (Core.FieldName "application")

_Query_aggregate = (Core.FieldName "aggregate")

_Query_LetQuery = (Core.FieldName "LetQuery")

_Query_match = (Core.FieldName "match")

_Query_select = (Core.FieldName "select")

_Query_value = (Core.FieldName "value")

data SelectQuery = 
  SelectQuery {
    selectQueryDistinct :: Bool,
    selectQueryProjection :: Projections}
  deriving (Eq, Ord, Read, Show)

_SelectQuery = (Core.Name "hydra/langs/tinkerpop/queries.SelectQuery")

_SelectQuery_distinct = (Core.FieldName "distinct")

_SelectQuery_projection = (Core.FieldName "projection")

data UnaryExpression = 
  UnaryExpression {
    unaryExpressionOperator :: UnaryOperator,
    unaryExpressionOperand :: Expression}
  deriving (Eq, Ord, Read, Show)

_UnaryExpression = (Core.Name "hydra/langs/tinkerpop/queries.UnaryExpression")

_UnaryExpression_operator = (Core.FieldName "operator")

_UnaryExpression_operand = (Core.FieldName "operand")

data UnaryOperator = 
  UnaryOperatorNegate 
  deriving (Eq, Ord, Read, Show)

_UnaryOperator = (Core.Name "hydra/langs/tinkerpop/queries.UnaryOperator")

_UnaryOperator_negate = (Core.FieldName "negate")

newtype Variable = 
  Variable {
    unVariable :: String}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra/langs/tinkerpop/queries.Variable")

data VertexPattern = 
  VertexPattern {
    vertexPatternVariable :: (Maybe Variable),
    vertexPatternLabel :: (Maybe PropertyGraph.VertexLabel),
    vertexPatternProperties :: [PropertyPattern],
    vertexPatternEdges :: [EdgeProjectionPattern]}
  deriving (Eq, Ord, Read, Show)

_VertexPattern = (Core.Name "hydra/langs/tinkerpop/queries.VertexPattern")

_VertexPattern_variable = (Core.FieldName "variable")

_VertexPattern_label = (Core.FieldName "label")

_VertexPattern_properties = (Core.FieldName "properties")

_VertexPattern_edges = (Core.FieldName "edges")