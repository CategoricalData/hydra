-- Note: this is an automatically generated file. Do not edit.

-- | A common model for pattern-matching queries over property graphs

module Hydra.Pg.Query where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data AggregationQuery = 
  AggregationQueryCount 
  deriving (Eq, Ord, Read, Show)

_AggregationQuery = (Core.Name "hydra.pg.query.AggregationQuery")

_AggregationQuery_count = (Core.Name "count")

newtype ApplicationQuery = 
  ApplicationQuery {
    unApplicationQuery :: [Query]}
  deriving (Eq, Ord, Read, Show)

_ApplicationQuery = (Core.Name "hydra.pg.query.ApplicationQuery")

data AssociativeExpression = 
  AssociativeExpression {
    associativeExpressionOperator :: BinaryOperator,
    associativeExpressionOperands :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_AssociativeExpression = (Core.Name "hydra.pg.query.AssociativeExpression")

_AssociativeExpression_operator = (Core.Name "operator")

_AssociativeExpression_operands = (Core.Name "operands")

data BinaryExpression = 
  BinaryExpression {
    binaryExpressionLeft :: Expression,
    binaryExpressionOperator :: BinaryOperator,
    binaryExpressionRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_BinaryExpression = (Core.Name "hydra.pg.query.BinaryExpression")

_BinaryExpression_left = (Core.Name "left")

_BinaryExpression_operator = (Core.Name "operator")

_BinaryExpression_right = (Core.Name "right")

data BinaryBooleanOperator = 
  BinaryBooleanOperatorAnd  |
  BinaryBooleanOperatorOr  |
  BinaryBooleanOperatorXor 
  deriving (Eq, Ord, Read, Show)

_BinaryBooleanOperator = (Core.Name "hydra.pg.query.BinaryBooleanOperator")

_BinaryBooleanOperator_and = (Core.Name "and")

_BinaryBooleanOperator_or = (Core.Name "or")

_BinaryBooleanOperator_xor = (Core.Name "xor")

data BinaryOperator = 
  BinaryOperatorBoolean BinaryBooleanOperator |
  BinaryOperatorComparison ComparisonOperator |
  BinaryOperatorPower 
  deriving (Eq, Ord, Read, Show)

_BinaryOperator = (Core.Name "hydra.pg.query.BinaryOperator")

_BinaryOperator_boolean = (Core.Name "boolean")

_BinaryOperator_comparison = (Core.Name "comparison")

_BinaryOperator_power = (Core.Name "power")

data Binding = 
  Binding {
    bindingKey :: Variable,
    bindingValue :: Query}
  deriving (Eq, Ord, Read, Show)

_Binding = (Core.Name "hydra.pg.query.Binding")

_Binding_key = (Core.Name "key")

_Binding_value = (Core.Name "value")

data ComparisonOperator = 
  ComparisonOperatorEq  |
  ComparisonOperatorNeq  |
  ComparisonOperatorLt  |
  ComparisonOperatorLte  |
  ComparisonOperatorGt  |
  ComparisonOperatorGte 
  deriving (Eq, Ord, Read, Show)

_ComparisonOperator = (Core.Name "hydra.pg.query.ComparisonOperator")

_ComparisonOperator_eq = (Core.Name "eq")

_ComparisonOperator_neq = (Core.Name "neq")

_ComparisonOperator_lt = (Core.Name "lt")

_ComparisonOperator_lte = (Core.Name "lte")

_ComparisonOperator_gt = (Core.Name "gt")

_ComparisonOperator_gte = (Core.Name "gte")

data EdgeProjectionPattern = 
  EdgeProjectionPattern {
    edgeProjectionPatternDirection :: Model.Direction,
    edgeProjectionPatternLabel :: (Maybe Model.EdgeLabel),
    edgeProjectionPatternProperties :: [PropertyPattern],
    edgeProjectionPatternVertex :: (Maybe VertexPattern)}
  deriving (Eq, Ord, Read, Show)

_EdgeProjectionPattern = (Core.Name "hydra.pg.query.EdgeProjectionPattern")

_EdgeProjectionPattern_direction = (Core.Name "direction")

_EdgeProjectionPattern_label = (Core.Name "label")

_EdgeProjectionPattern_properties = (Core.Name "properties")

_EdgeProjectionPattern_vertex = (Core.Name "vertex")

data Expression = 
  ExpressionAssociative AssociativeExpression |
  ExpressionBinary BinaryExpression |
  ExpressionProperty PropertyProjection |
  ExpressionUnary UnaryExpression |
  ExpressionVariable Variable |
  ExpressionVertex VertexPattern
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra.pg.query.Expression")

_Expression_associative = (Core.Name "associative")

_Expression_binary = (Core.Name "binary")

_Expression_property = (Core.Name "property")

_Expression_unary = (Core.Name "unary")

_Expression_variable = (Core.Name "variable")

_Expression_vertex = (Core.Name "vertex")

data LetQuery = 
  LetQuery {
    letQueryBindings :: [Binding],
    letQueryEnvironment :: Query}
  deriving (Eq, Ord, Read, Show)

_LetQuery = (Core.Name "hydra.pg.query.LetQuery")

_LetQuery_bindings = (Core.Name "bindings")

_LetQuery_environment = (Core.Name "environment")

data MatchQuery = 
  MatchQuery {
    matchQueryOptional :: Bool,
    matchQueryPattern :: [Projection],
    matchQueryWhere :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_MatchQuery = (Core.Name "hydra.pg.query.MatchQuery")

_MatchQuery_optional = (Core.Name "optional")

_MatchQuery_pattern = (Core.Name "pattern")

_MatchQuery_where = (Core.Name "where")

data Projection = 
  Projection {
    projectionValue :: Expression,
    projectionAs :: (Maybe Variable)}
  deriving (Eq, Ord, Read, Show)

_Projection = (Core.Name "hydra.pg.query.Projection")

_Projection_value = (Core.Name "value")

_Projection_as = (Core.Name "as")

data Projections = 
  Projections {
    projectionsAll :: Bool,
    projectionsExplicit :: [Projection]}
  deriving (Eq, Ord, Read, Show)

_Projections = (Core.Name "hydra.pg.query.Projections")

_Projections_all = (Core.Name "all")

_Projections_explicit = (Core.Name "explicit")

data PropertyPattern = 
  PropertyPattern {
    propertyPatternKey :: Model.PropertyKey,
    propertyPatternValue :: PropertyValuePattern}
  deriving (Eq, Ord, Read, Show)

_PropertyPattern = (Core.Name "hydra.pg.query.PropertyPattern")

_PropertyPattern_key = (Core.Name "key")

_PropertyPattern_value = (Core.Name "value")

data PropertyProjection = 
  PropertyProjection {
    propertyProjectionBase :: Expression,
    propertyProjectionKey :: Model.PropertyKey}
  deriving (Eq, Ord, Read, Show)

_PropertyProjection = (Core.Name "hydra.pg.query.PropertyProjection")

_PropertyProjection_base = (Core.Name "base")

_PropertyProjection_key = (Core.Name "key")

newtype PropertyValue = 
  PropertyValue {
    unPropertyValue :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyValue = (Core.Name "hydra.pg.query.PropertyValue")

data PropertyValuePattern = 
  PropertyValuePatternVariable Model.PropertyKey |
  PropertyValuePatternValue String
  deriving (Eq, Ord, Read, Show)

_PropertyValuePattern = (Core.Name "hydra.pg.query.PropertyValuePattern")

_PropertyValuePattern_variable = (Core.Name "variable")

_PropertyValuePattern_value = (Core.Name "value")

data Query = 
  QueryApplication ApplicationQuery |
  QueryAggregate AggregationQuery |
  QueryLetQuery LetQuery |
  QueryMatch MatchQuery |
  QuerySelect SelectQuery |
  QueryValue String
  deriving (Eq, Ord, Read, Show)

_Query = (Core.Name "hydra.pg.query.Query")

_Query_application = (Core.Name "application")

_Query_aggregate = (Core.Name "aggregate")

_Query_LetQuery = (Core.Name "LetQuery")

_Query_match = (Core.Name "match")

_Query_select = (Core.Name "select")

_Query_value = (Core.Name "value")

data SelectQuery = 
  SelectQuery {
    selectQueryDistinct :: Bool,
    selectQueryProjection :: Projections}
  deriving (Eq, Ord, Read, Show)

_SelectQuery = (Core.Name "hydra.pg.query.SelectQuery")

_SelectQuery_distinct = (Core.Name "distinct")

_SelectQuery_projection = (Core.Name "projection")

data UnaryExpression = 
  UnaryExpression {
    unaryExpressionOperator :: UnaryOperator,
    unaryExpressionOperand :: Expression}
  deriving (Eq, Ord, Read, Show)

_UnaryExpression = (Core.Name "hydra.pg.query.UnaryExpression")

_UnaryExpression_operator = (Core.Name "operator")

_UnaryExpression_operand = (Core.Name "operand")

data UnaryOperator = 
  UnaryOperatorNegate 
  deriving (Eq, Ord, Read, Show)

_UnaryOperator = (Core.Name "hydra.pg.query.UnaryOperator")

_UnaryOperator_negate = (Core.Name "negate")

newtype Variable = 
  Variable {
    unVariable :: String}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra.pg.query.Variable")

data VertexPattern = 
  VertexPattern {
    vertexPatternVariable :: (Maybe Variable),
    vertexPatternLabel :: (Maybe Model.VertexLabel),
    vertexPatternProperties :: [PropertyPattern],
    vertexPatternEdges :: [EdgeProjectionPattern]}
  deriving (Eq, Ord, Read, Show)

_VertexPattern = (Core.Name "hydra.pg.query.VertexPattern")

_VertexPattern_variable = (Core.Name "variable")

_VertexPattern_label = (Core.Name "label")

_VertexPattern_properties = (Core.Name "properties")

_VertexPattern_edges = (Core.Name "edges")
