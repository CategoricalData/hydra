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

_AggregationQuery_count = (Core.Name "count")

_AggregationQuery_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.AggregationQuery"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "count"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype ApplicationQuery = 
  ApplicationQuery {
    unApplicationQuery :: [Query]}
  deriving (Eq, Ord, Read, Show)

_ApplicationQuery = (Core.Name "hydra/langs/tinkerpop/queries.ApplicationQuery")

_ApplicationQuery_type_ = (Core.TypeList _Query_type_)

data AssociativeExpression = 
  AssociativeExpression {
    associativeExpressionOperator :: BinaryOperator,
    associativeExpressionOperands :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_AssociativeExpression = (Core.Name "hydra/langs/tinkerpop/queries.AssociativeExpression")

_AssociativeExpression_operator = (Core.Name "operator")

_AssociativeExpression_operands = (Core.Name "operands")

_AssociativeExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.AssociativeExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _BinaryOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operands"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)}]}))

data BinaryExpression = 
  BinaryExpression {
    binaryExpressionLeft :: Expression,
    binaryExpressionOperator :: BinaryOperator,
    binaryExpressionRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_BinaryExpression = (Core.Name "hydra/langs/tinkerpop/queries.BinaryExpression")

_BinaryExpression_left = (Core.Name "left")

_BinaryExpression_operator = (Core.Name "operator")

_BinaryExpression_right = (Core.Name "right")

_BinaryExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.BinaryExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _BinaryOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = _Expression_type_}]}))

data BinaryBooleanOperator = 
  BinaryBooleanOperatorAnd  |
  BinaryBooleanOperatorOr  |
  BinaryBooleanOperatorXor 
  deriving (Eq, Ord, Read, Show)

_BinaryBooleanOperator = (Core.Name "hydra/langs/tinkerpop/queries.BinaryBooleanOperator")

_BinaryBooleanOperator_and = (Core.Name "and")

_BinaryBooleanOperator_or = (Core.Name "or")

_BinaryBooleanOperator_xor = (Core.Name "xor")

_BinaryBooleanOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.BinaryBooleanOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "and"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "or"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "xor"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data BinaryOperator = 
  BinaryOperatorBoolean BinaryBooleanOperator |
  BinaryOperatorComparison ComparisonOperator |
  BinaryOperatorPower 
  deriving (Eq, Ord, Read, Show)

_BinaryOperator = (Core.Name "hydra/langs/tinkerpop/queries.BinaryOperator")

_BinaryOperator_boolean = (Core.Name "boolean")

_BinaryOperator_comparison = (Core.Name "comparison")

_BinaryOperator_power = (Core.Name "power")

_BinaryOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.BinaryOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = _BinaryBooleanOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "comparison"),
      Core.fieldTypeType = _ComparisonOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "power"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data Binding = 
  Binding {
    bindingKey :: Variable,
    bindingValue :: Query}
  deriving (Eq, Ord, Read, Show)

_Binding = (Core.Name "hydra/langs/tinkerpop/queries.Binding")

_Binding_key = (Core.Name "key")

_Binding_value = (Core.Name "value")

_Binding_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.Binding"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = _Variable_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _Query_type_}]}))

data ComparisonOperator = 
  ComparisonOperatorEq  |
  ComparisonOperatorNeq  |
  ComparisonOperatorLt  |
  ComparisonOperatorLte  |
  ComparisonOperatorGt  |
  ComparisonOperatorGte 
  deriving (Eq, Ord, Read, Show)

_ComparisonOperator = (Core.Name "hydra/langs/tinkerpop/queries.ComparisonOperator")

_ComparisonOperator_eq = (Core.Name "eq")

_ComparisonOperator_neq = (Core.Name "neq")

_ComparisonOperator_lt = (Core.Name "lt")

_ComparisonOperator_lte = (Core.Name "lte")

_ComparisonOperator_gt = (Core.Name "gt")

_ComparisonOperator_gte = (Core.Name "gte")

_ComparisonOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.ComparisonOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "eq"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "neq"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lt"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lte"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gt"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gte"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data EdgeProjectionPattern = 
  EdgeProjectionPattern {
    edgeProjectionPatternDirection :: PropertyGraph.Direction,
    edgeProjectionPatternLabel :: (Maybe PropertyGraph.EdgeLabel),
    edgeProjectionPatternProperties :: [PropertyPattern],
    edgeProjectionPatternVertex :: (Maybe VertexPattern)}
  deriving (Eq, Ord, Read, Show)

_EdgeProjectionPattern = (Core.Name "hydra/langs/tinkerpop/queries.EdgeProjectionPattern")

_EdgeProjectionPattern_direction = (Core.Name "direction")

_EdgeProjectionPattern_label = (Core.Name "label")

_EdgeProjectionPattern_properties = (Core.Name "properties")

_EdgeProjectionPattern_vertex = (Core.Name "vertex")

_EdgeProjectionPattern_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.EdgeProjectionPattern"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "direction"),
      Core.fieldTypeType = PropertyGraph._Direction_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "label"),
      Core.fieldTypeType = (Core.TypeOptional PropertyGraph._EdgeLabel_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "properties"),
      Core.fieldTypeType = (Core.TypeList _PropertyPattern_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "vertex"),
      Core.fieldTypeType = (Core.TypeOptional _VertexPattern_type_)}]}))

data Expression = 
  ExpressionAssociative AssociativeExpression |
  ExpressionBinary BinaryExpression |
  ExpressionProperty PropertyProjection |
  ExpressionUnary UnaryExpression |
  ExpressionVariable Variable |
  ExpressionVertex VertexPattern
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra/langs/tinkerpop/queries.Expression")

_Expression_associative = (Core.Name "associative")

_Expression_binary = (Core.Name "binary")

_Expression_property = (Core.Name "property")

_Expression_unary = (Core.Name "unary")

_Expression_variable = (Core.Name "variable")

_Expression_vertex = (Core.Name "vertex")

_Expression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.Expression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "associative"),
      Core.fieldTypeType = _AssociativeExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "binary"),
      Core.fieldTypeType = _BinaryExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _PropertyProjection_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unary"),
      Core.fieldTypeType = _UnaryExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Variable_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "vertex"),
      Core.fieldTypeType = _VertexPattern_type_}]}))

data LetQuery = 
  LetQuery {
    letQueryBindings :: [Binding],
    letQueryEnvironment :: Query}
  deriving (Eq, Ord, Read, Show)

_LetQuery = (Core.Name "hydra/langs/tinkerpop/queries.LetQuery")

_LetQuery_bindings = (Core.Name "bindings")

_LetQuery_environment = (Core.Name "environment")

_LetQuery_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.LetQuery"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bindings"),
      Core.fieldTypeType = (Core.TypeList _Binding_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "environment"),
      Core.fieldTypeType = _Query_type_}]}))

data MatchQuery = 
  MatchQuery {
    matchQueryOptional :: Bool,
    matchQueryPattern :: [Projection],
    matchQueryWhere :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_MatchQuery = (Core.Name "hydra/langs/tinkerpop/queries.MatchQuery")

_MatchQuery_optional = (Core.Name "optional")

_MatchQuery_pattern = (Core.Name "pattern")

_MatchQuery_where = (Core.Name "where")

_MatchQuery_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.MatchQuery"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "optional"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = (Core.TypeList _Projection_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "where"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)}]}))

data Projection = 
  Projection {
    projectionValue :: Expression,
    projectionAs :: (Maybe Variable)}
  deriving (Eq, Ord, Read, Show)

_Projection = (Core.Name "hydra/langs/tinkerpop/queries.Projection")

_Projection_value = (Core.Name "value")

_Projection_as = (Core.Name "as")

_Projection_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.Projection"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "as"),
      Core.fieldTypeType = (Core.TypeOptional _Variable_type_)}]}))

data Projections = 
  Projections {
    projectionsAll :: Bool,
    projectionsExplicit :: [Projection]}
  deriving (Eq, Ord, Read, Show)

_Projections = (Core.Name "hydra/langs/tinkerpop/queries.Projections")

_Projections_all = (Core.Name "all")

_Projections_explicit = (Core.Name "explicit")

_Projections_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.Projections"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "all"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "explicit"),
      Core.fieldTypeType = (Core.TypeList _Projection_type_)}]}))

data PropertyPattern = 
  PropertyPattern {
    propertyPatternKey :: PropertyGraph.PropertyKey,
    propertyPatternValue :: PropertyValuePattern}
  deriving (Eq, Ord, Read, Show)

_PropertyPattern = (Core.Name "hydra/langs/tinkerpop/queries.PropertyPattern")

_PropertyPattern_key = (Core.Name "key")

_PropertyPattern_value = (Core.Name "value")

_PropertyPattern_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.PropertyPattern"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = PropertyGraph._PropertyKey_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _PropertyValuePattern_type_}]}))

data PropertyProjection = 
  PropertyProjection {
    propertyProjectionBase :: Expression,
    propertyProjectionKey :: PropertyGraph.PropertyKey}
  deriving (Eq, Ord, Read, Show)

_PropertyProjection = (Core.Name "hydra/langs/tinkerpop/queries.PropertyProjection")

_PropertyProjection_base = (Core.Name "base")

_PropertyProjection_key = (Core.Name "key")

_PropertyProjection_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.PropertyProjection"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "base"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = PropertyGraph._PropertyKey_type_}]}))

newtype PropertyValue = 
  PropertyValue {
    unPropertyValue :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyValue = (Core.Name "hydra/langs/tinkerpop/queries.PropertyValue")

_PropertyValue_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data PropertyValuePattern = 
  PropertyValuePatternVariable PropertyGraph.PropertyKey |
  PropertyValuePatternValue String
  deriving (Eq, Ord, Read, Show)

_PropertyValuePattern = (Core.Name "hydra/langs/tinkerpop/queries.PropertyValuePattern")

_PropertyValuePattern_variable = (Core.Name "variable")

_PropertyValuePattern_value = (Core.Name "value")

_PropertyValuePattern_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.PropertyValuePattern"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = PropertyGraph._PropertyKey_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

data Query = 
  QueryApplication ApplicationQuery |
  QueryAggregate AggregationQuery |
  QueryLetQuery LetQuery |
  QueryMatch MatchQuery |
  QuerySelect SelectQuery |
  QueryValue String
  deriving (Eq, Ord, Read, Show)

_Query = (Core.Name "hydra/langs/tinkerpop/queries.Query")

_Query_application = (Core.Name "application")

_Query_aggregate = (Core.Name "aggregate")

_Query_LetQuery = (Core.Name "LetQuery")

_Query_match = (Core.Name "match")

_Query_select = (Core.Name "select")

_Query_value = (Core.Name "value")

_Query_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.Query"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "application"),
      Core.fieldTypeType = _ApplicationQuery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "aggregate"),
      Core.fieldTypeType = _AggregationQuery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "LetQuery"),
      Core.fieldTypeType = _LetQuery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "match"),
      Core.fieldTypeType = _MatchQuery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "select"),
      Core.fieldTypeType = _SelectQuery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

data SelectQuery = 
  SelectQuery {
    selectQueryDistinct :: Bool,
    selectQueryProjection :: Projections}
  deriving (Eq, Ord, Read, Show)

_SelectQuery = (Core.Name "hydra/langs/tinkerpop/queries.SelectQuery")

_SelectQuery_distinct = (Core.Name "distinct")

_SelectQuery_projection = (Core.Name "projection")

_SelectQuery_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.SelectQuery"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "distinct"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "projection"),
      Core.fieldTypeType = _Projections_type_}]}))

data UnaryExpression = 
  UnaryExpression {
    unaryExpressionOperator :: UnaryOperator,
    unaryExpressionOperand :: Expression}
  deriving (Eq, Ord, Read, Show)

_UnaryExpression = (Core.Name "hydra/langs/tinkerpop/queries.UnaryExpression")

_UnaryExpression_operator = (Core.Name "operator")

_UnaryExpression_operand = (Core.Name "operand")

_UnaryExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.UnaryExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _UnaryOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operand"),
      Core.fieldTypeType = _Expression_type_}]}))

data UnaryOperator = 
  UnaryOperatorNegate 
  deriving (Eq, Ord, Read, Show)

_UnaryOperator = (Core.Name "hydra/langs/tinkerpop/queries.UnaryOperator")

_UnaryOperator_negate = (Core.Name "negate")

_UnaryOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.UnaryOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "negate"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype Variable = 
  Variable {
    unVariable :: String}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra/langs/tinkerpop/queries.Variable")

_Variable_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data VertexPattern = 
  VertexPattern {
    vertexPatternVariable :: (Maybe Variable),
    vertexPatternLabel :: (Maybe PropertyGraph.VertexLabel),
    vertexPatternProperties :: [PropertyPattern],
    vertexPatternEdges :: [EdgeProjectionPattern]}
  deriving (Eq, Ord, Read, Show)

_VertexPattern = (Core.Name "hydra/langs/tinkerpop/queries.VertexPattern")

_VertexPattern_variable = (Core.Name "variable")

_VertexPattern_label = (Core.Name "label")

_VertexPattern_properties = (Core.Name "properties")

_VertexPattern_edges = (Core.Name "edges")

_VertexPattern_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/queries.VertexPattern"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = (Core.TypeOptional _Variable_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "label"),
      Core.fieldTypeType = (Core.TypeOptional PropertyGraph._VertexLabel_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "properties"),
      Core.fieldTypeType = (Core.TypeList _PropertyPattern_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "edges"),
      Core.fieldTypeType = (Core.TypeList _EdgeProjectionPattern_type_)}]}))