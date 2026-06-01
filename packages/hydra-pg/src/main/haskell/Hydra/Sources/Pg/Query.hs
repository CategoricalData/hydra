module Hydra.Sources.Pg.Query where

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
import qualified Hydra.Sources.Pg.Model      as PgModel


ns :: ModuleName
ns = ModuleName "hydra.pg.query"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [PgModel.ns, Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A common model for pattern-matching queries over property graphs"))}
  where
    definitions = [
      aggregationQuery,
      applicationQuery,
      associativeExpression,
      binaryExpression,
      binaryBooleanOperator,
      binaryOperator,
      binding,
      comparisonOperator,
      edgeProjectionPattern,
      expression,
      letQuery,
      matchQuery,
      projection,
      projections,
      propertyPattern,
      propertyProjection,
      propertyValue,
      propertyValuePattern,
      query,
      selectQuery,
      unaryExpression,
      unaryOperator,
      variable,
      vertexPattern]

aggregationQuery :: TypeDefinition
aggregationQuery = define "AggregationQuery" $
  T.union [
    "count">: T.unit]

applicationQuery :: TypeDefinition
applicationQuery = define "ApplicationQuery" $
  T.wrap $ nonemptyList $ q "Query"

associativeExpression :: TypeDefinition
associativeExpression = define "AssociativeExpression" $
  T.record [
    "operator">: q "BinaryOperator",
    "operands">: nonemptyList $ q "Expression"]

binaryBooleanOperator :: TypeDefinition
binaryBooleanOperator = define "BinaryBooleanOperator" $
  T.enum ["and", "or", "xor"]

binaryExpression :: TypeDefinition
binaryExpression = define "BinaryExpression" $
  T.record [
    "left">: q "Expression",
    "operator">: q "BinaryOperator",
    "right">: q "Expression"]

binaryOperator :: TypeDefinition
binaryOperator = define "BinaryOperator" $
  T.union [
    "boolean">: q "BinaryBooleanOperator",
    "comparison">: q "ComparisonOperator",
    "power">: T.unit]

binding :: TypeDefinition
binding = define "Binding" $
  T.record [
    "key">: q "Variable",
    "value">: q "Query"]

comparisonOperator :: TypeDefinition
comparisonOperator = define "ComparisonOperator" $
  T.enum ["eq", "neq", "lt", "lte", "gt", "gte"]

edgeProjectionPattern :: TypeDefinition
edgeProjectionPattern = define "EdgeProjectionPattern" $
  T.record [
    "direction">: pg "Direction",
    "label">: T.maybe $ pg "EdgeLabel",
    "properties">: T.list $ q "PropertyPattern",
    "vertex">: T.maybe $ q "VertexPattern"]

expression :: TypeDefinition
expression = define "Expression" $
  T.union [
    "associative">: q "AssociativeExpression",
    "binary">: q "BinaryExpression",
    "property">: q "PropertyProjection",
    "unary">: q "UnaryExpression",
    "variable">: q "Variable",
    "vertex">: q "VertexPattern"]

letQuery :: TypeDefinition
letQuery = define "LetQuery" $
  T.record [
    "bindings">: T.list $ q "Binding",
    "environment">: q "Query"]

matchQuery :: TypeDefinition
matchQuery = define "MatchQuery" $
  T.record [
    "optional">: T.boolean,
    "pattern">: T.list $ q "Projection",
    "where">: T.maybe $ q "Expression"]

pg :: String -> Type
pg = typeref $ PgModel.ns

projection :: TypeDefinition
projection = define "Projection" $
  T.record [
    "value">: q "Expression",
    "as">: T.maybe $ q "Variable"]

projections :: TypeDefinition
projections = define "Projections" $
  T.record [
    "all">: T.boolean,
    "explicit">: T.list $ q "Projection"]

propertyPattern :: TypeDefinition
propertyPattern = define "PropertyPattern" $
  T.record [
    "key">: pg "PropertyKey",
    "value">: q "PropertyValuePattern"]

propertyProjection :: TypeDefinition
propertyProjection = define "PropertyProjection" $
  T.record [
    "base">: q "Expression",
    "key">: pg "PropertyKey"]

-- TODO: temporary
propertyValue :: TypeDefinition
propertyValue = define "PropertyValue" $ T.wrap T.string

propertyValuePattern :: TypeDefinition
propertyValuePattern = define "PropertyValuePattern" $
  T.union [
    "variable">: pg "PropertyKey",
    "value">: T.string] -- TODO: re-use pg property value parameter

q :: String -> Type
q = typeref ns

query :: TypeDefinition
query = define "Query" $
  T.union [
    "application">: q "ApplicationQuery",
    "aggregate">: q "AggregationQuery",
    "LetQuery">: q "LetQuery",
    "match">: q "MatchQuery",
    "select">: q "SelectQuery",
    "value">: T.string]

selectQuery :: TypeDefinition
selectQuery = define "SelectQuery" $
  T.record [
    "distinct">: T.boolean,
    "projection">: q "Projections"]

unaryExpression :: TypeDefinition
unaryExpression = define "UnaryExpression" $
  T.record [
    "operator">: q "UnaryOperator",
    "operand">: q "Expression"]

unaryOperator :: TypeDefinition
unaryOperator = define "UnaryOperator" $
  T.enum ["negate"]

variable :: TypeDefinition
variable = define "Variable" $ T.wrap T.string

vertexPattern :: TypeDefinition
vertexPattern = define "VertexPattern" $
  T.record [
    "variable">: T.maybe $ q "Variable",
    "label">: T.maybe $ pg "VertexLabel",
    "properties">: T.list $ q "PropertyPattern",
    "edges">: T.list $ q "EdgeProjectionPattern"]
