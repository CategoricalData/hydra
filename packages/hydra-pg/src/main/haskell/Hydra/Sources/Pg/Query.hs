module Hydra.Sources.Pg.Query where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
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
      binaryBooleanOperator,
      binaryExpression,
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
  doc "An aggregation query over a property graph" $
  T.union [
    "count">: T.unit]

applicationQuery :: TypeDefinition
applicationQuery = define "ApplicationQuery" $
  doc "A non-empty sequence of queries applied in order" $
  T.wrap $ nonemptyList $ q "Query"

associativeExpression :: TypeDefinition
associativeExpression = define "AssociativeExpression" $
  doc "An associative expression: a binary operator applied to a non-empty list of operands" $
  T.record [
    "operator">: q "BinaryOperator",
    "operands">: nonemptyList $ q "Expression"]

binaryBooleanOperator :: TypeDefinition
binaryBooleanOperator = define "BinaryBooleanOperator" $
  doc "A binary boolean operator: and, or, or xor" $
  T.enum ["and", "or", "xor"]

binaryExpression :: TypeDefinition
binaryExpression = define "BinaryExpression" $
  doc "A binary expression: a left operand, an operator, and a right operand" $
  T.record [
    "left">: q "Expression",
    "operator">: q "BinaryOperator",
    "right">: q "Expression"]

binaryOperator :: TypeDefinition
binaryOperator = define "BinaryOperator" $
  doc "A binary operator: boolean, comparison, or exponentiation" $
  T.union [
    "boolean">: q "BinaryBooleanOperator",
    "comparison">: q "ComparisonOperator",
    "power">: T.unit]

binding :: TypeDefinition
binding = define "Binding" $
  doc "A variable bound to the result of a query" $
  T.record [
    "key">: q "Variable",
    "value">: q "Query"]

comparisonOperator :: TypeDefinition
comparisonOperator = define "ComparisonOperator" $
  doc "A comparison operator: equal, not-equal, or an ordering relation" $
  T.enum ["eq", "neq", "lt", "lte", "gt", "gte"]

edgeProjectionPattern :: TypeDefinition
edgeProjectionPattern = define "EdgeProjectionPattern" $
  doc "A pattern matching an edge, its direction, optional label, properties, and adjacent vertex" $
  T.record [
    "direction">: pg "Direction",
    "label">: T.optional $ pg "EdgeLabel",
    "properties">: T.list $ q "PropertyPattern",
    "vertex">: T.optional $ q "VertexPattern"]

expression :: TypeDefinition
expression = define "Expression" $
  doc "An expression in a property graph query" $
  T.union [
    "associative">: q "AssociativeExpression",
    "binary">: q "BinaryExpression",
    "property">: q "PropertyProjection",
    "unary">: q "UnaryExpression",
    "variable">: q "Variable",
    "vertex">: q "VertexPattern"]

letQuery :: TypeDefinition
letQuery = define "LetQuery" $
  doc "A let query: variable bindings evaluated in an environment query" $
  T.record [
    "bindings">: T.list $ q "Binding",
    "environment">: q "Query"]

matchQuery :: TypeDefinition
matchQuery = define "MatchQuery" $
  doc "A pattern-matching query, with optional matching semantics and a filter condition" $
  T.record [
    "optional">: T.boolean,
    "pattern">: T.list $ q "Projection",
    "where">: T.optional $ q "Expression"]

pg :: String -> Type
pg = typeref $ PgModel.ns

projection :: TypeDefinition
projection = define "Projection" $
  doc "A projected expression, optionally bound to a variable" $
  T.record [
    "value">: q "Expression",
    "as">: T.optional $ q "Variable"]

projections :: TypeDefinition
projections = define "Projections" $
  doc "A set of projections, optionally including all fields" $
  T.record [
    "all">: T.boolean,
    "explicit">: T.list $ q "Projection"]

propertyPattern :: TypeDefinition
propertyPattern = define "PropertyPattern" $
  doc "A pattern matching a property by key against a value pattern" $
  T.record [
    "key">: pg "PropertyKey",
    "value">: q "PropertyValuePattern"]

propertyProjection :: TypeDefinition
propertyProjection = define "PropertyProjection" $
  doc "A projection of a property, by key, from a base expression" $
  T.record [
    "base">: q "Expression",
    "key">: pg "PropertyKey"]

-- TODO: temporary
propertyValue :: TypeDefinition
propertyValue = define "PropertyValue" $
  doc "A property value" $
  T.wrap T.string

propertyValuePattern :: TypeDefinition
propertyValuePattern = define "PropertyValuePattern" $
  doc "A pattern matching a property value: a variable binding or a literal value" $
  T.union [
    "variable">: pg "PropertyKey",
    "value">: T.string] -- TODO: re-use pg property value parameter

q :: String -> Type
q = typeref ns

query :: TypeDefinition
query = define "Query" $
  doc "A property graph query" $
  T.union [
    "application">: q "ApplicationQuery",
    "aggregate">: q "AggregationQuery",
    "LetQuery">: q "LetQuery",
    "match">: q "MatchQuery",
    "select">: q "SelectQuery",
    "value">: T.string]

selectQuery :: TypeDefinition
selectQuery = define "SelectQuery" $
  doc "A select query, with optional distinctness and a set of projections" $
  T.record [
    "distinct">: T.boolean,
    "projection">: q "Projections"]

unaryExpression :: TypeDefinition
unaryExpression = define "UnaryExpression" $
  doc "A unary expression: an operator applied to a single operand" $
  T.record [
    "operator">: q "UnaryOperator",
    "operand">: q "Expression"]

unaryOperator :: TypeDefinition
unaryOperator = define "UnaryOperator" $
  doc "A unary operator: negation" $
  T.enum ["negate"]

variable :: TypeDefinition
variable = define "Variable" $
  doc "A query variable name" $
  T.wrap T.string

vertexPattern :: TypeDefinition
vertexPattern = define "VertexPattern" $
  doc "A pattern matching a vertex, its optional binding variable and label, properties, and adjacent edges" $
  T.record [
    "variable">: T.optional $ q "Variable",
    "label">: T.optional $ pg "VertexLabel",
    "properties">: T.list $ q "PropertyPattern",
    "edges">: T.list $ q "EdgeProjectionPattern"]
