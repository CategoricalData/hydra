module Hydra.Ext.Sources.Pg.Graphson.Syntax where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.pg.graphson.syntax"

define :: String -> Type -> Binding
define = defineType ns

gson :: String -> Type
gson = typeref ns

module_ :: Module
module_ = Module ns elements [] [Core.ns] $
    Just ("A syntax model for TinkerPop's GraphSON format."
      ++ " This model is designed to be as inclusive as possible, supporting GraphSON 4.0 as well as earlier versions."
      ++ " See https://github.com/apache/tinkerpop/blob/master/docs/src/dev/io/graphson.asciidoc.")
  where
    elements = [
      bigDecimalValue,
      compositeTypedValue,
      dateTime,
      doubleValue,
      duration,
      edgeLabel,
      floatValue,
      map_,
      adjacentEdge,
      primitiveTypedValue,
      propertyKey,
      typeName_,
      uuid_,
      value_,
      valuePair,
      vertex,
      vertexLabel,
      vertexPropertyValue]

bigDecimalValue :: Binding
bigDecimalValue = define "BigDecimalValue" $
  T.wrap T.string

compositeTypedValue :: Binding
compositeTypedValue = define "CompositeTypedValue" $
  T.record [
    "type">: gson "TypeName",
    "fields">: gson "Map"]

dateTime :: Binding
dateTime = define "DateTime" $
  T.wrap T.string

doubleValue :: Binding
doubleValue = define "DoubleValue" $
  T.union [
    "finite">: T.float64,
    "infinity">: T.unit,
    "negativeInfinity">: T.unit,
    "notANumber">: T.unit]

duration :: Binding
duration = define "Duration" $
  T.wrap T.string

edgeLabel :: Binding
edgeLabel = define "EdgeLabel" $
  T.wrap T.string

floatValue :: Binding
floatValue = define "FloatValue" $
  T.union [
    "finite">: T.float32,
    "infinity">: T.unit,
    "negativeInfinity">: T.unit,
    "notANumber">: T.unit]

map_ :: Binding
map_ = define "Map" $
  T.wrap $ T.list $ gson "ValuePair"

adjacentEdge :: Binding
adjacentEdge = define "AdjacentEdge" $
  T.record [
    "id">: gson "Value",
    "vertexId">: gson "Value",
    "properties">: T.map (gson "PropertyKey") (gson "Value")]

primitiveTypedValue :: Binding
primitiveTypedValue = define "PrimitiveTypedValue" $
  T.record [
    "type">: gson "TypeName",
    "value">: T.string]

propertyKey :: Binding
propertyKey = define "PropertyKey" $
  T.wrap T.string

typeName_ :: Binding
typeName_ = define "TypeName" $
  T.wrap T.string

uuid_ :: Binding
uuid_ = define "Uuid" $
  T.wrap T.string

-- Note: the following are currently unsupported as values:
--   * BulkSet
--   * Direction
--   * Edge
--   * Error Result
--   * Graph
--   * Path
--   * Property
--   * Standard Request
--   * Standard Result
--   * T (enum value)
--   * Tree
--   * Vertex
--   * VertexProperty
value_ :: Binding
value_ = define "Value" $
  T.union [
    "bigDecimal">: gson "BigDecimalValue",
    "bigInteger">: T.bigint,
    "binary">: T.string,  -- Binary data is base64-encoded as a string in GraphSON
    "boolean">: T.boolean,
    "byte">: T.uint8,
    "char">: T.uint32,
    "composite">: gson "CompositeTypedValue",
    "dateTime">: gson "DateTime",
    "double">: gson "DoubleValue",
    "duration">: gson "Duration",
    "float">: gson "FloatValue",
    "integer">: T.int32,
    "list">: T.list $ gson "Value",
    "long">: T.int64,
    "map">: gson "Map",
    "null">: T.unit,
    "primitive">: gson "PrimitiveTypedValue",
    "set">: T.list $ gson "Value",
    "short">: T.int16,
    "string">: T.string,
    "uuid">: gson "Uuid"]

valuePair :: Binding
valuePair = define "ValuePair" $
  T.record [
    "first">: gson "Value",
    "second">: gson "Value"]

vertex :: Binding
vertex = define "Vertex" $
  T.record [
    "id">: gson "Value",
    "label">: T.optional $ gson "VertexLabel",
    "inEdges">: T.map (gson "EdgeLabel") (nonemptyList $ gson "AdjacentEdge"),
    "outEdges">: T.map (gson "EdgeLabel") (nonemptyList $ gson "AdjacentEdge"),
    "properties">: T.map (gson "PropertyKey") (nonemptyList $ gson "VertexPropertyValue")]

vertexLabel :: Binding
vertexLabel = define "VertexLabel" $
  T.wrap T.string

vertexPropertyValue :: Binding
vertexPropertyValue = define "VertexPropertyValue" $
  T.record [
    "id">: T.optional $ gson "Value",
    "value">: gson "Value"]
