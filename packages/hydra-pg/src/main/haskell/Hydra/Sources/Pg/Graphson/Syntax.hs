module Hydra.Sources.Pg.Graphson.Syntax where

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


ns :: ModuleName
ns = ModuleName "hydra.pg.graphson.syntax"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A syntax model for TinkerPop's GraphSON format."
      ++ " This model is designed to be as inclusive as possible, supporting GraphSON 4.0 as well as earlier versions."
      ++ " See https://github.com/apache/tinkerpop/blob/master/docs/src/dev/io/graphson.asciidoc."))}
  where
    definitions = [
      adjacentEdge,
      bigDecimalValue,
      compositeTypedValue,
      dateTime,
      doubleValue,
      duration,
      edgeLabel,
      floatValue,
      map_,
      primitiveTypedValue,
      propertyKey,
      typeName_,
      uuid_,
      value_,
      valuePair,
      vertex,
      vertexLabel,
      vertexPropertyValue]

adjacentEdge :: TypeDefinition
adjacentEdge = define "AdjacentEdge" $
  doc "A GraphSON edge as referenced from an adjacent vertex, with its own properties" $
  T.record [
    "id">: gson "Value",
    "vertexId">: gson "Value",
    "properties">: T.map (gson "PropertyKey") (gson "Value")]

bigDecimalValue :: TypeDefinition
bigDecimalValue = define "BigDecimalValue" $
  doc "A GraphSON big-decimal value, represented as a string" $
  T.wrap T.string

compositeTypedValue :: TypeDefinition
compositeTypedValue = define "CompositeTypedValue" $
  doc "A GraphSON composite value tagged with a GraphSON type name" $
  T.record [
    "type">: gson "TypeName",
    "fields">: gson "Map"]

dateTime :: TypeDefinition
dateTime = define "DateTime" $
  doc "A GraphSON date-time value, represented as a string" $
  T.wrap T.string

doubleValue :: TypeDefinition
doubleValue = define "DoubleValue" $
  doc "A GraphSON double-precision floating-point value, including IEEE special values" $
  T.union [
    "finite">: T.float64,
    "infinity">: T.unit,
    "negativeInfinity">: T.unit,
    "notANumber">: T.unit]

duration :: TypeDefinition
duration = define "Duration" $
  doc "A GraphSON duration value, represented as a string" $
  T.wrap T.string

edgeLabel :: TypeDefinition
edgeLabel = define "EdgeLabel" $
  doc "A GraphSON edge label, represented as a string" $
  T.wrap T.string

floatValue :: TypeDefinition
floatValue = define "FloatValue" $
  doc "A GraphSON single-precision floating-point value, including IEEE special values" $
  T.union [
    "finite">: T.float32,
    "infinity">: T.unit,
    "negativeInfinity">: T.unit,
    "notANumber">: T.unit]

gson :: String -> Type
gson = typeref ns

map_ :: TypeDefinition
map_ = define "Map" $
  doc "A GraphSON map, represented as a list of key/value pairs" $
  T.wrap $ T.list $ gson "ValuePair"

primitiveTypedValue :: TypeDefinition
primitiveTypedValue = define "PrimitiveTypedValue" $
  doc "A GraphSON primitive value tagged with a GraphSON type name" $
  T.record [
    "type">: gson "TypeName",
    "value">: T.string]

propertyKey :: TypeDefinition
propertyKey = define "PropertyKey" $
  doc "A GraphSON property key, represented as a string" $
  T.wrap T.string

typeName_ :: TypeDefinition
typeName_ = define "TypeName" $
  doc "A GraphSON type name, represented as a string" $
  T.wrap T.string

uuid_ :: TypeDefinition
uuid_ = define "Uuid" $
  doc "A GraphSON UUID value, represented as a string" $
  T.wrap T.string

valuePair :: TypeDefinition
valuePair = define "ValuePair" $
  doc "A key/value pair in a GraphSON map" $
  T.record [
    "first">: gson "Value",
    "second">: gson "Value"]

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
value_ :: TypeDefinition
value_ = define "Value" $
  doc "A GraphSON value, covering the subset of GraphSON value types this model supports" $
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

vertex :: TypeDefinition
vertex = define "Vertex" $
  doc "A GraphSON vertex, with its incident edges and properties" $
  T.record [
    "id">: gson "Value",
    "label">: T.optional $ gson "VertexLabel",
    "inEdges">: T.map (gson "EdgeLabel") (nonemptyList $ gson "AdjacentEdge"),
    "outEdges">: T.map (gson "EdgeLabel") (nonemptyList $ gson "AdjacentEdge"),
    "properties">: T.map (gson "PropertyKey") (nonemptyList $ gson "VertexPropertyValue")]

vertexLabel :: TypeDefinition
vertexLabel = define "VertexLabel" $
  doc "A GraphSON vertex label, represented as a string" $
  T.wrap T.string

vertexPropertyValue :: TypeDefinition
vertexPropertyValue = define "VertexPropertyValue" $
  doc "A GraphSON vertex property value, with an optional property id" $
  T.record [
    "id">: T.optional $ gson "Value",
    "value">: gson "Value"]
