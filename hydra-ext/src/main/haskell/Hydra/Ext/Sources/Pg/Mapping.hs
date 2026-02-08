module Hydra.Ext.Sources.Pg.Mapping where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                    ((>:), (@@))
import qualified Hydra.Dsl.Types                    as T
import qualified Hydra.Sources.Kernel.Types.Core    as Core
import qualified Data.List                          as L
import qualified Data.Map                           as M
import qualified Data.Set                           as S
import qualified Data.Maybe                         as Y

-- Additional imports
import qualified Hydra.Sources.Kernel.Types.Compute as Compute
import qualified Hydra.Ext.Sources.Pg.Model         as PgModel


ns :: Namespace
ns = Namespace "hydra.pg.mapping"

define :: String -> Type -> Binding
define = defineType ns

mappings :: String -> Type
mappings = typeref ns

compute :: String -> Type
compute = typeref $ Compute.ns

core :: String -> Type
core = typeref $ Core.ns

v3 :: String -> Type
v3 = typeref $ PgModel.ns

module_ :: Module
module_ = Module ns elements
    [PgModel.ns, Core.ns, Compute.ns] [Core.ns] $
    Just "A model for property graph mapping specifications. See https://github.com/CategoricalData/hydra/wiki/Property-graphs"
  where
    elements = [
      annotationSchema,
      edgeSpec,
      elementSpec,
      propertySpec,
      schema,
      valueSpec,
      vertexSpec]

annotationSchema :: Binding
annotationSchema = define "AnnotationSchema" $
  doc "Configurable annotation keys for property graph mapping specifications" $
  T.record [
    "vertexLabel">: T.string,
    "edgeLabel">: T.string,
    "vertexId">: T.string,
    "edgeId">: T.string,
    "propertyKey">: T.string,
    "propertyValue">: T.string,
    "outVertex">: T.string,
    "outVertexLabel">: T.string,
    "inVertex">: T.string,
    "inVertexLabel">: T.string,
    "outEdge">: T.string,
    "outEdgeLabel">: T.string,
    "inEdge">: T.string,
    "inEdgeLabel">: T.string,
    "ignore">: T.string]

edgeSpec :: Binding
edgeSpec = define "EdgeSpec" $
  doc "A mapping specification producing edges of a specified label." $
  T.record [
    "label">:
      doc "The label of the target edges, which must conform to the edge type associated with that label." $
      v3 "EdgeLabel",
    "id">:
      doc "A specification of the id of each target edge" $
      mappings "ValueSpec",
    "out">:
      doc "A specification of the out-vertex reference of each target edge" $
      mappings "ValueSpec",
    "in">:
      doc "A specification of the in-vertex reference of each target edge" $
      mappings "ValueSpec",
    "properties">:
      doc "Zero or more property specifications for each target edge" $
      T.list $ mappings "PropertySpec"]

elementSpec :: Binding
elementSpec = define "ElementSpec" $
  doc "Either a vertex specification or an edge specification" $
  T.union [
    "vertex">: mappings "VertexSpec",
    "edge">: mappings "EdgeSpec"]

propertySpec :: Binding
propertySpec = define "PropertySpec" $
  doc "A mapping specification producing properties of a specified key, and values of the appropriate type." $
  T.record [
    "key">:
      doc "The key of the target properties" $
      v3 "PropertyKey",
    "value">:
      doc "A specification of the value of each target property, which must conform to the type associated with the property key" $
      mappings "ValueSpec"]

schema :: Binding
schema = define "Schema" $
  doc "A set of mappings which translates between Hydra terms and annotations, and application-specific property graph types" $
  T.forAlls ["s", "t", "v"] $
    T.record [
      "vertexIdTypes">: compute "Coder" @@ "s" @@ "s" @@ core "Type" @@ "t",
      "vertexIds">: compute "Coder" @@ "s" @@ "s" @@ core "Term" @@ "v",
      "edgeIdTypes">: compute "Coder" @@ "s" @@ "s" @@ core "Type" @@ "t",
      "edgeIds">: compute "Coder" @@ "s" @@ "s" @@ core "Term" @@ "v",
      "propertyTypes">: compute "Coder" @@ "s" @@ "s" @@ core "Type" @@ "t",
      "propertyValues">: compute "Coder" @@ "s" @@ "s" @@ core "Term" @@ "v",
      "annotations">: mappings "AnnotationSchema",
      "defaultVertexId">: "v",
      "defaultEdgeId">: "v"]

valueSpec :: Binding
valueSpec = define "ValueSpec" $
  doc "A mapping specification producing values (usually literal values) whose type is understood in context" $
  T.union [
    "value">:
      doc "A trivial no-op specification which passes the entire value"
      T.unit,
    "pattern">:
      doc "A compact path representing the function, e.g. engine-${engineInfo/model/name}"
      T.string]

vertexSpec :: Binding
vertexSpec = define "VertexSpec" $
  doc "A mapping specification producing vertices of a specified label" $
  T.record [
    "label">:
      doc "The label of the target vertices, which must conform to the vertex type associated with that label." $
      v3 "VertexLabel",
    "id">:
      doc "A specification of the id of each target vertex" $
      mappings "ValueSpec",
    "properties">:
      doc "Zero or more property specifications for each target vertex" $
      T.list $ mappings "PropertySpec"]
