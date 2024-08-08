-- | A model for property graph mapping specifications. See https://github.com/CategoricalData/hydra/wiki/Property-graphs

module Hydra.Langs.Tinkerpop.Mappings where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Langs.Tinkerpop.PropertyGraph as PropertyGraph
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | Configurable annotation keys for property graph mapping specifications
data AnnotationSchema = 
  AnnotationSchema {
    annotationSchemaVertexLabel :: String,
    annotationSchemaEdgeLabel :: String,
    annotationSchemaVertexId :: String,
    annotationSchemaEdgeId :: String,
    annotationSchemaPropertyKey :: String,
    annotationSchemaPropertyValue :: String,
    annotationSchemaOutVertex :: String,
    annotationSchemaOutVertexLabel :: String,
    annotationSchemaInVertex :: String,
    annotationSchemaInVertexLabel :: String,
    annotationSchemaOutEdge :: String,
    annotationSchemaOutEdgeLabel :: String,
    annotationSchemaInEdge :: String,
    annotationSchemaInEdgeLabel :: String,
    annotationSchemaIgnore :: String}
  deriving (Eq, Ord, Read, Show)

_AnnotationSchema = (Core.Name "hydra/langs/tinkerpop/mappings.AnnotationSchema")

_AnnotationSchema_vertexLabel = (Core.Name "vertexLabel")

_AnnotationSchema_edgeLabel = (Core.Name "edgeLabel")

_AnnotationSchema_vertexId = (Core.Name "vertexId")

_AnnotationSchema_edgeId = (Core.Name "edgeId")

_AnnotationSchema_propertyKey = (Core.Name "propertyKey")

_AnnotationSchema_propertyValue = (Core.Name "propertyValue")

_AnnotationSchema_outVertex = (Core.Name "outVertex")

_AnnotationSchema_outVertexLabel = (Core.Name "outVertexLabel")

_AnnotationSchema_inVertex = (Core.Name "inVertex")

_AnnotationSchema_inVertexLabel = (Core.Name "inVertexLabel")

_AnnotationSchema_outEdge = (Core.Name "outEdge")

_AnnotationSchema_outEdgeLabel = (Core.Name "outEdgeLabel")

_AnnotationSchema_inEdge = (Core.Name "inEdge")

_AnnotationSchema_inEdgeLabel = (Core.Name "inEdgeLabel")

_AnnotationSchema_ignore = (Core.Name "ignore")

_AnnotationSchema_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/mappings.AnnotationSchema"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "vertexLabel"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "edgeLabel"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "vertexId"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "edgeId"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "propertyKey"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "propertyValue"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "outVertex"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "outVertexLabel"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inVertex"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inVertexLabel"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "outEdge"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "outEdgeLabel"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inEdge"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inEdgeLabel"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ignore"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

-- | A mapping specification producing edges of a specified label.
data EdgeSpec = 
  EdgeSpec {
    -- | The label of the target edges, which must conform to the edge type associated with that label.
    edgeSpecLabel :: PropertyGraph.EdgeLabel,
    -- | A specification of the id of each target edge
    edgeSpecId :: ValueSpec,
    -- | A specification of the out-vertex reference of each target edge
    edgeSpecOut :: ValueSpec,
    -- | A specification of the in-vertex reference of each target edge
    edgeSpecIn :: ValueSpec,
    -- | Zero or more property specifications for each target edge
    edgeSpecProperties :: [PropertySpec]}
  deriving (Eq, Ord, Read, Show)

_EdgeSpec = (Core.Name "hydra/langs/tinkerpop/mappings.EdgeSpec")

_EdgeSpec_label = (Core.Name "label")

_EdgeSpec_id = (Core.Name "id")

_EdgeSpec_out = (Core.Name "out")

_EdgeSpec_in = (Core.Name "in")

_EdgeSpec_properties = (Core.Name "properties")

_EdgeSpec_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/mappings.EdgeSpec"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "label"),
      Core.fieldTypeType = PropertyGraph._EdgeLabel_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "id"),
      Core.fieldTypeType = _ValueSpec_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "out"),
      Core.fieldTypeType = _ValueSpec_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "in"),
      Core.fieldTypeType = _ValueSpec_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "properties"),
      Core.fieldTypeType = (Core.TypeList _PropertySpec_type_)}]}))

-- | Either a vertex specification or an edge specification
data ElementSpec = 
  ElementSpecVertex VertexSpec |
  ElementSpecEdge EdgeSpec
  deriving (Eq, Ord, Read, Show)

_ElementSpec = (Core.Name "hydra/langs/tinkerpop/mappings.ElementSpec")

_ElementSpec_vertex = (Core.Name "vertex")

_ElementSpec_edge = (Core.Name "edge")

_ElementSpec_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/mappings.ElementSpec"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "vertex"),
      Core.fieldTypeType = _VertexSpec_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "edge"),
      Core.fieldTypeType = _EdgeSpec_type_}]}))

-- | A mapping specification producing properties of a specified key, and values of the appropriate type.
data PropertySpec = 
  PropertySpec {
    -- | The key of the target properties
    propertySpecKey :: PropertyGraph.PropertyKey,
    -- | A specification of the value of each target property, which must conform to the type associated with the property key
    propertySpecValue :: ValueSpec}
  deriving (Eq, Ord, Read, Show)

_PropertySpec = (Core.Name "hydra/langs/tinkerpop/mappings.PropertySpec")

_PropertySpec_key = (Core.Name "key")

_PropertySpec_value = (Core.Name "value")

_PropertySpec_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/mappings.PropertySpec"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = PropertyGraph._PropertyKey_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _ValueSpec_type_}]}))

-- | A set of mappings which translates between Hydra terms and annotations, and application-specific property graph types
data Schema s t v = 
  Schema {
    schemaVertexIdTypes :: (Compute.Coder s s Core.Type t),
    schemaVertexIds :: (Compute.Coder s s Core.Term v),
    schemaEdgeIdTypes :: (Compute.Coder s s Core.Type t),
    schemaEdgeIds :: (Compute.Coder s s Core.Term v),
    schemaPropertyTypes :: (Compute.Coder s s Core.Type t),
    schemaPropertyValues :: (Compute.Coder s s Core.Term v),
    schemaAnnotations :: AnnotationSchema,
    schemaDefaultVertexId :: v,
    schemaDefaultEdgeId :: v}

_Schema = (Core.Name "hydra/langs/tinkerpop/mappings.Schema")

_Schema_vertexIdTypes = (Core.Name "vertexIdTypes")

_Schema_vertexIds = (Core.Name "vertexIds")

_Schema_edgeIdTypes = (Core.Name "edgeIdTypes")

_Schema_edgeIds = (Core.Name "edgeIds")

_Schema_propertyTypes = (Core.Name "propertyTypes")

_Schema_propertyValues = (Core.Name "propertyValues")

_Schema_annotations = (Core.Name "annotations")

_Schema_defaultVertexId = (Core.Name "defaultVertexId")

_Schema_defaultEdgeId = (Core.Name "defaultEdgeId")

_Schema_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "s"),
  Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
    Core.lambdaTypeParameter = (Core.Name "t"),
    Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
      Core.lambdaTypeParameter = (Core.Name "v"),
      Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/mappings.Schema"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "vertexIdTypes"),
            Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = Compute._Coder_type_,
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                Core.applicationTypeArgument = Core._Type_type_})),
              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "vertexIds"),
            Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = Compute._Coder_type_,
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                Core.applicationTypeArgument = Core._Term_type_})),
              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "edgeIdTypes"),
            Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = Compute._Coder_type_,
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                Core.applicationTypeArgument = Core._Type_type_})),
              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "edgeIds"),
            Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = Compute._Coder_type_,
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                Core.applicationTypeArgument = Core._Term_type_})),
              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "propertyTypes"),
            Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = Compute._Coder_type_,
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                Core.applicationTypeArgument = Core._Type_type_})),
              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "propertyValues"),
            Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = Compute._Coder_type_,
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                Core.applicationTypeArgument = Core._Term_type_})),
              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "annotations"),
            Core.fieldTypeType = _AnnotationSchema_type_},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "defaultVertexId"),
            Core.fieldTypeType = (Core.TypeVariable (Core.Name "v"))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "defaultEdgeId"),
            Core.fieldTypeType = (Core.TypeVariable (Core.Name "v"))}]}))}))}))}))

-- | A mapping specification producing values (usually literal values) whose type is understood in context
data ValueSpec = 
  -- | A trivial no-op specification which passes the entire value
  ValueSpecValue  |
  -- | A compact path representing the function, e.g. engine-${engineInfo/model/name}
  ValueSpecPattern String
  deriving (Eq, Ord, Read, Show)

_ValueSpec = (Core.Name "hydra/langs/tinkerpop/mappings.ValueSpec")

_ValueSpec_value = (Core.Name "value")

_ValueSpec_pattern = (Core.Name "pattern")

_ValueSpec_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/mappings.ValueSpec"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

-- | A mapping specification producing vertices of a specified label
data VertexSpec = 
  VertexSpec {
    -- | The label of the target vertices, which must conform to the vertex type associated with that label.
    vertexSpecLabel :: PropertyGraph.VertexLabel,
    -- | A specification of the id of each target vertex
    vertexSpecId :: ValueSpec,
    -- | Zero or more property specifications for each target vertex
    vertexSpecProperties :: [PropertySpec]}
  deriving (Eq, Ord, Read, Show)

_VertexSpec = (Core.Name "hydra/langs/tinkerpop/mappings.VertexSpec")

_VertexSpec_label = (Core.Name "label")

_VertexSpec_id = (Core.Name "id")

_VertexSpec_properties = (Core.Name "properties")

_VertexSpec_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/mappings.VertexSpec"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "label"),
      Core.fieldTypeType = PropertyGraph._VertexLabel_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "id"),
      Core.fieldTypeType = _ValueSpec_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "properties"),
      Core.fieldTypeType = (Core.TypeList _PropertySpec_type_)}]}))