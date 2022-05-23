module Hydra.Ext.Tinkerpop.Features where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

-- Base interface for features that relate to supporting different data types.
data DataTypeFeatures 
  = DataTypeFeatures {
    dataTypeFeaturesSupportsBooleanArrayValues :: Bool,
    dataTypeFeaturesSupportsBooleanValues :: Bool,
    dataTypeFeaturesSupportsByteArrayValues :: Bool,
    dataTypeFeaturesSupportsByteValues :: Bool,
    dataTypeFeaturesSupportsDoubleArrayValues :: Bool,
    dataTypeFeaturesSupportsDoubleValues :: Bool,
    dataTypeFeaturesSupportsFloatArrayValues :: Bool,
    dataTypeFeaturesSupportsFloatValues :: Bool,
    dataTypeFeaturesSupportsIntegerArrayValues :: Bool,
    dataTypeFeaturesSupportsIntegerValues :: Bool,
    dataTypeFeaturesSupportsLongArrayValues :: Bool,
    dataTypeFeaturesSupportsLongValues :: Bool,
    dataTypeFeaturesSupportsMapValues :: Bool,
    dataTypeFeaturesSupportsMixedListValues :: Bool,
    dataTypeFeaturesSupportsSerializableValues :: Bool,
    dataTypeFeaturesSupportsStringArrayValues :: Bool,
    dataTypeFeaturesSupportsStringValues :: Bool,
    dataTypeFeaturesSupportsUniformListValues :: Bool}
  deriving (Eq, Ord, Read, Show)

_DataTypeFeatures = (Core.Name "hydra/ext/tinkerpop/features.DataTypeFeatures")

_DataTypeFeatures_supportsBooleanArrayValues = (Core.FieldName "supportsBooleanArrayValues")

_DataTypeFeatures_supportsBooleanValues = (Core.FieldName "supportsBooleanValues")

_DataTypeFeatures_supportsByteArrayValues = (Core.FieldName "supportsByteArrayValues")

_DataTypeFeatures_supportsByteValues = (Core.FieldName "supportsByteValues")

_DataTypeFeatures_supportsDoubleArrayValues = (Core.FieldName "supportsDoubleArrayValues")

_DataTypeFeatures_supportsDoubleValues = (Core.FieldName "supportsDoubleValues")

_DataTypeFeatures_supportsFloatArrayValues = (Core.FieldName "supportsFloatArrayValues")

_DataTypeFeatures_supportsFloatValues = (Core.FieldName "supportsFloatValues")

_DataTypeFeatures_supportsIntegerArrayValues = (Core.FieldName "supportsIntegerArrayValues")

_DataTypeFeatures_supportsIntegerValues = (Core.FieldName "supportsIntegerValues")

_DataTypeFeatures_supportsLongArrayValues = (Core.FieldName "supportsLongArrayValues")

_DataTypeFeatures_supportsLongValues = (Core.FieldName "supportsLongValues")

_DataTypeFeatures_supportsMapValues = (Core.FieldName "supportsMapValues")

_DataTypeFeatures_supportsMixedListValues = (Core.FieldName "supportsMixedListValues")

_DataTypeFeatures_supportsSerializableValues = (Core.FieldName "supportsSerializableValues")

_DataTypeFeatures_supportsStringArrayValues = (Core.FieldName "supportsStringArrayValues")

_DataTypeFeatures_supportsStringValues = (Core.FieldName "supportsStringValues")

_DataTypeFeatures_supportsUniformListValues = (Core.FieldName "supportsUniformListValues")

-- Features that are related to Edge operations.
data EdgeFeatures 
  = EdgeFeatures {
    edgeFeaturesElementFeatures :: ElementFeatures,
    edgeFeaturesProperties :: EdgePropertyFeatures,
    edgeFeaturesSupportsAddEdges :: Bool,
    edgeFeaturesSupportsRemoveEdges :: Bool,
    edgeFeaturesSupportsUpsert :: Bool}
  deriving (Eq, Ord, Read, Show)

_EdgeFeatures = (Core.Name "hydra/ext/tinkerpop/features.EdgeFeatures")

_EdgeFeatures_elementFeatures = (Core.FieldName "elementFeatures")

_EdgeFeatures_properties = (Core.FieldName "properties")

_EdgeFeatures_supportsAddEdges = (Core.FieldName "supportsAddEdges")

_EdgeFeatures_supportsRemoveEdges = (Core.FieldName "supportsRemoveEdges")

_EdgeFeatures_supportsUpsert = (Core.FieldName "supportsUpsert")

-- Features that are related to Edge Property objects.
data EdgePropertyFeatures 
  = EdgePropertyFeatures {edgePropertyFeaturesPropertyFeatures :: PropertyFeatures}
  deriving (Eq, Ord, Read, Show)

_EdgePropertyFeatures = (Core.Name "hydra/ext/tinkerpop/features.EdgePropertyFeatures")

_EdgePropertyFeatures_propertyFeatures = (Core.FieldName "propertyFeatures")

-- Features that are related to Element objects.
data ElementFeatures 
  = ElementFeatures {
    elementFeaturesSupportsAddProperty :: Bool,
    elementFeaturesSupportsAnyIds :: Bool,
    elementFeaturesSupportsCustomIds :: Bool,
    elementFeaturesSupportsNumericIds :: Bool,
    elementFeaturesSupportsRemoveProperty :: Bool,
    elementFeaturesSupportsStringIds :: Bool,
    elementFeaturesSupportsUserSuppliedIds :: Bool,
    elementFeaturesSupportsUuidIds :: Bool}
  deriving (Eq, Ord, Read, Show)

_ElementFeatures = (Core.Name "hydra/ext/tinkerpop/features.ElementFeatures")

_ElementFeatures_supportsAddProperty = (Core.FieldName "supportsAddProperty")

_ElementFeatures_supportsAnyIds = (Core.FieldName "supportsAnyIds")

_ElementFeatures_supportsCustomIds = (Core.FieldName "supportsCustomIds")

_ElementFeatures_supportsNumericIds = (Core.FieldName "supportsNumericIds")

_ElementFeatures_supportsRemoveProperty = (Core.FieldName "supportsRemoveProperty")

_ElementFeatures_supportsStringIds = (Core.FieldName "supportsStringIds")

_ElementFeatures_supportsUserSuppliedIds = (Core.FieldName "supportsUserSuppliedIds")

_ElementFeatures_supportsUuidIds = (Core.FieldName "supportsUuidIds")

-- Additional features which are needed for the complete specification of language constaints in Hydra, above and beyond TinkerPop Graph.Features
data ExtraFeatures m 
  = ExtraFeatures {extraFeaturesSupportsMapKey :: (Core.Type m -> Bool)}

_ExtraFeatures = (Core.Name "hydra/ext/tinkerpop/features.ExtraFeatures")

_ExtraFeatures_supportsMapKey = (Core.FieldName "supportsMapKey")

-- An interface that represents the capabilities of a Graph implementation. By default all methods of features return true and it is up to implementers to disable feature they don't support. Users should check features prior to using various functions of TinkerPop to help ensure code portability across implementations. For example, a common usage would be to check if a graph supports transactions prior to calling the commit method on Graph.tx().
-- 
-- As an additional notice to Graph Providers, feature methods will be used by the test suite to determine which tests will be ignored and which will be executed, therefore proper setting of these features is essential to maximizing the amount of testing performed by the suite. Further note, that these methods may be called by the TinkerPop core code to determine what operations may be appropriately executed which will have impact on features utilized by users.
data Features 
  = Features {
    featuresEdge :: EdgeFeatures,
    featuresGraph :: GraphFeatures,
    featuresVertex :: VertexFeatures}
  deriving (Eq, Ord, Read, Show)

_Features = (Core.Name "hydra/ext/tinkerpop/features.Features")

_Features_edge = (Core.FieldName "edge")

_Features_graph = (Core.FieldName "graph")

_Features_vertex = (Core.FieldName "vertex")

-- Features specific to a operations of a graph.
data GraphFeatures 
  = GraphFeatures {
    graphFeaturesSupportsComputer :: Bool,
    graphFeaturesSupportsConcurrentAccess :: Bool,
    graphFeaturesSupportsIoRead :: Bool,
    graphFeaturesSupportsIoWrite :: Bool,
    graphFeaturesSupportsPersistence :: Bool,
    graphFeaturesSupportsThreadedTransactions :: Bool,
    graphFeaturesSupportsTransactions :: Bool,
    graphFeaturesVariables :: VariableFeatures}
  deriving (Eq, Ord, Read, Show)

_GraphFeatures = (Core.Name "hydra/ext/tinkerpop/features.GraphFeatures")

_GraphFeatures_supportsComputer = (Core.FieldName "supportsComputer")

_GraphFeatures_supportsConcurrentAccess = (Core.FieldName "supportsConcurrentAccess")

_GraphFeatures_supportsIoRead = (Core.FieldName "supportsIoRead")

_GraphFeatures_supportsIoWrite = (Core.FieldName "supportsIoWrite")

_GraphFeatures_supportsPersistence = (Core.FieldName "supportsPersistence")

_GraphFeatures_supportsThreadedTransactions = (Core.FieldName "supportsThreadedTransactions")

_GraphFeatures_supportsTransactions = (Core.FieldName "supportsTransactions")

_GraphFeatures_variables = (Core.FieldName "variables")

-- A base interface for Edge or Vertex Property features.
data PropertyFeatures 
  = PropertyFeatures {
    propertyFeaturesDataTypeFeatures :: DataTypeFeatures,
    propertyFeaturesSupportsProperties :: Bool}
  deriving (Eq, Ord, Read, Show)

_PropertyFeatures = (Core.Name "hydra/ext/tinkerpop/features.PropertyFeatures")

_PropertyFeatures_dataTypeFeatures = (Core.FieldName "dataTypeFeatures")

_PropertyFeatures_supportsProperties = (Core.FieldName "supportsProperties")

-- Features for Graph.Variables.
data VariableFeatures 
  = VariableFeatures {
    variableFeaturesDataTypeFeatures :: DataTypeFeatures,
    variableFeaturesSupportsVariables :: Bool}
  deriving (Eq, Ord, Read, Show)

_VariableFeatures = (Core.Name "hydra/ext/tinkerpop/features.VariableFeatures")

_VariableFeatures_dataTypeFeatures = (Core.FieldName "dataTypeFeatures")

_VariableFeatures_supportsVariables = (Core.FieldName "supportsVariables")

-- Features that are related to Vertex operations.
data VertexFeatures 
  = VertexFeatures {
    vertexFeaturesElementFeatures :: ElementFeatures,
    vertexFeaturesProperties :: VertexPropertyFeatures,
    vertexFeaturesSupportsAddVertices :: Bool,
    vertexFeaturesSupportsDuplicateMultiProperties :: Bool,
    vertexFeaturesSupportsMetaProperties :: Bool,
    vertexFeaturesSupportsMultiProperties :: Bool,
    vertexFeaturesSupportsRemoveVertices :: Bool,
    vertexFeaturesSupportsUpsert :: Bool}
  deriving (Eq, Ord, Read, Show)

_VertexFeatures = (Core.Name "hydra/ext/tinkerpop/features.VertexFeatures")

_VertexFeatures_elementFeatures = (Core.FieldName "elementFeatures")

_VertexFeatures_properties = (Core.FieldName "properties")

_VertexFeatures_supportsAddVertices = (Core.FieldName "supportsAddVertices")

_VertexFeatures_supportsDuplicateMultiProperties = (Core.FieldName "supportsDuplicateMultiProperties")

_VertexFeatures_supportsMetaProperties = (Core.FieldName "supportsMetaProperties")

_VertexFeatures_supportsMultiProperties = (Core.FieldName "supportsMultiProperties")

_VertexFeatures_supportsRemoveVertices = (Core.FieldName "supportsRemoveVertices")

_VertexFeatures_supportsUpsert = (Core.FieldName "supportsUpsert")

-- Features that are related to Vertex Property objects.
data VertexPropertyFeatures 
  = VertexPropertyFeatures {
    vertexPropertyFeaturesDataTypeFeatures :: DataTypeFeatures,
    vertexPropertyFeaturesPropertyFeatures :: PropertyFeatures,
    vertexPropertyFeaturesElementFeatures :: ElementFeatures,
    vertexPropertyFeaturesSupportsRemove :: Bool}
  deriving (Eq, Ord, Read, Show)

_VertexPropertyFeatures = (Core.Name "hydra/ext/tinkerpop/features.VertexPropertyFeatures")

_VertexPropertyFeatures_dataTypeFeatures = (Core.FieldName "dataTypeFeatures")

_VertexPropertyFeatures_propertyFeatures = (Core.FieldName "propertyFeatures")

_VertexPropertyFeatures_elementFeatures = (Core.FieldName "elementFeatures")

_VertexPropertyFeatures_supportsRemove = (Core.FieldName "supportsRemove")