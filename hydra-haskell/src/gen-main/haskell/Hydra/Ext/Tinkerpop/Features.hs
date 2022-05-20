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

_DataTypeFeatures = "hydra/ext/tinkerpop/features.DataTypeFeatures"

_DataTypeFeatures_supportsBooleanArrayValues = "supportsBooleanArrayValues"

_DataTypeFeatures_supportsBooleanValues = "supportsBooleanValues"

_DataTypeFeatures_supportsByteArrayValues = "supportsByteArrayValues"

_DataTypeFeatures_supportsByteValues = "supportsByteValues"

_DataTypeFeatures_supportsDoubleArrayValues = "supportsDoubleArrayValues"

_DataTypeFeatures_supportsDoubleValues = "supportsDoubleValues"

_DataTypeFeatures_supportsFloatArrayValues = "supportsFloatArrayValues"

_DataTypeFeatures_supportsFloatValues = "supportsFloatValues"

_DataTypeFeatures_supportsIntegerArrayValues = "supportsIntegerArrayValues"

_DataTypeFeatures_supportsIntegerValues = "supportsIntegerValues"

_DataTypeFeatures_supportsLongArrayValues = "supportsLongArrayValues"

_DataTypeFeatures_supportsLongValues = "supportsLongValues"

_DataTypeFeatures_supportsMapValues = "supportsMapValues"

_DataTypeFeatures_supportsMixedListValues = "supportsMixedListValues"

_DataTypeFeatures_supportsSerializableValues = "supportsSerializableValues"

_DataTypeFeatures_supportsStringArrayValues = "supportsStringArrayValues"

_DataTypeFeatures_supportsStringValues = "supportsStringValues"

_DataTypeFeatures_supportsUniformListValues = "supportsUniformListValues"

-- Features that are related to Edge operations.
data EdgeFeatures 
  = EdgeFeatures {
    edgeFeaturesElementFeatures :: ElementFeatures,
    edgeFeaturesProperties :: EdgePropertyFeatures,
    edgeFeaturesSupportsAddEdges :: Bool,
    edgeFeaturesSupportsRemoveEdges :: Bool,
    edgeFeaturesSupportsUpsert :: Bool}
  deriving (Eq, Ord, Read, Show)

_EdgeFeatures = "hydra/ext/tinkerpop/features.EdgeFeatures"

_EdgeFeatures_elementFeatures = "elementFeatures"

_EdgeFeatures_properties = "properties"

_EdgeFeatures_supportsAddEdges = "supportsAddEdges"

_EdgeFeatures_supportsRemoveEdges = "supportsRemoveEdges"

_EdgeFeatures_supportsUpsert = "supportsUpsert"

-- Features that are related to Edge Property objects.
data EdgePropertyFeatures 
  = EdgePropertyFeatures {edgePropertyFeaturesPropertyFeatures :: PropertyFeatures}
  deriving (Eq, Ord, Read, Show)

_EdgePropertyFeatures = "hydra/ext/tinkerpop/features.EdgePropertyFeatures"

_EdgePropertyFeatures_propertyFeatures = "propertyFeatures"

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

_ElementFeatures = "hydra/ext/tinkerpop/features.ElementFeatures"

_ElementFeatures_supportsAddProperty = "supportsAddProperty"

_ElementFeatures_supportsAnyIds = "supportsAnyIds"

_ElementFeatures_supportsCustomIds = "supportsCustomIds"

_ElementFeatures_supportsNumericIds = "supportsNumericIds"

_ElementFeatures_supportsRemoveProperty = "supportsRemoveProperty"

_ElementFeatures_supportsStringIds = "supportsStringIds"

_ElementFeatures_supportsUserSuppliedIds = "supportsUserSuppliedIds"

_ElementFeatures_supportsUuidIds = "supportsUuidIds"

-- An interface that represents the capabilities of a Graph implementation. By default all methods of features return true and it is up to implementers to disable feature they don't support. Users should check features prior to using various functions of TinkerPop to help ensure code portability across implementations. For example, a common usage would be to check if a graph supports transactions prior to calling the commit method on Graph.tx().
-- 
-- As an additional notice to Graph Providers, feature methods will be used by the test suite to determine which tests will be ignored and which will be executed, therefore proper setting of these features is essential to maximizing the amount of testing performed by the suite. Further note, that these methods may be called by the TinkerPop core code to determine what operations may be appropriately executed which will have impact on features utilized by users.
data Features 
  = Features {
    featuresEdge :: EdgeFeatures,
    featuresGraph :: GraphFeatures,
    featuresVertex :: VertexFeatures}
  deriving (Eq, Ord, Read, Show)

_Features = "hydra/ext/tinkerpop/features.Features"

_Features_edge = "edge"

_Features_graph = "graph"

_Features_vertex = "vertex"

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

_GraphFeatures = "hydra/ext/tinkerpop/features.GraphFeatures"

_GraphFeatures_supportsComputer = "supportsComputer"

_GraphFeatures_supportsConcurrentAccess = "supportsConcurrentAccess"

_GraphFeatures_supportsIoRead = "supportsIoRead"

_GraphFeatures_supportsIoWrite = "supportsIoWrite"

_GraphFeatures_supportsPersistence = "supportsPersistence"

_GraphFeatures_supportsThreadedTransactions = "supportsThreadedTransactions"

_GraphFeatures_supportsTransactions = "supportsTransactions"

_GraphFeatures_variables = "variables"

-- A base interface for Edge or Vertex Property features.
data PropertyFeatures 
  = PropertyFeatures {
    propertyFeaturesDataTypeFeatures :: DataTypeFeatures,
    propertyFeaturesSupportsProperties :: Bool}
  deriving (Eq, Ord, Read, Show)

_PropertyFeatures = "hydra/ext/tinkerpop/features.PropertyFeatures"

_PropertyFeatures_dataTypeFeatures = "dataTypeFeatures"

_PropertyFeatures_supportsProperties = "supportsProperties"

-- Features for Graph.Variables.
data VariableFeatures 
  = VariableFeatures {
    variableFeaturesDataTypeFeatures :: DataTypeFeatures,
    variableFeaturesSupportsVariables :: Bool}
  deriving (Eq, Ord, Read, Show)

_VariableFeatures = "hydra/ext/tinkerpop/features.VariableFeatures"

_VariableFeatures_dataTypeFeatures = "dataTypeFeatures"

_VariableFeatures_supportsVariables = "supportsVariables"

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

_VertexFeatures = "hydra/ext/tinkerpop/features.VertexFeatures"

_VertexFeatures_elementFeatures = "elementFeatures"

_VertexFeatures_properties = "properties"

_VertexFeatures_supportsAddVertices = "supportsAddVertices"

_VertexFeatures_supportsDuplicateMultiProperties = "supportsDuplicateMultiProperties"

_VertexFeatures_supportsMetaProperties = "supportsMetaProperties"

_VertexFeatures_supportsMultiProperties = "supportsMultiProperties"

_VertexFeatures_supportsRemoveVertices = "supportsRemoveVertices"

_VertexFeatures_supportsUpsert = "supportsUpsert"

-- Features that are related to Vertex Property objects.
data VertexPropertyFeatures 
  = VertexPropertyFeatures {
    vertexPropertyFeaturesDataTypeFeatures :: DataTypeFeatures,
    vertexPropertyFeaturesPropertyFeatures :: PropertyFeatures,
    vertexPropertyFeaturesElementFeatures :: ElementFeatures,
    vertexPropertyFeaturesSupportsRemove :: Bool}
  deriving (Eq, Ord, Read, Show)

_VertexPropertyFeatures = "hydra/ext/tinkerpop/features.VertexPropertyFeatures"

_VertexPropertyFeatures_dataTypeFeatures = "dataTypeFeatures"

_VertexPropertyFeatures_propertyFeatures = "propertyFeatures"

_VertexPropertyFeatures_elementFeatures = "elementFeatures"

_VertexPropertyFeatures_supportsRemove = "supportsRemove"