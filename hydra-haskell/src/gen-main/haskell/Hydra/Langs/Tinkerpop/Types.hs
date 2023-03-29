-- | A model for typed TinkerPop vertices, edges, and properties

module Hydra.Langs.Tinkerpop.Types where

import qualified Hydra.Core as Core
import qualified Hydra.Langs.Tinkerpop.V3 as V3
import Data.List
import Data.Map
import Data.Set

-- | The type of a TinkerPop edge
data EdgeType t = 
  EdgeType {
    edgeTypeLabel :: V3.EdgeLabel,
    edgeTypeOut :: (VertexType t),
    edgeTypeIn :: (VertexType t),
    edgeTypeProperties :: (Map V3.PropertyKey t)}
  deriving (Eq, Ord, Read, Show)

_EdgeType = (Core.Name "hydra/langs/tinkerpop/types.EdgeType")

_EdgeType_label = (Core.FieldName "label")

_EdgeType_out = (Core.FieldName "out")

_EdgeType_in = (Core.FieldName "in")

_EdgeType_properties = (Core.FieldName "properties")

-- | The kind of an element: vertex or edge
data ElementKind = 
  ElementKindVertex  |
  ElementKindEdge 
  deriving (Eq, Ord, Read, Show)

_ElementKind = (Core.Name "hydra/langs/tinkerpop/types.ElementKind")

_ElementKind_vertex = (Core.FieldName "vertex")

_ElementKind_edge = (Core.FieldName "edge")

-- | The type of a TinkerPop vertex or edge
data ElementType t = 
  ElementTypeVertex (VertexType t) |
  ElementTypeEdge (EdgeType t)
  deriving (Eq, Ord, Read, Show)

_ElementType = (Core.Name "hydra/langs/tinkerpop/types.ElementType")

_ElementType_vertex = (Core.FieldName "vertex")

_ElementType_edge = (Core.FieldName "edge")

-- | The type of a TinkerPop property
data PropertyType t = 
  PropertyType {
    propertyTypeKey :: V3.PropertyKey,
    propertyTypeValue :: t}
  deriving (Eq, Ord, Read, Show)

_PropertyType = (Core.Name "hydra/langs/tinkerpop/types.PropertyType")

_PropertyType_key = (Core.FieldName "key")

_PropertyType_value = (Core.FieldName "value")

-- | The type of a TinkerPop vertex
data VertexType t = 
  VertexType {
    vertexTypeLabel :: V3.VertexLabel,
    vertexTypeProperties :: (Map V3.PropertyKey t)}
  deriving (Eq, Ord, Read, Show)

_VertexType = (Core.Name "hydra/langs/tinkerpop/types.VertexType")

_VertexType_label = (Core.FieldName "label")

_VertexType_properties = (Core.FieldName "properties")