-- | A model for typed TinkerPop vertices, edges, and properties

module Hydra.Langs.Tinkerpop.Types where

import qualified Hydra.Core as Core
import qualified Hydra.Langs.Tinkerpop.V3 as V3
import Data.List
import Data.Map
import Data.Set

-- | The type of a TinkerPop edge
data EdgeType v e p = 
  EdgeType {
    edgeTypeLabel :: V3.EdgeLabel,
    edgeTypeId :: e,
    edgeTypeOut :: (VertexType v p),
    edgeTypeIn :: (VertexType v p),
    edgeTypeProperties :: (Map V3.PropertyKey p)}
  deriving (Eq, Ord, Read, Show)

_EdgeType = (Core.Name "hydra/langs/tinkerpop/types.EdgeType")

_EdgeType_label = (Core.FieldName "label")

_EdgeType_id = (Core.FieldName "id")

_EdgeType_out = (Core.FieldName "out")

_EdgeType_in = (Core.FieldName "in")

_EdgeType_properties = (Core.FieldName "properties")

-- | The type of a Tinkerpop vertex or edge
data ElementType v e p = 
  ElementTypeVertex (VertexType v p) |
  ElementTypeEdge (EdgeType v e p)
  deriving (Eq, Ord, Read, Show)

_ElementType = (Core.Name "hydra/langs/tinkerpop/types.ElementType")

_ElementType_vertex = (Core.FieldName "vertex")

_ElementType_edge = (Core.FieldName "edge")

-- | The type of a TinkerPop vertex
data VertexType v p = 
  VertexType {
    vertexTypeLabel :: V3.VertexLabel,
    vertexTypeId :: v,
    vertexTypeProperties :: (Map V3.PropertyKey p)}
  deriving (Eq, Ord, Read, Show)

_VertexType = (Core.Name "hydra/langs/tinkerpop/types.VertexType")

_VertexType_label = (Core.FieldName "label")

_VertexType_id = (Core.FieldName "id")

_VertexType_properties = (Core.FieldName "properties")