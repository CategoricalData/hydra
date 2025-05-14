module Hydra.Dsl.Pg.Mappings where

import Hydra.Core
import Hydra.Phantoms
import Hydra.Pg.Model
import Hydra.Dsl.Phantoms

import qualified Data.Map as M


-- | A graph which does not assume that vertex or edge ids are unique.
--   This is useful in mappings because the id specifications for vertices and/or edges may be non-unique.
data LazyGraph v = LazyGraph
  { lazyGraphVertices :: [Vertex v]
  , lazyGraphEdges :: [Edge v]}

columnValue :: String -> String -> TTerm (a -> Maybe b)
columnValue sourceTable sourceColumn = project (Name sourceTable) (Name sourceColumn)

edge :: String -> TTerm e -> TTerm vo -> TTerm vi -> [Property Term] -> Edge Term
edge label (TTerm idValue) (TTerm outValue) (TTerm inValue) propertiesList = Edge (EdgeLabel label) idValue outValue inValue $
  M.fromList $ fmap (\(Property k v) -> (k, v)) propertiesList

graph :: [Vertex v] -> [Edge v] -> LazyGraph v
graph = LazyGraph

property :: String -> TTerm v -> Property Term
property key (TTerm value) = Property (PropertyKey key) value

simpleEdge :: String -> TTerm vo -> TTerm vi -> [Property Term] -> Edge Term
simpleEdge label = edge label unit

vertex :: String -> TTerm v -> [Property Term] -> Vertex Term
vertex label (TTerm idValue) propertiesList = Vertex (VertexLabel label) idValue $
  M.fromList $ fmap (\(Property k v) -> (k, v)) propertiesList
