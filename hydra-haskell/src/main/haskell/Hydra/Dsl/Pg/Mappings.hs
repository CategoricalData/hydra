-- | A DSL for defining mappings between relational data and property graphs in Hydra.
module Hydra.Dsl.Pg.Mappings where

import Hydra.Core
import Hydra.Phantoms
import Hydra.Pg.Model
import Hydra.Dsl.Phantoms

import qualified Data.Map as M


-- * Helper types

-- | A graph which does not assume that vertex or edge ids are unique.
--   This is useful in mappings because the id specifications for vertices and/or edges may be non-unique.
data LazyGraph v = LazyGraph
  { lazyGraphVertices :: [Vertex v]
  , lazyGraphEdges :: [Edge v]}

-- * Relational data mapping functions

-- | Create a mapping function that extracts a typed value from a specific column in a table
-- Example: columnValue "Users" "email"
columnValue :: String -> String -> TTerm (a -> Maybe b)
columnValue sourceTable sourceColumn = project (Name sourceTable) (Name sourceColumn)

-- * Graph construction functions

-- | Create a property with the given key and value
-- Example: property "name" (string "John")
property :: String -> TTerm v -> Property Term
property key (TTerm value) = Property (PropertyKey key) value

-- | Create a vertex with the given label, ID, and properties
-- Example: vertex "Person" personId [property "name" (string "John"), property "age" (int32 42)]
vertex :: String -> TTerm v -> [Property Term] -> Vertex Term
vertex label (TTerm idValue) propertiesList = Vertex (VertexLabel label) idValue $
  M.fromList $ fmap (\(Property k v) -> (k, v)) propertiesList

-- | Create an edge with the given label, ID, source vertex, target vertex, and properties
-- Example: edge "knows" edgeId person1 person2 [property "since" (int32 2020)]
edge :: String -> TTerm e -> TTerm vo -> TTerm vi -> [Property Term] -> Edge Term
edge label (TTerm idValue) (TTerm outValue) (TTerm inValue) propertiesList = Edge (EdgeLabel label) idValue outValue inValue $
  M.fromList $ fmap (\(Property k v) -> (k, v)) propertiesList

-- | Create an edge with a unit ID (when the edge id isn't significant)
-- Example: simpleEdge "knows" person1 person2 [property "since" (int32 2020)]
simpleEdge :: String -> TTerm vo -> TTerm vi -> [Property Term] -> Edge Term
simpleEdge label = edge label unit

-- | Create a lazy graph from vertices and edges, without enforcing id uniqueness
-- Example: graph [person1, person2] [knows]
-- Where:
--   person1 = vertex "Person" (string "p1") [property "name" (string "John")]
--   person2 = vertex "Person" (string "p2") [property "name" (string "Jane")]
--   knows = edge "knows" (string "e1") person1 person2 [property "since" (int32 2020)]
graph :: [Vertex v] -> [Edge v] -> LazyGraph v
graph = LazyGraph
