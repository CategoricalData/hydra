-- | A DSL for defining mappings between relational data and property graphs in Hydra.
module Hydra.Overlay.Haskell.Dsl.Pg.Mappings where

import Hydra.Core
import Hydra.Typed
import Hydra.Rewriting
import Hydra.Pg.Model
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms

import qualified Data.Map as M

-- * Helpers

temporaryTableName = "(((temp)))"

replaceTableName tableName = rewriteTerm $ \recurse term -> case term of
  TermProject (Projection tname fname) -> if unName tname == temporaryTableName
    then TermProject $ Projection (Name tableName) fname
    else term
  _ -> recurse term

-- * Graph construction functions

-- | Create a mapping function that selects a specified column from a table
column :: String -> TypedTerm (r -> Maybe a)
column sourceColumn = project (Name temporaryTableName) (Name sourceColumn)

-- | Create an edge with the given label, ID, source vertex, target vertex, and properties
-- Example: edge "social_network.csv" "knows" (constant unit) (column "person_1") (column "person_2")
--            [property "since" $ column "knows_since"]
edge :: String -> String -> TypedTerm e -> TypedTerm vo -> TypedTerm vi -> [Property Term] -> Edge Term
edge tableName label (TypedTerm idValue) (TypedTerm outValue) (TypedTerm inValue) propertiesList = Edge
  (EdgeLabel label)
  (replaceTableName tableName idValue)
  (replaceTableName tableName outValue)
  (replaceTableName tableName inValue)
  (M.fromList $ fmap (\(Property k v) -> (k, replaceTableName tableName v)) propertiesList)

-- | Create an edge with a unit ID (when the edge id isn't significant)
-- Example: simpleEdge "social_network.csv" "knows" (column "person_1") (column "person_2")
--            [property "since" $ column "knows_since"]
edgeNoId :: String -> String -> TypedTerm vo -> TypedTerm vi -> [Property Term] -> Edge Term
edgeNoId tableName label = edge tableName label (constant unit)

-- | Create a lazy graph from vertices and edges, without enforcing id uniqueness
-- Example: graph [person1, person2] [knows]
-- Where:
--   person1 = vertex "Person" (string "p1") [property "name" (string "John")]
--   person2 = vertex "Person" (string "p2") [property "name" (string "Jane")]
--   knows = edge "knows" (string "e1") person1 person2 [property "since" (int32 2020)]
graph :: [Vertex v] -> [Edge v] -> LazyGraph v
graph = LazyGraph

-- | Create a property with the given key and value
-- Example: property "name" (string "John")
property :: String -> TypedTerm v -> Property Term
property key (TypedTerm value) = Property (PropertyKey key) value

-- | Create a vertex derived from the given table with the given label, ID, and properties
-- Example: vertex "people.csv" "Person" personId [property "name" (column "full_name"), property "age" (column "age)]
vertex :: String -> String -> TypedTerm v -> [Property Term] -> Vertex Term
vertex tableName label (TypedTerm idValue) propertiesList = Vertex
  (VertexLabel label)
  (replaceTableName tableName idValue)
  (M.fromList $ fmap (\(Property k v) -> (k, replaceTableName tableName v)) propertiesList)
