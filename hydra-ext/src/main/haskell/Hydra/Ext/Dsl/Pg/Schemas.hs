-- | A DSL for constructing property graph schemas and instances in Hydra.
module Hydra.Ext.Dsl.Pg.Schemas where

import Hydra.Pg.Model
import Hydra.Core
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.Map as M

-- * Type-level functions (for schema definitions)

-- | Create a property type definition with the given key and type
-- Example: propertyType "name" Types.string
propertyType :: String -> t -> PropertyType t
propertyType key typ = PropertyType (PropertyKey key) typ False

-- | Mark a property type as required
-- Example: required (propertyType "name" Types.string)
required :: PropertyType t -> PropertyType t
required (PropertyType key typ _) = PropertyType key typ True

-- | Define a vertex type with the given label, ID type, and property types
-- Example: vertexType "Person" Types.string [required (propertyType "name" Types.string), propertyType "age" Types.int32]
vertexType :: String -> t -> [PropertyType t] -> VertexType t
vertexType label id properties = VertexType (VertexLabel label) id properties

-- | Define an edge type with the given label, ID type, source vertex label, target vertex label, and property types
-- Example: edgeType "KNOWS" Types.string "Person" "Person" [propertyType "since" Types.int32]
edgeType :: String -> t -> String -> String -> [PropertyType t] -> EdgeType t
edgeType label id outLabel inLabel properties = EdgeType (EdgeLabel label) id (VertexLabel outLabel) (VertexLabel inLabel) properties

-- | Define a simple edge type with a unit ID type
-- Example: simpleEdgeType "KNOWS" "Person" "Person" [propertyType "since" Types.int32]
simpleEdgeType :: String -> String -> String -> [PropertyType Type] -> EdgeType Type
simpleEdgeType label = edgeType label Types.unit

-- | Create a graph schema from vertex types and edge types
-- Example: schema [personType] [knowsType]
-- Where:
--   personType = vertexType "Person" Types.string [required (propertyType "name" Types.string)]
--   knowsType = edgeType "KNOWS" Types.string "Person" "Person" [propertyType "since" Types.int32]
schema :: [VertexType t] -> [EdgeType t] -> GraphSchema t
schema vtypes etypes = GraphSchema
  (M.fromList $ fmap (\vt -> (vertexTypeLabel vt, vt)) vtypes)
  (M.fromList $ fmap (\et -> (edgeTypeLabel et, et)) etypes)

-- * Term-level functions (for graph instances)

-- | Create a property with the given key and value
-- Example: property "name" (Terms.string "John")
property :: String -> v -> Property v
property key value = Property (PropertyKey key) value

-- | Create a vertex with the given label, ID, and properties
-- Example: vertex "Person" (Terms.string "p1") [property "name" (Terms.string "John"), property "age" (Terms.int32 42)]
vertex :: String -> v -> [Property v] -> Vertex v
vertex label idValue propertiesList = Vertex (VertexLabel label) idValue $
  M.fromList $ fmap (\(Property k v) -> (k, v)) propertiesList

-- | Create an edge with the given label, ID, source vertex, target vertex, and properties
-- Example: edge "KNOWS" (Terms.string "e1") person1 person2 [property "since" (Terms.int32 2020)]
edge :: String -> v -> v -> v -> [Property v] -> Edge v
edge label idValue outValue inValue propertiesList = Edge (EdgeLabel label) idValue outValue inValue $
  M.fromList $ fmap (\(Property k v) -> (k, v)) propertiesList

-- | Create a graph from vertices and edges
-- Example: graph [person1, person2] [knows]
-- Where:
--   person1 = vertex "Person" (Terms.string "p1") [property "name" (Terms.string "John")]
--   person2 = vertex "Person" (Terms.string "p2") [property "name" (Terms.string "Jane")]
--   knows = edge "KNOWS" (Terms.string "e1") person1 person2 [property "since" (Terms.int32 2020)]
graph :: Ord v => [Vertex v] -> [Edge v] -> Graph v
graph vertices edges = Graph
  (M.fromList $ fmap (\v -> (vertexId v, v)) vertices)
  (M.fromList $ fmap (\e -> (edgeId e, e)) edges)
