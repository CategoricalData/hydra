module Hydra.Dsl.Pg.Schemas where

import Hydra.Pg.Model
import Hydra.Core
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M


edge :: String -> v -> v -> v -> [Property v] -> Edge v
edge label idValue outValue inValue propertiesList = Edge (EdgeLabel label) idValue outValue inValue $
  M.fromList $ fmap (\(Property k v) -> (k, v)) propertiesList

edgeType :: String -> t -> String -> String -> [PropertyType t] -> EdgeType t
edgeType label id outLabel inLabel properties = EdgeType (EdgeLabel label) id (VertexLabel outLabel) (VertexLabel inLabel) properties

graph :: Ord v => [Vertex v] -> [Edge v] -> Graph v
graph vertices edges = Graph
  (M.fromList $ fmap (\v -> (vertexId v, v)) vertices)
  (M.fromList $ fmap (\e -> (edgeId e, e)) edges)

property :: String -> v -> Property v
property key value = Property (PropertyKey key) value

propertyType :: String -> t -> PropertyType t
propertyType key typ = PropertyType (PropertyKey key) typ False

required :: PropertyType t -> PropertyType t
required (PropertyType key typ _) = PropertyType key typ True

schema :: [VertexType t] -> [EdgeType t] -> GraphSchema t
schema vtypes etypes = GraphSchema
  (M.fromList $ fmap (\vt -> (vertexTypeLabel vt, vt)) vtypes)
  (M.fromList $ fmap (\et -> (edgeTypeLabel et, et)) etypes)

simpleEdgeType :: String -> String -> String -> [PropertyType Type] -> EdgeType Type
simpleEdgeType label = edgeType label Types.unit

vertex :: String -> v -> [Property v] -> Vertex v
vertex label idValue propertiesList = Vertex (VertexLabel label) idValue $
  M.fromList $ fmap (\(Property k v) -> (k, v)) propertiesList

vertexType :: String -> t -> [PropertyType t] -> VertexType t
vertexType label id properties = VertexType (VertexLabel label) id properties
