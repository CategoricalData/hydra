module Hydra.Dsl.Pg where

import Hydra.Pg.Model
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M


edge :: String -> v -> v -> v -> [Property v] -> Edge v
edge label idValue outValue inValue propertiesList = Edge (EdgeLabel label) idValue outValue inValue $
  M.fromList $ fmap (\(Property k v) -> (k, v)) propertiesList

edgeType :: String -> t -> VertexLabel -> VertexLabel -> [PropertyType t] -> EdgeType t
edgeType label id outLabel inLabel properties = EdgeType (EdgeLabel label) id outLabel inLabel properties

property :: String -> v -> Property v
property key value = Property (PropertyKey key) value

propertyType :: String -> t -> PropertyType t
propertyType key typ = PropertyType (PropertyKey key) typ False

required :: PropertyType t -> PropertyType t
required (PropertyType key typ _) = PropertyType key typ True

vertex :: String -> v -> [Property v] -> Vertex v
vertex label idValue propertiesList = Vertex (VertexLabel label) idValue $
  M.fromList $ fmap (\(Property k v) -> (k, v)) propertiesList

vertexType :: String -> t -> [PropertyType t] -> VertexType t
vertexType label id properties = VertexType (VertexLabel label) id properties
