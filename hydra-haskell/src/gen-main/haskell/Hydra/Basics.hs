-- | A tier-2 module of basic functions for working with types and terms.

module Hydra.Basics where

import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

elementsToGraph :: (Graph.Graph -> Maybe Graph.Graph -> [Graph.Element] -> Graph.Graph)
elementsToGraph parent schema elements =  
  let toPair = (\el -> (Graph.elementName el, el))
  in Graph.Graph {
    Graph.graphElements = (Maps.fromList (Lists.map toPair elements)),
    Graph.graphEnvironment = (Graph.graphEnvironment parent),
    Graph.graphTypes = (Graph.graphTypes parent),
    Graph.graphBody = (Graph.graphBody parent),
    Graph.graphPrimitives = (Graph.graphPrimitives parent),
    Graph.graphSchema = schema}