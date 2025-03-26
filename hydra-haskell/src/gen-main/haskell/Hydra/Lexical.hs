-- | A module for lexical operations over graphs.

module Hydra.Lexical where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

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

lookupPrimitive :: (Graph.Graph -> Core.Name -> Maybe Graph.Primitive)
lookupPrimitive g name = (Maps.lookup name (Graph.graphPrimitives g))
