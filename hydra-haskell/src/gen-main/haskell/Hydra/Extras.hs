-- | Basic functions which depend on primitive functions

module Hydra.Extras where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Maps as Maps
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

lookupPrimitive :: (Graph.Graph -> Core.Name -> Maybe Graph.Primitive)
lookupPrimitive g name = (Maps.lookup name (Graph.graphPrimitives g))