-- Note: this is an automatically generated file. Do not edit.

-- | String representations of hydra.graph types

module Hydra.Show.Graph where

import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Show a graph as a string
graph :: (Graph.Graph -> String)
graph graph =  
  let elements = (Maps.elems (Graph.graphElements graph)) 
      elementStrs = (Lists.map Core.binding elements)
  in (Strings.cat [
    "{",
    Strings.intercalate ", " elementStrs,
    "}"])
