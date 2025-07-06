-- | String representations of hydra.graph types

module Hydra.Show.Graph where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Show an element as a string
element :: (Graph.Element -> String)
element el =  
  let name = (Core.unName (Graph.elementName el)) 
      t = (Graph.elementTerm el)
      typeStr = (Optionals.maybe "" (\ts -> Strings.cat2 " : " (Core_.typeScheme ts)) (Graph.elementType el))
  in (Strings.cat [
    name,
    " = ",
    Core_.term t,
    typeStr])

-- | Show a graph as a string
graph :: (Graph.Graph -> String)
graph graph =  
  let elements = (Maps.elems (Graph.graphElements graph)) 
      elementStrs = (Lists.map element elements)
  in (Strings.cat [
    "{",
    Strings.intercalate ", " elementStrs,
    "}"])
