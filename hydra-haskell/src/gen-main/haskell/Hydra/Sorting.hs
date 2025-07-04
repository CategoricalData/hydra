-- | Utilities for sorting.

module Hydra.Sorting where

import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Tarjan as Tarjan
import qualified Hydra.Topology as Topology
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

createOrderingIsomorphism :: (Ord t0) => ([t0] -> [t0] -> Topology.OrderingIsomorphism t1)
createOrderingIsomorphism sourceOrd targetOrd =  
  let sourceToTargetMapping = (\els ->  
          let mp = (Maps.fromList (Lists.zip sourceOrd els))
          in (Optionals.cat (Lists.map (\n -> Maps.lookup n mp) targetOrd))) 
      targetToSourceMapping = (\els ->  
              let mp = (Maps.fromList (Lists.zip targetOrd els))
              in (Optionals.cat (Lists.map (\n -> Maps.lookup n mp) sourceOrd)))
  in Topology.OrderingIsomorphism {
    Topology.orderingIsomorphismEncode = sourceToTargetMapping,
    Topology.orderingIsomorphismDecode = targetToSourceMapping}

topologicalSort :: (Ord t0) => ([(t0, [t0])] -> Mantle.Either [[t0]] [t0])
topologicalSort pairs =  
  let sccs = (topologicalSortComponents pairs) 
      isCycle = (\scc -> Logic.not (Lists.null (Lists.tail scc)))
      withCycles = (Lists.filter isCycle sccs)
  in (Logic.ifElse (Lists.null withCycles) (Mantle.EitherRight (Lists.concat sccs)) (Mantle.EitherLeft withCycles))

topologicalSortComponents :: (Ord t0) => ([(t0, [t0])] -> [[t0]])
topologicalSortComponents pairs =  
  let graphResult = (Tarjan.adjacencyListsToGraph pairs) 
      g = (fst graphResult)
      getKey = (snd graphResult)
  in (Lists.map (\component -> Lists.map getKey component) (Tarjan.stronglyConnectedComponents g))
