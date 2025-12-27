-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for sorting.

module Hydra.Sorting where

import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Tarjan as Tarjan
import qualified Hydra.Topology as Topology
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

createOrderingIsomorphism :: (Ord t0) => ([t0] -> [t0] -> Topology.OrderingIsomorphism t1)
createOrderingIsomorphism sourceOrd targetOrd =  
  let sourceToTargetMapping = (\els ->  
          let mp = (Maps.fromList (Lists.zip sourceOrd els))
          in (Maybes.cat (Lists.map (\n -> Maps.lookup n mp) targetOrd)))
  in  
    let targetToSourceMapping = (\els ->  
            let mp = (Maps.fromList (Lists.zip targetOrd els))
            in (Maybes.cat (Lists.map (\n -> Maps.lookup n mp) sourceOrd)))
    in Topology.OrderingIsomorphism {
      Topology.orderingIsomorphismEncode = sourceToTargetMapping,
      Topology.orderingIsomorphismDecode = targetToSourceMapping}

findReachableNodes :: (Ord t0) => ((t0 -> S.Set t0) -> t0 -> S.Set t0)
findReachableNodes adj root =  
  let visit = (\visited -> \node ->  
          let toVisit = (Sets.difference (adj node) visited)
          in (Logic.ifElse (Sets.null toVisit) visited (Lists.foldl (\v -> \n -> visit (Sets.insert n v) n) visited (Sets.toList toVisit))))
  in (visit (Sets.singleton root) root)

topologicalSort :: (Ord t0) => ([(t0, [t0])] -> Either [[t0]] [t0])
topologicalSort pairs =  
  let sccs = (topologicalSortComponents pairs)
  in  
    let isCycle = (\scc -> Logic.not (Lists.null (Lists.tail scc)))
    in  
      let withCycles = (Lists.filter isCycle sccs)
      in (Logic.ifElse (Lists.null withCycles) (Right (Lists.concat sccs)) (Left withCycles))

topologicalSortComponents :: (Ord t0) => ([(t0, [t0])] -> [[t0]])
topologicalSortComponents pairs =  
  let graphResult = (Tarjan.adjacencyListsToGraph pairs)
  in  
    let g = (Pairs.first graphResult)
    in (Lists.map (\comp -> Lists.map (Pairs.second graphResult) comp) (Tarjan.stronglyConnectedComponents g))

topologicalSortNodes :: (Ord t1) => ((t0 -> t1) -> (t0 -> [t1]) -> [t0] -> [[t0]])
topologicalSortNodes getKey getAdj nodes =  
  let nodesByKey = (Maps.fromList (Lists.map (\n -> (getKey n, n)) nodes))
  in  
    let pairs = (Lists.map (\n -> (getKey n, (getAdj n))) nodes)
    in  
      let comps = (topologicalSortComponents pairs)
      in (Lists.map (\c -> Maybes.cat (Lists.map (\k -> Maps.lookup k nodesByKey) c)) comps)
