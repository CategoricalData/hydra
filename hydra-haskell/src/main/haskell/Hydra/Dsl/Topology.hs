module Hydra.Dsl.Topology where

import Hydra.Kernel
import Hydra.Dsl.Phantoms
import Hydra.Topology as Topology

import qualified Data.Map as M
import qualified Data.Set as S


orderingIsomorphism :: TTerm ([a] -> [a])
                  -> TTerm ([a] -> [a])
                  -> TTerm (Topology.OrderingIsomorphism a)
orderingIsomorphism encode decode = record _OrderingIsomorphism [
    _OrderingIsomorphism_encode>>: encode,
    _OrderingIsomorphism_decode>>: decode]

tarjanState :: TTerm Int
            -> TTerm (M.Map Vertex Int)
            -> TTerm (M.Map Vertex Int)
            -> TTerm [Vertex]
            -> TTerm (S.Set Vertex)
            -> TTerm [[Vertex]]
            -> TTerm TarjanState
tarjanState counter indices lowLinks stack onStack sccs = record _TarjanState [
    _TarjanState_counter>>: counter,
    _TarjanState_indices>>: indices,
    _TarjanState_lowLinks>>: lowLinks,
    _TarjanState_stack>>: stack,
    _TarjanState_onStack>>: onStack,
    _TarjanState_sccs>>: sccs]

tarjanStateCounter :: TTerm TarjanState -> TTerm Int
tarjanStateCounter ts = project _TarjanState _TarjanState_counter @@ ts

tarjanStateIndices :: TTerm TarjanState -> TTerm (M.Map Vertex Int)
tarjanStateIndices ts = project _TarjanState _TarjanState_indices @@ ts

tarjanStateLowLinks :: TTerm TarjanState -> TTerm (M.Map Vertex Int)
tarjanStateLowLinks ts = project _TarjanState _TarjanState_lowLinks @@ ts

tarjanStateStack :: TTerm TarjanState -> TTerm [Vertex]
tarjanStateStack ts = project _TarjanState _TarjanState_stack @@ ts

tarjanStateOnStack :: TTerm TarjanState -> TTerm (S.Set Vertex)
tarjanStateOnStack ts = project _TarjanState _TarjanState_onStack @@ ts

tarjanStateSccs :: TTerm TarjanState -> TTerm [[Vertex]]
tarjanStateSccs ts = project _TarjanState _TarjanState_sccs @@ ts

tarjanStateWithCounter :: TTerm TarjanState -> TTerm Int -> TTerm TarjanState
tarjanStateWithCounter ts counter = tarjanState
    counter
    (Hydra.Dsl.Topology.tarjanStateIndices ts)
    (Hydra.Dsl.Topology.tarjanStateLowLinks ts)
    (Hydra.Dsl.Topology.tarjanStateStack ts)
    (Hydra.Dsl.Topology.tarjanStateOnStack ts)
    (Hydra.Dsl.Topology.tarjanStateSccs ts)

tarjanStateWithIndices :: TTerm TarjanState -> TTerm (M.Map Vertex Int) -> TTerm TarjanState
tarjanStateWithIndices ts indices = tarjanState
    (Hydra.Dsl.Topology.tarjanStateCounter ts)
    indices
    (Hydra.Dsl.Topology.tarjanStateLowLinks ts)
    (Hydra.Dsl.Topology.tarjanStateStack ts)
    (Hydra.Dsl.Topology.tarjanStateOnStack ts)
    (Hydra.Dsl.Topology.tarjanStateSccs ts)

tarjanStateWithLowLinks :: TTerm TarjanState -> TTerm (M.Map Vertex Int) -> TTerm TarjanState
tarjanStateWithLowLinks ts lowLinks = tarjanState
    (Hydra.Dsl.Topology.tarjanStateCounter ts)
    (Hydra.Dsl.Topology.tarjanStateIndices ts)
    lowLinks
    (Hydra.Dsl.Topology.tarjanStateStack ts)
    (Hydra.Dsl.Topology.tarjanStateOnStack ts)
    (Hydra.Dsl.Topology.tarjanStateSccs ts)

tarjanStateWithStack :: TTerm TarjanState -> TTerm [Vertex] -> TTerm TarjanState
tarjanStateWithStack ts stack = tarjanState
    (Hydra.Dsl.Topology.tarjanStateCounter ts)
    (Hydra.Dsl.Topology.tarjanStateIndices ts)
    (Hydra.Dsl.Topology.tarjanStateLowLinks ts)
    stack
    (Hydra.Dsl.Topology.tarjanStateOnStack ts)
    (Hydra.Dsl.Topology.tarjanStateSccs ts)

tarjanStateWithOnStack :: TTerm TarjanState -> TTerm (S.Set Vertex) -> TTerm TarjanState
tarjanStateWithOnStack ts onStack = tarjanState
    (Hydra.Dsl.Topology.tarjanStateCounter ts)
    (Hydra.Dsl.Topology.tarjanStateIndices ts)
    (Hydra.Dsl.Topology.tarjanStateLowLinks ts)
    (Hydra.Dsl.Topology.tarjanStateStack ts)
    onStack
    (Hydra.Dsl.Topology.tarjanStateSccs ts)

tarjanStateWithSccs :: TTerm TarjanState -> TTerm [[Vertex]] -> TTerm TarjanState
tarjanStateWithSccs ts sccs = tarjanState
    (Hydra.Dsl.Topology.tarjanStateCounter ts)
    (Hydra.Dsl.Topology.tarjanStateIndices ts)
    (Hydra.Dsl.Topology.tarjanStateLowLinks ts)
    (Hydra.Dsl.Topology.tarjanStateStack ts)
    (Hydra.Dsl.Topology.tarjanStateOnStack ts)
    sccs
