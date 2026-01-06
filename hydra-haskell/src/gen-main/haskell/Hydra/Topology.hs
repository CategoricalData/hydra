-- Note: this is an automatically generated file. Do not edit.

-- | A model for simple graphs as adjacency lists

module Hydra.Topology where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A directed graph represented as an adjacency list mapping vertices to their outgoing neighbors
type Graph = (M.Map Vertex [Vertex])

_Graph = (Core.Name "hydra.topology.Graph")

data OrderingIsomorphism a = 
  OrderingIsomorphism {
    -- | Mapping from source ordering to target ordering
    orderingIsomorphismEncode :: ([a] -> [a]),
    -- | Mapping from target ordering to source ordering
    orderingIsomorphismDecode :: ([a] -> [a])}

_OrderingIsomorphism = (Core.Name "hydra.topology.OrderingIsomorphism")

_OrderingIsomorphism_encode = (Core.Name "encode")

_OrderingIsomorphism_decode = (Core.Name "decode")

data TarjanState = 
  TarjanState {
    -- | Next available index for vertices in the DFS traversal
    tarjanStateCounter :: Int,
    -- | Mapping from vertices to their indices in the DFS traversal
    tarjanStateIndices :: (M.Map Vertex Int),
    -- | Mapping from vertices to their lowest reachable index in the DFS traversal
    tarjanStateLowLinks :: (M.Map Vertex Int),
    -- | Current DFS stack, with vertices in reverse order
    tarjanStateStack :: [Vertex],
    -- | Set of vertices currently on the stack, for quick lookup
    tarjanStateOnStack :: (S.Set Vertex),
    -- | Accumulated strongly connected components, each a list of vertices
    tarjanStateSccs :: [[Vertex]]}
  deriving (Eq, Ord, Read, Show)

_TarjanState = (Core.Name "hydra.topology.TarjanState")

_TarjanState_counter = (Core.Name "counter")

_TarjanState_indices = (Core.Name "indices")

_TarjanState_lowLinks = (Core.Name "lowLinks")

_TarjanState_stack = (Core.Name "stack")

_TarjanState_onStack = (Core.Name "onStack")

_TarjanState_sccs = (Core.Name "sccs")

-- | A graph vertex, represented as a 32-bit integer identifier
type Vertex = Int

_Vertex = (Core.Name "hydra.topology.Vertex")
