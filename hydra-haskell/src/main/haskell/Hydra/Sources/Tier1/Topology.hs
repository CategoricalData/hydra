module Hydra.Sources.Tier1.Topology where

-- Standard type-level Tier-1 imports
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms       as Terms
import           Hydra.Dsl.Types       as Types
import           Hydra.Sources.Tier0.Core
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Maybe            as Y

import Hydra.Sources.Tier1.Compute


hydraTopologyModule :: Module
hydraTopologyModule = Module ns elements [] [hydraCoreModule] $
    Just "A model for simple graphs as adjacency lists"
  where
    ns = Namespace "hydra.topology"
    topo = typeref ns
    def = datatype ns

    elements = [

      def "Graph" $ Types.map (topo "Vertex") (list $ topo "Vertex"),

      def "TarjanState" $ record [
        "counter">:
          doc "Next available index for vertices in the DFS traversal"
          int32,
        "indices">:
          doc "Mapping from vertices to their indices in the DFS traversal" $
          Types.map (topo "Vertex") int32,
        "lowLinks">:
          doc "Mapping from vertices to their lowest reachable index in the DFS traversal" $
          Types.map (topo "Vertex") int32,
        "stack">:
          doc "Current DFS stack, with vertices in reverse order" $
          list $ topo "Vertex",
        "onStack">:
          doc "Set of vertices currently on the stack, for quick lookup" $
          Types.set (topo "Vertex"),
        "sccs">:
          doc "Accumulated strongly connected components, each a list of vertices" $
          list $ list $ topo "Vertex"],

      def "Vertex" $ int32]
