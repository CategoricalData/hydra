module Hydra.Sources.Kernel.Types.Topology where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms                 as Terms
import           Hydra.Dsl.Types                 as Types
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


module_ :: Module
module_ = Module ns elements [] [Core.module_] $
    Just "A model for simple graphs as adjacency lists"
  where
    ns = Namespace "hydra.topology"
    topo = typeref ns
    def = datatype ns

    elements = [

      def "Graph" $
        doc "A directed graph represented as an adjacency list mapping vertices to their outgoing neighbors" $
        Types.map (topo "Vertex") (list $ topo "Vertex"),

      -- Note: useful, but currently unused
      def "OrderingIsomorphism" $ forAlls ["a"] $ record [
        "encode">:
          doc "Mapping from source ordering to target ordering" $
          list (var "a") --> list (var "a"),
        "decode">:
          doc "Mapping from target ordering to source ordering" $
          list (var "a") --> list (var "a")],

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

      def "Vertex" $
        doc "A graph vertex, represented as a 32-bit integer identifier" $
        int32]
