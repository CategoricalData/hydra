module Hydra.Sources.Kernel.Types.Topology where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.topology"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleDescription = Just "A model for simple graphs as adjacency lists"}
  where
    definitions = [
      graph,
      orderingIsomorphism,
      tarjanState,
      vertex]

graph :: Binding
graph = define "Graph" $
  doc "A directed graph represented as an adjacency list mapping vertices to their outgoing neighbors" $
  T.map vertex (T.list vertex)

orderingIsomorphism :: Binding
orderingIsomorphism = define "OrderingIsomorphism" $
  doc "A pair of inverse permutations on lists, used to relate two orderings of the same elements" $
  T.forAll "a" $ T.record [
    "encode">:
      doc "Mapping from source ordering to target ordering" $
      T.list "a" ~> T.list "a",
    "decode">:
      doc "Mapping from target ordering to source ordering" $
      T.list "a" ~> T.list "a"]

tarjanState :: Binding
tarjanState = define "TarjanState" $
  doc "State carried by Tarjan's strongly connected components algorithm during a depth-first traversal" $
  T.record [
    "counter">:
      doc "Next available index for vertices in the DFS traversal"
      T.int32,
    "indices">:
      doc "Mapping from vertices to their indices in the DFS traversal" $
      T.map vertex T.int32,
    "lowLinks">:
      doc "Mapping from vertices to their lowest reachable index in the DFS traversal" $
      T.map vertex T.int32,
    "stack">:
      doc "Current DFS stack, with vertices in reverse order" $
      T.list vertex,
    "onStack">:
      doc "Set of vertices currently on the stack, for quick lookup" $
      T.set vertex,
    "sccs">:
      doc "Accumulated strongly connected components, each a list of vertices" $
      T.list $ T.list vertex]

vertex :: Binding
vertex = define "Vertex" $
  doc "A graph vertex, represented as a 32-bit integer identifier" $
  T.int32
