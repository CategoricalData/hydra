
module Hydra.Sources.Kernel.Terms.Tarjan where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors    as Accessors
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Grammar      as Grammar
import qualified Hydra.Dsl.Grammars          as Grammars
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Constants as Constants

import qualified Hydra.Topology as Topo


ns :: Namespace
ns = Namespace "hydra.tarjan"

module_ :: Module
module_ = Module ns elements
    [Constants.ns]
    kernelTypesNamespaces $
    Just ("This implementation of Tarjan's algorithm was originally based on GraphSCC by Iavor S. Diatchki:"
      <> " https://hackage.haskell.org/package/GraphSCC.")
  where
   elements = [
     toBinding adjacencyListsToGraph,
     toBinding stronglyConnectedComponents,
     toBinding initialState,
     toBinding popStackUntil,
     toBinding strongConnect]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

adjacencyListsToGraph :: TBinding ([(key, [key])] -> (Topo.Graph, Topo.Vertex -> key))
adjacencyListsToGraph = define "adjacencyListsToGraph" $
  doc ("Given a list of adjacency lists represented as (key, [key]) pairs,"
    <> " construct a graph along with a function mapping each vertex (an Int)"
    <> " back to its original key.") $
  "edges0" ~>
  "sortedEdges" <~ Lists.sortOn (unaryFunction Pairs.first) (var "edges0") $
  "indexedEdges" <~ Lists.zip (Math.range (int32 0) (Lists.length (var "sortedEdges"))) (var "sortedEdges") $
  "keyToVertex" <~ Maps.fromList (Lists.map
    ("vkNeighbors" ~>
      "v" <~ Pairs.first (var "vkNeighbors") $
      "kNeighbors" <~ Pairs.second (var "vkNeighbors") $
      "k" <~ Pairs.first (var "kNeighbors") $
      pair (var "k") (var "v"))
    (var "indexedEdges")) $
  "vertexMap" <~ Maps.fromList (Lists.map
    ("vkNeighbors" ~>
      "v" <~ Pairs.first (var "vkNeighbors") $
      "kNeighbors" <~ Pairs.second (var "vkNeighbors") $
      "k" <~ Pairs.first (var "kNeighbors") $
      pair (var "v") (var "k"))
    (var "indexedEdges")) $
  "graph" <~ Maps.fromList (Lists.map
    ("vkNeighbors" ~>
      "v" <~ Pairs.first (var "vkNeighbors") $
      "kNeighbors" <~ Pairs.second (var "vkNeighbors") $
      "neighbors" <~ Pairs.second (var "kNeighbors") $
      pair (var "v") (Maybes.mapMaybe ("k" ~> Maps.lookup (var "k") (var "keyToVertex")) (var "neighbors")))
    (var "indexedEdges")) $
  "vertexToKey" <~ ("v" ~> Maybes.fromJust (Maps.lookup (var "v") (var "vertexMap"))) $
  pair (var "graph") (var "vertexToKey")

initialState :: TBinding Topo.TarjanState
initialState = define "initialState" $
  doc "Initial state for Tarjan's algorithm" $
  Topology.tarjanState (int32 0) Maps.empty Maps.empty (list ([] :: [TTerm Topo.Vertex])) Sets.empty (list ([] :: [TTerm [Topo.Vertex]]))

popStackUntil :: TBinding (Topo.Vertex -> Topo.TarjanState -> ([Topo.Vertex], Topo.TarjanState))
popStackUntil = define "popStackUntil" $
  doc "Pop vertices off the stack until the given vertex is reached, collecting the current strongly connected component" $
  "v" ~> "st0" ~>
  "go" <~ ("acc" ~> "st" ~>
    "x" <~ Lists.head (Topology.tarjanStateStack (var "st")) $
    "xs" <~ Lists.tail (Topology.tarjanStateStack (var "st")) $
    "newSt" <~ Topology.tarjanStateWithStack (var "st") (var "xs") $
    "newSt2" <~ Topology.tarjanStateWithOnStack (var "newSt") (Sets.delete (var "x") (Topology.tarjanStateOnStack (var "st"))) $
    "acc'" <~ Lists.cons (var "x") (var "acc") $
    Logic.ifElse (Equality.equal (var "x") (var "v"))
      (pair (Lists.reverse (var "acc'")) (var "newSt2"))
      (var "go" @@ var "acc'" @@ var "newSt2")) $
  var "go" @@ list ([] :: [TTerm Topo.Vertex]) @@ var "st0"

strongConnect :: TBinding (Topo.Graph -> Topo.Vertex -> Topo.TarjanState -> Topo.TarjanState)
strongConnect = define "strongConnect" $
  doc "Visit a vertex and recursively explore its successors" $
  "graph" ~> "v" ~> "st" ~>
  "i" <~ Topology.tarjanStateCounter (var "st") $
  "newSt" <~ Topology.tarjanState
    (Math.add (var "i") (int32 1))
    (Maps.insert (var "v") (var "i") (Topology.tarjanStateIndices (var "st")))
    (Maps.insert (var "v") (var "i") (Topology.tarjanStateLowLinks (var "st")))
    (Lists.cons (var "v") (Topology.tarjanStateStack (var "st")))
    (Sets.insert (var "v") (Topology.tarjanStateOnStack (var "st")))
    (Topology.tarjanStateSccs (var "st")) $
  "neighbors" <~ Maps.findWithDefault (list ([] :: [TTerm Topo.Vertex])) (var "v") (var "graph") $
  "processNeighbor" <~ ("st_" ~> "w" ~>
    "lowLink" <~ ("s" ~>
      "lowV1" <~ Maps.findWithDefault Constants.maxInt32 (var "v") (Topology.tarjanStateLowLinks (var "s")) $
      "idx_w" <~ Maps.findWithDefault Constants.maxInt32 (var "w") (Topology.tarjanStateIndices (var "s")) $
      Topology.tarjanStateWithLowLinks (var "s")
        (Maps.insert (var "v") (Equality.min (var "lowV1") (var "idx_w")) (Topology.tarjanStateLowLinks (var "s")))) $
    Logic.ifElse (Logic.not (Maps.member (var "w") (Topology.tarjanStateIndices (var "st_"))))
      ("stAfter" <~ strongConnect @@ var "graph" @@ var "w" @@ var "st_" $
       "lowV2" <~ Maps.findWithDefault Constants.maxInt32 (var "v") (Topology.tarjanStateLowLinks (var "stAfter")) $
       "low_w" <~ Maps.findWithDefault Constants.maxInt32 (var "w") (Topology.tarjanStateLowLinks (var "stAfter")) $
       Topology.tarjanStateWithLowLinks (var "stAfter")
         (Maps.insert (var "v") (Equality.min (var "lowV2") (var "low_w")) (Topology.tarjanStateLowLinks (var "stAfter"))))
      (Logic.ifElse (Sets.member (var "w") (Topology.tarjanStateOnStack (var "st_")))
        (var "lowLink" @@ var "st_")
        (var "st_"))) $
  "stAfterNeighbors" <~ Lists.foldl (var "processNeighbor") (var "newSt") (var "neighbors") $
  "low_v" <~ Maps.findWithDefault Constants.maxInt32 (var "v") (Topology.tarjanStateLowLinks (var "stAfterNeighbors")) $
  "idx_v" <~ Maps.findWithDefault Constants.maxInt32 (var "v") (Topology.tarjanStateIndices (var "stAfterNeighbors")) $
  Logic.ifElse (Equality.equal (var "low_v") (var "idx_v"))
    ("compResult" <~ popStackUntil @@ var "v" @@ var "stAfterNeighbors" $
     "comp" <~ Pairs.first (var "compResult") $
     "stPopped" <~ Pairs.second (var "compResult") $
     Topology.tarjanStateWithSccs (var "stPopped") (Lists.cons (var "comp") (Topology.tarjanStateSccs (var "stPopped"))))
    (var "stAfterNeighbors")

stronglyConnectedComponents :: TBinding (Topo.Graph -> [[Topo.Vertex]])
stronglyConnectedComponents = define "stronglyConnectedComponents" $
  doc "Compute the strongly connected components of the given graph. The components are returned in reverse topological order" $
  "graph" ~>
  "verts" <~ Maps.keys (var "graph") $
  "finalState" <~ Lists.foldl
    ("st" ~> "v" ~> Logic.ifElse (Maps.member (var "v") (Topology.tarjanStateIndices (var "st")))
      (var "st")
      (strongConnect @@ var "graph" @@ var "v" @@ var "st"))
    (asTerm initialState)
    (var "verts") $
  Lists.reverse (Lists.map (unaryFunction Lists.sort) (Topology.tarjanStateSccs (var "finalState")))
