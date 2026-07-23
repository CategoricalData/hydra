{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Sorting where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  adjacencyListToMap, adjacencyListsToGraph, createOrderingIsomorphism, findReachableNodes,
  initialState, popStackUntil, propagateTags, strongConnect, stronglyConnectedComponents,
  topologicalSort, topologicalSortComponents, topologicalSortNodes)
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Functions as Functions
import qualified Hydra.Dsl.Lib.Ordering as Ordering
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Constants as Constants

import qualified Hydra.Topology as Topo


ns :: ModuleName
ns = ModuleName "hydra.sorting"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Constants.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just ("Utilities for sorting."
      <> " This module includes an implementation of Tarjan's algorithm,"
      <> " originally based on GraphSCC by Iavor S. Diatchki:"
      <> " https://hackage.haskell.org/package/GraphSCC."
      <> " Tarjan was chosen because it computes strongly connected components in O(V+E),"
      <> " in a single pass, and yields the components in reverse topological order — exactly the"
      <> " shape every consumer in the kernel (HM let-generalization, schema graph construction,"
      <> " target-language emission) needs."))}
  where
   definitions = [
     toDefinition (adjacencyListToMap :: TypedTermDefinition ([(Int, [b])] -> M.Map Int [b])),
     toDefinition (adjacencyListsToGraph :: TypedTermDefinition ([(Int, [Int])] -> (Topo.Graph, Topo.Vertex -> Maybe Int))),
     toDefinition (createOrderingIsomorphism :: TypedTermDefinition ([Int] -> [Int] -> Topo.OrderingIsomorphism b)),
     toDefinition (findReachableNodes :: TypedTermDefinition ((Int -> S.Set Int) -> Int -> S.Set Int)),
     toDefinition initialState,
     toDefinition popStackUntil,
     toDefinition (propagateTags :: TypedTermDefinition ([(Int, [Int])] -> [(Int, [Int])] -> [(Int, S.Set Int)])),
     toDefinition strongConnect,
     toDefinition stronglyConnectedComponents,
     toDefinition (topologicalSort :: TypedTermDefinition ([(Int, [Int])] -> Either [[Int]] [Int])),
     toDefinition (topologicalSortComponents :: TypedTermDefinition ([(Int, [Int])] -> [[Int]])),
     toDefinition (topologicalSortNodes :: TypedTermDefinition ((x -> Int) -> (x -> [Int]) -> [x] -> [[x]]))]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- Several functions below carry an `Ord` constraint and a `forall` (e.g. `forall a. Ord a =>`),
-- which is unusual for an authoring-DSL definition. The reason: the generated `Hydra.Dsl.Lib.*`
-- DSLs (unlike the old hand-written `Hydra.Dsl.Meta.Lib.*`) expose each primitive's class
-- constraints, so `Maps.*`/`Sets.*` over a polymorphic key now require `Ord` here, plus a
-- per-use `:: TypedTerm <key>` annotation (phantom-var annotations don't propagate across uses).
-- The constraint also forces a placeholder concrete type at registration in `definitions` (e.g.
-- `toDefinition (adjacencyListToMap :: TypedTermDefinition ([(Int, [b])] -> M.Map Int [b]))`) — the
-- `Int` there is arbitrary, only satisfying GHC; the emitted term is type-agnostic. See #467.
adjacencyListToMap :: forall a b. Ord a => TypedTermDefinition ([(a, [b])] -> M.Map a [b])
adjacencyListToMap = define "adjacencyListToMap" $
  doc "Convert an adjacency list to a map, concatenating values for duplicate keys" $
  "pairs" ~>
  Lists.foldl
    ("mp" ~> "p" ~>
      "k" <~ Pairs.first (var "p") $
      "vs" <~ Pairs.second (var "p") $
      "existing" <~ Optionals.cases (Maps.lookup (var "k" :: TypedTerm a) (var "mp")) (list ([] :: [TypedTerm a])) (reify Functions.identity) $
      Maps.insert (var "k" :: TypedTerm a) (Lists.concat2 (var "existing") (var "vs")) (var "mp"))
    (Maps.empty :: TypedTerm (M.Map a [b]))
    (var "pairs")

adjacencyListsToGraph :: forall key. Ord key => TypedTermDefinition ([(key, [key])] -> (Topo.Graph, Topo.Vertex -> Maybe key))
adjacencyListsToGraph = define "adjacencyListsToGraph" $
  doc ("Given a list of adjacency lists represented as (key, [key]) pairs,"
    <> " construct a graph along with a function mapping each vertex (an Int)"
    <> " back to its original key (Nothing for unknown vertices).") $
  "edges0" ~>
  "sortedEdges" <~ Lists.sortBy (reify Pairs.first) (var "edges0") $
  "indexedEdges" <~ Lists.zip (Math.range (int32 0) (Lists.length (var "sortedEdges"))) (var "sortedEdges") $
  "keyToVertex" <~ (Maps.fromList (Lists.map
    ("vkNeighbors" ~>
      "v" <~ Pairs.first (var "vkNeighbors") $
      "kNeighbors" <~ Pairs.second (var "vkNeighbors") $
      "k" <~ Pairs.first (var "kNeighbors") $
      pair (var "k") (var "v"))
    (var "indexedEdges")) :: TypedTerm (M.Map key Topo.Vertex)) $
  "vertexMap" <~ (Maps.fromList (Lists.map
    ("vkNeighbors" ~>
      "v" <~ Pairs.first (var "vkNeighbors") $
      "kNeighbors" <~ Pairs.second (var "vkNeighbors") $
      "k" <~ Pairs.first (var "kNeighbors") $
      pair (var "v") (var "k"))
    (var "indexedEdges")) :: TypedTerm (M.Map Topo.Vertex key)) $
  "graph" <~ (Maps.fromList (Lists.map
    ("vkNeighbors" ~>
      "v" <~ Pairs.first (var "vkNeighbors") $
      "kNeighbors" <~ Pairs.second (var "vkNeighbors") $
      "neighbors" <~ Pairs.second (var "kNeighbors") $
      pair (var "v") (Optionals.mapOptional ("k" ~> Maps.lookup (var "k" :: TypedTerm key) (var "keyToVertex")) (var "neighbors")))
    (var "indexedEdges")) :: TypedTerm (M.Map Topo.Vertex [Topo.Vertex])) $
  "vertexToKey" <~ ("v" ~> Maps.lookup (var "v" :: TypedTerm Topo.Vertex) (var "vertexMap")) $
  pair (var "graph") (var "vertexToKey")

createOrderingIsomorphism :: forall a b. Ord a => TypedTermDefinition ([a] -> [a] -> Topo.OrderingIsomorphism b)
createOrderingIsomorphism = define "createOrderingIsomorphism" $
  doc "Construct an OrderingIsomorphism between two orderings of the same elements. The two list arguments must be permutations of each other; the result is a pair of mappings that transport an element list from one ordering to the other." $
  "sourceOrd" ~> "targetOrd" ~>
  "sourceToTargetMapping" <~ ("els" ~>
    "mp" <~ (Maps.fromList (Lists.zip (var "sourceOrd") (var "els")) :: TypedTerm (M.Map a b)) $
    Optionals.givens $ Lists.map ("n" ~> Maps.lookup (var "n" :: TypedTerm a) (var "mp")) (var "targetOrd")) $
  "targetToSourceMapping" <~ ("els" ~>
    "mp" <~ (Maps.fromList (Lists.zip (var "targetOrd") (var "els")) :: TypedTerm (M.Map a b)) $
    Optionals.givens $ Lists.map ("n" ~> Maps.lookup (var "n" :: TypedTerm a) (var "mp")) (var "sourceOrd")) $
  Topology.orderingIsomorphism (var "sourceToTargetMapping") (var "targetToSourceMapping")

findReachableNodes :: forall a. Ord a => TypedTermDefinition ((a -> S.Set a) -> a -> S.Set a)
findReachableNodes = define "findReachableNodes" $
  doc "Given an adjacency function and a distinguished root node, find all reachable nodes (including the root node)" $
  "adj" ~> "root" ~>
  "visit" <~ ("visited" ~> "node" ~>
    "toVisit" <~ (Sets.difference (var "adj" @@ var "node") (var "visited" :: TypedTerm (S.Set a)) :: TypedTerm (S.Set a)) $
    Logic.ifElse (Sets.null (var "toVisit" :: TypedTerm (S.Set a)))
      (var "visited")
      (Lists.foldl
        ("v" ~> "n" ~> var "visit" @@ Sets.insert (var "n" :: TypedTerm a) (var "v") @@ var "n")
        (var "visited")
        (Sets.toList (var "toVisit" :: TypedTerm (S.Set a))))) $
  var "visit" @@ Sets.singleton (var "root" :: TypedTerm a) @@ var "root"

initialState :: TypedTermDefinition Topo.TarjanState
initialState = define "initialState" $
  doc "Initial state for Tarjan's algorithm" $
  Topology.tarjanState (int32 0) (Maps.empty :: TypedTerm (M.Map Topo.Vertex Int)) (Maps.empty :: TypedTerm (M.Map Topo.Vertex Int)) (list ([] :: [TypedTerm Topo.Vertex])) (Sets.empty :: TypedTerm (S.Set Topo.Vertex)) (list ([] :: [TypedTerm [Topo.Vertex]]))

popStackUntil :: TypedTermDefinition (Topo.Vertex -> Topo.TarjanState -> ([Topo.Vertex], Topo.TarjanState))
popStackUntil = define "popStackUntil" $
  doc "Pop vertices off the stack until the given vertex is reached, collecting the current strongly connected component" $
  "v" ~> "st0" ~>
  "go" <~ ("acc" ~> "st" ~>
    -- Empty stack: return whatever we've collected (unreachable when Tarjan's
    -- state invariants hold, since v is always on the stack when this is called).
    Optionals.cases (Lists.uncons $ Topology.tarjanStateStack (var "st")) (pair (Lists.reverse (var "acc")) (var "st")) ("uc" ~>
        "x" <~ Pairs.first (var "uc") $
        "xs" <~ Pairs.second (var "uc") $
        "newSt" <~ Topology.tarjanStateWithStack (var "st") (var "xs") $
        "newSt2" <~ Topology.tarjanStateWithOnStack (var "newSt") (Sets.delete (var "x") (Topology.tarjanStateOnStack (var "st"))) $
        "acc'" <~ Lists.cons (var "x") (var "acc") $
        Logic.ifElse (Equality.equal (var "x") (var "v"))
          (pair (Lists.reverse (var "acc'")) (var "newSt2"))
          (var "go" @@ var "acc'" @@ var "newSt2"))) $
  var "go" @@ list ([] :: [TypedTerm Topo.Vertex]) @@ var "st0"

propagateTags :: forall a t. (Ord a, Ord t) => TypedTermDefinition ([(a, [a])] -> [(a, [t])] -> [(a, S.Set t)])
propagateTags = define "propagateTags" $
  doc ("Given a graph as an adjacency list of edges and a list of explicit tags per node,"
    <> " compute the full set of tags for each node by propagating tags through edges."
    <> " If there is an edge from n1 to n2 and n2 has tag t, then n1 also has tag t."
    <> " Note: pairs in the output are not ordered.") $
  "edges" ~> "nodeTags" ~>
  -- Build adjacency map
  "adjMap" <~ (adjacencyListToMap @@ var "edges" :: TypedTerm (M.Map a [a])) $
  -- Build initial tag map: convert each [t] to Set t
  "tagMap" <~ Maps.map (reify Sets.fromList) (adjacencyListToMap @@ var "nodeTags" :: TypedTerm (M.Map a [t])) $
  -- Collect all nodes
  "allNodes" <~ Sets.toList (Sets.fromList $ Lists.concat2
    (Lists.map (reify Pairs.first) (var "edges"))
    (Lists.map (reify Pairs.first) (var "nodeTags")) :: TypedTerm (S.Set a)) $
  -- For each node, find all reachable nodes and collect their tags
  "getTagsForNode" <~ ("node" ~>
    "reachable" <~ ((findReachableNodes :: TypedTermDefinition ((a -> S.Set a) -> a -> S.Set a))
      @@ ("n" ~> Sets.fromList $ Optionals.cases (Maps.lookup (var "n" :: TypedTerm a) (var "adjMap")) (list ([] :: [TypedTerm a])) (reify Functions.identity))
      @@ var "node" :: TypedTerm (S.Set a)) $
    Sets.unions (Lists.map
      ("n" ~> Optionals.cases (Maps.lookup (var "n" :: TypedTerm a) (var "tagMap")) (Sets.empty :: TypedTerm (S.Set t)) (reify Functions.identity))
      (Sets.toList (var "reachable" :: TypedTerm (S.Set a))) :: TypedTerm [S.Set t])) $
  Lists.map ("n" ~> pair (var "n") (var "getTagsForNode" @@ var "n")) (var "allNodes")

strongConnect :: TypedTermDefinition (Topo.Graph -> Topo.Vertex -> Topo.TarjanState -> Topo.TarjanState)
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
  "neighbors" <~ Maps.findWithDefault (list ([] :: [TypedTerm Topo.Vertex])) (var "v" :: TypedTerm Topo.Vertex) (var "graph") $
  "processNeighbor" <~ ("st_" ~> "w" ~>
    "lowLink" <~ ("s" ~>
      "lowV1" <~ Maps.findWithDefault (asTerm Constants.maxInt32) (var "v") (Topology.tarjanStateLowLinks (var "s")) $
      "idx_w" <~ Maps.findWithDefault (asTerm Constants.maxInt32) (var "w") (Topology.tarjanStateIndices (var "s")) $
      Topology.tarjanStateWithLowLinks (var "s")
        (Maps.insert (var "v") (Ordering.min (var "lowV1") (var "idx_w")) (Topology.tarjanStateLowLinks (var "s")))) $
    Logic.ifElse (Logic.not (Maps.member (var "w") (Topology.tarjanStateIndices (var "st_"))))
      ("stAfter" <~ strongConnect @@ var "graph" @@ var "w" @@ var "st_" $
       "lowV2" <~ Maps.findWithDefault (asTerm Constants.maxInt32) (var "v") (Topology.tarjanStateLowLinks (var "stAfter")) $
       "low_w" <~ Maps.findWithDefault (asTerm Constants.maxInt32) (var "w") (Topology.tarjanStateLowLinks (var "stAfter")) $
       Topology.tarjanStateWithLowLinks (var "stAfter")
         (Maps.insert (var "v") (Ordering.min (var "lowV2") (var "low_w")) (Topology.tarjanStateLowLinks (var "stAfter"))))
      (Logic.ifElse (Sets.member (var "w") (Topology.tarjanStateOnStack (var "st_")))
        (var "lowLink" @@ var "st_")
        (var "st_"))) $
  "stAfterNeighbors" <~ Lists.foldl (var "processNeighbor") (var "newSt") (var "neighbors") $
  "low_v" <~ Maps.findWithDefault (asTerm Constants.maxInt32) (var "v") (Topology.tarjanStateLowLinks (var "stAfterNeighbors")) $
  "idx_v" <~ Maps.findWithDefault (asTerm Constants.maxInt32) (var "v") (Topology.tarjanStateIndices (var "stAfterNeighbors")) $
  Logic.ifElse (Equality.equal (var "low_v") (var "idx_v"))
    ("compResult" <~ popStackUntil @@ var "v" @@ var "stAfterNeighbors" $
     "comp" <~ Pairs.first (var "compResult") $
     "stPopped" <~ Pairs.second (var "compResult") $
     Topology.tarjanStateWithSccs (var "stPopped") (Lists.cons (var "comp") (Topology.tarjanStateSccs (var "stPopped"))))
    (var "stAfterNeighbors")

stronglyConnectedComponents :: TypedTermDefinition (Topo.Graph -> [[Topo.Vertex]])
stronglyConnectedComponents = define "stronglyConnectedComponents" $
  doc "Compute the strongly connected components of the given graph. The components are returned in reverse topological order" $
  "graph" ~>
  "verts" <~ Maps.keys (var "graph" :: TypedTerm Topo.Graph) $
  "finalState" <~ Lists.foldl
    ("st" ~> "v" ~> Logic.ifElse (Maps.member (var "v") (Topology.tarjanStateIndices (var "st")))
      (var "st")
      (strongConnect @@ var "graph" @@ var "v" @@ var "st"))
    (asTerm initialState)
    (var "verts") $
  Lists.reverse (Lists.map (reify Lists.sort) (Topology.tarjanStateSccs (var "finalState")))

topologicalSort :: forall a. Ord a => TypedTermDefinition ([(a, [a])] -> Either [[a]] [a])
topologicalSort = define "topologicalSort" $
  doc ("Sort a directed acyclic graph (DAG) based on an adjacency list."
    <> " Yields a list of nontrivial strongly connected components if the graph has cycles, otherwise a simple list.") $
  "pairs" ~>
  "sccs" <~ (topologicalSortComponents @@ var "pairs" :: TypedTerm [[a]]) $
  -- A component is a cycle iff it has more than one element.
  "isCycle" <~ ("scc" ~> Ordering.gt (Lists.length $ var "scc") (int32 1)) $
  "withCycles" <~ Lists.filter (var "isCycle") (var "sccs") $
  Logic.ifElse (Lists.null $ var "withCycles")
    (right $ Lists.concat $ var "sccs")
    (left $ var "withCycles")

topologicalSortComponents :: forall a. Ord a => TypedTermDefinition ([(a, [a])] -> [[a]])
topologicalSortComponents = define "topologicalSortComponents" $
  doc ("Find the strongly connected components (including cycles and isolated vertices) of a graph,"
    <> " in (reverse) topological order, i.e. dependencies before dependents") $
  "pairs" ~>
  "graphResult" <~ (adjacencyListsToGraph @@ var "pairs" :: TypedTerm (Topo.Graph, Topo.Vertex -> Maybe a)) $
  "g" <~ Pairs.first (var "graphResult") $
  -- vertexToKey returns Maybe key; filter out Nothing (unreachable for
  -- vertices produced by stronglyConnectedComponents of the same graph).
  Lists.map ("comp" ~> Optionals.mapOptional (Pairs.second $ var "graphResult") (var "comp")) $
    stronglyConnectedComponents @@ var "g"

topologicalSortNodes :: forall x a. Ord a => TypedTermDefinition ((x -> a) -> (x -> [a]) -> [x] -> [[x]])
topologicalSortNodes = define "topologicalSortNodes" $
  doc ("Sort a directed acyclic graph (DAG) of nodes using two helper functions:"
    <> " one for node keys, and one for the adjacency list of connected node keys."
    <> " The result is a list of strongly-connected components (cycles), in which singleton lists represent acyclic nodes.") $
  "getKey" ~> "getAdj" ~> "nodes" ~>
  "nodesByKey" <~ (Maps.fromList (Lists.map ("n" ~> pair (var "getKey" @@ var "n") (var "n")) (var "nodes")) :: TypedTerm (M.Map a x)) $
  "pairs" <~ Lists.map ("n" ~> pair (var "getKey" @@ var "n") (var "getAdj" @@ var "n")) (var "nodes") $
  "comps" <~ (topologicalSortComponents @@ var "pairs" :: TypedTerm [[a]]) $
  Lists.map ("c" ~> Optionals.givens $ Lists.map ("k" ~> Maps.lookup (var "k" :: TypedTerm a) (var "nodesByKey")) (var "c")) (var "comps")
