{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Tarjan where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads

import qualified Hydra.Topology as Topo


module_ :: Module
module_ = Module (Namespace "hydra.tarjan") elements
    [Constants.module_, Monads.module_]
    kernelTypesModules $
    Just ("This implementation of Tarjan's algorithm was originally based on GraphSCC by Iavor S. Diatchki:"
      <> " https://hackage.haskell.org/package/GraphSCC.")
  where
   elements = [
     el adjacencyListsToGraphDef,
     el stronglyConnectedComponentsDef,
     el initialStateDef,
     el popStackUntilDef,
     el strongConnectDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

adjacencyListsToGraphDef :: TElement ([(key, [key])] -> (Topo.Graph, Topo.Vertex -> key))
adjacencyListsToGraphDef = define "adjacencyListsToGraph" $
  doc ("Given a list of adjacency lists represented as (key, [key]) pairs,"
    <> " construct a graph along with a function mapping each vertex (an Int)"
    <> " back to its original key.") $
  withOrd "t0" $
  lambda "edges0" $ lets [
    "sortedEdges">: Lists.sortOn (unaryFunction first) (var "edges0"),
    "indexedEdges">: Lists.zip (Math.range (int32 0) (Lists.length $ var "sortedEdges")) (var "sortedEdges"),
    "keyToVertex">: Maps.fromList $ Lists.map
      (lambda "vkNeighbors" $ lets [
        "v">: first $ var "vkNeighbors",
        "kNeighbors">: second $ var "vkNeighbors",
        "k">: first $ var "kNeighbors"]
        $ pair (var "k") (var "v"))
      (var "indexedEdges"),
    "vertexMap">: Maps.fromList $ Lists.map
      (lambda "vkNeighbors" $ lets [
        "v">: first $ var "vkNeighbors",
        "kNeighbors">: second $ var "vkNeighbors",
        "k">: first $ var "kNeighbors"]
        $ pair (var "v") (var "k"))
      (var "indexedEdges"),
    "graph">: Maps.fromList $ Lists.map
      (lambda "vkNeighbors" $ lets [
        "v">: first $ var "vkNeighbors",
        "kNeighbors">: second $ var "vkNeighbors",
        "neighbors">: second $ var "kNeighbors"]
        $ pair (var "v") (Optionals.mapMaybe (lambda "k" $ Maps.lookup (var "k") (var "keyToVertex")) (var "neighbors")))
      (var "indexedEdges"),
    "vertexToKey">: lambda "v" $ Optionals.fromJust $ Maps.lookup (var "v") (var "vertexMap")]
    $ pair (var "graph") (var "vertexToKey")

initialStateDef :: TElement Topo.TarjanState
initialStateDef = define "initialState" $
  doc "Initial state for Tarjan's algorithm" $
  Topology.tarjanState (int32 0) Maps.empty Maps.empty (list []) Sets.empty (list [])

popStackUntilDef :: TElement (Topo.Vertex -> Flow Topo.TarjanState [Topo.Vertex])
popStackUntilDef = define "popStackUntil" $
  doc "Pop vertices off the stack until the given vertex is reached, collecting the current strongly connected component" $
  lambda "v" $ lets [
    "go">: lambda "acc" $
      Flows.bind (ref Monads.getStateDef) $
        lambda "st" $
          Logic.ifElse (Lists.null $ Topology.tarjanStateStack $ var "st")
            (Flows.fail $ string "popStackUntil: empty stack")
            (lets [
              "x">: Lists.head $ Topology.tarjanStateStack $ var "st",
              "xs">: Lists.tail $ Topology.tarjanStateStack $ var "st",
              "newSt">: Topology.tarjanStateWithStack (var "st") (var "xs"),
              "newSt2">: Topology.tarjanStateWithOnStack (var "newSt") (Sets.delete (var "x") (Topology.tarjanStateOnStack $ var "st")),
              "acc'">: Lists.cons (var "x") (var "acc")]
              $ Flows.bind (ref Monads.putStateDef @@ var "newSt2") $
                lambda "_" $
                  Logic.ifElse (Equality.equal (var "x") (var "v"))
                    (Flows.pure $ Lists.reverse $ var "acc'")
                    (var "go" @@ var "acc'"))]
    $ var "go" @@ list []

strongConnectDef :: TElement (Topo.Graph -> Topo.Vertex -> Flow Topo.TarjanState ())
strongConnectDef = define "strongConnect" $
  doc "Visit a vertex and recursively explore its successors" $
  lambdas ["graph", "v"] $
    Flows.bind (ref Monads.getStateDef) $
      lambda "st" $ lets [
        "i">: Topology.tarjanStateCounter $ var "st",
        "newSt">: Topology.tarjanState
          (Math.add (var "i") (int32 1))
          (Maps.insert (var "v") (var "i") (Topology.tarjanStateIndices $ var "st"))
          (Maps.insert (var "v") (var "i") (Topology.tarjanStateLowLinks $ var "st"))
          (Lists.cons (var "v") (Topology.tarjanStateStack $ var "st"))
          (Sets.insert (var "v") (Topology.tarjanStateOnStack $ var "st"))
          (Topology.tarjanStateSccs $ var "st"),
        "neighbors">: Maps.findWithDefault (list []) (var "v") (var "graph"),
        "processNeighbor">: lambda "w" $
          Flows.bind (ref Monads.getStateDef) $
            lambda "st'" $
              Logic.ifElse (Logic.not $ Maps.member (var "w") (Topology.tarjanStateIndices $ var "st'"))
                (Flows.bind (ref strongConnectDef @@ var "graph" @@ var "w") $
                  lambda "_" $
                    Flows.bind (ref Monads.getStateDef) $
                      lambda "stAfter" $ lets [
                        "low_v">: Maps.findWithDefault (ref Constants.maxInt32Def) (var "v") (Topology.tarjanStateLowLinks $ var "stAfter"),
                        "low_w">: Maps.findWithDefault (ref Constants.maxInt32Def) (var "w") (Topology.tarjanStateLowLinks $ var "stAfter")]
                        $ Flows.bind (ref Monads.modifyDef @@ (lambda "s" $
                            Topology.tarjanStateWithLowLinks (var "s")
                              (Maps.insert (var "v") (Equality.min (var "low_v") (var "low_w")) (Topology.tarjanStateLowLinks $ var "s")))) $
                          lambda "_" $ Flows.pure unit)
                (Logic.ifElse (Sets.member (var "w") (Topology.tarjanStateOnStack $ var "st'"))
                  (lets [
                    "low_v">: Maps.findWithDefault (ref Constants.maxInt32Def) (var "v") (Topology.tarjanStateLowLinks $ var "st'"),
                    "idx_w">: Maps.findWithDefault (ref Constants.maxInt32Def) (var "w") (Topology.tarjanStateIndices $ var "st'")]
                    $ Flows.bind (ref Monads.modifyDef @@ (lambda "s" $
                        Topology.tarjanStateWithLowLinks (var "s")
                          (Maps.insert (var "v") (Equality.min (var "low_v") (var "idx_w")) (Topology.tarjanStateLowLinks $ var "s")))) $
                      lambda "_" $ Flows.pure unit)
                  (Flows.pure unit))]
        $ Flows.bind (ref Monads.putStateDef @@ var "newSt") $
          lambda "_" $
            Flows.bind (Flows.mapList (var "processNeighbor") (var "neighbors")) $
              lambda "_" $
                Flows.bind (ref Monads.getStateDef) $
                  lambda "stFinal" $ lets [
                    "low_v">: Maps.findWithDefault (ref Constants.maxInt32Def) (var "v") (Topology.tarjanStateLowLinks $ var "stFinal"),
                    "idx_v">: Maps.findWithDefault (ref Constants.maxInt32Def) (var "v") (Topology.tarjanStateIndices $ var "stFinal")]
                    $ Logic.ifElse (Equality.equal (var "low_v") (var "idx_v"))
                        (Flows.bind (ref popStackUntilDef @@ var "v") $
                          lambda "comp" $
                            Flows.bind (ref Monads.modifyDef @@ (lambda "s" $
                              Topology.tarjanStateWithSccs (var "s") (Lists.cons (var "comp") (Topology.tarjanStateSccs $ var "s")))) $
                              lambda "_" $ Flows.pure unit)
                        (Flows.pure unit)

stronglyConnectedComponentsDef :: TElement (Topo.Graph -> [[Topo.Vertex]])
stronglyConnectedComponentsDef = define "stronglyConnectedComponents" $
  doc "Compute the strongly connected components of the given graph. The components are returned in reverse topological order" $
  lambda "graph" $ lets [
    "verts">: Maps.keys $ var "graph",
    "processVertex">: lambda "v" $
      Flows.bind (Flows.map (lambda "st" $ Maps.member (var "v") (Topology.tarjanStateIndices $ var "st")) $ ref Monads.getStateDef) $
        lambda "visited" $
          Logic.ifElse (Logic.not $ var "visited")
            (ref strongConnectDef @@ var "graph" @@ var "v")
            (Flows.pure unit),
    "finalState">: ref Monads.execDef @@ (Flows.mapList (var "processVertex") (var "verts")) @@ ref initialStateDef]
    $ Lists.reverse $ Lists.map (unaryFunction Lists.sort) $ Topology.tarjanStateSccs $ var "finalState"
