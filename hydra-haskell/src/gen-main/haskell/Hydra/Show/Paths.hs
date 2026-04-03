-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for working with subterm steps and paths.

module Hydra.Show.Paths where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Paths as Paths
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Convert a subterm step to a string representation
subtermStep :: Paths.SubtermStep -> Maybe String
subtermStep step =

      let idx = \i -> Nothing
          idxSuff = \suffix -> \i -> Maybes.map (\s -> Strings.cat2 s suffix) (idx i)
      in case step of
        Paths.SubtermStepAnnotatedBody -> Nothing
        Paths.SubtermStepApplicationFunction -> Just "fun"
        Paths.SubtermStepApplicationArgument -> Just "arg"
        Paths.SubtermStepLambdaBody -> Just "body"
        Paths.SubtermStepUnionCasesDefault -> Just "default"
        Paths.SubtermStepUnionCasesBranch v0 -> Just (Strings.cat2 "." (Core.unName v0))
        Paths.SubtermStepLetBody -> Just "in"
        Paths.SubtermStepLetBinding v0 -> Just (Strings.cat2 (Core.unName v0) "=")
        Paths.SubtermStepListElement v0 -> idx v0
        Paths.SubtermStepMapKey v0 -> idxSuff ".key" v0
        Paths.SubtermStepMapValue v0 -> idxSuff ".value" v0
        Paths.SubtermStepMaybeTerm -> Just "just"
        Paths.SubtermStepProductTerm v0 -> idx v0
        Paths.SubtermStepRecordField v0 -> Just (Strings.cat2 "." (Core.unName v0))
        Paths.SubtermStepSetElement v0 -> idx v0
        Paths.SubtermStepSumTerm -> Nothing
        Paths.SubtermStepTypeLambdaBody -> Nothing
        Paths.SubtermStepTypeApplicationTerm -> Nothing
        Paths.SubtermStepInjectionTerm -> Nothing
        Paths.SubtermStepWrappedTerm -> Nothing

-- | Build a subterm graph from a term
termToSubtermGraph :: M.Map Module.Namespace String -> Core.Term -> Paths.SubtermGraph
termToSubtermGraph namespaces term =

      let dontCareStep = Paths.SubtermStepAnnotatedBody
          helper =
                  \ids -> \mroot -> \path -> \state -> \stepTerm ->
                    let step = Pairs.first stepTerm
                        currentTerm = Pairs.second stepTerm
                        nodesEdges = Pairs.first state
                        visited = Pairs.second state
                        nodes = Pairs.first nodesEdges
                        edges = Pairs.second nodesEdges
                        nextPath = Lists.cons step path
                    in case currentTerm of
                      Core.TermLet v0 ->
                        let bindings = Core.letBindings v0
                            env = Core.letBody v0
                            bindingNames = Lists.map Core.bindingName bindings
                            addBindingName =
                                    \nodesVisitedIds -> \name ->
                                      let currentNodesVisited = Pairs.first nodesVisitedIds
                                          currentIds = Pairs.second nodesVisitedIds
                                          currentNodes = Pairs.first currentNodesVisited
                                          currentVisited = Pairs.second currentNodesVisited
                                          rawLabel = Names.compactName namespaces name
                                          uniqueLabel = Names.uniqueLabel currentVisited rawLabel
                                          node =
                                                  Paths.SubtermNode {
                                                    Paths.subtermNodeName = name,
                                                    Paths.subtermNodeLabel = rawLabel,
                                                    Paths.subtermNodeId = uniqueLabel}
                                          newVisited = Sets.insert uniqueLabel currentVisited
                                          newNodes = Lists.cons node currentNodes
                                          newIds = Maps.insert name node currentIds
                                      in ((newNodes, newVisited), newIds)
                            nodesVisitedIds1 = Lists.foldl addBindingName (([], visited), ids) bindingNames
                            nodes1 = Pairs.first (Pairs.first nodesVisitedIds1)
                            visited1 = Pairs.second (Pairs.first nodesVisitedIds1)
                            ids1 = Pairs.second nodesVisitedIds1
                            addBindingTerm =
                                    \currentState -> \nodeBinding ->
                                      let root = Pairs.first nodeBinding
                                          binding = Pairs.second nodeBinding
                                          term1 = Core.bindingTerm binding
                                      in (helper ids1 (Just root) [] currentState (dontCareStep, term1))
                            nodeBindingPairs = Lists.zip nodes1 bindings
                            stateAfterBindings = Lists.foldl addBindingTerm ((Lists.concat2 nodes1 nodes, edges), visited1) nodeBindingPairs
                        in (helper ids1 mroot nextPath stateAfterBindings (Paths.SubtermStepLetBody, env))
                      Core.TermVariable v0 -> Maybes.maybe state (\root -> Maybes.maybe state (\node ->
                        let edge =
                                Paths.SubtermEdge {
                                  Paths.subtermEdgeSource = root,
                                  Paths.subtermEdgePath = (Paths.SubtermPath (Lists.reverse nextPath)),
                                  Paths.subtermEdgeTarget = node}
                            newEdges = Lists.cons edge edges
                        in ((nodes, newEdges), visited)) (Maps.lookup v0 ids)) mroot
                      _ -> Lists.foldl (helper ids mroot nextPath) state (Rewriting.subtermsWithSteps currentTerm)
          initialState = (([], []), Sets.empty)
          result = helper Maps.empty Nothing [] initialState (dontCareStep, term)
          finalNodesEdges = Pairs.first result
          finalNodes = Pairs.first finalNodesEdges
          finalEdges = Pairs.second finalNodesEdges
      in Paths.SubtermGraph {
        Paths.subtermGraphNodes = finalNodes,
        Paths.subtermGraphEdges = finalEdges}
