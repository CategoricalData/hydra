-- | Utilities for working with term accessors.

module Hydra.Show.Accessors where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a term accessor to a string representation
termAccessor :: (Accessors.TermAccessor -> Maybe String)
termAccessor accessor =  
  let idx = (\i -> Nothing) 
      idxSuff = (\suffix -> \i -> Optionals.map (\s -> Strings.cat2 s suffix) (idx i))
  in ((\x -> case x of
    Accessors.TermAccessorAnnotatedSubject -> Nothing
    Accessors.TermAccessorApplicationFunction -> (Just "fun")
    Accessors.TermAccessorApplicationArgument -> (Just "arg")
    Accessors.TermAccessorLambdaBody -> (Just "body")
    Accessors.TermAccessorUnionCasesDefault -> (Just "default")
    Accessors.TermAccessorUnionCasesBranch v1 -> (Just (Strings.cat2 "." (Core.unName v1)))
    Accessors.TermAccessorLetBody -> (Just "in")
    Accessors.TermAccessorLetBinding v1 -> (Just (Strings.cat2 (Core.unName v1) "="))
    Accessors.TermAccessorListElement v1 -> (idx v1)
    Accessors.TermAccessorMapKey v1 -> (idxSuff ".key" v1)
    Accessors.TermAccessorMapValue v1 -> (idxSuff ".value" v1)
    Accessors.TermAccessorOptionalTerm -> (Just "just")
    Accessors.TermAccessorProductTerm v1 -> (idx v1)
    Accessors.TermAccessorRecordField v1 -> (Just (Strings.cat2 "." (Core.unName v1)))
    Accessors.TermAccessorSetElement v1 -> (idx v1)
    Accessors.TermAccessorSumTerm -> Nothing
    Accessors.TermAccessorTypeLambdaBody -> Nothing
    Accessors.TermAccessorTypeApplicationTerm -> Nothing
    Accessors.TermAccessorInjectionTerm -> Nothing
    Accessors.TermAccessorWrappedTerm -> Nothing) accessor)

-- | Build an accessor graph from a term
termToAccessorGraph :: (M.Map Module.Namespace String -> Core.Term -> Accessors.AccessorGraph)
termToAccessorGraph namespaces term =  
  let dontCareAccessor = Accessors.TermAccessorAnnotatedSubject 
      helper = (\ids -> \mroot -> \path -> \state -> \accessorTerm ->  
              let accessor = (fst accessorTerm) 
                  currentTerm = (snd accessorTerm)
                  nodesEdges = (fst state)
                  visited = (snd state)
                  nodes = (fst nodesEdges)
                  edges = (snd nodesEdges)
                  nextPath = (Lists.cons accessor path)
              in ((\x -> case x of
                Core.TermLet v1 ->  
                  let bindings = (Core.letBindings v1) 
                      env = (Core.letBody v1)
                      bindingNames = (Lists.map Core.bindingName bindings)
                      addBindingName = (\nodesVisitedIds -> \name ->  
                              let currentNodesVisited = (fst nodesVisitedIds) 
                                  currentIds = (snd nodesVisitedIds)
                                  currentNodes = (fst currentNodesVisited)
                                  currentVisited = (snd currentNodesVisited)
                                  rawLabel = (Names.compactName namespaces name)
                                  uniqueLabel = (Names.uniqueLabel currentVisited rawLabel)
                                  node = Accessors.AccessorNode {
                                          Accessors.accessorNodeName = name,
                                          Accessors.accessorNodeLabel = rawLabel,
                                          Accessors.accessorNodeId = uniqueLabel}
                                  newVisited = (Sets.insert uniqueLabel currentVisited)
                                  newNodes = (Lists.cons node currentNodes)
                                  newIds = (Maps.insert name node currentIds)
                              in ((newNodes, newVisited), newIds))
                      nodesVisitedIds1 = (Lists.foldl addBindingName (([], visited), ids) bindingNames)
                      nodes1 = (fst (fst nodesVisitedIds1))
                      visited1 = (snd (fst nodesVisitedIds1))
                      ids1 = (snd nodesVisitedIds1)
                      addBindingTerm = (\currentState -> \nodeBinding ->  
                              let root = (fst nodeBinding) 
                                  binding = (snd nodeBinding)
                                  term1 = (Core.bindingTerm binding)
                              in (helper ids1 (Just root) [] currentState (dontCareAccessor, term1)))
                      nodeBindingPairs = (Lists.zip nodes1 bindings)
                      stateAfterBindings = (Lists.foldl addBindingTerm ((Lists.concat2 nodes1 nodes, edges), visited1) nodeBindingPairs)
                  in (helper ids1 mroot nextPath stateAfterBindings (Accessors.TermAccessorLetBody, env))
                Core.TermVariable v1 -> (Optionals.maybe state (\root -> Optionals.maybe state (\node ->  
                  let edge = Accessors.AccessorEdge {
                          Accessors.accessorEdgeSource = root,
                          Accessors.accessorEdgePath = (Accessors.AccessorPath (Lists.reverse nextPath)),
                          Accessors.accessorEdgeTarget = node} 
                      newEdges = (Lists.cons edge edges)
                  in ((nodes, newEdges), visited)) (Maps.lookup v1 ids)) mroot)
                _ -> (Lists.foldl (helper ids mroot nextPath) state (Rewriting.subtermsWithAccessors currentTerm))) currentTerm))
      initialState = (([], []), Sets.empty)
      result = (helper Maps.empty Nothing [] initialState (dontCareAccessor, term))
      finalNodesEdges = (fst result)
      finalNodes = (fst finalNodesEdges)
      finalEdges = (snd finalNodesEdges)
  in Accessors.AccessorGraph {
    Accessors.accessorGraphNodes = finalNodes,
    Accessors.accessorGraphEdges = finalEdges}
