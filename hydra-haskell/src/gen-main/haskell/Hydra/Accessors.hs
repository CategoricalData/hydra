-- | Utilities for working with term accessors.

module Hydra.Accessors where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Module as Module
import qualified Hydra.Qnames as Qnames
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a term accessor to a string representation
showTermAccessor :: (Mantle.TermAccessor -> Maybe String)
showTermAccessor accessor =  
  let idx = (\i -> Nothing) 
      idxSuff = (\suffix -> \i -> Optionals.map (\s -> Strings.cat2 s suffix) (idx i))
  in ((\x -> case x of
    Mantle.TermAccessorAnnotatedSubject -> Nothing
    Mantle.TermAccessorApplicationFunction -> (Just "fun")
    Mantle.TermAccessorApplicationArgument -> (Just "arg")
    Mantle.TermAccessorLambdaBody -> (Just "body")
    Mantle.TermAccessorUnionCasesDefault -> (Just "default")
    Mantle.TermAccessorUnionCasesBranch v1 -> (Just (Strings.cat2 "." (Core.unName v1)))
    Mantle.TermAccessorLetEnvironment -> (Just "in")
    Mantle.TermAccessorLetBinding v1 -> (Just (Strings.cat2 (Core.unName v1) "="))
    Mantle.TermAccessorListElement v1 -> (idx v1)
    Mantle.TermAccessorMapKey v1 -> (idxSuff ".key" v1)
    Mantle.TermAccessorMapValue v1 -> (idxSuff ".value" v1)
    Mantle.TermAccessorOptionalTerm -> (Just "just")
    Mantle.TermAccessorProductTerm v1 -> (idx v1)
    Mantle.TermAccessorRecordField v1 -> (Just (Strings.cat2 "." (Core.unName v1)))
    Mantle.TermAccessorSetElement v1 -> (idx v1)
    Mantle.TermAccessorSumTerm -> Nothing
    Mantle.TermAccessorTypeAbstractionBody -> Nothing
    Mantle.TermAccessorTypeApplicationTerm -> Nothing
    Mantle.TermAccessorTypedTerm -> Nothing
    Mantle.TermAccessorInjectionTerm -> Nothing
    Mantle.TermAccessorWrappedTerm -> Nothing) accessor)

-- | Build an accessor graph from a term
termToAccessorGraph :: (M.Map Module.Namespace String -> Core.Term -> Mantle.AccessorGraph)
termToAccessorGraph namespaces term =  
  let dontCareAccessor = Mantle.TermAccessorAnnotatedSubject 
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
                      env = (Core.letEnvironment v1)
                      bindingNames = (Lists.map Core.letBindingName bindings)
                      addBindingName = (\nodesVisitedIds -> \name ->  
                              let currentNodesVisited = (fst nodesVisitedIds) 
                                  currentIds = (snd nodesVisitedIds)
                                  currentNodes = (fst currentNodesVisited)
                                  currentVisited = (snd currentNodesVisited)
                                  rawLabel = (toCompactName namespaces name)
                                  uniqueLabel = (toUniqueLabel currentVisited rawLabel)
                                  node = Mantle.AccessorNode {
                                          Mantle.accessorNodeName = name,
                                          Mantle.accessorNodeLabel = rawLabel,
                                          Mantle.accessorNodeId = uniqueLabel}
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
                                  term1 = (Core.letBindingTerm binding)
                              in (helper ids1 (Just root) [] currentState (dontCareAccessor, term1)))
                      nodeBindingPairs = (Lists.zip nodes1 bindings)
                      stateAfterBindings = (Lists.foldl addBindingTerm ((Lists.concat2 nodes1 nodes, edges), visited1) nodeBindingPairs)
                  in (helper ids1 mroot nextPath stateAfterBindings (Mantle.TermAccessorLetEnvironment, env))
                Core.TermVariable v1 -> (Optionals.maybe state (\root -> Optionals.maybe state (\node ->  
                  let edge = Mantle.AccessorEdge {
                          Mantle.accessorEdgeSource = root,
                          Mantle.accessorEdgePath = (Mantle.AccessorPath (Lists.reverse nextPath)),
                          Mantle.accessorEdgeTarget = node} 
                      newEdges = (Lists.cons edge edges)
                  in ((nodes, newEdges), visited)) (Maps.lookup v1 ids)) mroot)
                _ -> (Lists.foldl (helper ids mroot nextPath) state (Rewriting.subtermsWithAccessors currentTerm))) currentTerm))
      initialState = (([], []), Sets.empty)
      result = (helper Maps.empty Nothing [] initialState (dontCareAccessor, term))
      finalNodesEdges = (fst result)
      finalNodes = (fst finalNodesEdges)
      finalEdges = (snd finalNodesEdges)
  in Mantle.AccessorGraph {
    Mantle.accessorGraphNodes = finalNodes,
    Mantle.accessorGraphEdges = finalEdges}

-- | Convert a name to a compact string representation
toCompactName :: (M.Map Module.Namespace String -> Core.Name -> String)
toCompactName namespaces name =  
  let qualName = (Qnames.qualifyName name) 
      mns = (Module.qualifiedNameNamespace qualName)
      local = (Module.qualifiedNameLocal qualName)
  in (Optionals.maybe (Core.unName name) (\ns -> Optionals.maybe local (\pre -> Strings.cat [
    pre,
    ":",
    local]) (Maps.lookup ns namespaces)) mns)

-- | Generate a unique label by appending apostrophes if needed
toUniqueLabel :: (S.Set String -> String -> String)
toUniqueLabel visited l = (Logic.ifElse (Sets.member l visited) (toUniqueLabel visited (Strings.cat2 l "'")) l)
