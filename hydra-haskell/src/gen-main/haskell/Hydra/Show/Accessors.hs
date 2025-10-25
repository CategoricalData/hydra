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
  in  
    let idxSuff = (\suffix -> \i -> Optionals.map (\s -> Strings.cat2 s suffix) (idx i))
    in ((\x -> case x of
      Accessors.TermAccessorAnnotatedBody -> Nothing
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
  let dontCareAccessor = Accessors.TermAccessorAnnotatedBody
  in  
    let helper = (\ids -> \mroot -> \path -> \state -> \accessorTerm ->  
            let accessor = (fst accessorTerm)
            in  
              let currentTerm = (snd accessorTerm)
              in  
                let nodesEdges = (fst state)
                in  
                  let visited = (snd state)
                  in  
                    let nodes = (fst nodesEdges)
                    in  
                      let edges = (snd nodesEdges)
                      in  
                        let nextPath = (Lists.cons accessor path)
                        in ((\x -> case x of
                          Core.TermLet v1 ->  
                            let bindings = (Core.letBindings v1)
                            in  
                              let env = (Core.letBody v1)
                              in  
                                let bindingNames = (Lists.map Core.bindingName bindings)
                                in  
                                  let addBindingName = (\nodesVisitedIds -> \name ->  
                                          let currentNodesVisited = (fst nodesVisitedIds)
                                          in  
                                            let currentIds = (snd nodesVisitedIds)
                                            in  
                                              let currentNodes = (fst currentNodesVisited)
                                              in  
                                                let currentVisited = (snd currentNodesVisited)
                                                in  
                                                  let rawLabel = (Names.compactName namespaces name)
                                                  in  
                                                    let uniqueLabel = (Names.uniqueLabel currentVisited rawLabel)
                                                    in  
                                                      let node = Accessors.AccessorNode {
                                                              Accessors.accessorNodeName = name,
                                                              Accessors.accessorNodeLabel = rawLabel,
                                                              Accessors.accessorNodeId = uniqueLabel}
                                                      in  
                                                        let newVisited = (Sets.insert uniqueLabel currentVisited)
                                                        in  
                                                          let newNodes = (Lists.cons node currentNodes)
                                                          in  
                                                            let newIds = (Maps.insert name node currentIds)
                                                            in ((newNodes, newVisited), newIds))
                                  in  
                                    let nodesVisitedIds1 = (Lists.foldl addBindingName (([], visited), ids) bindingNames)
                                    in  
                                      let nodes1 = (fst (fst nodesVisitedIds1))
                                      in  
                                        let visited1 = (snd (fst nodesVisitedIds1))
                                        in  
                                          let ids1 = (snd nodesVisitedIds1)
                                          in  
                                            let addBindingTerm = (\currentState -> \nodeBinding ->  
                                                    let root = (fst nodeBinding)
                                                    in  
                                                      let binding = (snd nodeBinding)
                                                      in  
                                                        let term1 = (Core.bindingTerm binding)
                                                        in (helper ids1 (Just root) [] currentState (dontCareAccessor, term1)))
                                            in  
                                              let nodeBindingPairs = (Lists.zip nodes1 bindings)
                                              in  
                                                let stateAfterBindings = (Lists.foldl addBindingTerm ((Lists.concat2 nodes1 nodes, edges), visited1) nodeBindingPairs)
                                                in (helper ids1 mroot nextPath stateAfterBindings (Accessors.TermAccessorLetBody, env))
                          Core.TermVariable v1 -> (Optionals.maybe state (\root -> Optionals.maybe state (\node ->  
                            let edge = Accessors.AccessorEdge {
                                    Accessors.accessorEdgeSource = root,
                                    Accessors.accessorEdgePath = (Accessors.AccessorPath (Lists.reverse nextPath)),
                                    Accessors.accessorEdgeTarget = node}
                            in  
                              let newEdges = (Lists.cons edge edges)
                              in ((nodes, newEdges), visited)) (Maps.lookup v1 ids)) mroot)
                          _ -> (Lists.foldl (helper ids mroot nextPath) state (Rewriting.subtermsWithAccessors currentTerm))) currentTerm))
    in  
      let initialState = (([], []), Sets.empty)
      in  
        let result = (helper Maps.empty Nothing [] initialState (dontCareAccessor, term))
        in  
          let finalNodesEdges = (fst result)
          in  
            let finalNodes = (fst finalNodesEdges)
            in  
              let finalEdges = (snd finalNodesEdges)
              in Accessors.AccessorGraph {
                Accessors.accessorGraphNodes = finalNodes,
                Accessors.accessorGraphEdges = finalEdges}
