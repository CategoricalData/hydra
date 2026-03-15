-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.accessors

module Hydra.Dsl.Accessors where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

accessorEdge :: (Accessors.AccessorNode -> Accessors.AccessorPath -> Accessors.AccessorNode -> Accessors.AccessorEdge)
accessorEdge source path target = Accessors.AccessorEdge {
  Accessors.accessorEdgeSource = source,
  Accessors.accessorEdgePath = path,
  Accessors.accessorEdgeTarget = target}

accessorEdgeSource :: (Accessors.AccessorEdge -> Accessors.AccessorNode)
accessorEdgeSource = Accessors.accessorEdgeSource

accessorEdgePath :: (Accessors.AccessorEdge -> Accessors.AccessorPath)
accessorEdgePath = Accessors.accessorEdgePath

accessorEdgeTarget :: (Accessors.AccessorEdge -> Accessors.AccessorNode)
accessorEdgeTarget = Accessors.accessorEdgeTarget

accessorEdgeWithSource :: (Accessors.AccessorEdge -> Accessors.AccessorNode -> Accessors.AccessorEdge)
accessorEdgeWithSource original newVal = Accessors.AccessorEdge {
  Accessors.accessorEdgeSource = newVal,
  Accessors.accessorEdgePath = (Accessors.accessorEdgePath original),
  Accessors.accessorEdgeTarget = (Accessors.accessorEdgeTarget original)}

accessorEdgeWithPath :: (Accessors.AccessorEdge -> Accessors.AccessorPath -> Accessors.AccessorEdge)
accessorEdgeWithPath original newVal = Accessors.AccessorEdge {
  Accessors.accessorEdgeSource = (Accessors.accessorEdgeSource original),
  Accessors.accessorEdgePath = newVal,
  Accessors.accessorEdgeTarget = (Accessors.accessorEdgeTarget original)}

accessorEdgeWithTarget :: (Accessors.AccessorEdge -> Accessors.AccessorNode -> Accessors.AccessorEdge)
accessorEdgeWithTarget original newVal = Accessors.AccessorEdge {
  Accessors.accessorEdgeSource = (Accessors.accessorEdgeSource original),
  Accessors.accessorEdgePath = (Accessors.accessorEdgePath original),
  Accessors.accessorEdgeTarget = newVal}

accessorGraph :: ([Accessors.AccessorNode] -> [Accessors.AccessorEdge] -> Accessors.AccessorGraph)
accessorGraph nodes edges = Accessors.AccessorGraph {
  Accessors.accessorGraphNodes = nodes,
  Accessors.accessorGraphEdges = edges}

accessorGraphNodes :: (Accessors.AccessorGraph -> [Accessors.AccessorNode])
accessorGraphNodes = Accessors.accessorGraphNodes

accessorGraphEdges :: (Accessors.AccessorGraph -> [Accessors.AccessorEdge])
accessorGraphEdges = Accessors.accessorGraphEdges

accessorGraphWithNodes :: (Accessors.AccessorGraph -> [Accessors.AccessorNode] -> Accessors.AccessorGraph)
accessorGraphWithNodes original newVal = Accessors.AccessorGraph {
  Accessors.accessorGraphNodes = newVal,
  Accessors.accessorGraphEdges = (Accessors.accessorGraphEdges original)}

accessorGraphWithEdges :: (Accessors.AccessorGraph -> [Accessors.AccessorEdge] -> Accessors.AccessorGraph)
accessorGraphWithEdges original newVal = Accessors.AccessorGraph {
  Accessors.accessorGraphNodes = (Accessors.accessorGraphNodes original),
  Accessors.accessorGraphEdges = newVal}

accessorNode :: (Core.Name -> String -> String -> Accessors.AccessorNode)
accessorNode name label id = Accessors.AccessorNode {
  Accessors.accessorNodeName = name,
  Accessors.accessorNodeLabel = label,
  Accessors.accessorNodeId = id}

accessorNodeName :: (Accessors.AccessorNode -> Core.Name)
accessorNodeName = Accessors.accessorNodeName

accessorNodeLabel :: (Accessors.AccessorNode -> String)
accessorNodeLabel = Accessors.accessorNodeLabel

accessorNodeId :: (Accessors.AccessorNode -> String)
accessorNodeId = Accessors.accessorNodeId

accessorNodeWithName :: (Accessors.AccessorNode -> Core.Name -> Accessors.AccessorNode)
accessorNodeWithName original newVal = Accessors.AccessorNode {
  Accessors.accessorNodeName = newVal,
  Accessors.accessorNodeLabel = (Accessors.accessorNodeLabel original),
  Accessors.accessorNodeId = (Accessors.accessorNodeId original)}

accessorNodeWithLabel :: (Accessors.AccessorNode -> String -> Accessors.AccessorNode)
accessorNodeWithLabel original newVal = Accessors.AccessorNode {
  Accessors.accessorNodeName = (Accessors.accessorNodeName original),
  Accessors.accessorNodeLabel = newVal,
  Accessors.accessorNodeId = (Accessors.accessorNodeId original)}

accessorNodeWithId :: (Accessors.AccessorNode -> String -> Accessors.AccessorNode)
accessorNodeWithId original newVal = Accessors.AccessorNode {
  Accessors.accessorNodeName = (Accessors.accessorNodeName original),
  Accessors.accessorNodeLabel = (Accessors.accessorNodeLabel original),
  Accessors.accessorNodeId = newVal}

accessorPath :: ([Accessors.TermAccessor] -> Accessors.AccessorPath)
accessorPath x = (Accessors.AccessorPath x)

unAccessorPath :: (Accessors.AccessorPath -> [Accessors.TermAccessor])
unAccessorPath = Accessors.unAccessorPath

termAccessorAnnotatedBody :: Accessors.TermAccessor
termAccessorAnnotatedBody = Accessors.TermAccessorAnnotatedBody

termAccessorApplicationFunction :: Accessors.TermAccessor
termAccessorApplicationFunction = Accessors.TermAccessorApplicationFunction

termAccessorApplicationArgument :: Accessors.TermAccessor
termAccessorApplicationArgument = Accessors.TermAccessorApplicationArgument

termAccessorLambdaBody :: Accessors.TermAccessor
termAccessorLambdaBody = Accessors.TermAccessorLambdaBody

termAccessorUnionCasesDefault :: Accessors.TermAccessor
termAccessorUnionCasesDefault = Accessors.TermAccessorUnionCasesDefault

termAccessorUnionCasesBranch :: (Core.Name -> Accessors.TermAccessor)
termAccessorUnionCasesBranch x = (Accessors.TermAccessorUnionCasesBranch x)

termAccessorLetBody :: Accessors.TermAccessor
termAccessorLetBody = Accessors.TermAccessorLetBody

termAccessorLetBinding :: (Core.Name -> Accessors.TermAccessor)
termAccessorLetBinding x = (Accessors.TermAccessorLetBinding x)

termAccessorListElement :: (Int -> Accessors.TermAccessor)
termAccessorListElement x = (Accessors.TermAccessorListElement x)

termAccessorMapKey :: (Int -> Accessors.TermAccessor)
termAccessorMapKey x = (Accessors.TermAccessorMapKey x)

termAccessorMapValue :: (Int -> Accessors.TermAccessor)
termAccessorMapValue x = (Accessors.TermAccessorMapValue x)

termAccessorMaybeTerm :: Accessors.TermAccessor
termAccessorMaybeTerm = Accessors.TermAccessorMaybeTerm

termAccessorProductTerm :: (Int -> Accessors.TermAccessor)
termAccessorProductTerm x = (Accessors.TermAccessorProductTerm x)

termAccessorRecordField :: (Core.Name -> Accessors.TermAccessor)
termAccessorRecordField x = (Accessors.TermAccessorRecordField x)

termAccessorSetElement :: (Int -> Accessors.TermAccessor)
termAccessorSetElement x = (Accessors.TermAccessorSetElement x)

termAccessorSumTerm :: Accessors.TermAccessor
termAccessorSumTerm = Accessors.TermAccessorSumTerm

termAccessorTypeLambdaBody :: Accessors.TermAccessor
termAccessorTypeLambdaBody = Accessors.TermAccessorTypeLambdaBody

termAccessorTypeApplicationTerm :: Accessors.TermAccessor
termAccessorTypeApplicationTerm = Accessors.TermAccessorTypeApplicationTerm

termAccessorInjectionTerm :: Accessors.TermAccessor
termAccessorInjectionTerm = Accessors.TermAccessorInjectionTerm

termAccessorWrappedTerm :: Accessors.TermAccessor
termAccessorWrappedTerm = Accessors.TermAccessorWrappedTerm
