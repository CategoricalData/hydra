module Hydra.Dsl.Accessors where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Accessors

import qualified Data.Map as M
import qualified Data.Maybe as Y


accessorEdge :: TTerm AccessorNode -> TTerm AccessorPath -> TTerm AccessorNode -> TTerm AccessorEdge
accessorEdge source path target = record _AccessorEdge [
  _AccessorEdge_source>>: source,
  _AccessorEdge_path>>: path,
  _AccessorEdge_target>>: target]

accessorEdgeSource = injectUnit _AccessorEdge _AccessorEdge_source
accessorEdgePath = injectUnit _AccessorEdge _AccessorEdge_path
accessorEdgeTarget = injectUnit _AccessorEdge _AccessorEdge_target

accessorGraph :: TTerm [AccessorNode] -> TTerm [AccessorEdge] -> TTerm AccessorGraph
accessorGraph nodes edges = record _AccessorGraph [
  _AccessorGraph_nodes>>: nodes,
  _AccessorGraph_edges>>: edges]

accessorGraphNodes = injectUnit _AccessorGraph _AccessorGraph_nodes
accessorGraphEdges = injectUnit _AccessorGraph _AccessorGraph_edges

accessorNode :: TTerm Name -> TTerm String -> TTerm String -> TTerm AccessorNode
accessorNode name label id = record _AccessorNode [
  _AccessorNode_name>>: name,
  _AccessorNode_label>>: label,
  _AccessorNode_id>>: id]

accessorNodeName = injectUnit _AccessorNode _AccessorNode_name
accessorNodeLabel = injectUnit _AccessorNode _AccessorNode_label
accessorNodeId = injectUnit _AccessorNode _AccessorNode_id

accessorPath :: TTerm [TermAccessor] -> TTerm AccessorPath
accessorPath path = wrap _AccessorPath path

termAccessorAnnotatedBody :: TTerm TermAccessor
termAccessorAnnotatedBody = injectUnit _TermAccessor _TermAccessor_annotatedBody

termAccessorApplicationFunction :: TTerm TermAccessor
termAccessorApplicationFunction = injectUnit _TermAccessor _TermAccessor_applicationFunction

termAccessorApplicationArgument :: TTerm TermAccessor
termAccessorApplicationArgument = injectUnit _TermAccessor _TermAccessor_applicationArgument

termAccessorLambdaBody :: TTerm TermAccessor
termAccessorLambdaBody = injectUnit _TermAccessor _TermAccessor_lambdaBody

termAccessorUnionCasesDefault :: TTerm TermAccessor
termAccessorUnionCasesDefault = injectUnit _TermAccessor _TermAccessor_unionCasesDefault

termAccessorUnionCasesBranch :: TTerm Name -> TTerm TermAccessor
termAccessorUnionCasesBranch = inject _TermAccessor _TermAccessor_unionCasesBranch

termAccessorLetEnvironment :: TTerm TermAccessor
termAccessorLetEnvironment = injectUnit _TermAccessor _TermAccessor_letBody

termAccessorLetBinding :: TTerm Name -> TTerm TermAccessor
termAccessorLetBinding = inject _TermAccessor _TermAccessor_letBinding

termAccessorListElement :: TTerm Int -> TTerm TermAccessor
termAccessorListElement = inject _TermAccessor _TermAccessor_listElement

termAccessorMapKey :: TTerm Int -> TTerm TermAccessor
termAccessorMapKey = inject _TermAccessor _TermAccessor_mapKey

termAccessorMapValue :: TTerm Int -> TTerm TermAccessor
termAccessorMapValue = inject _TermAccessor _TermAccessor_mapValue

termAccessorOptionalTerm :: TTerm TermAccessor
termAccessorOptionalTerm = injectUnit _TermAccessor _TermAccessor_maybeTerm

termAccessorProductTerm :: TTerm Int -> TTerm TermAccessor
termAccessorProductTerm = inject _TermAccessor _TermAccessor_productTerm

termAccessorRecordField :: TTerm Name -> TTerm TermAccessor
termAccessorRecordField = inject _TermAccessor _TermAccessor_recordField

termAccessorSetElement :: TTerm Int -> TTerm TermAccessor
termAccessorSetElement = inject _TermAccessor _TermAccessor_setElement

termAccessorSumTerm :: TTerm TermAccessor
termAccessorSumTerm = injectUnit _TermAccessor _TermAccessor_sumTerm

termAccessorTypeLambdaBody :: TTerm TermAccessor
termAccessorTypeLambdaBody = injectUnit _TermAccessor _TermAccessor_typeLambdaBody

termAccessorTypeApplicationTerm :: TTerm TermAccessor
termAccessorTypeApplicationTerm = injectUnit _TermAccessor _TermAccessor_typeApplicationTerm

termAccessorInjectionTerm :: TTerm TermAccessor
termAccessorInjectionTerm = injectUnit _TermAccessor _TermAccessor_injectionTerm

termAccessorWrappedTerm :: TTerm TermAccessor
termAccessorWrappedTerm = injectUnit _TermAccessor _TermAccessor_wrappedTerm

unAccessorPath :: TTerm AccessorPath -> TTerm [TermAccessor]
unAccessorPath path = unwrap _AccessorPath @@ path
