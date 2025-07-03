module Hydra.Dsl.Accessors where

import Hydra.Kernel
import Hydra.Dsl.Phantoms
import Hydra.Accessors

import qualified Data.Map as M
import qualified Data.Maybe as Y


accessorEdge :: TTerm AccessorNode -> TTerm AccessorPath -> TTerm AccessorNode -> TTerm AccessorEdge
accessorEdge source path target = record _AccessorEdge [
  _AccessorEdge_source>>: source,
  _AccessorEdge_path>>: path,
  _AccessorEdge_target>>: target]

accessorEdgeSource = unitVariant _AccessorEdge _AccessorEdge_source
accessorEdgePath = unitVariant _AccessorEdge _AccessorEdge_path
accessorEdgeTarget = unitVariant _AccessorEdge _AccessorEdge_target

accessorGraph :: TTerm [AccessorNode] -> TTerm [AccessorEdge] -> TTerm AccessorGraph
accessorGraph nodes edges = record _AccessorGraph [
  _AccessorGraph_nodes>>: nodes,
  _AccessorGraph_edges>>: edges]

accessorGraphNodes = unitVariant _AccessorGraph _AccessorGraph_nodes
accessorGraphEdges = unitVariant _AccessorGraph _AccessorGraph_edges

accessorNode :: TTerm Name -> TTerm String -> TTerm String -> TTerm AccessorNode
accessorNode name label id = record _AccessorNode [
  _AccessorNode_name>>: name,
  _AccessorNode_label>>: label,
  _AccessorNode_id>>: id]

accessorNodeName = unitVariant _AccessorNode _AccessorNode_name
accessorNodeLabel = unitVariant _AccessorNode _AccessorNode_label
accessorNodeId = unitVariant _AccessorNode _AccessorNode_id

accessorPath :: TTerm [TermAccessor] -> TTerm AccessorPath
accessorPath path = wrap _AccessorPath path

termAccessorAnnotatedSubject :: TTerm TermAccessor
termAccessorAnnotatedSubject = unitVariant _TermAccessor _TermAccessor_annotatedSubject

termAccessorApplicationFunction :: TTerm TermAccessor
termAccessorApplicationFunction = unitVariant _TermAccessor _TermAccessor_applicationFunction

termAccessorApplicationArgument :: TTerm TermAccessor
termAccessorApplicationArgument = unitVariant _TermAccessor _TermAccessor_applicationArgument

termAccessorLambdaBody :: TTerm TermAccessor
termAccessorLambdaBody = unitVariant _TermAccessor _TermAccessor_lambdaBody

termAccessorUnionCasesDefault :: TTerm TermAccessor
termAccessorUnionCasesDefault = unitVariant _TermAccessor _TermAccessor_unionCasesDefault

termAccessorUnionCasesBranch :: TTerm Name -> TTerm TermAccessor
termAccessorUnionCasesBranch = variant _TermAccessor _TermAccessor_unionCasesBranch

termAccessorLetEnvironment :: TTerm TermAccessor
termAccessorLetEnvironment = unitVariant _TermAccessor _TermAccessor_letEnvironment

termAccessorLetBinding :: TTerm Name -> TTerm TermAccessor
termAccessorLetBinding = variant _TermAccessor _TermAccessor_letBinding

termAccessorListElement :: TTerm Int -> TTerm TermAccessor
termAccessorListElement = variant _TermAccessor _TermAccessor_listElement

termAccessorMapKey :: TTerm Int -> TTerm TermAccessor
termAccessorMapKey = variant _TermAccessor _TermAccessor_mapKey

termAccessorMapValue :: TTerm Int -> TTerm TermAccessor
termAccessorMapValue = variant _TermAccessor _TermAccessor_mapValue

termAccessorOptionalTerm :: TTerm TermAccessor
termAccessorOptionalTerm = unitVariant _TermAccessor _TermAccessor_optionalTerm

termAccessorProductTerm :: TTerm Int -> TTerm TermAccessor
termAccessorProductTerm = variant _TermAccessor _TermAccessor_productTerm

termAccessorRecordField :: TTerm Name -> TTerm TermAccessor
termAccessorRecordField = variant _TermAccessor _TermAccessor_recordField

termAccessorSetElement :: TTerm Int -> TTerm TermAccessor
termAccessorSetElement = variant _TermAccessor _TermAccessor_setElement

termAccessorSumTerm :: TTerm TermAccessor
termAccessorSumTerm = unitVariant _TermAccessor _TermAccessor_sumTerm

termAccessorTypeAbstractionBody :: TTerm TermAccessor
termAccessorTypeAbstractionBody = unitVariant _TermAccessor _TermAccessor_typeAbstractionBody

termAccessorTypeApplicationTerm :: TTerm TermAccessor
termAccessorTypeApplicationTerm = unitVariant _TermAccessor _TermAccessor_typeApplicationTerm

termAccessorInjectionTerm :: TTerm TermAccessor
termAccessorInjectionTerm = unitVariant _TermAccessor _TermAccessor_injectionTerm

termAccessorWrappedTerm :: TTerm TermAccessor
termAccessorWrappedTerm = unitVariant _TermAccessor _TermAccessor_wrappedTerm

unAccessorPath :: TTerm AccessorPath -> TTerm [TermAccessor]
unAccessorPath path = unwrap _AccessorPath @@ path
