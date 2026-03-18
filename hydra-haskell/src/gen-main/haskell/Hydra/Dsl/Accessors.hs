-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.accessors

module Hydra.Dsl.Accessors where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

accessorEdge :: Phantoms.TTerm Accessors.AccessorNode -> Phantoms.TTerm Accessors.AccessorPath -> Phantoms.TTerm Accessors.AccessorNode -> Phantoms.TTerm Accessors.AccessorEdge
accessorEdge source path target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)}]}))

accessorEdgeSource :: Phantoms.TTerm Accessors.AccessorEdge -> Phantoms.TTerm Accessors.AccessorNode
accessorEdgeSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
        Core.projectionField = (Core.Name "source")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

accessorEdgePath :: Phantoms.TTerm Accessors.AccessorEdge -> Phantoms.TTerm Accessors.AccessorPath
accessorEdgePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
        Core.projectionField = (Core.Name "path")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

accessorEdgeTarget :: Phantoms.TTerm Accessors.AccessorEdge -> Phantoms.TTerm Accessors.AccessorNode
accessorEdgeTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
        Core.projectionField = (Core.Name "target")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

accessorEdgeWithSource :: Phantoms.TTerm Accessors.AccessorEdge -> Phantoms.TTerm Accessors.AccessorNode -> Phantoms.TTerm Accessors.AccessorEdge
accessorEdgeWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
              Core.projectionField = (Core.Name "path")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

accessorEdgeWithPath :: Phantoms.TTerm Accessors.AccessorEdge -> Phantoms.TTerm Accessors.AccessorPath -> Phantoms.TTerm Accessors.AccessorEdge
accessorEdgeWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

accessorEdgeWithTarget :: Phantoms.TTerm Accessors.AccessorEdge -> Phantoms.TTerm Accessors.AccessorNode -> Phantoms.TTerm Accessors.AccessorEdge
accessorEdgeWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
              Core.projectionField = (Core.Name "path")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

accessorGraph :: Phantoms.TTerm [Accessors.AccessorNode] -> Phantoms.TTerm [Accessors.AccessorEdge] -> Phantoms.TTerm Accessors.AccessorGraph
accessorGraph nodes edges =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.accessors.AccessorGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Phantoms.unTTerm nodes)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm edges)}]}))

accessorGraphNodes :: Phantoms.TTerm Accessors.AccessorGraph -> Phantoms.TTerm [Accessors.AccessorNode]
accessorGraphNodes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorGraph"),
        Core.projectionField = (Core.Name "nodes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

accessorGraphEdges :: Phantoms.TTerm Accessors.AccessorGraph -> Phantoms.TTerm [Accessors.AccessorEdge]
accessorGraphEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorGraph"),
        Core.projectionField = (Core.Name "edges")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

accessorGraphWithNodes :: Phantoms.TTerm Accessors.AccessorGraph -> Phantoms.TTerm [Accessors.AccessorNode] -> Phantoms.TTerm Accessors.AccessorGraph
accessorGraphWithNodes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.accessors.AccessorGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorGraph"),
              Core.projectionField = (Core.Name "edges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

accessorGraphWithEdges :: Phantoms.TTerm Accessors.AccessorGraph -> Phantoms.TTerm [Accessors.AccessorEdge] -> Phantoms.TTerm Accessors.AccessorGraph
accessorGraphWithEdges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.accessors.AccessorGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorGraph"),
              Core.projectionField = (Core.Name "nodes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

accessorNode :: Phantoms.TTerm Core.Name -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Accessors.AccessorNode
accessorNode name label id =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.accessors.AccessorNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)}]}))

accessorNodeName :: Phantoms.TTerm Accessors.AccessorNode -> Phantoms.TTerm Core.Name
accessorNodeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorNode"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

accessorNodeLabel :: Phantoms.TTerm Accessors.AccessorNode -> Phantoms.TTerm String
accessorNodeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorNode"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

accessorNodeId :: Phantoms.TTerm Accessors.AccessorNode -> Phantoms.TTerm String
accessorNodeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorNode"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

accessorNodeWithName :: Phantoms.TTerm Accessors.AccessorNode -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Accessors.AccessorNode
accessorNodeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.accessors.AccessorNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorNode"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorNode"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

accessorNodeWithLabel :: Phantoms.TTerm Accessors.AccessorNode -> Phantoms.TTerm String -> Phantoms.TTerm Accessors.AccessorNode
accessorNodeWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.accessors.AccessorNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorNode"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorNode"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

accessorNodeWithId :: Phantoms.TTerm Accessors.AccessorNode -> Phantoms.TTerm String -> Phantoms.TTerm Accessors.AccessorNode
accessorNodeWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.accessors.AccessorNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorNode"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.accessors.AccessorNode"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

accessorPath :: Phantoms.TTerm [Accessors.TermAccessor] -> Phantoms.TTerm Accessors.AccessorPath
accessorPath x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.accessors.AccessorPath"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unAccessorPath :: Phantoms.TTerm Accessors.AccessorPath -> Phantoms.TTerm [Accessors.TermAccessor]
unAccessorPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.accessors.AccessorPath")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termAccessorAnnotatedBody :: Phantoms.TTerm Accessors.TermAccessor
termAccessorAnnotatedBody =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotatedBody"),
        Core.fieldTerm = Core.TermUnit}}))

termAccessorApplicationFunction :: Phantoms.TTerm Accessors.TermAccessor
termAccessorApplicationFunction =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationFunction"),
        Core.fieldTerm = Core.TermUnit}}))

termAccessorApplicationArgument :: Phantoms.TTerm Accessors.TermAccessor
termAccessorApplicationArgument =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationArgument"),
        Core.fieldTerm = Core.TermUnit}}))

termAccessorLambdaBody :: Phantoms.TTerm Accessors.TermAccessor
termAccessorLambdaBody =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambdaBody"),
        Core.fieldTerm = Core.TermUnit}}))

termAccessorUnionCasesDefault :: Phantoms.TTerm Accessors.TermAccessor
termAccessorUnionCasesDefault =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unionCasesDefault"),
        Core.fieldTerm = Core.TermUnit}}))

termAccessorUnionCasesBranch :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Accessors.TermAccessor
termAccessorUnionCasesBranch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unionCasesBranch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termAccessorLetBody :: Phantoms.TTerm Accessors.TermAccessor
termAccessorLetBody =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "letBody"),
        Core.fieldTerm = Core.TermUnit}}))

termAccessorLetBinding :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Accessors.TermAccessor
termAccessorLetBinding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "letBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termAccessorListElement :: Phantoms.TTerm Int -> Phantoms.TTerm Accessors.TermAccessor
termAccessorListElement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listElement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termAccessorMapKey :: Phantoms.TTerm Int -> Phantoms.TTerm Accessors.TermAccessor
termAccessorMapKey x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapKey"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termAccessorMapValue :: Phantoms.TTerm Int -> Phantoms.TTerm Accessors.TermAccessor
termAccessorMapValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termAccessorMaybeTerm :: Phantoms.TTerm Accessors.TermAccessor
termAccessorMaybeTerm =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybeTerm"),
        Core.fieldTerm = Core.TermUnit}}))

termAccessorProductTerm :: Phantoms.TTerm Int -> Phantoms.TTerm Accessors.TermAccessor
termAccessorProductTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "productTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termAccessorRecordField :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Accessors.TermAccessor
termAccessorRecordField x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recordField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termAccessorSetElement :: Phantoms.TTerm Int -> Phantoms.TTerm Accessors.TermAccessor
termAccessorSetElement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "setElement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termAccessorSumTerm :: Phantoms.TTerm Accessors.TermAccessor
termAccessorSumTerm =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sumTerm"),
        Core.fieldTerm = Core.TermUnit}}))

termAccessorTypeLambdaBody :: Phantoms.TTerm Accessors.TermAccessor
termAccessorTypeLambdaBody =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLambdaBody"),
        Core.fieldTerm = Core.TermUnit}}))

termAccessorTypeApplicationTerm :: Phantoms.TTerm Accessors.TermAccessor
termAccessorTypeApplicationTerm =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeApplicationTerm"),
        Core.fieldTerm = Core.TermUnit}}))

termAccessorInjectionTerm :: Phantoms.TTerm Accessors.TermAccessor
termAccessorInjectionTerm =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "injectionTerm"),
        Core.fieldTerm = Core.TermUnit}}))

termAccessorWrappedTerm :: Phantoms.TTerm Accessors.TermAccessor
termAccessorWrappedTerm =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrappedTerm"),
        Core.fieldTerm = Core.TermUnit}}))
