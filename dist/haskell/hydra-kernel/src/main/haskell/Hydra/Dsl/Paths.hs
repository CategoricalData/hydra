-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.paths

module Hydra.Dsl.Paths where

import qualified Hydra.Core as Core
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

subtermEdge :: Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm Paths.SubtermEdge
subtermEdge source path target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermEdge"),
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

subtermEdgePath :: Phantoms.TTerm Paths.SubtermEdge -> Phantoms.TTerm Paths.SubtermPath
subtermEdgePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtermEdgeSource :: Phantoms.TTerm Paths.SubtermEdge -> Phantoms.TTerm Paths.SubtermNode
subtermEdgeSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
        Core.projectionField = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtermEdgeTarget :: Phantoms.TTerm Paths.SubtermEdge -> Phantoms.TTerm Paths.SubtermNode
subtermEdgeTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
        Core.projectionField = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtermEdgeWithPath :: Phantoms.TTerm Paths.SubtermEdge -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Paths.SubtermEdge
subtermEdgeWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subtermEdgeWithSource :: Phantoms.TTerm Paths.SubtermEdge -> Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm Paths.SubtermEdge
subtermEdgeWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subtermEdgeWithTarget :: Phantoms.TTerm Paths.SubtermEdge -> Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm Paths.SubtermEdge
subtermEdgeWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subtermGraph :: Phantoms.TTerm [Paths.SubtermNode] -> Phantoms.TTerm [Paths.SubtermEdge] -> Phantoms.TTerm Paths.SubtermGraph
subtermGraph nodes edges =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Phantoms.unTTerm nodes)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm edges)}]}))

subtermGraphEdges :: Phantoms.TTerm Paths.SubtermGraph -> Phantoms.TTerm [Paths.SubtermEdge]
subtermGraphEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
        Core.projectionField = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtermGraphNodes :: Phantoms.TTerm Paths.SubtermGraph -> Phantoms.TTerm [Paths.SubtermNode]
subtermGraphNodes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
        Core.projectionField = (Core.Name "nodes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtermGraphWithEdges :: Phantoms.TTerm Paths.SubtermGraph -> Phantoms.TTerm [Paths.SubtermEdge] -> Phantoms.TTerm Paths.SubtermGraph
subtermGraphWithEdges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
              Core.projectionField = (Core.Name "nodes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subtermGraphWithNodes :: Phantoms.TTerm Paths.SubtermGraph -> Phantoms.TTerm [Paths.SubtermNode] -> Phantoms.TTerm Paths.SubtermGraph
subtermGraphWithNodes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
              Core.projectionField = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subtermNode :: Phantoms.TTerm Core.Name -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Paths.SubtermNode
subtermNode name label id =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermNode"),
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

subtermNodeId :: Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm String
subtermNodeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtermNodeLabel :: Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm String
subtermNodeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtermNodeName :: Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm Core.Name
subtermNodeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtermNodeWithId :: Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm String -> Phantoms.TTerm Paths.SubtermNode
subtermNodeWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subtermNodeWithLabel :: Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm String -> Phantoms.TTerm Paths.SubtermNode
subtermNodeWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subtermNodeWithName :: Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtermNode
subtermNodeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subtermPath :: Phantoms.TTerm [Paths.SubtermStep] -> Phantoms.TTerm Paths.SubtermPath
subtermPath x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.paths.SubtermPath"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

subtermStepAnnotatedBody :: Phantoms.TTerm Paths.SubtermStep
subtermStepAnnotatedBody =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotatedBody"),
        Core.fieldTerm = Core.TermUnit}}))

subtermStepApplicationArgument :: Phantoms.TTerm Paths.SubtermStep
subtermStepApplicationArgument =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationArgument"),
        Core.fieldTerm = Core.TermUnit}}))

subtermStepApplicationFunction :: Phantoms.TTerm Paths.SubtermStep
subtermStepApplicationFunction =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationFunction"),
        Core.fieldTerm = Core.TermUnit}}))

subtermStepInjectionTerm :: Phantoms.TTerm Paths.SubtermStep
subtermStepInjectionTerm =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "injectionTerm"),
        Core.fieldTerm = Core.TermUnit}}))

subtermStepLambdaBody :: Phantoms.TTerm Paths.SubtermStep
subtermStepLambdaBody =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambdaBody"),
        Core.fieldTerm = Core.TermUnit}}))

subtermStepLetBinding :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtermStep
subtermStepLetBinding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "letBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

subtermStepLetBody :: Phantoms.TTerm Paths.SubtermStep
subtermStepLetBody =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "letBody"),
        Core.fieldTerm = Core.TermUnit}}))

subtermStepListElement :: Phantoms.TTerm Int -> Phantoms.TTerm Paths.SubtermStep
subtermStepListElement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listElement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

subtermStepMapKey :: Phantoms.TTerm Int -> Phantoms.TTerm Paths.SubtermStep
subtermStepMapKey x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapKey"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

subtermStepMapValue :: Phantoms.TTerm Int -> Phantoms.TTerm Paths.SubtermStep
subtermStepMapValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

subtermStepMaybeTerm :: Phantoms.TTerm Paths.SubtermStep
subtermStepMaybeTerm =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybeTerm"),
        Core.fieldTerm = Core.TermUnit}}))

subtermStepProductTerm :: Phantoms.TTerm Int -> Phantoms.TTerm Paths.SubtermStep
subtermStepProductTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "productTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

subtermStepRecordField :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtermStep
subtermStepRecordField x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recordField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

subtermStepSetElement :: Phantoms.TTerm Int -> Phantoms.TTerm Paths.SubtermStep
subtermStepSetElement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "setElement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

subtermStepSumTerm :: Phantoms.TTerm Paths.SubtermStep
subtermStepSumTerm =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sumTerm"),
        Core.fieldTerm = Core.TermUnit}}))

subtermStepTypeApplicationTerm :: Phantoms.TTerm Paths.SubtermStep
subtermStepTypeApplicationTerm =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeApplicationTerm"),
        Core.fieldTerm = Core.TermUnit}}))

subtermStepTypeLambdaBody :: Phantoms.TTerm Paths.SubtermStep
subtermStepTypeLambdaBody =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLambdaBody"),
        Core.fieldTerm = Core.TermUnit}}))

subtermStepUnionCasesBranch :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtermStep
subtermStepUnionCasesBranch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unionCasesBranch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

subtermStepUnionCasesDefault :: Phantoms.TTerm Paths.SubtermStep
subtermStepUnionCasesDefault =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unionCasesDefault"),
        Core.fieldTerm = Core.TermUnit}}))

subtermStepWrappedTerm :: Phantoms.TTerm Paths.SubtermStep
subtermStepWrappedTerm =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrappedTerm"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeEdge :: Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm Paths.SubtypePath -> Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm Paths.SubtypeEdge
subtypeEdge source path target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
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

subtypeEdgePath :: Phantoms.TTerm Paths.SubtypeEdge -> Phantoms.TTerm Paths.SubtypePath
subtypeEdgePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtypeEdgeSource :: Phantoms.TTerm Paths.SubtypeEdge -> Phantoms.TTerm Paths.SubtypeNode
subtypeEdgeSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
        Core.projectionField = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtypeEdgeTarget :: Phantoms.TTerm Paths.SubtypeEdge -> Phantoms.TTerm Paths.SubtypeNode
subtypeEdgeTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
        Core.projectionField = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtypeEdgeWithPath :: Phantoms.TTerm Paths.SubtypeEdge -> Phantoms.TTerm Paths.SubtypePath -> Phantoms.TTerm Paths.SubtypeEdge
subtypeEdgeWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subtypeEdgeWithSource :: Phantoms.TTerm Paths.SubtypeEdge -> Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm Paths.SubtypeEdge
subtypeEdgeWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subtypeEdgeWithTarget :: Phantoms.TTerm Paths.SubtypeEdge -> Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm Paths.SubtypeEdge
subtypeEdgeWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subtypeGraph :: Phantoms.TTerm [Paths.SubtypeNode] -> Phantoms.TTerm [Paths.SubtypeEdge] -> Phantoms.TTerm Paths.SubtypeGraph
subtypeGraph nodes edges =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Phantoms.unTTerm nodes)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm edges)}]}))

subtypeGraphEdges :: Phantoms.TTerm Paths.SubtypeGraph -> Phantoms.TTerm [Paths.SubtypeEdge]
subtypeGraphEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
        Core.projectionField = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtypeGraphNodes :: Phantoms.TTerm Paths.SubtypeGraph -> Phantoms.TTerm [Paths.SubtypeNode]
subtypeGraphNodes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
        Core.projectionField = (Core.Name "nodes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtypeGraphWithEdges :: Phantoms.TTerm Paths.SubtypeGraph -> Phantoms.TTerm [Paths.SubtypeEdge] -> Phantoms.TTerm Paths.SubtypeGraph
subtypeGraphWithEdges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
              Core.projectionField = (Core.Name "nodes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subtypeGraphWithNodes :: Phantoms.TTerm Paths.SubtypeGraph -> Phantoms.TTerm [Paths.SubtypeNode] -> Phantoms.TTerm Paths.SubtypeGraph
subtypeGraphWithNodes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
              Core.projectionField = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subtypeNode :: Phantoms.TTerm Core.Name -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Paths.SubtypeNode
subtypeNode name label id =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeNode"),
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

subtypeNodeId :: Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm String
subtypeNodeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtypeNodeLabel :: Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm String
subtypeNodeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtypeNodeName :: Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm Core.Name
subtypeNodeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtypeNodeWithId :: Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm String -> Phantoms.TTerm Paths.SubtypeNode
subtypeNodeWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subtypeNodeWithLabel :: Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm String -> Phantoms.TTerm Paths.SubtypeNode
subtypeNodeWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subtypeNodeWithName :: Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtypeNode
subtypeNodeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subtypePath :: Phantoms.TTerm [Paths.SubtypeStep] -> Phantoms.TTerm Paths.SubtypePath
subtypePath x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.paths.SubtypePath"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

subtypeStepAnnotatedBody :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepAnnotatedBody =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotatedBody"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepApplicationArgument :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepApplicationArgument =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationArgument"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepApplicationFunction :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepApplicationFunction =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationFunction"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepEitherLeft :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepEitherLeft =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eitherLeft"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepEitherRight :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepEitherRight =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eitherRight"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepForallBody :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepForallBody =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forallBody"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepFunctionCodomain :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepFunctionCodomain =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionCodomain"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepFunctionDomain :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepFunctionDomain =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionDomain"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepListElement :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepListElement =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listElement"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepMapKeys :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepMapKeys =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapKeys"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepMapValues :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepMapValues =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapValues"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepMaybeElement :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepMaybeElement =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybeElement"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepPairFirst :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepPairFirst =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pairFirst"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepPairSecond :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepPairSecond =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pairSecond"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepRecordField :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtypeStep
subtypeStepRecordField x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recordField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

subtypeStepSetElement :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepSetElement =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "setElement"),
        Core.fieldTerm = Core.TermUnit}}))

subtypeStepUnionField :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtypeStep
subtypeStepUnionField x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unionField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

subtypeStepWrappedType :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepWrappedType =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrappedType"),
        Core.fieldTerm = Core.TermUnit}}))

unSubtermPath :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm [Paths.SubtermStep]
unSubtermPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.paths.SubtermPath")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSubtypePath :: Phantoms.TTerm Paths.SubtypePath -> Phantoms.TTerm [Paths.SubtypeStep]
unSubtypePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.paths.SubtypePath")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
