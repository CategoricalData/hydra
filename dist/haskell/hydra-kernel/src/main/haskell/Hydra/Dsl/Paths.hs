-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.paths

module Hydra.Dsl.Paths where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.paths.SubtermEdge
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
-- | DSL accessor for the path field of hydra.paths.SubtermEdge
subtermEdgePath :: Phantoms.TTerm Paths.SubtermEdge -> Phantoms.TTerm Paths.SubtermPath
subtermEdgePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the source field of hydra.paths.SubtermEdge
subtermEdgeSource :: Phantoms.TTerm Paths.SubtermEdge -> Phantoms.TTerm Paths.SubtermNode
subtermEdgeSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
        Core.projectionFieldName = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the target field of hydra.paths.SubtermEdge
subtermEdgeTarget :: Phantoms.TTerm Paths.SubtermEdge -> Phantoms.TTerm Paths.SubtermNode
subtermEdgeTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the path field of hydra.paths.SubtermEdge
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
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the source field of hydra.paths.SubtermEdge
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
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the target field of hydra.paths.SubtermEdge
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
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.paths.SubtermGraph
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
-- | DSL accessor for the edges field of hydra.paths.SubtermGraph
subtermGraphEdges :: Phantoms.TTerm Paths.SubtermGraph -> Phantoms.TTerm [Paths.SubtermEdge]
subtermGraphEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the nodes field of hydra.paths.SubtermGraph
subtermGraphNodes :: Phantoms.TTerm Paths.SubtermGraph -> Phantoms.TTerm [Paths.SubtermNode]
subtermGraphNodes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
        Core.projectionFieldName = (Core.Name "nodes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the edges field of hydra.paths.SubtermGraph
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
              Core.projectionFieldName = (Core.Name "nodes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the nodes field of hydra.paths.SubtermGraph
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
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.paths.SubtermNode
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
-- | DSL accessor for the id field of hydra.paths.SubtermNode
subtermNodeId :: Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm String
subtermNodeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the label field of hydra.paths.SubtermNode
subtermNodeLabel :: Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm String
subtermNodeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.paths.SubtermNode
subtermNodeName :: Phantoms.TTerm Paths.SubtermNode -> Phantoms.TTerm Core.Name
subtermNodeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the id field of hydra.paths.SubtermNode
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the label field of hydra.paths.SubtermNode
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.paths.SubtermNode
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.paths.SubtermPath wrapper
subtermPath :: Phantoms.TTerm [Paths.SubtermStep] -> Phantoms.TTerm Paths.SubtermPath
subtermPath x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.paths.SubtermPath"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the annotatedBody variant of hydra.paths.SubtermStep
subtermStepAnnotatedBody :: Phantoms.TTerm Paths.SubtermStep
subtermStepAnnotatedBody =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotatedBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the applicationArgument variant of hydra.paths.SubtermStep
subtermStepApplicationArgument :: Phantoms.TTerm Paths.SubtermStep
subtermStepApplicationArgument =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationArgument"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the applicationFunction variant of hydra.paths.SubtermStep
subtermStepApplicationFunction :: Phantoms.TTerm Paths.SubtermStep
subtermStepApplicationFunction =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationFunction"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the injectionTerm variant of hydra.paths.SubtermStep
subtermStepInjectionTerm :: Phantoms.TTerm Paths.SubtermStep
subtermStepInjectionTerm =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "injectionTerm"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lambdaBody variant of hydra.paths.SubtermStep
subtermStepLambdaBody :: Phantoms.TTerm Paths.SubtermStep
subtermStepLambdaBody =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambdaBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the letBinding variant of hydra.paths.SubtermStep
subtermStepLetBinding :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtermStep
subtermStepLetBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "letBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the letBody variant of hydra.paths.SubtermStep
subtermStepLetBody :: Phantoms.TTerm Paths.SubtermStep
subtermStepLetBody =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "letBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the listElement variant of hydra.paths.SubtermStep
subtermStepListElement :: Phantoms.TTerm Int -> Phantoms.TTerm Paths.SubtermStep
subtermStepListElement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listElement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the mapKey variant of hydra.paths.SubtermStep
subtermStepMapKey :: Phantoms.TTerm Int -> Phantoms.TTerm Paths.SubtermStep
subtermStepMapKey x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapKey"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the mapValue variant of hydra.paths.SubtermStep
subtermStepMapValue :: Phantoms.TTerm Int -> Phantoms.TTerm Paths.SubtermStep
subtermStepMapValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the maybeTerm variant of hydra.paths.SubtermStep
subtermStepMaybeTerm :: Phantoms.TTerm Paths.SubtermStep
subtermStepMaybeTerm =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybeTerm"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the productTerm variant of hydra.paths.SubtermStep
subtermStepProductTerm :: Phantoms.TTerm Int -> Phantoms.TTerm Paths.SubtermStep
subtermStepProductTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "productTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the recordField variant of hydra.paths.SubtermStep
subtermStepRecordField :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtermStep
subtermStepRecordField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recordField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the setElement variant of hydra.paths.SubtermStep
subtermStepSetElement :: Phantoms.TTerm Int -> Phantoms.TTerm Paths.SubtermStep
subtermStepSetElement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "setElement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the sumTerm variant of hydra.paths.SubtermStep
subtermStepSumTerm :: Phantoms.TTerm Paths.SubtermStep
subtermStepSumTerm =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sumTerm"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeApplicationTerm variant of hydra.paths.SubtermStep
subtermStepTypeApplicationTerm :: Phantoms.TTerm Paths.SubtermStep
subtermStepTypeApplicationTerm =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeApplicationTerm"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeLambdaBody variant of hydra.paths.SubtermStep
subtermStepTypeLambdaBody :: Phantoms.TTerm Paths.SubtermStep
subtermStepTypeLambdaBody =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLambdaBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unionCasesBranch variant of hydra.paths.SubtermStep
subtermStepUnionCasesBranch :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtermStep
subtermStepUnionCasesBranch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unionCasesBranch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unionCasesDefault variant of hydra.paths.SubtermStep
subtermStepUnionCasesDefault :: Phantoms.TTerm Paths.SubtermStep
subtermStepUnionCasesDefault =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unionCasesDefault"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the wrappedTerm variant of hydra.paths.SubtermStep
subtermStepWrappedTerm :: Phantoms.TTerm Paths.SubtermStep
subtermStepWrappedTerm =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrappedTerm"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.paths.SubtypeEdge
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
-- | DSL accessor for the path field of hydra.paths.SubtypeEdge
subtypeEdgePath :: Phantoms.TTerm Paths.SubtypeEdge -> Phantoms.TTerm Paths.SubtypePath
subtypeEdgePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the source field of hydra.paths.SubtypeEdge
subtypeEdgeSource :: Phantoms.TTerm Paths.SubtypeEdge -> Phantoms.TTerm Paths.SubtypeNode
subtypeEdgeSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
        Core.projectionFieldName = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the target field of hydra.paths.SubtypeEdge
subtypeEdgeTarget :: Phantoms.TTerm Paths.SubtypeEdge -> Phantoms.TTerm Paths.SubtypeNode
subtypeEdgeTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the path field of hydra.paths.SubtypeEdge
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
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the source field of hydra.paths.SubtypeEdge
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
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the target field of hydra.paths.SubtypeEdge
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
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.paths.SubtypeGraph
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
-- | DSL accessor for the edges field of hydra.paths.SubtypeGraph
subtypeGraphEdges :: Phantoms.TTerm Paths.SubtypeGraph -> Phantoms.TTerm [Paths.SubtypeEdge]
subtypeGraphEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the nodes field of hydra.paths.SubtypeGraph
subtypeGraphNodes :: Phantoms.TTerm Paths.SubtypeGraph -> Phantoms.TTerm [Paths.SubtypeNode]
subtypeGraphNodes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
        Core.projectionFieldName = (Core.Name "nodes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the edges field of hydra.paths.SubtypeGraph
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
              Core.projectionFieldName = (Core.Name "nodes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the nodes field of hydra.paths.SubtypeGraph
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
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.paths.SubtypeNode
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
-- | DSL accessor for the id field of hydra.paths.SubtypeNode
subtypeNodeId :: Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm String
subtypeNodeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the label field of hydra.paths.SubtypeNode
subtypeNodeLabel :: Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm String
subtypeNodeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.paths.SubtypeNode
subtypeNodeName :: Phantoms.TTerm Paths.SubtypeNode -> Phantoms.TTerm Core.Name
subtypeNodeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the id field of hydra.paths.SubtypeNode
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the label field of hydra.paths.SubtypeNode
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.paths.SubtypeNode
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.paths.SubtypePath wrapper
subtypePath :: Phantoms.TTerm [Paths.SubtypeStep] -> Phantoms.TTerm Paths.SubtypePath
subtypePath x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.paths.SubtypePath"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the annotatedBody variant of hydra.paths.SubtypeStep
subtypeStepAnnotatedBody :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepAnnotatedBody =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotatedBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the applicationArgument variant of hydra.paths.SubtypeStep
subtypeStepApplicationArgument :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepApplicationArgument =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationArgument"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the applicationFunction variant of hydra.paths.SubtypeStep
subtypeStepApplicationFunction :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepApplicationFunction =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationFunction"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the eitherLeft variant of hydra.paths.SubtypeStep
subtypeStepEitherLeft :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepEitherLeft =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eitherLeft"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the eitherRight variant of hydra.paths.SubtypeStep
subtypeStepEitherRight :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepEitherRight =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eitherRight"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the forallBody variant of hydra.paths.SubtypeStep
subtypeStepForallBody :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepForallBody =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forallBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the functionCodomain variant of hydra.paths.SubtypeStep
subtypeStepFunctionCodomain :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepFunctionCodomain =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionCodomain"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the functionDomain variant of hydra.paths.SubtypeStep
subtypeStepFunctionDomain :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepFunctionDomain =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionDomain"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the listElement variant of hydra.paths.SubtypeStep
subtypeStepListElement :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepListElement =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listElement"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mapKeys variant of hydra.paths.SubtypeStep
subtypeStepMapKeys :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepMapKeys =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapKeys"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mapValues variant of hydra.paths.SubtypeStep
subtypeStepMapValues :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepMapValues =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapValues"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the maybeElement variant of hydra.paths.SubtypeStep
subtypeStepMaybeElement :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepMaybeElement =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybeElement"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pairFirst variant of hydra.paths.SubtypeStep
subtypeStepPairFirst :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepPairFirst =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pairFirst"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pairSecond variant of hydra.paths.SubtypeStep
subtypeStepPairSecond :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepPairSecond =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pairSecond"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the recordField variant of hydra.paths.SubtypeStep
subtypeStepRecordField :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtypeStep
subtypeStepRecordField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recordField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the setElement variant of hydra.paths.SubtypeStep
subtypeStepSetElement :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepSetElement =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "setElement"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unionField variant of hydra.paths.SubtypeStep
subtypeStepUnionField :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Paths.SubtypeStep
subtypeStepUnionField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unionField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the wrappedType variant of hydra.paths.SubtypeStep
subtypeStepWrappedType :: Phantoms.TTerm Paths.SubtypeStep
subtypeStepWrappedType =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrappedType"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the body of hydra.paths.SubtermPath
unSubtermPath :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm [Paths.SubtermStep]
unSubtermPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.paths.SubtermPath")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.paths.SubtypePath
unSubtypePath :: Phantoms.TTerm Paths.SubtypePath -> Phantoms.TTerm [Paths.SubtypeStep]
unSubtypePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.paths.SubtypePath")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
