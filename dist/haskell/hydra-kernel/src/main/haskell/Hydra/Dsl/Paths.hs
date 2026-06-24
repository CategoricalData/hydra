-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.paths

module Hydra.Dsl.Paths where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Paths as Paths
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.paths.SubtermEdge
subtermEdge :: Typed.TypedTerm Paths.SubtermNode -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Paths.SubtermNode -> Typed.TypedTerm Paths.SubtermEdge
subtermEdge source path target =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Typed.unTypedTerm target)}]}))
-- | DSL accessor for the path field of hydra.paths.SubtermEdge
subtermEdgePath :: Typed.TypedTerm Paths.SubtermEdge -> Typed.TypedTerm Paths.SubtermPath
subtermEdgePath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the source field of hydra.paths.SubtermEdge
subtermEdgeSource :: Typed.TypedTerm Paths.SubtermEdge -> Typed.TypedTerm Paths.SubtermNode
subtermEdgeSource x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
        Core.projectionFieldName = (Core.Name "source")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the target field of hydra.paths.SubtermEdge
subtermEdgeTarget :: Typed.TypedTerm Paths.SubtermEdge -> Typed.TypedTerm Paths.SubtermNode
subtermEdgeTarget x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the path field of hydra.paths.SubtermEdge
subtermEdgeWithPath :: Typed.TypedTerm Paths.SubtermEdge -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Paths.SubtermEdge
subtermEdgeWithPath original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the source field of hydra.paths.SubtermEdge
subtermEdgeWithSource :: Typed.TypedTerm Paths.SubtermEdge -> Typed.TypedTerm Paths.SubtermNode -> Typed.TypedTerm Paths.SubtermEdge
subtermEdgeWithSource original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the target field of hydra.paths.SubtermEdge
subtermEdgeWithTarget :: Typed.TypedTerm Paths.SubtermEdge -> Typed.TypedTerm Paths.SubtermNode -> Typed.TypedTerm Paths.SubtermEdge
subtermEdgeWithTarget original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.paths.SubtermGraph
subtermGraph :: Typed.TypedTerm [Paths.SubtermNode] -> Typed.TypedTerm [Paths.SubtermEdge] -> Typed.TypedTerm Paths.SubtermGraph
subtermGraph nodes edges =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Typed.unTypedTerm nodes)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Typed.unTypedTerm edges)}]}))
-- | DSL accessor for the edges field of hydra.paths.SubtermGraph
subtermGraphEdges :: Typed.TypedTerm Paths.SubtermGraph -> Typed.TypedTerm [Paths.SubtermEdge]
subtermGraphEdges x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the nodes field of hydra.paths.SubtermGraph
subtermGraphNodes :: Typed.TypedTerm Paths.SubtermGraph -> Typed.TypedTerm [Paths.SubtermNode]
subtermGraphNodes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
        Core.projectionFieldName = (Core.Name "nodes")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the edges field of hydra.paths.SubtermGraph
subtermGraphWithEdges :: Typed.TypedTerm Paths.SubtermGraph -> Typed.TypedTerm [Paths.SubtermEdge] -> Typed.TypedTerm Paths.SubtermGraph
subtermGraphWithEdges original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
              Core.projectionFieldName = (Core.Name "nodes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the nodes field of hydra.paths.SubtermGraph
subtermGraphWithNodes :: Typed.TypedTerm Paths.SubtermGraph -> Typed.TypedTerm [Paths.SubtermNode] -> Typed.TypedTerm Paths.SubtermGraph
subtermGraphWithNodes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.paths.SubtermNode
subtermNode :: Typed.TypedTerm Core.Name -> Typed.TypedTerm String -> Typed.TypedTerm String -> Typed.TypedTerm Paths.SubtermNode
subtermNode name label id =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)}]}))
-- | DSL accessor for the id field of hydra.paths.SubtermNode
subtermNodeId :: Typed.TypedTerm Paths.SubtermNode -> Typed.TypedTerm String
subtermNodeId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.paths.SubtermNode
subtermNodeLabel :: Typed.TypedTerm Paths.SubtermNode -> Typed.TypedTerm String
subtermNodeLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.paths.SubtermNode
subtermNodeName :: Typed.TypedTerm Paths.SubtermNode -> Typed.TypedTerm Core.Name
subtermNodeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.paths.SubtermNode
subtermNodeWithId :: Typed.TypedTerm Paths.SubtermNode -> Typed.TypedTerm String -> Typed.TypedTerm Paths.SubtermNode
subtermNodeWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the label field of hydra.paths.SubtermNode
subtermNodeWithLabel :: Typed.TypedTerm Paths.SubtermNode -> Typed.TypedTerm String -> Typed.TypedTerm Paths.SubtermNode
subtermNodeWithLabel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.paths.SubtermNode
subtermNodeWithName :: Typed.TypedTerm Paths.SubtermNode -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Paths.SubtermNode
subtermNodeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.paths.SubtermPath wrapper
subtermPath :: Typed.TypedTerm [Paths.SubtermStep] -> Typed.TypedTerm Paths.SubtermPath
subtermPath x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.paths.SubtermPath"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the annotatedBody variant of hydra.paths.SubtermStep
subtermStepAnnotatedBody :: Typed.TypedTerm Paths.SubtermStep
subtermStepAnnotatedBody =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotatedBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the applicationArgument variant of hydra.paths.SubtermStep
subtermStepApplicationArgument :: Typed.TypedTerm Paths.SubtermStep
subtermStepApplicationArgument =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationArgument"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the applicationFunction variant of hydra.paths.SubtermStep
subtermStepApplicationFunction :: Typed.TypedTerm Paths.SubtermStep
subtermStepApplicationFunction =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationFunction"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the injectionTerm variant of hydra.paths.SubtermStep
subtermStepInjectionTerm :: Typed.TypedTerm Paths.SubtermStep
subtermStepInjectionTerm =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "injectionTerm"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lambdaBody variant of hydra.paths.SubtermStep
subtermStepLambdaBody :: Typed.TypedTerm Paths.SubtermStep
subtermStepLambdaBody =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambdaBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the letBinding variant of hydra.paths.SubtermStep
subtermStepLetBinding :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Paths.SubtermStep
subtermStepLetBinding x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "letBinding"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the letBody variant of hydra.paths.SubtermStep
subtermStepLetBody :: Typed.TypedTerm Paths.SubtermStep
subtermStepLetBody =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "letBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the listElement variant of hydra.paths.SubtermStep
subtermStepListElement :: Typed.TypedTerm Int -> Typed.TypedTerm Paths.SubtermStep
subtermStepListElement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listElement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the mapKey variant of hydra.paths.SubtermStep
subtermStepMapKey :: Typed.TypedTerm Int -> Typed.TypedTerm Paths.SubtermStep
subtermStepMapKey x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapKey"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the mapValue variant of hydra.paths.SubtermStep
subtermStepMapValue :: Typed.TypedTerm Int -> Typed.TypedTerm Paths.SubtermStep
subtermStepMapValue x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapValue"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the optionalTerm variant of hydra.paths.SubtermStep
subtermStepOptionalTerm :: Typed.TypedTerm Paths.SubtermStep
subtermStepOptionalTerm =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optionalTerm"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the productTerm variant of hydra.paths.SubtermStep
subtermStepProductTerm :: Typed.TypedTerm Int -> Typed.TypedTerm Paths.SubtermStep
subtermStepProductTerm x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "productTerm"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the recordField variant of hydra.paths.SubtermStep
subtermStepRecordField :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Paths.SubtermStep
subtermStepRecordField x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recordField"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the setElement variant of hydra.paths.SubtermStep
subtermStepSetElement :: Typed.TypedTerm Int -> Typed.TypedTerm Paths.SubtermStep
subtermStepSetElement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "setElement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sumTerm variant of hydra.paths.SubtermStep
subtermStepSumTerm :: Typed.TypedTerm Paths.SubtermStep
subtermStepSumTerm =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sumTerm"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeApplicationTerm variant of hydra.paths.SubtermStep
subtermStepTypeApplicationTerm :: Typed.TypedTerm Paths.SubtermStep
subtermStepTypeApplicationTerm =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeApplicationTerm"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeLambdaBody variant of hydra.paths.SubtermStep
subtermStepTypeLambdaBody :: Typed.TypedTerm Paths.SubtermStep
subtermStepTypeLambdaBody =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLambdaBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unionCasesBranch variant of hydra.paths.SubtermStep
subtermStepUnionCasesBranch :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Paths.SubtermStep
subtermStepUnionCasesBranch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unionCasesBranch"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unionCasesDefault variant of hydra.paths.SubtermStep
subtermStepUnionCasesDefault :: Typed.TypedTerm Paths.SubtermStep
subtermStepUnionCasesDefault =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unionCasesDefault"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the wrappedTerm variant of hydra.paths.SubtermStep
subtermStepWrappedTerm :: Typed.TypedTerm Paths.SubtermStep
subtermStepWrappedTerm =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrappedTerm"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.paths.SubtypeEdge
subtypeEdge :: Typed.TypedTerm Paths.SubtypeNode -> Typed.TypedTerm Paths.SubtypePath -> Typed.TypedTerm Paths.SubtypeNode -> Typed.TypedTerm Paths.SubtypeEdge
subtypeEdge source path target =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Typed.unTypedTerm target)}]}))
-- | DSL accessor for the path field of hydra.paths.SubtypeEdge
subtypeEdgePath :: Typed.TypedTerm Paths.SubtypeEdge -> Typed.TypedTerm Paths.SubtypePath
subtypeEdgePath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the source field of hydra.paths.SubtypeEdge
subtypeEdgeSource :: Typed.TypedTerm Paths.SubtypeEdge -> Typed.TypedTerm Paths.SubtypeNode
subtypeEdgeSource x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
        Core.projectionFieldName = (Core.Name "source")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the target field of hydra.paths.SubtypeEdge
subtypeEdgeTarget :: Typed.TypedTerm Paths.SubtypeEdge -> Typed.TypedTerm Paths.SubtypeNode
subtypeEdgeTarget x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the path field of hydra.paths.SubtypeEdge
subtypeEdgeWithPath :: Typed.TypedTerm Paths.SubtypeEdge -> Typed.TypedTerm Paths.SubtypePath -> Typed.TypedTerm Paths.SubtypeEdge
subtypeEdgeWithPath original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the source field of hydra.paths.SubtypeEdge
subtypeEdgeWithSource :: Typed.TypedTerm Paths.SubtypeEdge -> Typed.TypedTerm Paths.SubtypeNode -> Typed.TypedTerm Paths.SubtypeEdge
subtypeEdgeWithSource original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the target field of hydra.paths.SubtypeEdge
subtypeEdgeWithTarget :: Typed.TypedTerm Paths.SubtypeEdge -> Typed.TypedTerm Paths.SubtypeNode -> Typed.TypedTerm Paths.SubtypeEdge
subtypeEdgeWithTarget original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.paths.SubtypeGraph
subtypeGraph :: Typed.TypedTerm [Paths.SubtypeNode] -> Typed.TypedTerm [Paths.SubtypeEdge] -> Typed.TypedTerm Paths.SubtypeGraph
subtypeGraph nodes edges =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Typed.unTypedTerm nodes)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Typed.unTypedTerm edges)}]}))
-- | DSL accessor for the edges field of hydra.paths.SubtypeGraph
subtypeGraphEdges :: Typed.TypedTerm Paths.SubtypeGraph -> Typed.TypedTerm [Paths.SubtypeEdge]
subtypeGraphEdges x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the nodes field of hydra.paths.SubtypeGraph
subtypeGraphNodes :: Typed.TypedTerm Paths.SubtypeGraph -> Typed.TypedTerm [Paths.SubtypeNode]
subtypeGraphNodes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
        Core.projectionFieldName = (Core.Name "nodes")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the edges field of hydra.paths.SubtypeGraph
subtypeGraphWithEdges :: Typed.TypedTerm Paths.SubtypeGraph -> Typed.TypedTerm [Paths.SubtypeEdge] -> Typed.TypedTerm Paths.SubtypeGraph
subtypeGraphWithEdges original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
              Core.projectionFieldName = (Core.Name "nodes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the nodes field of hydra.paths.SubtypeGraph
subtypeGraphWithNodes :: Typed.TypedTerm Paths.SubtypeGraph -> Typed.TypedTerm [Paths.SubtypeNode] -> Typed.TypedTerm Paths.SubtypeGraph
subtypeGraphWithNodes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.paths.SubtypeNode
subtypeNode :: Typed.TypedTerm Core.Name -> Typed.TypedTerm String -> Typed.TypedTerm String -> Typed.TypedTerm Paths.SubtypeNode
subtypeNode name label id =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)}]}))
-- | DSL accessor for the id field of hydra.paths.SubtypeNode
subtypeNodeId :: Typed.TypedTerm Paths.SubtypeNode -> Typed.TypedTerm String
subtypeNodeId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.paths.SubtypeNode
subtypeNodeLabel :: Typed.TypedTerm Paths.SubtypeNode -> Typed.TypedTerm String
subtypeNodeLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.paths.SubtypeNode
subtypeNodeName :: Typed.TypedTerm Paths.SubtypeNode -> Typed.TypedTerm Core.Name
subtypeNodeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.paths.SubtypeNode
subtypeNodeWithId :: Typed.TypedTerm Paths.SubtypeNode -> Typed.TypedTerm String -> Typed.TypedTerm Paths.SubtypeNode
subtypeNodeWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the label field of hydra.paths.SubtypeNode
subtypeNodeWithLabel :: Typed.TypedTerm Paths.SubtypeNode -> Typed.TypedTerm String -> Typed.TypedTerm Paths.SubtypeNode
subtypeNodeWithLabel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.paths.SubtypeNode
subtypeNodeWithName :: Typed.TypedTerm Paths.SubtypeNode -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Paths.SubtypeNode
subtypeNodeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.paths.SubtypePath wrapper
subtypePath :: Typed.TypedTerm [Paths.SubtypeStep] -> Typed.TypedTerm Paths.SubtypePath
subtypePath x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.paths.SubtypePath"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the annotatedBody variant of hydra.paths.SubtypeStep
subtypeStepAnnotatedBody :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepAnnotatedBody =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotatedBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the applicationArgument variant of hydra.paths.SubtypeStep
subtypeStepApplicationArgument :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepApplicationArgument =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationArgument"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the applicationFunction variant of hydra.paths.SubtypeStep
subtypeStepApplicationFunction :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepApplicationFunction =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applicationFunction"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the eitherLeft variant of hydra.paths.SubtypeStep
subtypeStepEitherLeft :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepEitherLeft =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eitherLeft"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the eitherRight variant of hydra.paths.SubtypeStep
subtypeStepEitherRight :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepEitherRight =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eitherRight"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the forallBody variant of hydra.paths.SubtypeStep
subtypeStepForallBody :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepForallBody =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forallBody"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the functionCodomain variant of hydra.paths.SubtypeStep
subtypeStepFunctionCodomain :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepFunctionCodomain =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionCodomain"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the functionDomain variant of hydra.paths.SubtypeStep
subtypeStepFunctionDomain :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepFunctionDomain =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionDomain"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the listElement variant of hydra.paths.SubtypeStep
subtypeStepListElement :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepListElement =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listElement"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mapKeys variant of hydra.paths.SubtypeStep
subtypeStepMapKeys :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepMapKeys =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapKeys"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mapValues variant of hydra.paths.SubtypeStep
subtypeStepMapValues :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepMapValues =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapValues"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the optionalElement variant of hydra.paths.SubtypeStep
subtypeStepOptionalElement :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepOptionalElement =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optionalElement"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pairFirst variant of hydra.paths.SubtypeStep
subtypeStepPairFirst :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepPairFirst =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pairFirst"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pairSecond variant of hydra.paths.SubtypeStep
subtypeStepPairSecond :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepPairSecond =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pairSecond"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the recordField variant of hydra.paths.SubtypeStep
subtypeStepRecordField :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Paths.SubtypeStep
subtypeStepRecordField x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recordField"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the setElement variant of hydra.paths.SubtypeStep
subtypeStepSetElement :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepSetElement =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "setElement"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unionField variant of hydra.paths.SubtypeStep
subtypeStepUnionField :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Paths.SubtypeStep
subtypeStepUnionField x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unionField"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wrappedType variant of hydra.paths.SubtypeStep
subtypeStepWrappedType :: Typed.TypedTerm Paths.SubtypeStep
subtypeStepWrappedType =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrappedType"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the body of hydra.paths.SubtermPath
unSubtermPath :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm [Paths.SubtermStep]
unSubtermPath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.paths.SubtermPath")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.paths.SubtypePath
unSubtypePath :: Typed.TypedTerm Paths.SubtypePath -> Typed.TypedTerm [Paths.SubtypeStep]
unSubtypePath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.paths.SubtypePath")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
