-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.workflow

module Hydra.Dsl.Workflow where

import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Workflow as Workflow
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

hydraSchemaSpec :: Phantoms.TTerm [Packaging.Module] -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Workflow.HydraSchemaSpec
hydraSchemaSpec modules typeName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.workflow.HydraSchemaSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTTerm modules)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)}]}))

hydraSchemaSpecModules :: Phantoms.TTerm Workflow.HydraSchemaSpec -> Phantoms.TTerm [Packaging.Module]
hydraSchemaSpecModules x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.workflow.HydraSchemaSpec"),
        Core.projectionField = (Core.Name "modules")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hydraSchemaSpecTypeName :: Phantoms.TTerm Workflow.HydraSchemaSpec -> Phantoms.TTerm Core.Name
hydraSchemaSpecTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.workflow.HydraSchemaSpec"),
        Core.projectionField = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hydraSchemaSpecWithModules :: Phantoms.TTerm Workflow.HydraSchemaSpec -> Phantoms.TTerm [Packaging.Module] -> Phantoms.TTerm Workflow.HydraSchemaSpec
hydraSchemaSpecWithModules original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.workflow.HydraSchemaSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.HydraSchemaSpec"),
              Core.projectionField = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

hydraSchemaSpecWithTypeName :: Phantoms.TTerm Workflow.HydraSchemaSpec -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Workflow.HydraSchemaSpec
hydraSchemaSpecWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.workflow.HydraSchemaSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.HydraSchemaSpec"),
              Core.projectionField = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

schemaSpecFile :: Phantoms.TTerm String -> Phantoms.TTerm Workflow.SchemaSpec
schemaSpecFile x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.workflow.SchemaSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "file"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaSpecHydra :: Phantoms.TTerm Workflow.HydraSchemaSpec -> Phantoms.TTerm Workflow.SchemaSpec
schemaSpecHydra x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.workflow.SchemaSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hydra"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaSpecProvided :: Phantoms.TTerm Workflow.SchemaSpec
schemaSpecProvided =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.workflow.SchemaSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "provided"),
        Core.fieldTerm = Core.TermUnit}}))

transformWorkflow :: Phantoms.TTerm String -> Phantoms.TTerm Workflow.SchemaSpec -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Workflow.TransformWorkflow
transformWorkflow name schemaSpec srcDir destDir =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "schemaSpec"),
          Core.fieldTerm = (Phantoms.unTTerm schemaSpec)},
        Core.Field {
          Core.fieldName = (Core.Name "srcDir"),
          Core.fieldTerm = (Phantoms.unTTerm srcDir)},
        Core.Field {
          Core.fieldName = (Core.Name "destDir"),
          Core.fieldTerm = (Phantoms.unTTerm destDir)}]}))

transformWorkflowDestDir :: Phantoms.TTerm Workflow.TransformWorkflow -> Phantoms.TTerm String
transformWorkflowDestDir x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
        Core.projectionField = (Core.Name "destDir")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

transformWorkflowName :: Phantoms.TTerm Workflow.TransformWorkflow -> Phantoms.TTerm String
transformWorkflowName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

transformWorkflowSchemaSpec :: Phantoms.TTerm Workflow.TransformWorkflow -> Phantoms.TTerm Workflow.SchemaSpec
transformWorkflowSchemaSpec x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
        Core.projectionField = (Core.Name "schemaSpec")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

transformWorkflowSrcDir :: Phantoms.TTerm Workflow.TransformWorkflow -> Phantoms.TTerm String
transformWorkflowSrcDir x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
        Core.projectionField = (Core.Name "srcDir")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

transformWorkflowWithDestDir :: Phantoms.TTerm Workflow.TransformWorkflow -> Phantoms.TTerm String -> Phantoms.TTerm Workflow.TransformWorkflow
transformWorkflowWithDestDir original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaSpec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "schemaSpec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "srcDir"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "srcDir")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "destDir"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

transformWorkflowWithName :: Phantoms.TTerm Workflow.TransformWorkflow -> Phantoms.TTerm String -> Phantoms.TTerm Workflow.TransformWorkflow
transformWorkflowWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "schemaSpec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "schemaSpec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "srcDir"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "srcDir")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "destDir"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "destDir")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

transformWorkflowWithSchemaSpec :: Phantoms.TTerm Workflow.TransformWorkflow -> Phantoms.TTerm Workflow.SchemaSpec -> Phantoms.TTerm Workflow.TransformWorkflow
transformWorkflowWithSchemaSpec original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaSpec"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "srcDir"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "srcDir")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "destDir"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "destDir")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

transformWorkflowWithSrcDir :: Phantoms.TTerm Workflow.TransformWorkflow -> Phantoms.TTerm String -> Phantoms.TTerm Workflow.TransformWorkflow
transformWorkflowWithSrcDir original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaSpec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "schemaSpec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "srcDir"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "destDir"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
              Core.projectionField = (Core.Name "destDir")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
