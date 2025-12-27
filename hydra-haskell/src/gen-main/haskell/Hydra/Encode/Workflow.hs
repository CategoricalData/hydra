-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.workflow

module Hydra.Encode.Workflow where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Encode.Module as Module
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Workflow as Workflow
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

hydraSchemaSpec :: (Workflow.HydraSchemaSpec -> Core.Term)
hydraSchemaSpec x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.workflow.HydraSchemaSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "modules"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map Module.module_ xs)) (Workflow.hydraSchemaSpecModules x))},
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (Core_.name (Workflow.hydraSchemaSpecTypeName x))}]}))

schemaSpec :: (Workflow.SchemaSpec -> Core.Term)
schemaSpec x = case x of
  Workflow.SchemaSpecHydra v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.workflow.SchemaSpec"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "hydra"),
      Core.fieldTerm = (hydraSchemaSpec v1)}}))
  Workflow.SchemaSpecFile v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.workflow.SchemaSpec"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "file"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v1))}}))
  Workflow.SchemaSpecProvided -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.workflow.SchemaSpec"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "provided"),
      Core.fieldTerm = Core.TermUnit}}))

transformWorkflow :: (Workflow.TransformWorkflow -> Core.Term)
transformWorkflow x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.workflow.TransformWorkflow"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Workflow.transformWorkflowName x))},
    Core.Field {
      Core.fieldName = (Core.Name "schemaSpec"),
      Core.fieldTerm = (schemaSpec (Workflow.transformWorkflowSchemaSpec x))},
    Core.Field {
      Core.fieldName = (Core.Name "srcDir"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Workflow.transformWorkflowSrcDir x))},
    Core.Field {
      Core.fieldName = (Core.Name "destDir"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Workflow.transformWorkflowDestDir x))}]}))
