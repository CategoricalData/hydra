-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.workflow

module Hydra.Dsl.Workflow where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import qualified Hydra.Workflow as Workflow
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

hydraSchemaSpec :: ([Module.Module] -> Core.Name -> Workflow.HydraSchemaSpec)
hydraSchemaSpec modules typeName = Workflow.HydraSchemaSpec {
  Workflow.hydraSchemaSpecModules = modules,
  Workflow.hydraSchemaSpecTypeName = typeName}

hydraSchemaSpecModules :: (Workflow.HydraSchemaSpec -> [Module.Module])
hydraSchemaSpecModules = Workflow.hydraSchemaSpecModules

hydraSchemaSpecTypeName :: (Workflow.HydraSchemaSpec -> Core.Name)
hydraSchemaSpecTypeName = Workflow.hydraSchemaSpecTypeName

hydraSchemaSpecWithModules :: (Workflow.HydraSchemaSpec -> [Module.Module] -> Workflow.HydraSchemaSpec)
hydraSchemaSpecWithModules original newVal = Workflow.HydraSchemaSpec {
  Workflow.hydraSchemaSpecModules = newVal,
  Workflow.hydraSchemaSpecTypeName = (Workflow.hydraSchemaSpecTypeName original)}

hydraSchemaSpecWithTypeName :: (Workflow.HydraSchemaSpec -> Core.Name -> Workflow.HydraSchemaSpec)
hydraSchemaSpecWithTypeName original newVal = Workflow.HydraSchemaSpec {
  Workflow.hydraSchemaSpecModules = (Workflow.hydraSchemaSpecModules original),
  Workflow.hydraSchemaSpecTypeName = newVal}

schemaSpecHydra :: (Workflow.HydraSchemaSpec -> Workflow.SchemaSpec)
schemaSpecHydra x = (Workflow.SchemaSpecHydra x)

schemaSpecFile :: (String -> Workflow.SchemaSpec)
schemaSpecFile x = (Workflow.SchemaSpecFile x)

schemaSpecProvided :: Workflow.SchemaSpec
schemaSpecProvided = Workflow.SchemaSpecProvided

transformWorkflow :: (String -> Workflow.SchemaSpec -> String -> String -> Workflow.TransformWorkflow)
transformWorkflow name schemaSpec srcDir destDir = Workflow.TransformWorkflow {
  Workflow.transformWorkflowName = name,
  Workflow.transformWorkflowSchemaSpec = schemaSpec,
  Workflow.transformWorkflowSrcDir = srcDir,
  Workflow.transformWorkflowDestDir = destDir}

transformWorkflowName :: (Workflow.TransformWorkflow -> String)
transformWorkflowName = Workflow.transformWorkflowName

transformWorkflowSchemaSpec :: (Workflow.TransformWorkflow -> Workflow.SchemaSpec)
transformWorkflowSchemaSpec = Workflow.transformWorkflowSchemaSpec

transformWorkflowSrcDir :: (Workflow.TransformWorkflow -> String)
transformWorkflowSrcDir = Workflow.transformWorkflowSrcDir

transformWorkflowDestDir :: (Workflow.TransformWorkflow -> String)
transformWorkflowDestDir = Workflow.transformWorkflowDestDir

transformWorkflowWithName :: (Workflow.TransformWorkflow -> String -> Workflow.TransformWorkflow)
transformWorkflowWithName original newVal = Workflow.TransformWorkflow {
  Workflow.transformWorkflowName = newVal,
  Workflow.transformWorkflowSchemaSpec = (Workflow.transformWorkflowSchemaSpec original),
  Workflow.transformWorkflowSrcDir = (Workflow.transformWorkflowSrcDir original),
  Workflow.transformWorkflowDestDir = (Workflow.transformWorkflowDestDir original)}

transformWorkflowWithSchemaSpec :: (Workflow.TransformWorkflow -> Workflow.SchemaSpec -> Workflow.TransformWorkflow)
transformWorkflowWithSchemaSpec original newVal = Workflow.TransformWorkflow {
  Workflow.transformWorkflowName = (Workflow.transformWorkflowName original),
  Workflow.transformWorkflowSchemaSpec = newVal,
  Workflow.transformWorkflowSrcDir = (Workflow.transformWorkflowSrcDir original),
  Workflow.transformWorkflowDestDir = (Workflow.transformWorkflowDestDir original)}

transformWorkflowWithSrcDir :: (Workflow.TransformWorkflow -> String -> Workflow.TransformWorkflow)
transformWorkflowWithSrcDir original newVal = Workflow.TransformWorkflow {
  Workflow.transformWorkflowName = (Workflow.transformWorkflowName original),
  Workflow.transformWorkflowSchemaSpec = (Workflow.transformWorkflowSchemaSpec original),
  Workflow.transformWorkflowSrcDir = newVal,
  Workflow.transformWorkflowDestDir = (Workflow.transformWorkflowDestDir original)}

transformWorkflowWithDestDir :: (Workflow.TransformWorkflow -> String -> Workflow.TransformWorkflow)
transformWorkflowWithDestDir original newVal = Workflow.TransformWorkflow {
  Workflow.transformWorkflowName = (Workflow.transformWorkflowName original),
  Workflow.transformWorkflowSchemaSpec = (Workflow.transformWorkflowSchemaSpec original),
  Workflow.transformWorkflowSrcDir = (Workflow.transformWorkflowSrcDir original),
  Workflow.transformWorkflowDestDir = newVal}
