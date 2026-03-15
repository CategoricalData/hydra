-- Note: this is an automatically generated file. Do not edit.

-- | A model for Hydra transformation workflows

module Hydra.Ext.Workflow where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The specification of a Hydra schema, provided as a set of modules and a distinguished type
data HydraSchemaSpec = 
  HydraSchemaSpec {
    -- | The modules to include in the schema graph
    hydraSchemaSpecModules :: [Module.Module],
    -- | The name of the top-level type; all data which passes through the workflow will be instances of this type
    hydraSchemaSpecTypeName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_HydraSchemaSpec = (Core.Name "hydra.ext.workflow.HydraSchemaSpec")

_HydraSchemaSpec_modules = (Core.Name "modules")

_HydraSchemaSpec_typeName = (Core.Name "typeName")

-- | The specification of a schema at the source end of a workflow
data SchemaSpec = 
  -- | A native Hydra schema
  SchemaSpecHydra HydraSchemaSpec |
  -- | A schema provided as a file, available at the given file path
  SchemaSpecFile String |
  -- | A schema which will be provided within the workflow
  SchemaSpecProvided 
  deriving (Eq, Ord, Read, Show)

_SchemaSpec = (Core.Name "hydra.ext.workflow.SchemaSpec")

_SchemaSpec_hydra = (Core.Name "hydra")

_SchemaSpec_file = (Core.Name "file")

_SchemaSpec_provided = (Core.Name "provided")

-- | The specification of a workflow which takes a schema specification, reads data from a directory, and writes data to another directory
data TransformWorkflow = 
  TransformWorkflow {
    -- | A descriptive name for the workflow
    transformWorkflowName :: String,
    -- | The schema specification
    transformWorkflowSchemaSpec :: SchemaSpec,
    -- | The source directory
    transformWorkflowSrcDir :: String,
    -- | The destination directory
    transformWorkflowDestDir :: String}
  deriving (Eq, Ord, Read, Show)

_TransformWorkflow = (Core.Name "hydra.ext.workflow.TransformWorkflow")

_TransformWorkflow_name = (Core.Name "name")

_TransformWorkflow_schemaSpec = (Core.Name "schemaSpec")

_TransformWorkflow_srcDir = (Core.Name "srcDir")

_TransformWorkflow_destDir = (Core.Name "destDir")
