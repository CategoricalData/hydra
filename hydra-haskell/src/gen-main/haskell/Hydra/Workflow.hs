-- | A model for Hydra transformation workflows

module Hydra.Workflow where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Module as Module
import Data.List
import Data.Map
import Data.Set

-- | The specification of a Hydra schema, provided as a set of modules and a distinguished type
data HydraSchemaSpec = 
  HydraSchemaSpec {
    -- | The modules to include in the schema graph
    hydraSchemaSpecModules :: [Module.Module Compute.Meta],
    -- | The name of the top-level type; all data which passes through the workflow will be instances of this type
    hydraSchemaSpecTypeName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_HydraSchemaSpec = (Core.Name "hydra/workflow.HydraSchemaSpec")

_HydraSchemaSpec_modules = (Core.FieldName "modules")

_HydraSchemaSpec_typeName = (Core.FieldName "typeName")

-- | The last mile of a transformation, which encodes and serializes terms to a file
data LastMile a = 
  LastMile {
    -- | An encoder for terms to a list of output objects
    lastMileEncoder :: (Core.Term Compute.Meta -> Mantle.Graph Compute.Meta -> Compute.Flow (Compute.Context Compute.Meta) [a]),
    -- | A function which serializes a list of output objects to a string representation
    lastMileSerializer :: ([a] -> String),
    -- | A file extension for the generated file(s)
    lastMileFileExtension :: String}

_LastMile = (Core.Name "hydra/workflow.LastMile")

_LastMile_encoder = (Core.FieldName "encoder")

_LastMile_serializer = (Core.FieldName "serializer")

_LastMile_fileExtension = (Core.FieldName "fileExtension")

-- | The specification of a schema at the source end of a workflow
data SchemaSpec = 
  -- | A native Hydra schema
  SchemaSpecHydra HydraSchemaSpec |
  -- | A schema provided as a file, available at the given file path
  SchemaSpecFile String |
  -- | A schema which will be provided within the workflow
  SchemaSpecProvided 
  deriving (Eq, Ord, Read, Show)

_SchemaSpec = (Core.Name "hydra/workflow.SchemaSpec")

_SchemaSpec_hydra = (Core.FieldName "hydra")

_SchemaSpec_file = (Core.FieldName "file")

_SchemaSpec_provided = (Core.FieldName "provided")

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

_TransformWorkflow = (Core.Name "hydra/workflow.TransformWorkflow")

_TransformWorkflow_name = (Core.FieldName "name")

_TransformWorkflow_schemaSpec = (Core.FieldName "schemaSpec")

_TransformWorkflow_srcDir = (Core.FieldName "srcDir")

_TransformWorkflow_destDir = (Core.FieldName "destDir")