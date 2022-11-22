module Hydra.Workflow where

import Hydra.All

import System.FilePath.Posix


data HydraSchemaSpec = HydraSchemaSpec {
  hydraSchemaSpecModules :: [Module Meta],
  hydraSchemaSpecTypeName :: Name}

data SchemaSpec = SchemaSpecFile FilePath | SchemaSpecHydra HydraSchemaSpec | SchemaSpecProvided

data TransformWorkflow = TransformWorkflow {
  transformWorkflowName :: String,
  transformWorkflowSchemaSpec :: SchemaSpec,
  transformWorkflowSrcDir :: FilePath,
  transformWorkflowDestDir :: FilePath}
