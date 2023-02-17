module Hydra.Tools.Workflow where

import Hydra.Kernel


data HydraSchemaSpec = HydraSchemaSpec {
  hydraSchemaSpecModules :: [Module Meta],
  hydraSchemaSpecTypeName :: Name}

data SchemaSpec = SchemaSpecFile String
  | SchemaSpecHydra HydraSchemaSpec
  | SchemaSpecProvided

data TransformWorkflow = TransformWorkflow {
  transformWorkflowName :: String,
  transformWorkflowSchemaSpec :: SchemaSpec,
  transformWorkflowSrcDir :: String,
  transformWorkflowDestDir :: String}
