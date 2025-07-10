module Hydra.Ext.Demos.AvroToPropertyGraphs where

import Hydra.Kernel
import Hydra.Ext.Tools.AvroWorkflows

import qualified System.FilePath as FP
import System.Directory


-- | Last mile which transforms a graph into JSON. See the README for usage.
transformAvroJsonToPg :: LastMile Graph x -> FilePath -> FilePath -> FilePath -> IO ()
transformAvroJsonToPg lastMile schemaFile dataDir outDir = do
  pwd <- getCurrentDirectory
  executeAvroTransformWorkflow lastMile $ TransformWorkflow "airplane info"
    (SchemaSpecFile $ FP.combine pwd schemaFile) (FP.combine pwd dataDir) outDir
