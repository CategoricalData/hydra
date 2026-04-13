-- | Java code generation for the GenPG demo.
--
-- This module provides functions to generate all Java modules needed for
-- the GenPG demo, including:
--   - hydra.pg.model, hydra.pg.mapping
--   - hydra.pg.graphson.* (coder, construct, syntax, utils)
--   - hydra.encode.pg.*, hydra.decode.pg.*
--   - hydra.demos.genpg.transform
--   - hydra.demos.genpg.sales (sales database/graph schemas and mapping)
--   - hydra.demos.genpg.health (health database/graph schemas and mapping)
--
-- Usage from GHCI:
--   > import Hydra.Ext.Demos.GenPG.GenerateJava
--   > generateJavaModules

module Hydra.Ext.Demos.GenPG.GenerateJava where

import Hydra.Ext.Generation
import Hydra.Sources.All (kernelModules)
import Hydra.Ext.Sources.All (hydraExtModules, genpgModules)
import Hydra.Ext.Demos.GenPG.Modules (salesModule, healthModule)
import System.Directory (createDirectoryIfMissing)
import System.IO (hFlush, stdout)


-- | Generate all Java modules for the GenPG demo.
--
-- This generates to demos/src/main/java:
--   - hydra.pg.* modules
--   - hydra.demos.genpg.transform
--   - hydra.demos.genpg.sales
--   - hydra.demos.genpg.health
generateJavaModules :: IO ()
generateJavaModules = do
  let outputDir = "../../demos/src/main/java"
  createDirectoryIfMissing True outputDir

  putStrLn "=== Generate Java GenPG Modules ==="
  putStrLn ""
  putStrLn $ "Output directory: " ++ outputDir
  putStrLn ""
  putStrLn "Generating modules:"
  putStrLn "  - hydra.pg.model"
  putStrLn "  - hydra.pg.mapping"
  putStrLn "  - hydra.pg.graphson.* (coder, construct, syntax, utils)"
  putStrLn "  - hydra.encode.pg.*, hydra.decode.pg.*"
  putStrLn "  - hydra.demos.genpg.transform"
  putStrLn "  - hydra.demos.genpg.sales"
  putStrLn "  - hydra.demos.genpg.health"
  putStrLn ""
  hFlush stdout

  -- Universe includes kernel and hydra-ext modules for dependency resolution
  -- We generate genpgModules plus the sales and health modules
  let universeModules = kernelModules ++ hydraExtModules ++ [salesModule, healthModule]
  let modulesToGenerate = genpgModules ++ [salesModule, healthModule]
  writeJava outputDir universeModules modulesToGenerate

  putStrLn ""
  putStrLn "=== Done! ==="
