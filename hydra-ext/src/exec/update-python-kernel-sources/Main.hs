#!/usr/bin/env stack
{- stack script
   --resolver lts-22.28
   --package hydra-ext
-}

-- | Generate Python kernel source modules.
--
-- This executable creates "source-level" Python modules that contain
-- term-encoded Module objects. Unlike the regular kernel modules which
-- contain application-level code (Python functions), these modules
-- contain the Module AST representation itself.
--
-- For example, hydra.formatting becomes:
--   - Regular: hydra/formatting.py with Python function definitions
--   - Sources: hydra/sources/formatting.py with a Module object as data
--
-- This is needed for Python tests that need to access kernel module
-- definitions as data (similar to how Haskell tests include kernelModules
-- in the test graph).

module Main where

import Hydra.Kernel
import Hydra.Ext.Generation
import Hydra.Sources.All (kernelModules)
import qualified Hydra.Encode.Module as EncodeModule
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import qualified Data.List as L


-- | Create a virtual "sources" module for a given kernel module.
-- The virtual module has:
--   - Namespace: hydra.sources.<original_without_hydra>
--   - Single binding named hydra.sources.<ns>.module with term-encoded Module
toSourcesModule :: Module -> Module
toSourcesModule mod = Module {
    moduleNamespace = sourcesNs,
    moduleElements = [moduleBinding],
    moduleTermDependencies = [],  -- The binding is self-contained term data
    moduleTypeDependencies = [Namespace "hydra.core", Namespace "hydra.module"],
    moduleDescription = Just $ "Source-level representation of " ++ unNamespace originalNs
  }
  where
    originalNs = moduleNamespace mod
    -- Transform "hydra.foo.bar" -> "hydra.sources.foo.bar"
    sourcesNs = Namespace $ "hydra.sources." ++ dropHydraPrefix (unNamespace originalNs)

    -- The binding name is the namespace + ".module"
    bindingName = Name $ unNamespace sourcesNs ++ ".module"

    -- Encode the module to a Term
    encodedModule = EncodeModule.module_ mod

    moduleBinding = Binding {
      bindingName = bindingName,
      bindingTerm = encodedModule,
      bindingType = Nothing
    }

    -- Drop "hydra." prefix from namespace
    dropHydraPrefix ns = case stripPrefix "hydra." ns of
      Just rest -> rest
      Nothing -> ns

    stripPrefix prefix str
      | take (length prefix) str == prefix = Just (drop (length prefix) str)
      | otherwise = Nothing


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStrLn "=== Generate Python kernel sources ==="
  putStrLn ""
  putStrLn "Creating source-level modules from kernel modules..."
  putStrLn ""

  let sourcesModules = L.map toSourcesModule kernelModules

  putStrLn $ "Generating " ++ show (length sourcesModules) ++ " source modules to ../hydra-python/src/main/python..."
  putStrLn ""

  -- Universe includes both kernel modules (for type resolution) and sources modules
  -- We only generate the sources modules
  writePython "../hydra-python/src/main/python" (kernelModules ++ sourcesModules) sourcesModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "Generated modules:"
  mapM_ (\m -> putStrLn $ "  hydra/sources/" ++ dropHydraPrefix (unNamespace (moduleNamespace m)) ++ ".py")
        kernelModules
  where
    dropHydraPrefix ns = case stripPrefix "hydra." ns of
      Just rest -> rest
      Nothing -> ns
    stripPrefix prefix str
      | take (length prefix) str == prefix = Just (drop (length prefix) str)
      | otherwise = Nothing
