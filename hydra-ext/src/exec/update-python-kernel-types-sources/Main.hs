-- | Generate Python kernel type source modules.
--
-- This executable creates "source-level" Python modules that contain
-- term-encoded Module objects for the kernel TYPE modules only.
-- These provide the type universe needed for JSON module decoding.
--
-- For example, hydra.core becomes:
--   hydra/sources/core.py with a Module object as data
--
-- Only the 22 kernelTypesModules are generated (not term modules).

module Main where

import Hydra.Kernel
import Hydra.Ext.Generation
import Hydra.Sources.Kernel.Types.All (kernelTypesModules)
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

  putStrLn "=== Generate Python kernel type sources ==="
  putStrLn ""
  putStrLn "Creating source-level modules from kernel TYPE modules..."
  putStrLn ""

  let sourcesModules = L.map toSourcesModule kernelTypesModules

  putStrLn $ "Generating " ++ show (length sourcesModules) ++ " source modules to ../hydra-python/src/gen-main/python..."
  putStrLn ""

  -- Universe includes both kernel type modules (for type resolution) and sources modules
  -- We only generate the sources modules
  writePython "../hydra-python/src/gen-main/python" (kernelTypesModules ++ sourcesModules) sourcesModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "Generated modules:"
  mapM_ (\m -> putStrLn $ "  hydra/sources/" ++ dropHydraPrefix (unNamespace (moduleNamespace m)) ++ ".py")
        kernelTypesModules
  where
    dropHydraPrefix ns = case stripPrefix "hydra." ns of
      Just rest -> rest
      Nothing -> ns
    stripPrefix prefix str
      | take (length prefix) str == prefix = Just (drop (length prefix) str)
      | otherwise = Nothing
