-- | Export all Hydra DSL modules to JSON, fanned out across per-package
-- directories under dist/json/.
--
-- Replaces the older update-json-main + update-json-ext split. Every module
-- in the universe is routed to its owning package's dist/json/<pkg>/src/main/json/
-- directory via the routing table in Hydra.PackageRouting.
--
-- Universe (deduped by namespace before writing):
--   * hydra-kernel: mainModules + evalLibModules + dslSourceModules
--   * hydra-haskell: haskellModules (already in mainModules; routed here)
--   * coder packages: hydraJavaModules, hydraPythonModules, hydraScalaModules,
--     hydraLispModules
--   * hydra-pg: hydraPgModules + pg decode/encode meta-sources + GenPGTransform
--   * hydra-rdf: hydraRdfModules
--
-- DSL wrapper modules (hydra.dsl.*) are generated in a second pass with the
-- same routing.

module Main where

import Hydra.Generation (writeModulesJsonPackageSplit, writeDslJsonPackageSplit)
import Hydra.PackageRouting (defaultDistJsonRoot)
import Hydra.Sources.Ext (
  mainModules, dslSourceModules, kernelModules, haskellModules, jsonModules, otherModules,
  hydraJavaModules, hydraPythonModules, hydraScalaModules, hydraLispModules,
  hydraPgModules, hydraRdfModules,
  hydraExtDecodingModules, hydraExtEncodingModules)
import Hydra.Sources.Eval.Lib.All (evalLibModules)

import qualified Hydra.Kernel as Kernel
import qualified Hydra.Sources.Demos.GenPG.Transform as GenPGTransform

import Control.Exception (catch, SomeException)
import qualified Data.List as L
import qualified Data.Set as S
import System.Environment (getArgs)
import System.Exit (exitFailure)


-- | Deduplicate a list of modules by namespace, keeping the first occurrence.
dedupByNamespace :: [Kernel.Module] -> [Kernel.Module]
dedupByNamespace = go S.empty
  where
    go _    []     = []
    go seen (m:ms)
      | ns `S.member` seen = go seen ms
      | otherwise          = m : go (S.insert ns seen) ms
      where ns = Kernel.moduleNamespace m

main :: IO ()
main = do
  distRoot <- parseDistRoot defaultDistJsonRoot

  let universe = dedupByNamespace $ L.concat
        [ mainModules
        , evalLibModules
        , dslSourceModules
        , hydraJavaModules
        , hydraPythonModules
        , hydraScalaModules
        , hydraLispModules
        , hydraPgModules
        , hydraRdfModules
        , hydraExtDecodingModules
        , hydraExtEncodingModules
        , [GenPGTransform.module_]
        ]

  putStrLn "=== Generate Hydra JSON modules (package-split) ==="
  putStrLn ""
  putStrLn $ "Generating " ++ show (length universe) ++ " modules to JSON, routed per package..."
  putStrLn $ "dist-json root: " ++ distRoot
  putStrLn ""

  result <- catch
    (writeModulesJsonPackageSplit True distRoot universe universe >> return True)
    (\e -> do
      putStrLn $ "Error: " ++ show (e :: SomeException)
      return False)

  putStrLn ""
  putStrLn "Generating DSL wrapper modules to JSON..."
  -- The DSL generator runs over the kernel-side type universe (kernel + json +
  -- other + haskell coder). This matches the old behavior of update-json-main:
  -- DSL modules are produced for every type in that universe.
  let dslInputMods = kernelModules ++ jsonModules ++ otherModules ++ haskellModules
  dslResult <- catch
    (writeDslJsonPackageSplit distRoot universe dslInputMods >> return True)
    (\e -> do
      putStrLn $ "Error generating DSL JSON: " ++ show (e :: SomeException)
      return False)

  if result && dslResult
    then do
      putStrLn ""
      putStrLn "=== Done! ==="
    else do
      putStrLn ""
      putStrLn "=== FAILED ==="
      exitFailure

-- | Parse an optional --dist-root argument. Retains the older --output-dir
-- flag name for call-site compatibility (it is interpreted as a dist-json
-- root now).
parseDistRoot :: String -> IO String
parseDistRoot defaultRoot = do
  args <- getArgs
  return $ go args
  where
    go ("--dist-root" : dir : _)  = dir
    go ("--output-dir" : dir : _) = dir
    go (_ : rest)                 = go rest
    go []                         = defaultRoot
