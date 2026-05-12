-- | Write per-package manifest.json files under dist/json/<pkg>/src/main/json/.
--
-- Each package's manifest lists only the namespaces it owns. Bootstrap
-- readers (bootstrap-from-json, language hosts) walk multiple package
-- manifests in dependency order rather than reading a single monolithic
-- manifest.

module Main where

import Hydra.Generation (writePerPackageManifestsJson)
import Hydra.PackageRouting (defaultDistJsonRoot)
import Hydra.Sources.Ext (
  mainModules, dslSourceModules, kernelModules, haskellModules, jsonModules, otherModules,
  hydraBenchModules,
  hydraCoqModules, hydraGoModules, hydraJavaModules, hydraJavaScriptModules,
  hydraPythonModules, hydraScalaModules, hydraLispModules,
  hydraPgModules, hydraRdfModules, hydraWasmModules,
  hydraExtPackageModules,
  hydraExtDecodingModules, hydraExtEncodingModules)
import Hydra.Sources.Kernel.Lib.Defaults.All (defaultLibModules)
import Hydra.Sources.Test.All (testModules)

import qualified Hydra.Kernel as Kernel
import qualified Hydra.Sources.Demos.GenPG.Transform as GenPGTransform

import qualified Data.List as L
import qualified Data.Set as S
import qualified System.Environment


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
  args <- System.Environment.getArgs
  let includeBench = "--include-bench" `elem` args

  putStrLn "=== Generate per-package JSON manifests ==="
  putStrLn ""

  -- The full main-side universe mirrors update-json-main's input: every
  -- module written to JSON should appear in exactly one package's manifest.
  -- hydra-bench is opt-in: --include-bench (set by bin/sync-bench.sh) adds the
  -- synthetic inference workloads. Default sync omits them.
  let extraBench = if includeBench then hydraBenchModules else []
  let mainUniverse = dedupByNamespace $ L.concat
        [ mainModules
        , defaultLibModules
        , dslSourceModules
        , extraBench
        , hydraCoqModules
        , hydraGoModules
        , hydraJavaModules
        , hydraJavaScriptModules
        , hydraPythonModules
        , hydraScalaModules
        , hydraLispModules
        , hydraPgModules
        , hydraRdfModules
        , hydraWasmModules
        , hydraExtPackageModules
        , hydraExtDecodingModules
        , hydraExtEncodingModules
        , [GenPGTransform.module_]
        ]
  -- DSL generator input: the kernel-side type universe. Matches update-json-main.
  let dslInputMods = kernelModules ++ jsonModules ++ otherModules ++ haskellModules ++ hydraPythonModules

  writePerPackageManifestsJson defaultDistJsonRoot mainUniverse dslInputMods mainUniverse testModules
  putStrLn ""
  putStrLn "=== Done! ==="
