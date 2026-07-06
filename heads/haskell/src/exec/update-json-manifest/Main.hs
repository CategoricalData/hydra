-- | Write per-package manifest.json files under dist/json/<pkg>/src/main/json/.
--
-- Each package's manifest lists only the namespaces it owns. Bootstrap
-- readers (bootstrap-from-json, language hosts) walk multiple package
-- manifests in dependency order rather than reading a single monolithic
-- manifest.

module Main where

import Hydra.Generation (writePerPackageManifestsJson, generateEncoderModules, generateDecoderModules)
import Hydra.PackageRouting (defaultDistJsonRoot, buildRoutingMap)
import Hydra.Sources.Ext (
  mainModules, dslSourceModules,
  hydraBenchModules,
  hydraCoqModules, hydraGoModules, hydraJvmModules, hydraJavaModules, hydraTypeScriptModules,
  hydraPythonModules, hydraScalaModules, hydraLispModules,
  hydraPgModules, hydraRdfModules, hydraWasmModules,
  hydraBuildModules, hydraBuildTestModules,
  hydraExtPackageModules,
  allDslModules, allEncodingModules, extRoutingInput)
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
      where ns = Kernel.moduleName m

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
  let baseUniverse = dedupByNamespace $ L.concat
        [ mainModules
        , dslSourceModules
        , extraBench
        , hydraCoqModules
        , hydraGoModules
        , hydraJvmModules
        , hydraJavaModules
        , hydraTypeScriptModules
        , hydraPythonModules
        , hydraScalaModules
        , hydraLispModules
        , hydraPgModules
        , hydraRdfModules
        , hydraWasmModules
        , hydraBuildModules
        , hydraExtPackageModules
        , [GenPGTransform.module_]
        ]
  encMods <- generateEncoderModules baseUniverse allEncodingModules
  decMods <- generateDecoderModules baseUniverse allEncodingModules
  let mainUniverse = dedupByNamespace (baseUniverse ++ encMods ++ decMods)
  -- Derived-module source list (#474): every package's derivedMainModules,
  -- the source modules from which dsl/encode/decode are derived. Emitted
  -- directly as the manifest's derivedMainModules field, matching the same
  -- canonical list update-json-main feeds to writeDerivedJsonPackageSplit.
  let dslSrcMods = allDslModules
      encSrcMods = allEncodingModules

  let routingMap = buildRoutingMap extRoutingInput
  -- testModules folds in hydra-build's test modules (#546): they route to
  -- hydra-build's manifest via routingMap, which now maps hydra.test.build.*
  -- to hydra-build (see Ext.extRoutingInput). hydra-build is the first
  -- non-kernel package to contribute testModules here.
  let allTestModules = testModules ++ hydraBuildTestModules
  writePerPackageManifestsJson routingMap defaultDistJsonRoot dslSrcMods encSrcMods mainUniverse allTestModules
  putStrLn ""
  putStrLn "=== Done! ==="
