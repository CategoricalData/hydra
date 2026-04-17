-- | Layer 1 transform: Haskell DSL -> JSON, scoped to a single package and
-- source set.
--
-- Usage:
--   transform-haskell-dsl-to-json --package <pkg> --source-set <main|test>
--                                 [--dist-json-root <dir>]
--
-- Reads the named package's manifest module (via Hydra.Sources.Ext, which
-- re-exports every per-package manifest), selects the requested source set,
-- and writes that set's JSON under <dist-json-root>/<pkg>/src/<set>/json/.
--
-- The full main-side universe (all packages) is used as the type-resolution
-- context, so cross-package type references resolve correctly. Only the
-- requested package's modules are actually written to disk.
--
-- Empty source sets (e.g. 'test' on a package with no test modules) exit 0
-- cleanly with a one-line note; callers can invoke unconditionally.
--
-- This is a Layer 1 transform per the sync system design in
-- feature_290_packaging-plan.md: stateless, idempotent, single
-- (package, source-set) pair per invocation. Composition is the orchestrator's
-- job, not this exec's.

module Main where

import Hydra.Generation (writeModulesJson, writeModulesJsonPackageSplit, writeDslJsonPackageSplit)
import Hydra.PackageRouting (defaultDistJsonRoot, namespaceToPackage)
import Hydra.Sources.Ext (
  mainModules, dslSourceModules,
  kernelModules, haskellModules, jsonModules, otherModules,
  hydraJavaModules, hydraPythonModules, hydraScalaModules, hydraLispModules,
  hydraPgModules, hydraRdfModules, hydraWasmModules,
  hydraExtDecodingModules, hydraExtEncodingModules)
import Hydra.Sources.Eval.Lib.All (evalLibModules)
import Hydra.Sources.Test.All (testModules)

import qualified Hydra.Kernel as Kernel
import qualified Hydra.Sources.Demos.GenPG.Transform as GenPGTransform

import Control.Monad (when)
import qualified Data.List as L
import qualified Data.Set as S
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import qualified System.FilePath as FP


-- | Deduplicate a list of modules by namespace, keeping the first occurrence.
dedupByNamespace :: [Kernel.Module] -> [Kernel.Module]
dedupByNamespace = go S.empty
  where
    go _    []     = []
    go seen (m:ms)
      | ns `S.member` seen = go seen ms
      | otherwise          = m : go (S.insert ns seen) ms
      where ns = Kernel.moduleNamespace m


-- | All main-side modules across all packages, deduped. Used as the type-
-- resolution universe regardless of which package is being written.
--
-- Mirrors update-json-main's universe exactly, so per-package output from
-- this tool matches what the monolithic exec would produce. The long-tail
-- ext packages (hydra-coq, hydra-javascript, hydra-ext) are deliberately
-- excluded: they are generated as Haskell only via sync-ext.sh, not via
-- the JSON pipeline.
fullMainUniverse :: [Kernel.Module]
fullMainUniverse = dedupByNamespace $ L.concat
  [ mainModules
  , evalLibModules
  , dslSourceModules
  , hydraJavaModules
  , hydraPythonModules
  , hydraScalaModules
  , hydraLispModules
  , hydraPgModules
  , hydraRdfModules
  , hydraWasmModules
  , hydraExtDecodingModules
  , hydraExtEncodingModules
  , [GenPGTransform.module_]
  ]


data Options = Options
  { optPackage      :: Maybe String
  , optSourceSet    :: String
  , optDistJsonRoot :: FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { optPackage      = Nothing
  , optSourceSet    = "main"
  , optDistJsonRoot = defaultDistJsonRoot
  }


parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] = case optPackage opts of
      Nothing -> Left "Missing --package"
      Just _  -> Right opts
    go opts ("--package" : p : rest) = go (opts { optPackage = Just p }) rest
    go opts ("--source-set" : s : rest)
      | s == "main" || s == "test" = go (opts { optSourceSet = s }) rest
      | otherwise                  = Left $ "--source-set must be 'main' or 'test', got: " ++ s
    go opts ("--dist-json-root" : d : rest) = go (opts { optDistJsonRoot = d }) rest
    go _    (arg : _) = Left $ "Unknown argument: " ++ arg


usage :: String
usage = unlines
  [ "Usage: transform-haskell-dsl-to-json --package <pkg> [OPTIONS]"
  , ""
  , "Options:"
  , "  --package <pkg>          Package to transform (required). One of:"
  , "                           hydra-kernel, hydra-haskell, hydra-java,"
  , "                           hydra-python, hydra-scala, hydra-lisp,"
  , "                           hydra-pg, hydra-rdf, hydra-wasm."
  , "                           (Long-tail ext packages -- hydra-coq,"
  , "                           hydra-javascript, hydra-ext -- are Haskell-only"
  , "                           today; they have no JSON pipeline.)"
  , "  --source-set <main|test> Source set to transform (default: main)."
  , "                           'test' is only non-empty for hydra-kernel today."
  , "  --dist-json-root <dir>   Output root (default: ../../dist/json)."
  ]


-- | Return the test modules owned by the given package. Today, only the
-- kernel has any test modules; other packages return [].
packageTestModules :: String -> [Kernel.Module]
packageTestModules "hydra-kernel" = testModules
packageTestModules _              = []


-- | Return the type-defining modules whose DSL wrappers should be generated
-- for this package. Only kernel and hydra-haskell have DSL wrappers today;
-- other packages return [] and skip DSL generation.
--
-- Mirrors update-json-main's dslInputMods composition: the kernel DSL pass
-- covers kernel + json + other modules; the haskell DSL pass covers haskell
-- coder modules. Other packages don't produce DSL wrappers.
packageDslInputModules :: String -> [Kernel.Module]
packageDslInputModules "hydra-kernel"  = kernelModules ++ jsonModules ++ otherModules
packageDslInputModules "hydra-haskell" = haskellModules
packageDslInputModules _               = []


main :: IO ()
main = do
  args <- getArgs
  opts <- case parseArgs args of
    Left err -> do
      putStrLn $ "Error: " ++ err
      putStrLn ""
      putStrLn usage
      exitFailure
    Right o -> return o

  let pkg       = maybe "" id (optPackage opts)
      srcSet    = optSourceSet opts
      distRoot  = optDistJsonRoot opts
      outDir    = distRoot FP.</> pkg FP.</> "src" FP.</> srcSet FP.</> "json"

  putStrLn $ "=== Transform Haskell DSL -> JSON: " ++ pkg ++ " (" ++ srcSet ++ ") ==="

  let (mods, universe) = case srcSet of
        "main" ->
          -- Select modules owned by this package from the full main universe.
          let owned = filter (\m -> namespaceToPackage (Kernel.moduleNamespace m) == pkg)
                             fullMainUniverse
          in (owned, fullMainUniverse)
        "test" ->
          -- Test universe = full main universe + that package's test modules.
          let ts       = packageTestModules pkg
              testUniv = fullMainUniverse ++ ts
          in (ts, testUniv)
        _ -> ([], fullMainUniverse)  -- unreachable: parseArgs rejects other values

  when (null mods) $ do
    putStrLn $ "  (no modules in " ++ pkg ++ "/" ++ srcSet ++ "; skipping)"
    exitSuccess

  putStrLn $ "  Writing " ++ show (length mods) ++ " modules -> " ++ outDir

  -- For main: use the package-split writer, which routes by namespace to the
  -- right per-package main directory. For test: use the direct writer, since
  -- test output always goes to <pkg>/src/test/json/ regardless of namespace.
  case srcSet of
    "main" -> do
      writeModulesJsonPackageSplit True distRoot universe mods
      -- Second pass: generate DSL wrapper modules for the package's own
      -- type-defining modules. Only kernel and hydra-haskell have DSL
      -- wrappers today; other packages skip this pass.
      let dslInputs = packageDslInputModules pkg
      case dslInputs of
        [] -> return ()
        _  -> do
          putStrLn ""
          putStrLn $ "  Generating DSL wrapper modules for " ++ show (length dslInputs) ++ " type modules..."
          writeDslJsonPackageSplit distRoot universe dslInputs
    "test" -> writeModulesJson True outDir universe mods
    _      -> return ()

  putStrLn "=== Done. ==="
