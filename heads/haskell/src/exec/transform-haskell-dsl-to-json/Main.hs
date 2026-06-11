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
  hydraCoqModules, hydraGoModules, hydraJavaModules, hydraTypeScriptModules,
  hydraPythonModules, hydraScalaModules, hydraLispModules,
  hydraPgModules, hydraRdfModules, hydraWasmModules,
  hydraExtPackageModules,
  hydraExtDecodingModules, hydraExtEncodingModules)
import Hydra.Sources.Test.All (testModules)

import qualified Hydra.Kernel as Kernel
import qualified Hydra.Packaging as Packaging
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
      where ns = Kernel.moduleName m


-- | Native hosts own the DSL→JSON path for hydra-java and hydra-python (#344).
-- Their canonical hydra.java.* / hydra.python.* JSON is produced by
-- bin/generate-hydra-{java,python}-from-{java,python}.sh (Phase 0 of sync.sh).
-- We still keep their modules in 'fullMainUniverse' so they participate in the
-- inference universe and in DSL-wrapper synthesis, but we exclude them from
-- the term-level JSON write pass.
--
-- Legacy: the Haskell DSL copies at packages/hydra-{java,python}/src/main/haskell/
-- remain as a historical reference through 0.15 but no longer drive
-- dist/json/hydra-{java,python}/. To be deleted before 0.16.
isNativeOwned :: Kernel.Module -> Bool
isNativeOwned m =
  let ns = Packaging.unModuleName (Kernel.moduleName m)
  in L.isPrefixOf "hydra.java." ns || L.isPrefixOf "hydra.python." ns


-- | All main-side modules across all packages, deduped. Used as the type-
-- resolution universe regardless of which package is being written.
--
-- Every package's DSL sources are included here so that per-package JSON
-- output can route modules to their owning package via namespaceToPackage.
fullMainUniverse :: [Kernel.Module]
fullMainUniverse = dedupByNamespace $ L.concat
  [ mainModules
  , dslSourceModules
  , hydraCoqModules
  , hydraGoModules
  , hydraJavaModules
  , hydraTypeScriptModules
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


data Options = Options
  { optPackage          :: Maybe String
  , optSourceSet        :: String
  , optDistJsonRoot     :: FilePath
  , optIncludeJavaPython :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optPackage          = Nothing
  , optSourceSet        = "main"
  , optDistJsonRoot     = defaultDistJsonRoot
  , optIncludeJavaPython = False
  }


parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] = case optPackage opts of
      Nothing -> Right opts  -- no --package means "all packages" (batch mode)
      Just _  -> Right opts
    go opts ("--package" : p : rest) = go (opts { optPackage = Just p }) rest
    go opts ("--all" : rest) = go (opts { optPackage = Nothing }) rest
    go opts ("--source-set" : s : rest)
      | s == "main" || s == "test" = go (opts { optSourceSet = s }) rest
      | otherwise                  = Left $ "--source-set must be 'main' or 'test', got: " ++ s
    go opts ("--dist-json-root" : d : rest) = go (opts { optDistJsonRoot = d }) rest
    go opts ("--include-java-python" : rest) = go (opts { optIncludeJavaPython = True }) rest
    go _    (arg : _) = Left $ "Unknown argument: " ++ arg


usage :: String
usage = unlines
  [ "Usage: transform-haskell-dsl-to-json [--package <pkg>] [OPTIONS]"
  , ""
  , "Options:"
  , "  --package <pkg>          Package to transform. If omitted (or --all"
  , "                           is passed), every package is transformed in a"
  , "                           single Haskell-universe load."
  , "                           Packages: hydra-kernel, hydra-haskell,"
  , "                           hydra-java, hydra-python, hydra-scala,"
  , "                           hydra-lisp, hydra-coq, hydra-typescript,"
  , "                           hydra-pg, hydra-rdf, hydra-ext, hydra-wasm."
  , "  --all                    Batch mode: transform every package."
  , "  --source-set <main|test> Source set to transform (default: main)."
  , "                           'test' is only non-empty for hydra-kernel today."
  , "  --dist-json-root <dir>   Output root (default: ../../dist/json)."
  , "  --include-java-python    Force-write term-level JSON for hydra-java and"
  , "                           hydra-python. By default (#344) the native"
  , "                           generators in bin/generate-hydra-{java,python}-"
  , "                           from-{java,python}.sh own these paths; this flag"
  , "                           is for cold-start bootstrap when the native host"
  , "                           is not yet built."
  ]


-- | Every supported package (ordering matches hydra.json).
allPackages :: [String]
allPackages =
  [ "hydra-kernel"
  , "hydra-haskell"
  , "hydra-java"
  , "hydra-python"
  , "hydra-scala"
  , "hydra-go"
  , "hydra-lisp"
  , "hydra-coq"
  , "hydra-typescript"
  , "hydra-wasm"
  , "hydra-pg"
  , "hydra-rdf"
  , "hydra-ext"
  ]


-- | Return the test modules owned by the given package. Today, only the
-- kernel has any test modules; other packages return [].
packageTestModules :: String -> [Kernel.Module]
packageTestModules "hydra-kernel" = testModules
packageTestModules _              = []


-- | Return the type-defining modules whose DSL wrappers should be generated
-- for this package. The kernel DSL pass covers kernel + json + other modules;
-- the haskell DSL pass covers haskell coder modules; each coder package
-- (hydra-java, hydra-python, hydra-scala, hydra-lisp, hydra-go) maps to its
-- own type-defining modules so that Hydra/Dsl/<lang>/Syntax.hs phantom
-- helpers are regenerated whenever the syntax model changes. Other packages
-- still return [] and skip DSL generation.
packageDslInputModules :: String -> [Kernel.Module]
packageDslInputModules "hydra-kernel"  = kernelModules ++ jsonModules ++ otherModules
packageDslInputModules "hydra-haskell" = haskellModules
packageDslInputModules "hydra-java"    = hydraJavaModules
packageDslInputModules "hydra-python"  = hydraPythonModules
packageDslInputModules "hydra-scala"   = hydraScalaModules
packageDslInputModules "hydra-lisp"    = hydraLispModules
packageDslInputModules "hydra-go"      = hydraGoModules
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

  let srcSet           = optSourceSet opts
      distRoot         = optDistJsonRoot opts
      includeJavaPython = optIncludeJavaPython opts

  case optPackage opts of
    Just pkg -> runSinglePackage pkg srcSet distRoot includeJavaPython
    Nothing  -> runAllPackages      srcSet distRoot includeJavaPython

  putStrLn "=== Done. ==="

-- | Transform a single package. Historical behaviour, kept for scoped
-- callers that invoke one package at a time.
runSinglePackage :: String -> String -> FilePath -> Bool -> IO ()
runSinglePackage pkg srcSet distRoot includeJavaPython = do
  let outDir = distRoot FP.</> pkg FP.</> "src" FP.</> srcSet FP.</> "json"
  putStrLn $ "=== Transform Haskell DSL -> JSON: " ++ pkg ++ " (" ++ srcSet ++ ") ==="

  let (mods, universe) = case srcSet of
        "main" ->
          let owned = filter (\m -> namespaceToPackage (Kernel.moduleName m) == pkg)
                             fullMainUniverse
              -- #344: skip term-level JSON for hydra.java.*/hydra.python.* unless
              -- --include-java-python (cold-start bootstrap). The native generators
              -- own these paths in normal operation.
              kept | includeJavaPython = owned
                   | otherwise         = filter (not . isNativeOwned) owned
          in (kept, fullMainUniverse)
        "test" ->
          let ts       = packageTestModules pkg
              testUniv = fullMainUniverse ++ ts
          in (ts, testUniv)
        _ -> ([], fullMainUniverse)

  -- #344: for hydra-java/hydra-python in 'main' without --include-java-python,
  -- term-level JSON is owned by the native generator, so 'mods' may be empty.
  -- We still emit DSL wrappers below, so don't exit early in that case.
  let nativeOwnedPkg = srcSet == "main"
                       && not includeJavaPython
                       && (pkg == "hydra-java" || pkg == "hydra-python")
  when (null mods && not nativeOwnedPkg) $ do
    putStrLn $ "  (no modules in " ++ pkg ++ "/" ++ srcSet ++ "; skipping)"
    exitSuccess

  if nativeOwnedPkg
    then putStrLn $ "  (" ++ pkg
           ++ ": term-level JSON owned by native generator; see #344)"
    else putStrLn $ "  Writing " ++ show (length mods) ++ " modules -> " ++ outDir

  case srcSet of
    "main" -> do
      when (not (null mods)) $
        writeModulesJsonPackageSplit True distRoot universe mods
      let dslInputs = packageDslInputModules pkg
      case dslInputs of
        [] -> return ()
        _  -> do
          putStrLn ""
          putStrLn $ "  Generating DSL wrapper modules for "
            ++ show (length dslInputs) ++ " type modules..."
          writeDslJsonPackageSplit distRoot universe dslInputs
    "test" -> writeModulesJson True outDir universe mods
    _      -> return ()

-- | Batch mode: transform every package with one Haskell universe load.
-- writeModulesJsonPackageSplit routes modules to their owning package's
-- per-package JSON tree based on namespaceToPackage, so passing the whole
-- universe produces the same on-disk layout as per-package calls — but
-- we only pay the Haskell startup + DSL-compile cost once.
runAllPackages :: String -> FilePath -> Bool -> IO ()
runAllPackages srcSet distRoot includeJavaPython = do
  putStrLn $ "=== Transform Haskell DSL -> JSON: all packages (" ++ srcSet ++ ") ==="

  case srcSet of
    "main" -> do
      -- #344: term-level JSON for hydra.java.* / hydra.python.* is owned by
      -- the native generators in normal operation. Drop them from the write
      -- set unless --include-java-python (cold-start bootstrap). The full
      -- universe is still used for inference.
      let writeUniverse
            | includeJavaPython = fullMainUniverse
            | otherwise         = filter (not . isNativeOwned) fullMainUniverse
          excluded = length fullMainUniverse - length writeUniverse
      putStrLn $ "  Writing " ++ show (length writeUniverse)
        ++ " main modules (routed per-package)..."
      when (excluded > 0) $
        putStrLn $ "  (excluded " ++ show excluded
          ++ " hydra.java.*/hydra.python.* modules — owned by native generators; see #344)"
      writeModulesJsonPackageSplit True distRoot fullMainUniverse writeUniverse
      -- DSL wrappers: generate for every package that has type-defining
      -- inputs. The routing is implicit — wrapper namespaces like
      -- hydra.dsl.<pkg>.* resolve to their owning package via
      -- packagePrefixes in PackageRouting.
      let allDslInputs = concatMap packageDslInputModules allPackages
      when (not (null allDslInputs)) $ do
        putStrLn ""
        putStrLn $ "  Generating DSL wrapper modules for "
          ++ show (length allDslInputs) ++ " type modules..."
        writeDslJsonPackageSplit distRoot fullMainUniverse allDslInputs
    "test" -> do
      let allTestMods = concatMap packageTestModules allPackages
          testUniverse = fullMainUniverse ++ allTestMods
      when (null allTestMods) $ do
        putStrLn "  (no test modules in any package; skipping)"
        exitSuccess
      -- Today only hydra-kernel has tests, and its tests live at a flat
      -- hydra-kernel/src/test/json/ location. writeModulesJson is the
      -- direct (non-routing) writer.
      let kernelTestDir = distRoot FP.</> "hydra-kernel" FP.</> "src"
            FP.</> "test" FP.</> "json"
      putStrLn $ "  Writing " ++ show (length allTestMods)
        ++ " test modules -> " ++ kernelTestDir
      writeModulesJson True kernelTestDir testUniverse allTestMods
    _ -> return ()
