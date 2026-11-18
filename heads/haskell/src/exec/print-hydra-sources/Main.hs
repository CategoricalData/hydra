-- | Print textually-sourced kernel modules from their Haskell DSL to their .hydra path (#837).
--
-- This is the SUPPORTED way to (re)generate a .hydra file from its DSL source: it mirrors
-- verify-hydra-sources's own universe construction (package.json's "textualSources"/
-- "textualSourcesExclude" fields decide which modules are in scope), so the module selection the
-- printer uses is exactly what the parser-side guard checks against. A hand-written .hydra file
-- that happens to diverge from the DSL is not a legitimate alternative to running this tool.
--
-- For each selected module: print it from the DSL (PrintPackaging.moduleDefinitionWith), parse
-- the printed text back (ParsePackaging.parseModuleWith) against the SAME universe, and re-print
-- the parsed result. Only writes the file if the two printed forms are identical (a genuine
-- print -> parse -> print fixpoint) -- a module that fails to round-trip is reported and left on
-- disk unchanged, so a bad printer/parser interaction can never silently corrupt the committed
-- .hydra corpus.
--
-- Usage (run under bin/with-stack-slot.sh or via the bin/print-hydra-sources.sh wrapper):
--   stack exec print-hydra-sources                      -- every kernel module declared textually sourced
--   stack exec print-hydra-sources -- <ns1> <ns2> ...    -- only the named modules (must be in the textual-sources scope)
-- Exits non-zero if any selected module fails to round-trip.
module Main where

import Hydra.Kernel
import Hydra.Sources.All (kernelModules)
import Hydra.Generation (modulesToGraph, buildSchemaMap)
import Hydra.Sources.Ext (
  mainModules, dslSourceModules,
  hydraCoqModules, hydraGoModules, hydraJvmModules, hydraJavaModules, hydraTypeScriptModules,
  hydraPythonModules, hydraScalaModules, hydraLispModules,
  hydraPgModules, hydraRdfModules, hydraRustModules, hydraWasmModules,
  hydraBuildModules, hydraExtPackageModules)
import qualified Hydra.Sources.Demos.GenPG.Transform as GenPGTransform
import qualified Hydra.Sources.Test.All as TestAll
import Hydra.TextualSources (TextualSources(..), readTextualSources, readTextualExcludes, unionTypeNamesOf, universeNamesOf)
import qualified Hydra.Core.Codegen as CodeGeneration
import qualified Hydra.Core.Print.Packaging as PrintPackaging
import qualified Hydra.Core.Parse.Packaging as ParsePackaging
import Hydra.Core.Packaging

import Control.Monad (forM, when, unless, filterM)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stdout, hSetEncoding, utf8)

flushPut :: String -> IO ()
flushPut s = putStrLn s >> hFlush stdout

-- | The DSL-sourced universe, as assembled by update-json-main (minus hydra-bench and the natively
-- generated coder JSON) -- the same universe verify-hydra-sources checks against.
fullDslUniverse :: [Module]
fullDslUniverse = concat
  [ mainModules, dslSourceModules, hydraCoqModules, hydraGoModules, hydraJvmModules, hydraJavaModules
  , hydraTypeScriptModules, hydraPythonModules, hydraScalaModules, hydraLispModules, hydraPgModules
  , hydraRdfModules, hydraRustModules, hydraWasmModules, hydraBuildModules, hydraExtPackageModules
  , [GenPGTransform.module_], TestAll.testModules ]

dedupByName :: [Module] -> [Module]
dedupByName = go S.empty
  where
    go _ [] = []
    go seen (m:ms)
      | moduleName m `S.member` seen = go seen ms
      | otherwise = m : go (S.insert (moduleName m) seen) ms

-- | The set of module names a package's package.json declares as textually sourced (respecting
-- "textualSources"/"textualSourcesExclude"), restricted to names actually present in 'inUniverse'
-- -- mirrors Hydra.TextualSources.substituteTextualSources's own selection logic exactly, since a
-- printer that selected a different set than the parser-side guard checks would be worse than
-- useless. In particular, "all"/"main" are gated on the .hydra file ALREADY EXISTING under this
-- package (substituteTextualSources's own 'filterM' doesFileExist' step) -- the DSL universe is
-- shared across every package (e.g. hydra-kernel's "all" scope technically ranges over the whole
-- universe, but only the modules that already have a .hydra file under hydra-kernel's tree are
-- really in scope); otherwise a brand-new, never-before-printed module from an unrelated package
-- would get swept in and printed for the first time by accident, surfacing latent round-trip gaps
-- that are real but out of scope for this run. 'TextualOnly' is explicit by construction, so it is
-- not existence-gated -- an explicitly listed module is meant to be created if missing.
packageTextualModuleNames :: FilePath -> M.Map ModuleName Module -> IO [ModuleName]
packageTextualModuleNames pkgDir inUniverse = do
  flag <- readTextualSources pkgDir
  excludes <- readTextualExcludes pkgDir
  let excluded ns = any (`L.isPrefixOf` unModuleName ns) excludes
      existing nss = filterM (doesFileExist . hydraFilePath pkgDir) nss
  case flag of
    TextualNone -> return []
    TextualMain -> existing [ns | ns <- M.keys inUniverse, not ("hydra.core.test." `L.isPrefixOf` unModuleName ns), not (excluded ns)]
    TextualAll -> existing [ns | ns <- M.keys inUniverse, not (excluded ns)]
    TextualOnly nss -> return [ns | ns <- nss, M.member ns inUniverse]

hydraFilePath :: FilePath -> ModuleName -> FilePath
hydraFilePath pkgDir ns = pkgDir </> "src/main/hydra" </> CodeGeneration.moduleNameToPath ns ++ ".hydra"

-- | Print one module, round-trip check it, and write it only if the check passes.
printModule :: M.Map ModuleName (S.Set Name) -> S.Set Name -> FilePath -> Module -> IO Bool
printModule universe unionNames outPath m = do
  let printed = PrintPackaging.moduleDefinitionWith universe m
  case ParsePackaging.parseModuleWith universe unionNames printed of
    Left err -> do
      flushPut $ "  ✗ " ++ unModuleName (moduleName m) ++ " (parse-back failed: " ++ take 300 err ++ ")"
      return False
    Right parsed ->
      let reprinted = PrintPackaging.moduleDefinitionWith universe parsed
      in if printed == reprinted
        then do
          createDirectoryIfMissing True (takeDirectory outPath)
          writeFile outPath printed
          flushPut $ "  ✓ " ++ unModuleName (moduleName m) ++ " -> " ++ outPath
          return True
        else do
          flushPut $ "  ✗ " ++ unModuleName (moduleName m) ++ " (round-trip mismatch, NOT written)"
          let diffs = [(i, a, b) | (i, (a, b)) <- zip [0 :: Int ..] (zip printed reprinted), a /= b]
          case diffs of
            ((i, a, b):_) -> flushPut $ "    first differing char at offset " ++ show i ++ ": " ++ show a ++ " vs " ++ show b
            [] -> flushPut "    (same prefix, differ only in trailing length)"
          return False

main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  let packagesRoot = "../../packages"
      wanted = fmap ModuleName args
  rootOk <- doesDirectoryExist packagesRoot
  unless rootOk $ flushPut ("packages root not found: " ++ packagesRoot) >> exitFailure
  pkgs <- L.sort <$> listDirectory packagesRoot
  let allMods = dedupByName (kernelModules ++ fullDslUniverse)
      inUniverse = M.fromList [(moduleName m, m) | m <- allMods]
      universe = universeNamesOf allMods
      unionNames = unionTypeNamesOf allMods
  flushPut $ "=== print-hydra-sources: " ++ show (M.size inUniverse) ++ "-module universe ==="
  -- (package dir, [(module name, output path)]) per package that declares any textual sources.
  perPackage <- forM pkgs $ \pkg -> do
    let pkgDir = packagesRoot </> pkg
    textualNss <- packageTextualModuleNames pkgDir inUniverse
    let selected = if null wanted then textualNss else [ns | ns <- textualNss, ns `elem` wanted]
    return (pkgDir, [(ns, hydraFilePath pkgDir ns) | ns <- selected])
  let targets = [(pkgDir, ns, path, m) | (pkgDir, nss) <- perPackage, (ns, path) <- nss, Just m <- [M.lookup ns inUniverse]]
  when (null targets) $ do
    flushPut "no modules selected (check package.json \"textualSources\" and any requested module names)"
    exitFailure
  flushPut $ show (length targets) ++ " module(s) selected for printing."
  requestedButMissing <- return [ns | ns <- wanted, not (any (\(_, n, _, _) -> n == ns) targets)]
  mapM_ (\ns -> flushPut $ "  ✗ requested module not in textual-sources scope: " ++ unModuleName ns) requestedButMissing
  results <- forM targets $ \(_, _, path, m) -> printModule universe unionNames path m
  let failures = length (filter not results)
  if failures == 0 && null requestedButMissing
    then flushPut "=== SUCCESS ===" >> exitSuccess
    else do
      flushPut $ "=== FAILED === " ++ show failures ++ " module(s) failed to round-trip, " ++ show (length requestedButMissing) ++ " requested module(s) out of scope"
      exitFailure
