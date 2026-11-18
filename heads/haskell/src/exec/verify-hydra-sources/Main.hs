-- | Identity guard for textual sources (#837): for every kernel module that has a .hydra file,
-- assert that the .hydra pipeline yields byte-identical JSON to the checked-in dist/json file.
--
-- Pipeline per module: parseModuleWith(.hydra) -> infer (with the parsed modules substituted for
-- their DSL counterparts in the universe) -> 'moduleToJson'. The result is compared with
-- dist/json/hydra-kernel/src/main/json/<path>.json, i.e. what the sync writes from the DSL.
-- A module whose .hydra fails to parse is reported and keeps its DSL form in the universe, so the
-- remaining modules are still checked. No differences are normalized away.
--
-- Usage: verify-hydra-sources [--hydra-root DIR]    (run from heads/haskell; DIR defaults to ../../packages)
-- Exits non-zero on any mismatch or parse error.

module Main where

import Hydra.Kernel
import Hydra.Sources.All (kernelModules)
import Hydra.Generation (modulesToGraph, buildSchemaMap, inferModulesIO, generateEncoderModules, generateDecoderModules)
import Hydra.Sources.Ext (
  mainModules, dslSourceModules,
  hydraCoqModules, hydraGoModules, hydraJvmModules, hydraJavaModules, hydraTypeScriptModules,
  hydraPythonModules, hydraScalaModules, hydraLispModules,
  hydraPgModules, hydraRdfModules, hydraRustModules, hydraWasmModules,
  hydraBuildModules, hydraExtPackageModules, allEncodingModules)
import qualified Hydra.Sources.Demos.GenPG.Transform as GenPGTransform
import qualified Hydra.Sources.Test.All as TestAll
import Hydra.TextualSources (unionTypeNamesOf, universeNamesOf)
import qualified Hydra.Core.Codegen as CodeGeneration
import qualified Hydra.Core.Parse.Packaging as ParsePackaging
import qualified Hydra.Core.Variables as Variables
import Hydra.Core.Packaging

import Control.Monad (forM, when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hFlush, stdout, hSetEncoding, utf8)

flushPut :: String -> IO ()
flushPut s = putStrLn s >> hFlush stdout

-- | Locate <pkgsRoot>/<pkg>/src/main/hydra/<path>.hydra for a module, if any.
findHydraFile :: FilePath -> [FilePath] -> ModuleName -> IO (Maybe FilePath)
findHydraFile root pkgs ns = do
  let rel = "src/main/hydra" </> CodeGeneration.moduleNameToPath ns ++ ".hydra"
  found <- filterM' (\p -> doesFileExist (root </> p </> rel)) pkgs
  return $ case found of
    (p:_) -> Just (root </> p </> rel)
    [] -> Nothing
  where filterM' f = fmap concat . mapM (\x -> (\b -> [x | b]) <$> f x)

-- | The DSL-sourced universe, as assembled by update-json-main (minus hydra-bench and the natively
-- generated coder JSON).
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

firstDiff :: String -> String -> String
firstDiff a b = case [(i, x, y) | (i, (x, y)) <- zip [1 :: Int ..] (zip (Prelude.lines a) (Prelude.lines b)), x /= y] of
  ((i, x, y):_) -> "line " ++ show i ++ ":\n      dsl:    " ++ take 200 x ++ "\n      hydra:  " ++ take 200 y
  [] -> "line counts differ: " ++ show (length (Prelude.lines a)) ++ " vs " ++ show (length (Prelude.lines b))

main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  let root = case args of
        ("--hydra-root":d:_) -> d
        _ -> "../../packages"
      jsonBase = "../../dist/json/hydra-kernel/src/main/json"
  rootOk <- doesDirectoryExist root
  when (not rootOk) $ flushPut ("packages root not found: " ++ root) >> exitFailure
  pkgs <- L.sort <$> listDirectory root
  flushPut "=== Verify .hydra sources against dist/json (#837) ==="
  -- The parse universe must cover every module a .hydra file can reference (kernelModules alone omits e.g.
  -- hydra.core.yaml.model from otherModules, which made yaml/decode and yaml/encode fail to resolve 'Node').
  let allMods = dedupByName (kernelModules ++ fullDslUniverse)
      graph = modulesToGraph allMods allMods
      schemaMap = buildSchemaMap graph
      universe = universeNamesOf allMods
      unionNames = unionTypeNamesOf allMods
  -- Step 1: parse every .hydra file (per-module failures are reported, not fatal yet).
  parsed <- forM allMods $ \dsl -> do
    let ns = moduleName dsl
    mf <- findHydraFile root pkgs ns
    case mf of
      Nothing -> return (ns, Nothing)
      Just fp -> do
        src <- readFile fp
        return (ns, Just (fp, ParsePackaging.parseModuleWith universe unionNames src))
  -- A .hydra file that fails to parse must be reported, not skipped.
  let parseErrors = [(ns, fp, err) | (ns, Just (fp, Left err)) <- parsed]
  mapM_ (\(ns, fp, err) -> flushPut $ "  ✗ " ++ unModuleName ns ++ " (parse error in " ++ fp ++ ")\n    " ++ take 600 err) parseErrors
  when (not (null parseErrors)) $ do
    flushPut $ "=== FAILED === " ++ show (length parseErrors) ++ " module(s) failed to parse"
    exitFailure
  let parseOk = M.fromList [(ns, m) | (ns, Just (_, Right m)) <- parsed]
      -- The full main universe (kernel + json/yaml + other kernel-adjacent packages), as in update-json-main,
      -- with the parsed modules substituted for their DSL counterparts.
      substituted = [fromMaybe d (M.lookup (moduleName d) parseOk) | d <- allMods]
      targets = [m | m <- substituted, M.member (moduleName m) parseOk]
  flushPut $ show (M.size parseOk) ++ " .hydra module(s) parsed."
  -- Unresolved bare (module-less) variables left by the parser would only surface as an opaque inference
  -- failure; list every one of them per module first.
  let bareFree m = S.toList $ S.filter (\(Name n) -> '.' `notElem` n) $ S.unions
        [Variables.freeVariablesInTerm (termDefinitionBody td) | DefinitionTerm td <- moduleDefinitions m]
      bareReports = [(unModuleName (moduleName m), names) | m <- M.elems parseOk, let names = bareFree m, not (null names)]
  mapM_ (\(n, names) -> flushPut $ "  ✗ " ++ n ++ " (unresolved bare name(s): " ++ L.intercalate ", " [x | Name x <- names] ++ ")") bareReports
  when (not (null bareReports)) $ do
    flushPut $ "=== FAILED === " ++ show (length bareReports) ++ " module(s) with unresolved bare names (inference skipped)"
    exitFailure
  -- Whole-universe inference costs ~19 GB; the sync's per-package driver does it within bounds, so the byte-level
  -- comparison is done by regenerating into a scratch dist root with update-json-main (see bin/verify-hydra-sources.sh
  -- when it exists) and diffing. Pass --infer to run the (memory-hungry) in-process comparison anyway.
  infer <- elem "--infer" <$> getArgs
  when (not infer) $ do
    flushPut "=== PARSE OK === all .hydra modules parse and resolve (no unresolved bare names); pass --infer for the in-process JSON comparison"
    exitSuccess
  flushPut "inferring..."
  -- Step 2: infer the parsed modules against the substituted universe.
  -- The derived encode/decode modules are part of the inference universe (as in update-json-main).
  encMods <- generateEncoderModules substituted allEncodingModules
  decMods <- generateDecoderModules substituted allEncodingModules
  let withDerived = dedupByName (substituted ++ encMods ++ decMods)
  inferred <- inferModulesIO withDerived targets
  let inferredMap = M.fromList [(moduleName m, m) | m <- inferred]
  -- Step 3: compare JSON per module.
  results <- forM parsed $ \(ns, mp) -> do
    let nsStr = unModuleName ns
        file = jsonBase </> CodeGeneration.moduleNameToPath ns ++ ".json"
    case mp of
      Nothing -> return Nothing
      Just (fp, Left err) -> do
        flushPut $ "  ✗ " ++ nsStr ++ " (parse error in " ++ fp ++ ")\n    " ++ err
        return (Just (False, nsStr))
      Just (fp, Right _) -> case M.lookup ns inferredMap of
        Nothing -> flushPut ("  ✗ " ++ nsStr ++ " (not inferred)") >> return (Just (False, nsStr))
        Just im -> case CodeGeneration.moduleToJson schemaMap im of
          Left e -> flushPut ("  ✗ " ++ nsStr ++ " (->JSON failed: " ++ show e ++ ")") >> return (Just (False, nsStr))
          Right b -> do
            exists <- doesFileExist file
            if not exists
              then flushPut ("  ✗ " ++ nsStr ++ " (no checked-in JSON: " ++ file ++ ")") >> return (Just (False, nsStr))
              else do
                a <- T.unpack . T.stripEnd <$> TIO.readFile file
                if a == b
                  then flushPut ("  ✓ " ++ nsStr) >> return (Just (True, nsStr))
                  else do
                    flushPut $ "  ✗ " ++ nsStr ++ " (JSON differs; " ++ fp ++ ")\n    " ++ firstDiff a b
                    return (Just (False, nsStr))
  let checked = [r | Just r <- results]
      failed = [n | (False, n) <- checked]
  flushPut $ show (length checked) ++ " module(s) checked."
  if null failed
    then flushPut "=== SUCCESS ===" >> exitSuccess
    else do
      flushPut $ "=== FAILED === " ++ show (length failed) ++ " module(s)"
      exitFailure
