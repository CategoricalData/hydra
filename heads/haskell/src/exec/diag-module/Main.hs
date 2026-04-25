-- Diagnostic: bisect the incremental-inference bug down to individual
-- definitions.
--
-- Modes:
--   stack exec diag-module -- <ns1,ns2,...>
--     Use full modules as the dirty set; load the rest from dist/json.
--
--   stack exec diag-module -- --subset <ns> <defName1,defName2,...>
--     Build a synthetic module containing only the named definitions
--     of <ns>, use that as the single dirty module, load the rest of
--     the universe (including the full version of <ns>) from dist/json.
--     This lets us narrow the failure to a specific definition.
--
--   stack exec diag-module -- --list-defs <ns>
--     Print the definition names of the module, one per line.
module Main where

import Hydra.Kernel
import Hydra.Generation (inferModulesGivenIO)
import qualified Hydra.Codegen as CodeGeneration
import qualified Hydra.Sources.All as All
import qualified Hydra.Sources.Ext as Ext
import qualified Hydra.Show.Errors as ShowError
import qualified Hydra.Json.Model as JsonModel
import Hydra.Dsl.Bootstrap (bootstrapGraph)
import qualified Hydra.PackageRouting as PackageRouting

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.Vector as V
import qualified Data.Text as T

import qualified Data.Map as M
import qualified Data.List as L
import qualified Control.Exception as E
import qualified Control.Monad as CM
import qualified System.Environment as Env
import qualified System.FilePath as FP
import qualified System.Directory as SD
import System.IO (hSetBuffering, stdout, BufferMode(..))

parseJsonFile :: FilePath -> IO (Either String JsonModel.Value)
parseJsonFile path = do
  bytes <- BS.readFile path
  case A.eitherDecode bytes of
    Left err -> return (Left err)
    Right aesonVal -> return (Right (aesonToJsonModel aesonVal))

aesonToJsonModel :: A.Value -> JsonModel.Value
aesonToJsonModel v = case v of
  A.Null -> JsonModel.ValueNull
  A.Bool b -> JsonModel.ValueBoolean b
  A.Number n -> JsonModel.ValueNumber n
  A.String t -> JsonModel.ValueString (T.unpack t)
  A.Array xs -> JsonModel.ValueArray (fmap aesonToJsonModel (V.toList xs))
  A.Object o -> JsonModel.ValueObject (M.fromList [(T.unpack (AK.toText k), aesonToJsonModel x) | (k, x) <- AKM.toList o])

loadModuleFromJson :: FilePath -> [Module] -> Namespace -> IO Module
loadModuleFromJson distJsonRoot universe ns = do
  let pkg = PackageRouting.namespaceToPackage ns
      pkgDir = distJsonRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json"
      filePath = pkgDir FP.</> CodeGeneration.namespaceToPath ns ++ ".json"
  parseResult <- parseJsonFile filePath
  case parseResult of
    Left err -> fail $ "JSON parse error for " ++ unNamespace ns ++ " at " ++ filePath ++ ": " ++ err
    Right jsonVal -> case CodeGeneration.decodeModuleFromJson bootstrapGraph universe jsonVal of
      Left err -> fail $ "Module decode error for " ++ unNamespace ns ++ ": " ++ ShowError.error err
      Right m -> return m

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- Env.getArgs
  case args of
    ["--list-defs", ns] -> listDefs ns
    ["--subset", ns, defSpec] -> runSubset ns defSpec
    [spec] -> runModules spec
    _      -> fail "Usage: diag-module [--list-defs <ns>] [--subset <ns> <def1,def2,...>] [<ns1,ns2,...>]"

listDefs :: String -> IO ()
listDefs nsStr = do
  let ns = Namespace nsStr
      universe = All.mainModules ++ Ext.hydraExtModules
  case L.find (\m -> moduleNamespace m == ns) universe of
    Nothing -> fail $ "No such module: " ++ nsStr
    Just m -> do
      let defs = moduleDefinitions m
      putStrLn $ "Definitions in " ++ nsStr ++ " (" ++ show (length defs) ++ " total):"
      mapM_ putStrLn (definitionNames defs)

definitionNames :: [Definition] -> [String]
definitionNames ds = fmap definitionName ds
  where
    definitionName d = case d of
      DefinitionTerm td -> unName (termDefinitionName td)
      DefinitionType td -> unName (typeDefinitionName td)

runModules :: String -> IO ()
runModules spec = do
  let universe = All.mainModules ++ Ext.hydraExtModules
  putStrLn $ "Universe: " ++ show (length universe) ++ " modules"
  let allKernelNss = [moduleNamespace m | m <- universe,
                       let ns = unNamespace (moduleNamespace m),
                       not ("hydra.test." `L.isPrefixOf` ns),
                       not ("hydra.ext." `L.isPrefixOf` ns),
                       not ("hydra.wasm." `L.isPrefixOf` ns),
                       not ("hydra.coq." `L.isPrefixOf` ns),
                       not ("hydra.javascript." `L.isPrefixOf` ns)]
      half = length allKernelNss `div` 2
      dirtyNss = case spec of
        "ALL"   -> allKernelNss
        "HALF1" -> take half allKernelNss
        "HALF2" -> drop half allKernelNss
        _ -> fmap Namespace (splitCommas spec)
  putStrLn $ "Dirty set: " ++ show (length dirtyNss) ++ " modules"
  CM.when (length dirtyNss <= 20) $
    mapM_ (\ns -> putStrLn $ "  " ++ unNamespace ns) dirtyNss
  let dirtySet = M.fromList [(ns, ()) | ns <- dirtyNss]
      dirtyMods = [m | m <- universe, M.member (moduleNamespace m) dirtySet]
      cleanMods = [m | m <- universe, not (M.member (moduleNamespace m) dirtySet)]
  runWithCleanAndDirty cleanMods dirtyMods

-- Subset mode: build a dirty module at namespace "hydra.bisect" that
-- contains CLONES of the named definitions from <ns>. The full <ns> stays
-- in the clean universe, so cross-definition references in the clones
-- resolve against it (i.e. clone of X calling Y sees clean X.Y). The
-- cloned definition names are `hydra.bisect.<lastComponent>`, so there's
-- no name collision with the clean universe.
--
-- This isolates whether a specific definition's term body / type
-- annotation triggers the unification bug, without perturbing the rest
-- of the universe.
runSubset :: String -> String -> IO ()
runSubset nsStr defSpec = do
  let ns = Namespace nsStr
      universe = All.mainModules ++ Ext.hydraExtModules
      wanted = splitCommas defSpec
      bisectNs = Namespace "hydra.bisect"
  case L.find (\m -> moduleNamespace m == ns) universe of
    Nothing -> fail $ "No such module: " ++ nsStr
    Just fullMod -> do
      let allDefs = moduleDefinitions fullMod
          lastCompOf s = L.reverse (L.takeWhile (/= '.') (L.reverse s))
          defMatches n d = case d of
            DefinitionTerm td -> lastCompOf (unName (termDefinitionName td)) == n
            DefinitionType td -> lastCompOf (unName (typeDefinitionName td)) == n
          picked = [d | n <- wanted, d <- allDefs, defMatches n d]
      let defName d = case d of
            DefinitionTerm td -> unName (termDefinitionName td)
            DefinitionType td -> unName (typeDefinitionName td)
      CM.when (length picked /= length wanted) $ do
        putStrLn "Warning: not all requested definitions were found."
        let found = fmap defName picked
            missing = filter (\n -> not (elem n found)) wanted
        mapM_ (\m -> putStrLn $ "  missing: " ++ m) missing
      let cloneDef d = case d of
            DefinitionTerm td ->
              DefinitionTerm td { termDefinitionName = rename (termDefinitionName td) }
            DefinitionType td ->
              DefinitionType td { typeDefinitionName = rename (typeDefinitionName td) }
          rename (Name full) =
            let simple = L.reverse (L.takeWhile (/= '.') (L.reverse full))
            in Name ("hydra.bisect." ++ simple)
          cloned = fmap cloneDef picked
      let syntheticDirty = Module {
            moduleNamespace = bisectNs,
            moduleDefinitions = cloned,
            moduleTermDependencies = [ns],
            moduleTypeDependencies = [ns],
            moduleDescription = Just "Bisection dummy module" }
      putStrLn $ "Universe: " ++ show (length universe) ++ " kept intact; dirty is new hydra.bisect with " ++ show (length picked) ++ " cloned defs:"
      mapM_ (\d -> putStrLn $ "  " ++ defName d ++ " → " ++ cloneName d) picked
      runWithCleanAndDirty universe [syntheticDirty]
  where
    cloneName d = case d of
      DefinitionTerm td -> "hydra.bisect." ++ lastComp (unName (termDefinitionName td))
      DefinitionType td -> "hydra.bisect." ++ lastComp (unName (typeDefinitionName td))
    lastComp s = L.reverse (L.takeWhile (/= '.') (L.reverse s))

runWithCleanAndDirty :: [Module] -> [Module] -> IO ()
runWithCleanAndDirty cleanMods dirtyMods = do
  putStrLn $ "Loading " ++ show (length cleanMods) ++ " clean modules from dist/json..."
  distJsonRoot <- SD.makeAbsolute "../../dist/json"
  cleanLoaded <- mapM (loadModuleFromJson distJsonRoot (cleanMods ++ dirtyMods) . moduleNamespace) cleanMods
  putStrLn "Running inferModulesGiven..."
  result <- E.try (inferModulesGivenIO (cleanLoaded ++ dirtyMods) dirtyMods) :: IO (Either E.SomeException [Module])
  case result of
    Left e -> do
      putStrLn "FAILED"
      putStrLn $ show e
    Right _ -> putStrLn "OK"

splitCommas :: String -> [String]
splitCommas [] = []
splitCommas s = let (h, t) = break (== ',') s
                in h : case t of
                     [] -> []
                     _  -> splitCommas (drop 1 t)
