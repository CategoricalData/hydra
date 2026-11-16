-- | Textual (.hydra) module sources for Phase 1 (#837).
--
-- A package declares which of its modules are sourced from .hydra text with a "textualSources" field in
-- its package.json: the string "all" (every module of the package that has a .hydra file), the string "main"
-- (like "all" but excluding the hydra.core.test.* test modules), or an explicit list of module names. 'substituteTextualSources' replaces those modules' compiled DSL
-- definitions in the universe with the modules parsed from their .hydra files; everything downstream
-- (validation, inference, routing, JSON writing) is unchanged, so a correctly flipped module yields
-- byte-identical JSON (see the verify-hydra-sources guard).
--
-- Failures are fatal: a listed module with no .hydra file, or a .hydra file that does not parse, stops
-- the sync; there is no fallback to the DSL copy.
module Hydra.TextualSources (
  TextualSources(..),
  readTextualSources,
  readTextualExcludes,
  unionTypeNamesOf,
  universeNamesOf,
  loadTextualModule,
  substituteTextualSources,
) where

import Hydra.Kernel
import Hydra.Core.Packaging
import qualified Hydra.Core.Codegen as CodeGeneration
import qualified Hydra.Core.Parse.Packaging as ParsePackaging

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

-- | Which of a package's modules are sourced from .hydra files.
data TextualSources
  = TextualNone
  | TextualAll
  | TextualMain
  | TextualOnly [ModuleName]
  deriving (Eq, Show)

-- | Read the "textualSources" field of a package directory's package.json. A missing file or field
-- means no textual sources; a malformed field is an error.
readTextualSources :: FilePath -> IO TextualSources
readTextualSources pkgDir = do
  let file = pkgDir </> "package.json"
  exists <- doesFileExist file
  if not exists then return TextualNone else do
    bytes <- BL.readFile file
    case A.eitherDecode bytes of
      Left err -> fail $ file ++ ": invalid JSON: " ++ err
      Right (A.Object km) -> case AKM.lookup (AK.fromString "textualSources") km of
        Nothing -> return TextualNone
        Just (A.String t) | T.unpack t == "all" -> return TextualAll
        Just (A.String t) | T.unpack t == "main" -> return TextualMain
        Just (A.Array a) -> TextualOnly <$> mapM (asModuleName file) (V.toList a)
        Just _ -> fail $ file ++ ": \"textualSources\" must be \"all\", \"main\" or a list of module names"
      Right _ -> fail $ file ++ ": expected a JSON object"
  where
    asModuleName file v = case v of
      A.String t -> return (ModuleName (T.unpack t))
      _ -> fail $ file ++ ": \"textualSources\" entries must be strings"

-- | The optional "textualSourcesExclude" field of a package's package.json: a list of module-name prefixes that
-- stay DSL-sourced even under "all"/"main" (e.g. "hydra.core.lib." while the primitive-parameter format of the
-- textual syntax is undecided).
readTextualExcludes :: FilePath -> IO [String]
readTextualExcludes pkgDir = do
  let file = pkgDir </> "package.json"
  exists <- doesFileExist file
  if not exists then return [] else do
    bytes <- BL.readFile file
    case A.eitherDecode bytes of
      Left err -> fail $ file ++ ": invalid JSON: " ++ err
      Right (A.Object km) -> case AKM.lookup (AK.fromString "textualSourcesExclude") km of
        Nothing -> return []
        Just (A.Array a) -> mapM asPrefix (V.toList a)
        Just _ -> fail $ file ++ ": \"textualSourcesExclude\" must be a list of module-name prefixes"
      Right _ -> fail $ file ++ ": expected a JSON object"
  where
    asPrefix v = case v of
      A.String t -> return (T.unpack t)
      _ -> fail "textualSourcesExclude entries must be strings"

definitionName :: Definition -> Name
definitionName d = case d of
  DefinitionTerm td -> termDefinitionName td
  DefinitionType td -> typeDefinitionName td
  DefinitionPrimitive pd -> primitiveDefinitionName pd

-- | ModuleName -> fully-qualified names declared by that module (the parser's universe argument).
universeNamesOf :: [Module] -> M.Map ModuleName (S.Set Name)
universeNamesOf mods = M.fromList
  [(moduleName m, S.fromList (definitionName <$> moduleDefinitions m)) | m <- mods]

-- | Fully-qualified names of every union type defined across the universe; lets the parser treat a
-- cross-module one-field construct as an injection rather than a record.
unionTypeNamesOf :: [Module] -> S.Set Name
unionTypeNamesOf mods = S.fromList
  [typeDefinitionName td | m <- mods, DefinitionType td <- moduleDefinitions m, isUnion (typeSchemeBody (typeDefinitionBody td))]
  where
    isUnion t = case t of
      TypeUnion _ -> True
      TypeForall f -> isUnion (forallTypeBody f)
      TypeAnnotated a -> isUnion (annotatedTypeBody a)
      _ -> False

-- | Parse one .hydra file against the given universe; fails on any parse error.
loadTextualModule :: M.Map ModuleName (S.Set Name) -> S.Set Name -> FilePath -> IO Module
loadTextualModule universe unionNames file = do
  src <- readFile file
  case ParsePackaging.parseModuleWith universe unionNames src of
    Left err -> fail $ file ++ ": " ++ err
    Right m -> return m

-- | Replace, in the given universe, every module that a package declares as textually sourced with the
-- module parsed from its .hydra file (packages/<pkg>/src/main/hydra/<module path>.hydra). 'packagesRoot'
-- is the directory holding the package directories.
substituteTextualSources :: FilePath -> [Module] -> IO [Module]
substituteTextualSources packagesRoot universeMods = do
  pkgs <- L.sort <$> listDirectory packagesRoot
  let universe = universeNamesOf universeMods
      unionNames = unionTypeNamesOf universeMods
      inUniverse = M.fromList [(moduleName m, m) | m <- universeMods]
  loaded <- fmap concat $ mapM (loadPackage universe unionNames inUniverse) pkgs
  let replacements = M.fromList [(moduleName m, m) | m <- loaded]
  return [M.findWithDefault m (moduleName m) replacements | m <- universeMods]
  where
    hydraFile pkg ns = packagesRoot </> pkg </> "src/main/hydra" </> CodeGeneration.moduleNameToPath ns ++ ".hydra"
    loadPackage universe unionNames inUniverse pkg = do
      flag <- readTextualSources (packagesRoot </> pkg)
      excludes <- readTextualExcludes (packagesRoot </> pkg)
      let excluded ns = any (`L.isPrefixOf` unModuleName ns) excludes
      case flag of
        TextualNone -> return []
        TextualMain -> do
          let mainOnly = [ns | ns <- M.keys inUniverse, not ("hydra.core.test." `L.isPrefixOf` unModuleName ns), not (excluded ns)]
          present <- filterM' (doesFileExist . hydraFile pkg) mainOnly
          mapM (loadTextualModule universe unionNames . hydraFile pkg) present
        TextualAll -> do
          present <- filterM' (doesFileExist . hydraFile pkg) [ns | ns <- M.keys inUniverse, not (excluded ns)]
          mapM (loadTextualModule universe unionNames . hydraFile pkg) present
        TextualOnly nss -> mapM (loadListed universe unionNames inUniverse pkg) nss
    loadListed universe unionNames inUniverse pkg ns = do
      let file = hydraFile pkg ns
      if not (M.member ns inUniverse)
        then fail $ packagesRoot </> pkg </> "package.json: textualSources lists " ++ unModuleName ns ++ ", which is not in the universe"
        else do
          exists <- doesFileExist file
          if exists then loadTextualModule universe unionNames file
            else fail $ packagesRoot </> pkg </> "package.json: textualSources lists " ++ unModuleName ns ++ " but " ++ file ++ " does not exist"
    filterM' p = fmap concat . mapM (\x -> (\b -> [x | b]) <$> p x)
