-- | Textual (.hydra) module sources for Phase 1 (#837).
--
-- A package declares which of its modules are sourced from .hydra text with a "textualSources" field in
-- its package.json: the string "all" (every module of the package that has a .hydra file), the string "main"
-- (like "all" but excluding the hydra.core.test.* test modules), or an explicit list of module names. 'substituteTextualSources' replaces those modules' compiled DSL
-- definitions in the universe with the modules parsed from their .hydra files; everything downstream
-- (validation, inference, routing, JSON writing) is unchanged, so a correctly flipped module yields
-- byte-identical JSON (see the verify-hydra-sources guard).
--
-- Failures are fatal: a listed module with no .hydra file, a .hydra file that does not parse, or a package
-- that declares textualSources but ends up substituting no module, stops the sync; there is no fallback to
-- the DSL copy. Each package with textual sources logs a line, because byte-identical output otherwise makes
-- a silent fall-back to the DSL undetectable.
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
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

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
-- is the directory holding the package directories, and 'owner' maps a module to the package that owns it
-- (its routing-map entry): a package's selection is restricted to the modules it owns, a package may not
-- list a module it does not own, and a .hydra file in a package's tree for a module the package does not own is
-- an error (it would otherwise substitute another package's module).
substituteTextualSources :: (ModuleName -> Maybe String) -> FilePath -> [Module] -> IO [Module]
substituteTextualSources owner packagesRoot universeMods = do
  pkgs <- L.sort <$> listDirectory packagesRoot
  let universe = universeNamesOf universeMods
      unionNames = unionTypeNamesOf universeMods
      inUniverse = M.fromList [(moduleName m, m) | m <- universeMods]
  loaded <- fmap concat $ mapM (loadPackageLogged universe unionNames inUniverse) pkgs
  let replacements = M.fromList [(moduleName m, m) | m <- loaded]
  return [M.findWithDefault m (moduleName m) replacements | m <- universeMods]
  where
    hydraFile pkg ns = packagesRoot </> pkg </> "src/main/hydra" </> CodeGeneration.moduleNameToPath ns ++ ".hydra"
    loadPackageLogged universe unionNames inUniverse pkg = do
      flag <- readTextualSources (packagesRoot </> pkg)
      excludes <- readTextualExcludes (packagesRoot </> pkg)
      mods <- loadPackage universe unionNames inUniverse pkg flag excludes
      if flag == TextualNone then return mods else do
        strays <- strayFiles pkg
        when' (not (null strays)) $ fail $ packagesRoot </> pkg </> "package.json: textualSources is declared, but "
          ++ "these .hydra files are for modules the package does not own: " ++ L.intercalate ", " strays
        when' (null mods) $ fail $ packagesRoot </> pkg </> "package.json: textualSources is declared but no module was"
          ++ " substituted (is " ++ packagesRoot ++ " the right packages root?)"
        let nExcluded = length [ns | ns <- M.keys inUniverse, any (`L.isPrefixOf` unModuleName ns) excludes]
        hPutStrLn stderr $ "textual sources: " ++ pkg ++ ": " ++ show (length mods) ++ " modules from .hydra ("
          ++ show nExcluded ++ " excluded) under " ++ packagesRoot
        return mods
    when' c a = if c then a else return ()
    ownedBy pkg ns = owner ns == Just pkg
    -- .hydra files under the package's tree whose module the package does not own (owned by another package or
    -- by none: both would be modules that this package's textualSources must never substitute)
    strayFiles pkg = do
      let root = packagesRoot </> pkg </> "src/main/hydra"
      files <- hydraFilesUnder root
      return [ f | f <- files
                 , owner (ModuleName (L.map slashToDot (dropExt (drop (length root + 1) f)))) /= Just pkg ]
    slashToDot c = if c == '/' then '.' else c
    dropExt f = if ".hydra" `L.isSuffixOf` f then take (length f - length ".hydra") f else f
    hydraFilesUnder dir = do
      isDir <- doesDirectoryExist dir
      if not isDir then return [] else do
        entries <- L.sort <$> listDirectory dir
        fmap concat $ mapM (\e -> do
          let path = dir </> e
          sub <- doesDirectoryExist path
          if sub then hydraFilesUnder path else return [path | ".hydra" `L.isSuffixOf` e]) entries
    loadPackage universe unionNames inUniverse pkg flag excludes = do
      let excluded ns = any (`L.isPrefixOf` unModuleName ns) excludes
      case flag of
        TextualNone -> return []
        TextualMain -> do
          let mainOnly = [ns | ns <- M.keys inUniverse, ownedBy pkg ns, not ("hydra.core.test." `L.isPrefixOf` unModuleName ns), not (excluded ns)]
          present <- filterM' (doesFileExist . hydraFile pkg) mainOnly
          mapM (loadTextualModule universe unionNames . hydraFile pkg) present
        TextualAll -> do
          present <- filterM' (doesFileExist . hydraFile pkg) [ns | ns <- M.keys inUniverse, ownedBy pkg ns, not (excluded ns)]
          mapM (loadTextualModule universe unionNames . hydraFile pkg) present
        TextualOnly nss -> mapM (loadListed universe unionNames inUniverse pkg) nss
    loadListed universe unionNames inUniverse pkg ns = do
      let file = hydraFile pkg ns
      if not (M.member ns inUniverse)
        then fail $ packagesRoot </> pkg </> "package.json: textualSources lists " ++ unModuleName ns ++ ", which is not in the universe"
        else if not (ownedBy pkg ns)
        then fail $ packagesRoot </> pkg </> "package.json: textualSources lists " ++ unModuleName ns ++ ", which is owned by "
          ++ maybe "no package" id (owner ns)
        else do
          exists <- doesFileExist file
          if exists then loadTextualModule universe unionNames file
            else fail $ packagesRoot </> pkg </> "package.json: textualSources lists " ++ unModuleName ns ++ " but " ++ file ++ " does not exist"
    filterM' p = fmap concat . mapM (\x -> (\b -> [x | b]) <$> p x)
