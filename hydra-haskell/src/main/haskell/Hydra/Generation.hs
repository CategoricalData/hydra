-- | Entry point for Hydra code generation utilities

module Hydra.Generation where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Ext.Haskell.Coder
import Hydra.Ext.Haskell.Language
import Hydra.Module (_Module)
import qualified Hydra.Json.Model as Json
import Hydra.Staging.Yaml.Modules
import Hydra.Staging.Yaml.Language
import Hydra.Sources.Libraries
import qualified Hydra.Decoding as Decoding
import qualified Hydra.Encoding as Encoding
import qualified Hydra.Sources.All as Sources
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import qualified Hydra.CodeGeneration as Generated

import qualified Control.Monad as CM
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy as BS
import qualified Data.Scientific as SC
import qualified Data.Vector as V
import Data.Word (Word8)
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified System.Directory as SD
import qualified Data.Maybe as Y


-- | Generate source files and write them to disk.
-- This is a thin I/O wrapper around 'generateSourceFiles'.
generateSources
  :: (Module -> [Definition] -> Flow Graph (M.Map FilePath String))
  -> Language
  -> Bool  -- ^ doInfer
  -> Bool  -- ^ doExpand
  -> Bool  -- ^ doHoistCaseStatements
  -> Bool  -- ^ doHoistPolymorphicLetBindings
  -> FilePath
  -> [Module]  -- ^ Universe
  -> [Module]  -- ^ Modules to generate
  -> IO ()
generateSources printDefinitions lang doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings basePath universeModules modulesToGenerate = do
    mfiles <- runFlow bootstrapGraph $
      Generated.generateSourceFiles printDefinitions lang doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings bootstrapGraph universeModules modulesToGenerate
    case mfiles of
      Nothing -> fail "Failed to generate source files"
      Just files -> mapM_ writePair files
  where
    writePair (path, s) = do
        let fullPath = FP.combine basePath path
        SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
        writeFile fullPath withNewline
      where
        withNewline = if L.isSuffixOf "\n" s then s else s ++ "\n"

-- | Build a graph from a list of modules using the Haskell bootstrapGraph.
-- Thin wrapper around modulesToGraphWith.
modulesToGraph :: [Module] -> [Module] -> Graph
modulesToGraph = Generated.modulesToGraph bootstrapGraph

printTrace :: Bool -> Trace -> IO ()
printTrace isError t = do
  CM.unless (L.null $ traceMessages t) $ do
      putStrLn $ if isError then "Flow failed. Messages:" else "Messages:"
      putStrLn $ indentLines $ traceSummary t

runFlow :: s -> Flow s a -> IO (Maybe a)
runFlow s f = do
    printTrace (Y.isNothing v) t
    return v
  where
    FlowState v _ t = unFlow f s emptyTrace

-- | Generate Haskell source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeHaskell = generateSources moduleToHaskell haskellLanguage True False False False

-- writeJson :: FP.FilePath -> [Module] -> IO ()
-- writeJson = generateSources Json.printModule

-- | YAML generation - only processes data modules (term definitions), skips schema modules
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeYaml :: FP.FilePath -> [Module] -> [Module] -> IO ()
writeYaml basePath universeModules modulesToGenerate = do
    mfiles <- runFlow bootstrapGraph (generateFiles modulesToGenerate)
    case mfiles of
      Nothing -> fail "Failed to generate YAML files"
      Just files -> mapM_ writePair files
  where
    constraints = languageConstraints yamlLanguage
    hasNativeTypes mod = not $ L.null $ L.filter isNativeType $ moduleElements mod

    -- Build the complete universe by computing transitive closure of dependencies
    namespaceMap = M.fromList [(moduleNamespace m, m) | m <- universeModules ++ modulesToGenerate]

    transitiveClosure :: [Module] -> S.Set Namespace
    transitiveClosure startMods = go (S.fromList $ moduleNamespace <$> startMods) S.empty
      where
        go pending visited
          | S.null pending = visited
          | otherwise =
              let newVisited = S.union visited pending
                  nextDeps = S.fromList $ concat
                    [moduleTermDependencies m ++ moduleTypeDependencies m
                    | ns <- S.toList pending
                    , Just m <- [M.lookup ns namespaceMap]]
                  newPending = S.difference nextDeps newVisited
              in go newPending newVisited

    allNeededNamespaces = transitiveClosure modulesToGenerate
    completeUniverse = [m | ns <- S.toList allNeededNamespaces, Just m <- [M.lookup ns namespaceMap]]
                    ++ modulesToGenerate

    generateFiles mods = do
        -- Only process data modules (modules without native types)
        let dataModules = L.filter (not . hasNativeTypes) mods
        if L.null dataModules
          then pure []
          else withTrace "generate YAML files" $ do
            let g0 = modulesToGraph completeUniverse completeUniverse  -- Use complete universe for full dependency resolution
                namespaces = fmap moduleNamespace dataModules
            -- Infer types on the data graph before adaptation (eta expansion requires types)
            g0' <- inferGraphTypes g0
            (g1, defLists) <- dataGraphToDefinitions constraints True True False False g0' namespaces
            withState g1 $ do
              maps <- CM.zipWithM forEachModule dataModules defLists
              return $ L.concat (M.toList <$> maps)
      where
        forEachModule mod defs = withTrace ("data module " ++ unNamespace (moduleNamespace mod)) $
          moduleToYaml mod (fmap DefinitionTerm defs)

    writePair (path, contents) = do
      let fullPath = basePath FP.</> path
      SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
      writeFile fullPath contents

-- | Generate and write the lexicon file (IO wrapper).
writeLexicon :: FilePath -> IO ()
writeLexicon path = do
  mcontent <- runFlow bootstrapGraph
    (Generated.inferAndGenerateLexicon bootstrapGraph Sources.kernelModules)
  case mcontent of
    Nothing -> fail "Lexicon generation failed"
    Just content -> do
      writeFile path content
      putStrLn $ "Lexicon written to " ++ path

-- | Generate the lexicon to the standard location
writeLexiconToStandardPath :: IO ()
writeLexiconToStandardPath = writeLexicon "../docs/hydra-lexicon.txt"

----------------------------------------

-- | IO wrapper for generateCoderModules. Evaluates the Flow and handles errors.
generateCoderModulesIO :: (Module -> Flow Graph (Maybe Module)) -> String -> [Module] -> [Module] -> IO [Module]
generateCoderModulesIO codec label universeModules typeModules = do
    let graph = modulesToGraph universeModules universeModules
    case graphSchema graph of
      Nothing -> fail "No schema graph available"
      Just schemaGraph -> do
        mresult <- runFlow schemaGraph (Generated.generateCoderModules codec bootstrapGraph universeModules typeModules)
        case mresult of
          Nothing -> fail $ "Failed to generate " ++ label ++ " modules"
          Just results -> return results

generateDecoderModules :: [Module] -> [Module] -> IO [Module]
generateDecoderModules = generateCoderModulesIO Decoding.decodeModule "decoder"

generateEncoderModules :: [Module] -> [Module] -> IO [Module]
generateEncoderModules = generateCoderModulesIO Encoding.encodeModule "encoder"

----------------------------------------

-- | Generate encoder/decoder Source modules for a list of type modules.
-- These are Source modules that define `module_` bindings containing the encoder Modules as Terms.
generateCoderSourceModules :: ([Module] -> [Module] -> IO [Module]) -> [Module] -> [Module] -> IO [Module]
generateCoderSourceModules generate universeModules typeModules = do
  sourceMods <- generate universeModules typeModules
  return $ fmap Generated.moduleToSourceModule sourceMods

generateDecoderSourceModules :: [Module] -> [Module] -> IO [Module]
generateDecoderSourceModules = generateCoderSourceModules generateDecoderModules

generateEncoderSourceModules :: [Module] -> [Module] -> IO [Module]
generateEncoderSourceModules = generateCoderSourceModules generateEncoderModules

----------------------------------------

writeCoderSourceHaskell :: ([Module] -> [Module] -> IO [Module]) -> FilePath -> [Module] -> [Module] -> IO ()
writeCoderSourceHaskell generate basePath universeModules typeModules = do
  sourceMods <- generateCoderSourceModules generate universeModules typeModules
  -- The source modules need the Module encoder/decoder and Core types
  writeHaskell basePath (universeModules ++ sourceMods) sourceMods

-- | Write decoder Source modules as Haskell to the given path.
-- These typically go to src/gen-main/haskell/Hydra/Sources/Decode/
writeDecoderSourceHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeDecoderSourceHaskell = writeCoderSourceHaskell generateDecoderModules

-- | Write encoder Source modules as Haskell to the given path.
-- These typically go to src/gen-main/haskell/Hydra/Sources/Encode/
writeEncoderSourceHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeEncoderSourceHaskell = writeCoderSourceHaskell generateEncoderModules

----------------------------------------

-- | Write encoder/decoder modules as Haskell to the given path.
-- First argument: generator function for encoder or decoder modules
-- Second argument: output directory
-- Third argument: universe modules (all modules for type/term resolution)
-- Fourth argument: type modules to generate encoders/decoders for
-- Note: This function bypasses type inference; for efficiency, we generate type signatures directly.
writeCoderHaskell :: ([Module] -> [Module] -> IO [Module]) -> FilePath -> [Module] -> [Module] -> IO ()
writeCoderHaskell generate basePath universeModules typeModules = do
    coderMods <- generate universeModules typeModules
    -- Add core types namespace to each encoder/decoder module's type dependencies
    -- since the encoders/decoders reference hydra.core.Term, hydra.core.Injection, etc.
    let withCoreDeps = fmap addCoreDep coderMods
    writeHaskell basePath universeModules withCoreDeps
  where
    addCoreDep m = m { moduleTypeDependencies = CoreTypes.ns : moduleTypeDependencies m }

writeDecoderHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeDecoderHaskell = writeCoderHaskell generateDecoderModules

writeEncoderHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeEncoderHaskell = writeCoderHaskell generateEncoderModules

----------------------------------------
-- Module Inference
----------------------------------------

-- | IO wrapper for inferModules. Evaluates the Flow and handles errors.
inferModulesIO :: [Module] -> [Module] -> IO [Module]
inferModulesIO universeMods targetMods = do
  let g0 = modulesToGraph universeMods universeMods
  mresult <- runFlow g0 (Generated.inferModules bootstrapGraph universeMods targetMods)
  case mresult of
    Nothing -> fail "Type inference failed on modules"
    Just mods -> return mods

----------------------------------------
-- JSON Module Export
----------------------------------------

-- | Write a single module to a JSON file.
-- The file path is derived from the module namespace.
writeModuleJson :: FilePath -> Module -> IO ()
writeModuleJson basePath mod = do
    case Generated.moduleToJson mod of
      Left err -> fail $ "Failed to convert module to JSON: " ++ unNamespace (moduleNamespace mod) ++ ": " ++ err
      Right jsonStr -> do
        let filePath = basePath FP.</> Generated.namespaceToPath (moduleNamespace mod) ++ ".json"
        SD.createDirectoryIfMissing True $ FP.takeDirectory filePath
        writeFile filePath (jsonStr ++ "\n")
        putStrLn $ "Wrote: " ++ filePath

-- | Write multiple modules to JSON files.
-- Each module is written to basePath/<namespace-path>.json
-- If doInfer is True, type inference is performed on the modules first.
-- The universe modules are used for type inference context (may include more modules
-- than those being written). If not inferring, the universe is ignored.
writeModulesJson :: Bool -> FilePath -> [Module] -> [Module] -> IO ()
writeModulesJson doInfer basePath universeMods mods = do
  mods' <- if doInfer then inferModulesIO universeMods mods else return mods
  mapM_ (writeModuleJson basePath) mods'

----------------------------------------
-- JSON Module Import
----------------------------------------

-- | Convert an Aeson JSON value to a Hydra JSON value.
aesonToHydra :: A.Value -> Json.Value
aesonToHydra v = case v of
  A.Object km -> Json.ValueObject $ M.fromList (mapPair <$> AKM.toList km)
    where
      mapPair (k, v') = (AK.toString k, aesonToHydra v')
  A.Array a -> Json.ValueArray (aesonToHydra <$> V.toList a)
  A.String t -> Json.ValueString $ T.unpack t
  A.Number s -> Json.ValueNumber $ SC.toRealFloat s
  A.Bool b -> Json.ValueBoolean b
  A.Null -> Json.ValueNull

-- | Parse a JSON file using Aeson and convert to Hydra JSON.
-- Pre-processes the content to escape control characters that the Hydra JSON writer
-- doesn't escape (e.g. null bytes in string literals).
parseJsonFile :: FilePath -> IO (Either String Json.Value)
parseJsonFile fp = do
  content <- BS.readFile fp
  let escaped = escapeControlCharsInJson content
  return $ aesonToHydra <$> A.eitherDecode escaped

-- | Escape unescaped control characters (< 0x20) inside JSON string literals.
-- Thin ByteString wrapper around Generated.escapeControlCharsInJson (which operates on [Int]).
escapeControlCharsInJson :: BS.ByteString -> BS.ByteString
escapeControlCharsInJson input =
  BS.pack $ fmap fromIntegral $ Generated.escapeControlCharsInJson $ fmap fromIntegral $ BS.unpack input

-- | Load modules from JSON files.
-- Takes a base path and a list of namespaces to load.
-- The universeModules are used to build the graph for type resolution.
-- When doStripTypeSchemes is True, TypeSchemes are stripped from term bindings
-- because they may contain stale types (e.g., bigfloat) that cause inference
-- errors after adaptation. The inference engine will reconstruct correct TypeSchemes.
-- When False, TypeSchemes are preserved (useful for ext modules that don't need
-- type adaptation and where stripping would cause inference to loop on recursive types).
loadModulesFromJson :: Bool -> FilePath -> [Module] -> [Namespace] -> IO [Module]
loadModulesFromJson doStripTypeSchemes basePath universeModules namespaces = do
  CM.forM namespaces $ \ns -> do
    let filePath = basePath FP.</> Generated.namespaceToPath ns ++ ".json"
    parseResult <- parseJsonFile filePath
    case parseResult of
      Left err -> fail $ "JSON parse error for " ++ unNamespace ns ++ ": " ++ err
      Right jsonVal -> case Generated.decodeModuleFromJson bootstrapGraph universeModules doStripTypeSchemes jsonVal of
        Left err -> fail $ "Module decode error for " ++ unNamespace ns ++ ": " ++ err
        Right mod -> do
          putStrLn $ "  Loaded: " ++ unNamespace ns
          return mod

-- | Discover all JSON module files in a directory and load them.
-- Scans the directory tree for .json files, converts paths to namespaces,
-- then loads all discovered modules.
-- TypeSchemes are stripped by default (suitable for main/kernel modules).
loadAllModulesFromJsonDir :: FilePath -> [Module] -> IO [Module]
loadAllModulesFromJsonDir = loadAllModulesFromJsonDirWith True

-- | Like loadAllModulesFromJsonDir but with control over TypeScheme stripping.
loadAllModulesFromJsonDirWith :: Bool -> FilePath -> [Module] -> IO [Module]
loadAllModulesFromJsonDirWith doStripTypeSchemes basePath universeModules = do
  namespaces <- discoverJsonNamespaces basePath
  putStrLn $ "  Discovered " ++ show (length namespaces) ++ " modules in " ++ basePath
  loadModulesFromJson doStripTypeSchemes basePath universeModules namespaces

-- | Discover namespaces from JSON files in a directory tree.
-- Converts file paths like "hydra/core.json" to Namespace "hydra.core".
discoverJsonNamespaces :: FilePath -> IO [Namespace]
discoverJsonNamespaces basePath = do
  exists <- SD.doesDirectoryExist basePath
  if not exists
    then return []
    else do
      files <- findJsonFiles basePath basePath
      return $ L.sort files
  where
    findJsonFiles root dir = do
      entries <- SD.listDirectory dir
      results <- CM.forM entries $ \entry -> do
        let path = dir FP.</> entry
        isDir <- SD.doesDirectoryExist path
        if isDir
          then findJsonFiles root path
          else if FP.takeExtension entry == ".json"
            then do
              let relPath = FP.makeRelative root path
                  ns = Namespace $ L.intercalate "." $ LS.splitOn "/" $ FP.dropExtension relPath
              return [ns]
            else return []
      return $ L.concat results
