-- | Entry point for Hydra code generation utilities

module Hydra.Generation where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Ext.Haskell.Coder
import Hydra.Ext.Haskell.Language
import Hydra.Module (_Module)
import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Parser as JsonParser
import qualified Hydra.Json.Writer as JsonWriter
import Hydra.Parsing (ParseResult(..), ParseSuccess(..), ParseError(..))
import Hydra.Staging.Yaml.Modules
import Hydra.Staging.Yaml.Language
import Hydra.Sources.Libraries
import qualified Hydra.Context as Context
import qualified Hydra.Decoding as Decoding
import qualified Hydra.Encoding as Encoding
import qualified Hydra.Error as Error
import qualified Hydra.Monads as Monads
import qualified Hydra.Sources.All as Sources
import qualified Hydra.Sources.Eval.Lib.All as EvalLib
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import qualified Hydra.CodeGeneration as CodeGeneration

import qualified Control.Monad as CM
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified System.Directory as SD
import qualified Data.Maybe as Y


-- | Format an InContext OtherError with trace information
formatError :: Context.InContext Error.OtherError -> String
formatError ic = formatError ic ++ traceInfo
  where
    cx = Context.inContextContext ic
    stack = Context.contextTrace cx
    traceInfo = if L.null stack then "" else " (" ++ L.intercalate " > " (reverse stack) ++ ")"

-- | Generate source files and write them to disk.
-- This is a thin I/O wrapper around 'generateSourceFiles'.
generateSources
  :: (Module -> [Definition] -> Context.Context -> Graph -> Either (Context.InContext Error.OtherError) (M.Map FilePath String))
  -> Language
  -> Bool  -- ^ doInfer
  -> Bool  -- ^ doExpand
  -> Bool  -- ^ doHoistCaseStatements
  -> Bool  -- ^ doHoistPolymorphicLetBindings
  -> FilePath
  -> [Module]  -- ^ Universe
  -> [Module]  -- ^ Modules to generate
  -> IO Int  -- ^ Number of files written
generateSources printDefinitions lang doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings basePath universeModules modulesToGenerate = do
    let cx = Monads.emptyContext
    case CodeGeneration.generateSourceFiles printDefinitions lang doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings bootstrapGraph universeModules modulesToGenerate cx of
      Left ic -> fail $ "Failed to generate source files: " ++ formatError ic
      Right files -> do
        mapM_ writePair files
        return $ length files
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
modulesToGraph = CodeGeneration.modulesToGraph bootstrapGraph


-- | Generate Haskell source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeHaskell :: FilePath -> [Module] -> [Module] -> IO Int
writeHaskell = generateSources moduleToHaskell haskellLanguage True False False False

-- writeJson :: FP.FilePath -> [Module] -> IO ()
-- writeJson = generateSources Json.printModule

-- | YAML generation - only processes data modules (term definitions), skips schema modules
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeYaml :: FP.FilePath -> [Module] -> [Module] -> IO ()
writeYaml basePath universeModules modulesToGenerate =
    case generateFiles modulesToGenerate of
      Left ic -> fail $ "Failed to generate YAML files: " ++ formatError ic
      Right files -> mapM_ writePair files
  where
    cx = Monads.emptyContext
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

    generateFiles mods =
        let dataModules = L.filter (not . hasNativeTypes) mods
        in if L.null dataModules
          then Right []
          else do
            let g0 = modulesToGraph completeUniverse completeUniverse
                namespaces = fmap moduleNamespace dataModules
                dataElements = L.filter (not . isNativeType) $ L.concatMap moduleElements completeUniverse
            -- Infer types on the data graph before adaptation (eta expansion requires types)
            ((g0', _inferredBindings), _cx1) <- inferGraphTypes cx dataElements g0
            (g1, defLists) <- wrapStringError $ dataGraphToDefinitions constraints True True False False dataElements g0' namespaces cx
            L.concat . fmap M.toList <$> CM.zipWithM (forEachModule g1) dataModules defLists

    forEachModule g1 mod defs = moduleToYaml mod (fmap DefinitionTerm defs) cx g1

    wrapStringError :: Either String a -> Either (Context.InContext Error.OtherError) a
    wrapStringError (Left err) = Left $ Context.InContext (Error.OtherError err) cx
    wrapStringError (Right a) = Right a

    writePair (path, contents) = do
      let fullPath = basePath FP.</> path
      SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
      writeFile fullPath contents

-- | Generate and write the lexicon file (IO wrapper).
writeLexicon :: FilePath -> IO ()
writeLexicon path = do
  case CodeGeneration.inferAndGenerateLexicon Monads.emptyContext bootstrapGraph Sources.kernelModules of
    Left err -> fail $ "Lexicon generation failed: " ++ err
    Right content -> do
      writeFile path content
      putStrLn $ "Lexicon written to " ++ path

-- | Generate the lexicon to the standard location
writeLexiconToStandardPath :: IO ()
writeLexiconToStandardPath = writeLexicon "../docs/hydra-lexicon.txt"

----------------------------------------

-- | IO wrapper for generateCoderModules. Evaluates the Either and handles errors.
generateCoderModulesIO :: (Context.Context -> Graph -> Module -> Either (Context.InContext Error.OtherError) (Maybe Module)) -> String -> [Module] -> [Module] -> IO [Module]
generateCoderModulesIO codec label universeModules typeModules = do
    let cx = Monads.emptyContext
    case CodeGeneration.generateCoderModules codec bootstrapGraph universeModules typeModules cx of
      Left ic -> fail $ "Failed to generate " ++ label ++ " modules: " ++ formatError ic
      Right results -> return results

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
  return $ fmap CodeGeneration.moduleToSourceModule sourceMods

generateDecoderSourceModules :: [Module] -> [Module] -> IO [Module]
generateDecoderSourceModules = generateCoderSourceModules generateDecoderModules

generateEncoderSourceModules :: [Module] -> [Module] -> IO [Module]
generateEncoderSourceModules = generateCoderSourceModules generateEncoderModules

----------------------------------------

writeCoderSourceHaskell :: ([Module] -> [Module] -> IO [Module]) -> FilePath -> [Module] -> [Module] -> IO ()
writeCoderSourceHaskell generate basePath universeModules typeModules = do
  sourceMods <- generateCoderSourceModules generate universeModules typeModules
  -- The source modules need the Module encoder/decoder and Core types
  _ <- writeHaskell basePath (universeModules ++ sourceMods) sourceMods
  return ()

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
    _ <- writeHaskell basePath universeModules withCoreDeps
    return ()
  where
    addCoreDep m = m { moduleTypeDependencies = CoreTypes.ns : moduleTypeDependencies m }

writeDecoderHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeDecoderHaskell = writeCoderHaskell generateDecoderModules

writeEncoderHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeEncoderHaskell = writeCoderHaskell generateEncoderModules

----------------------------------------
-- Module Inference
----------------------------------------

-- | IO wrapper for inferModules. Evaluates the Either and handles errors.
inferModulesIO :: [Module] -> [Module] -> IO [Module]
inferModulesIO universeMods targetMods = do
  case CodeGeneration.inferModules Monads.emptyContext bootstrapGraph universeMods targetMods of
    Left ic -> fail $ "Type inference failed: " ++ formatError ic
    Right mods -> return mods

----------------------------------------
-- JSON Module Export
----------------------------------------

-- | Write a single module to a JSON file.
-- The file path is derived from the module namespace.
writeModuleJson :: FilePath -> Module -> IO ()
writeModuleJson basePath mod = do
    case CodeGeneration.moduleToJson mod of
      Left err -> fail $ "Failed to convert module to JSON: " ++ unNamespace (moduleNamespace mod) ++ ": " ++ err
      Right jsonStr -> do
        let filePath = basePath FP.</> CodeGeneration.namespaceToPath (moduleNamespace mod) ++ ".json"
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

-- | Write a manifest.json listing module namespaces for kernelModules, mainModules, and testModules.
-- This allows Java and Python hosts to load the correct set of modules without directory scanning.
writeManifestJson :: FilePath -> IO ()
writeManifestJson basePath = do
    let jsonVal = Json.ValueObject $ M.fromList [
            ("evalLibModules", namespacesJson EvalLib.evalLibModules),
            ("kernelModules", namespacesJson Sources.kernelModules),
            ("mainModules", namespacesJson Sources.mainModules),
            ("testModules", namespacesJson Sources.testModules)]
        jsonStr = JsonWriter.printJson jsonVal
        filePath = basePath FP.</> "manifest.json"
    writeFile filePath (jsonStr ++ "\n")
    putStrLn $ "Wrote manifest: " ++ filePath
  where
    namespacesJson mods = Json.ValueArray $ fmap (Json.ValueString . unNamespace . moduleNamespace) mods

----------------------------------------
-- JSON Module Import
----------------------------------------

-- | Parse a JSON file using the native Hydra JSON parser.
-- Pre-processes the content to escape control characters that the Hydra JSON writer
-- doesn't escape (e.g. null bytes in string literals).
parseJsonFile :: FilePath -> IO (Either String Json.Value)
parseJsonFile fp = do
  content <- readFile fp
  let escaped = escapeControlCharsInJson content
  return $ case JsonParser.parseJson escaped of
    ParseResultSuccess success -> Right (parseSuccessValue success)
    ParseResultFailure err -> Left $ parseErrorMessage err

-- | Escape unescaped control characters (< 0x20) inside JSON string literals.
-- Thin String wrapper around CodeGeneration.escapeControlCharsInJson (which operates on [Int]).
escapeControlCharsInJson :: String -> String
escapeControlCharsInJson input =
  fmap (toEnum . fromIntegral) $ CodeGeneration.escapeControlCharsInJson $ fmap (fromIntegral . fromEnum) input

-- | Read a field from manifest.json as a list of Namespaces.
readManifestField :: FilePath -> String -> IO [Namespace]
readManifestField basePath fieldName = do
    let manifestPath = basePath FP.</> "manifest.json"
    parseResult <- parseJsonFile manifestPath
    case parseResult of
      Left err -> fail $ "Failed to parse manifest.json: " ++ err
      Right jsonVal -> case jsonVal of
        Json.ValueObject obj -> case M.lookup fieldName obj of
          Nothing -> fail $ "manifest.json missing field: " ++ fieldName
          Just (Json.ValueArray arr) -> return $ fmap toNamespace arr
          Just _ -> fail $ "manifest.json field " ++ fieldName ++ " is not an array"
        _ -> fail "manifest.json is not a JSON object"
  where
    toNamespace (Json.ValueString s) = Namespace s
    toNamespace _ = error $ "manifest.json: expected string in " ++ fieldName

-- | Load modules from JSON files for a list of namespaces.
-- Uses the universe modules to build the graph for type resolution.
-- When doStripTypeSchemes is True, TypeSchemes are stripped from term bindings.
loadModulesFromJson :: Bool -> FilePath -> [Module] -> [Namespace] -> IO [Module]
loadModulesFromJson doStripTypeSchemes basePath universeModules namespaces = do
    CM.forM namespaces $ \ns -> do
      let filePath = basePath FP.</> CodeGeneration.namespaceToPath ns ++ ".json"
      parseResult <- parseJsonFile filePath
      case parseResult of
        Left err -> fail $ "JSON parse error for " ++ unNamespace ns ++ ": " ++ err
        Right jsonVal -> case CodeGeneration.decodeModuleFromJson bootstrapGraph universeModules doStripTypeSchemes jsonVal of
          Left err -> fail $ "Module decode error for " ++ unNamespace ns ++ ": " ++ err
          Right mod -> do
            putStrLn $ "  Loaded: " ++ unNamespace ns
            return mod
