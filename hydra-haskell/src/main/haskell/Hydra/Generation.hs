-- | Entry point for Hydra code generation utilities

module Hydra.Generation (
  module Hydra.Generation,
  TestGenerator(..),
) where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Ext.Haskell.Coder
import Hydra.Ext.Haskell.Language
import Hydra.Packaging (_Module)
import Hydra.Testing (TestGroup(..), TestCaseWithMetadata(..), TestCase(..))
import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Writer as JsonWriter
import Hydra.Sources.Libraries
import qualified Hydra.Context as Context
import qualified Hydra.Decoding as Decoding
import qualified Hydra.Dsls as Dsls
import qualified Hydra.Encoding as Encoding
import qualified Hydra.Errors as Error
import qualified Hydra.Show.Errors as ShowError
import qualified Hydra.Sources.All as Sources
import qualified Hydra.Sources.Eval.Lib.All as EvalLib
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import qualified Hydra.Codegen as CodeGeneration
import Hydra.Test.Transform (collectTestCases, transformToCompiledTests)
import Hydra.Sources.Test.All (testModules)
import qualified Hydra.Inference as Inference

import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical

import qualified Control.Monad as CM
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy as BS
import qualified Data.Scientific as SC
import qualified Data.Vector as V
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.Directory as SD
import qualified Data.Maybe as Y
import Data.Char (isAlphaNum, isUpper, toLower, toUpper)
import Debug.Trace (trace)
import qualified Hydra.Lib.Strings as Strings


-- | A test generator for a specific target language
data TestGenerator a = TestGenerator {
  testGeneratorNamespacesForModule :: Module -> Graph -> Either String (Namespaces a),
  testGeneratorGenerateTestFile :: Module -> TestGroup -> Graph -> Either String (String, String),
  testGeneratorAggregatorFile :: Maybe (FilePath -> [Module] -> (FilePath, String))
}

-- | Format an InContext Error with trace information
formatError :: Context.InContext Error.Error -> String
formatError ic = showError (Context.inContextObject ic) ++ traceInfo
  where
    cx = Context.inContextContext ic
    stack = Context.contextTrace cx
    traceInfo = if L.null stack then "" else " (" ++ L.intercalate " > " (reverse stack) ++ ")"

showError :: Error.Error -> String
showError = ShowError.error

-- | Generate source files and write them to disk.
-- This is a thin I/O wrapper around 'generateSourceFiles'.
generateSources
  :: (Module -> [Definition] -> Context.Context -> Graph -> Either (Context.InContext Error.Error) (M.Map FilePath String))
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
    let cx = Context.Context [] [] M.empty
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
        cleaned = unlines $ map stripTrailingWhitespace $ lines s
        stripTrailingWhitespace line = reverse $ dropWhile (== ' ') $ reverse line
        withNewline = if L.isSuffixOf "\n" cleaned then cleaned else cleaned ++ "\n"

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

-- | Generate and write the lexicon file (IO wrapper).
writeLexicon :: FilePath -> IO ()
writeLexicon path = do
  case CodeGeneration.inferAndGenerateLexicon (Context [] [] M.empty) bootstrapGraph Sources.kernelModules of
    Left err -> fail $ "Lexicon generation failed: " ++ err
    Right content -> do
      writeFile path content
      putStrLn $ "Lexicon written to " ++ path

-- | Generate the lexicon to the standard location
writeLexiconToStandardPath :: IO ()
writeLexiconToStandardPath = writeLexicon "../docs/hydra-lexicon.txt"

----------------------------------------

-- | IO wrapper for generateCoderModules. Evaluates the Either and handles errors.
generateCoderModulesIO :: (Context.Context -> Graph -> Module -> Either (Context.InContext Error.Error) (Maybe Module)) -> String -> [Module] -> [Module] -> IO [Module]
generateCoderModulesIO codec label universeModules typeModules = do
    let cx = Context.Context [] [] M.empty
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
-- DSL Module Generation
----------------------------------------

-- | Write the hydra.dsls source module (the DSL generator itself) to Haskell.
-- The Dsls module is NOT included in the universe to avoid infinite recursion
-- during graph construction (its terms reference decoders that reference types).
writeDslSourceHaskell :: FilePath -> IO ()
writeDslSourceHaskell basePath = do
    _ <- writeHaskell basePath Sources.mainModules Sources.dslSourceModules
    return ()

generateDslModules :: [Module] -> [Module] -> IO [Module]
generateDslModules = generateCoderModulesIO Dsls.dslModule "DSL"

-- | Write DSL modules with doInfer=False. All bindings are fully typed.
writeDslHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeDslHaskell basePath universeModules typeModules = do
    dslMods <- generateDslModules universeModules typeModules
    let nonEmpty = filter (not . null . moduleDefinitions) dslMods
    let withCoreDeps = fmap addCoreDep nonEmpty
    _ <- generateSources moduleToHaskell haskellLanguage False False False False basePath universeModules withCoreDeps
    return ()
  where
    addCoreDep m = m { moduleTypeDependencies = CoreTypes.ns : moduleTypeDependencies m }

----------------------------------------
-- Module Inference
----------------------------------------

-- | IO wrapper for inferModules. Evaluates the Either and handles errors.
inferModulesIO :: [Module] -> [Module] -> IO [Module]
inferModulesIO universeMods targetMods = do
  case CodeGeneration.inferModules (Context [] [] M.empty) bootstrapGraph universeMods targetMods of
    Left ic -> fail $ "Type inference failed: " ++ formatError ic
    Right mods -> return mods

----------------------------------------
-- JSON Module Export
----------------------------------------

-- | Build a schema map (Name -> Type) from a graph's schema types.
-- Used by the JSON encoder/decoder to resolve type variables.
buildSchemaMap :: Graph -> M.Map Name Type
buildSchemaMap g = M.map extractType (graphSchemaTypes g)
  where
    extractType (TypeScheme _ t _) = stripTop t
    stripTop (TypeAnnotated (AnnotatedType t _)) = stripTop t
    stripTop t = t

-- | Write a single module to a JSON file.
-- The file path is derived from the module namespace.
writeModuleJson :: M.Map Name Type -> FilePath -> Module -> IO ()
writeModuleJson schemaMap basePath mod = do
    case CodeGeneration.moduleToJson schemaMap mod of
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
  let graph = modulesToGraph universeMods universeMods
      schemaMap = buildSchemaMap graph
  mapM_ (writeModuleJson schemaMap basePath) mods'

-- | Write DSL modules to JSON files.
writeDslJson :: FilePath -> [Module] -> [Module] -> IO ()
writeDslJson basePath universeModules typeModules = do
    dslMods <- generateDslModules universeModules typeModules
    let nonEmpty = filter (not . null . moduleDefinitions) dslMods
    writeModulesJson False basePath universeModules nonEmpty

-- | Write a manifest.json listing module namespaces for kernelModules, mainModules, and testModules.
-- This allows Java and Python hosts to load the correct set of modules without directory scanning.
writeManifestJson :: FilePath -> IO ()
writeManifestJson basePath = do
    dslMods <- generateDslModules Sources.mainModules Sources.kernelTypesModules
    let nonEmptyDsls = filter (not . null . moduleDefinitions) dslMods
    let jsonVal = Json.ValueObject $ M.fromList [
            ("dslModules", namespacesJson nonEmptyDsls),
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
-- Thin ByteString wrapper around CodeGeneration.escapeControlCharsInJson (which operates on [Int]).
escapeControlCharsInJson :: BS.ByteString -> BS.ByteString
escapeControlCharsInJson input =
  BS.pack $ fmap fromIntegral $ CodeGeneration.escapeControlCharsInJson $ fmap fromIntegral $ BS.unpack input

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

-- | Read a manifest field, trying a primary name first and falling back to an alternative.
readManifestFieldWithFallback :: FilePath -> String -> String -> IO [Namespace]
readManifestFieldWithFallback basePath primaryField fallbackField = do
    let manifestPath = basePath FP.</> "manifest.json"
    parseResult <- parseJsonFile manifestPath
    case parseResult of
      Left err -> fail $ "Failed to parse manifest.json: " ++ err
      Right jsonVal -> case jsonVal of
        Json.ValueObject obj -> case M.lookup primaryField obj of
          Just (Json.ValueArray arr) -> return $ fmap toNamespace arr
          _ -> case M.lookup fallbackField obj of
            Just (Json.ValueArray arr) -> return $ fmap toNamespace arr
            Nothing -> fail $ "manifest.json missing fields: " ++ primaryField ++ " and " ++ fallbackField
            Just _ -> fail $ "manifest.json field " ++ fallbackField ++ " is not an array"
        _ -> fail "manifest.json is not a JSON object"
  where
    toNamespace (Json.ValueString s) = Namespace s
    toNamespace _ = error $ "manifest.json: expected string in " ++ primaryField ++ "/" ++ fallbackField

-- | Load modules from JSON files for a list of namespaces.
-- Uses the universe modules to build the graph for type resolution.
loadModulesFromJson :: FilePath -> [Module] -> [Namespace] -> IO [Module]
loadModulesFromJson basePath universeModules namespaces = do
    CM.forM namespaces $ \ns -> do
      let filePath = basePath FP.</> CodeGeneration.namespaceToPath ns ++ ".json"
      parseResult <- parseJsonFile filePath
      case parseResult of
        Left err -> fail $ "JSON parse error for " ++ unNamespace ns ++ ": " ++ err
        Right jsonVal -> case CodeGeneration.decodeModuleFromJson bootstrapGraph universeModules jsonVal of
          Left err -> fail $ "Module decode error for " ++ unNamespace ns ++ ": " ++ err
          Right mod -> do
            putStrLn $ "  Loaded: " ++ unNamespace ns
            return mod

----------------------------------------
-- Test Generation
----------------------------------------

-- | Build namespaces for test group by creating a module with test terms and using the generator's namespacesForModule
buildNamespacesForTestGroup :: TestGenerator a -> Module -> TestGroup -> Graph -> Either String (Namespaces a)
buildNamespacesForTestGroup testGen testModule testGroup g = do
    let testCases = collectTestCases testGroup
        testTerms = concatMap extractTestTerms testCases
        testBindings = zipWith (\i term -> Binding (Name $ "_test_" ++ show i) term Nothing) ([0..] :: [Integer]) testTerms
        tempModule = testModule { moduleDefinitions = map bindingToDefinition testBindings }
    testGeneratorNamespacesForModule testGen tempModule g
  where
    extractTestTerms (TestCaseWithMetadata _ _tcase _ _) = []

-- | Build a mapping from module namespaces to test groups by matching on derived keys.
buildTestGroupMap :: [Namespace] -> TestGroup -> M.Map Namespace TestGroup
buildTestGroupMap subModuleNamespaces rootTestGroup =
  let subGroups = testGroupSubgroups rootTestGroup
      groupByName = M.fromList [(testGroupName g, g) | g <- subGroups]
      pairs = [(ns, group) |
               ns <- subModuleNamespaces,
               let expectedName = deriveTestGroupName ns,
               Just group <- [M.lookup expectedName groupByName]]
  in M.fromList pairs
  where
    deriveTestGroupName (Namespace ns) =
      let parts = Strings.splitOn "." ns
          withoutPrefix = drop 2 parts
      in case withoutPrefix of
        ("lib":rest) -> "hydra.lib." ++ L.intercalate "." rest ++ " primitives"
        ["json", "coder"] -> "JSON coder"
        ["json", "parser"] -> "JSON parsing"
        ["json", "writer"] -> "JSON serialization"
        [name] -> decamelize name
        parts' | not (null parts') && last parts' == "all" -> L.intercalate "." (init parts')
        _ -> L.intercalate "." withoutPrefix

    decamelize s = map toLower $ L.intercalate " " $ splitCamelCase s

    splitCamelCase [] = []
    splitCamelCase (c:cs) =
      let (word, rest) = span (not . isUpper) cs
      in (c:word) : splitCamelCase rest

-- | Create a lookup function from a test group hierarchy
createTestGroupLookup :: [Namespace] -> TestGroup -> (Namespace -> Maybe TestGroup)
createTestGroupLookup subModuleNamespaces rootTestGroup =
  let testGroupMap = buildTestGroupMap subModuleNamespaces rootTestGroup
  in \ns -> M.lookup ns testGroupMap

-- | Main entry point: generate generation test suite from test modules
generateGenerationTestSuite :: TestGenerator a -> FilePath -> [Module] -> (Namespace -> Maybe TestGroup) -> IO Bool
generateGenerationTestSuite testGen outDir modules lookupTestGroup = do
  putStrLn "Processing test modules..."

  putStrLn $ "Found " ++ show (length modules) ++ " test module(s)"
  putStrLn "Transforming test suite to generation tests..."

  let moduleTestPairs = [(mod, transformed) |
        mod <- modules,
        Just testGroup <- [lookupTestGroup (moduleNamespace mod)],
        Just transformed <- [transformToCompiledTests testGroup]]

  if null moduleTestPairs
    then do
      putStrLn "No generation tests to generate"
      return True
    else do
      putStrLn $ "Found " ++ show (length moduleTestPairs) ++ " module(s) with generation tests, generating to " ++ outDir

      let graph = modulesToGraph (Sources.mainModules ++ testModules) $ modules ++ extraModules

      putStrLn "Starting type inference..."
      let cx0 = Context [] [] M.empty
      case Inference.inferGraphTypes cx0 (graphToBindings graph) graph of
        Left ic -> do
          putStrLn $ "✗ Type inference failed: " ++ showError (inContextObject ic)
          return False
        Right ((g, _inferredBindings), _cx') -> do
          putStrLn $ "Type inference complete. Generating " ++ show (length moduleTestPairs) ++ " module(s)..."

          result <- generateAllFiles testGen g outDir moduleTestPairs writeFilePair

          case result of
            Left err -> do
              putStrLn $ "✗ Generation failed: " ++ err
              return False
            Right count -> do
              putStrLn $ "✓ Successfully generated " ++ show count ++ " test file(s)"
              return True
  where
    writeFilePair (fullPath, content) = do
      SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
      writeFile fullPath content
      putStrLn $ "  Generated: " ++ fullPath
    extraModules = [Formatting.module_, Lexical.module_, CoreTypes.module_]

-- | Generate all test files using the provided test generator and write them
generateAllFiles :: TestGenerator a -> Graph -> FilePath -> [(Module, TestGroup)] -> ((FilePath, String) -> IO ()) -> IO (Either String Int)
generateAllFiles testGen g baseDir modulePairs writeFile' = go 1 modulePairs
  where
    go _ [] = do
      case testGeneratorAggregatorFile testGen of
        Just genAggregator -> do
          let aggregator = genAggregator baseDir (map fst modulePairs)
          writeFile' aggregator
          return $ Right (length modulePairs + 1)
        Nothing -> return $ Right (length modulePairs)
    go idx ((sourceModule, testGroup):rest) = do
      let ns = moduleNamespace sourceModule
          generationModule = sourceModule {moduleNamespace = Namespace ("generation." ++ unNamespace ns)}
      trace ("  Generating module " ++ show idx ++ ": " ++ show ns) $ return ()
      case testGeneratorGenerateTestFile testGen generationModule testGroup g of
        Left err -> return $ Left err
        Right (filePath, content) -> do
          let fullPath = FP.combine baseDir filePath
          writeFile' (fullPath, content)
          go (idx + 1) rest
