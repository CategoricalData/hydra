-- | Core infrastructure for generating executable test suites from Hydra test specifications

module Hydra.Staging.Testing.Generation.Generate where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Generation
import Hydra.Dsl.Bootstrap (bootstrapGraph)
import Hydra.Staging.Testing.Generation.Transform
import qualified Hydra.Inference as Inference
import Hydra.Sources.All
import Hydra.Sources.Test.All (testModules)

import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Lib.Strings as Strings

import qualified System.Directory as SD
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import Data.Char (isAlphaNum, isUpper, toLower, toUpper)
import Debug.Trace (trace)


-- | Language-agnostic test generator abstraction
-- Parameterized by the namespace/module name type (e.g., H.ModuleName for Haskell)
data TestGenerator a = TestGenerator {
  -- | Build namespaces for a module, resolving all imports and primitives
  testGenNamespacesForModule :: Module -> Graph -> Either String (Namespaces a),

  -- | Create a test codec from resolved namespaces
  testGenCreateCodec :: Namespaces a -> TestCodec,

  -- | Generate a complete test file for a module and test group
  testGenGenerateTestFile :: Module -> TestGroup -> Graph -> Either String (FilePath, String),

  -- | Generate an aggregator file (e.g., Spec.hs for Haskell, conftest.py for Python)
  -- Takes base directory and list of modules, returns (filepath, content) or Nothing if not needed
  testGenAggregatorFile :: Maybe (FilePath -> [Module] -> (FilePath, String))
}


-- | Build namespaces for test group by creating a module with test terms and using the generator's namespacesForModule
buildNamespacesForTestGroup :: TestGenerator a -> Module -> TestGroup -> Graph -> Either String (Namespaces a)
buildNamespacesForTestGroup testGen testModule testGroup g = do
    -- Extract all test case terms
    let testCases = collectTestCases testGroup
        testTerms = concatMap extractTestTerms testCases

        -- Create bindings from test terms so they can be analyzed for dependencies
        testBindings = zipWith (\i term -> Binding (Name $ "_test_" ++ show i) term Nothing) ([0..] :: [Integer]) testTerms

        -- Create a temporary module with test terms as bindings
        tempModule = testModule { moduleElements = testBindings }

    -- Use the language-specific namespacesForModule which handles primitives correctly
    testGenNamespacesForModule testGen tempModule g
  where
    extractTestTerms (TestCaseWithMetadata _ tcase _ _) = case tcase of
      TestCaseDelegatedEvaluation (DelegatedEvaluationTestCase input output) -> [input, output]
      _ -> []

-- | Build a mapping from module namespaces to test groups by matching on derived keys.
-- This handles the case where submodules and subgroups may be in different orders
-- (e.g., due to code generation reordering).
-- Note: subModules must be provided explicitly as namespaces since Module dependencies are now just Namespaces.
buildTestGroupMap :: [Namespace] -> TestGroup -> M.Map Namespace TestGroup
buildTestGroupMap subModuleNamespaces rootTestGroup =
  let subGroups = testGroupSubgroups rootTestGroup
      -- Build a map from test group name to test group for lookup
      groupByName = M.fromList [(testGroupName g, g) | g <- subGroups]
      -- Match each module namespace to its test group by deriving the expected test group name
      pairs = [(ns, group) |
               ns <- subModuleNamespaces,
               let expectedName = deriveTestGroupName ns,
               Just group <- [M.lookup expectedName groupByName]]
  in M.fromList pairs
  where
    -- Derive the test group name from a module namespace
    -- Handles various patterns:
    -- e.g., "hydra.test.lib.chars" -> "hydra.lib.chars primitives"
    -- e.g., "hydra.test.formatting" -> "formatting"
    -- e.g., "hydra.test.checking.all" -> "checking"
    -- e.g., "hydra.test.etaExpansion" -> "eta expansion"
    -- e.g., "hydra.test.json.parser" -> "JSON parser"
    deriveTestGroupName (Namespace ns) =
      let parts = Strings.splitOn "." ns
          -- Remove "hydra.test." prefix and handle different patterns
          withoutPrefix = drop 2 parts  -- drop "hydra" and "test"
      in case withoutPrefix of
        ("lib":rest) -> "hydra.lib." ++ L.intercalate "." rest ++ " primitives"
        -- JSON special cases
        ["json", "coder"] -> "JSON coder"
        ["json", "parser"] -> "JSON parsing"
        ["json", "writer"] -> "JSON serialization"
        -- Handle camelCase conversion (e.g., etaExpansion -> eta expansion)
        [name] -> decamelize name
        -- Handle .all suffix (e.g., checking.all -> checking, inference.all -> inference)
        parts' | not (null parts') && last parts' == "all" -> L.intercalate "." (init parts')
        _ -> L.intercalate "." withoutPrefix

    -- Convert camelCase to space-separated lowercase
    decamelize s = map toLower $ L.intercalate " " $ splitCamelCase s

    -- Split a camelCase string into words
    splitCamelCase [] = []
    splitCamelCase (c:cs) =
      let (word, rest) = span (not . isUpper) cs
      in (c:word) : splitCamelCase rest

-- | Create a lookup function from a test group hierarchy
-- This walks the test group and module hierarchies to build the mapping
-- Note: subModuleNamespaces must be provided explicitly since Module dependencies are now Namespaces
createTestGroupLookup :: [Namespace] -> TestGroup -> (Namespace -> Maybe TestGroup)
createTestGroupLookup subModuleNamespaces rootTestGroup =
  let testGroupMap = buildTestGroupMap subModuleNamespaces rootTestGroup
  in \ns -> M.lookup ns testGroupMap

-- | Main entry point: generate generation test suite from test modules
-- Takes a test generator, output directory, test modules, and a lookup function for test groups
-- Note: testModules must be provided explicitly since Module dependencies are now Namespaces
generateGenerationTestSuite :: TestGenerator a -> FilePath -> [Module] -> (Namespace -> Maybe TestGroup) -> IO Bool
generateGenerationTestSuite testGen outDir modules lookupTestGroup = do
  putStrLn "Processing test modules..."

  putStrLn $ "Found " ++ show (length modules) ++ " test module(s)"
  putStrLn "Transforming test suite to generation tests..."

  -- Match modules with their test groups and transform
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

      let graph = modulesToGraph (mainModules ++ testModules) $ modules ++ extraModules

      -- Perform type inference ONCE upfront for the entire graph
      putStrLn "Starting type inference..."
      let cx0 = Context [] [] M.empty
      case Inference.inferGraphTypes cx0 (graphToBindings graph) graph of
        Left ic -> do
          putStrLn $ "✗ Type inference failed: " ++ showError (inContextObject ic)
          return False
        Right ((g, _inferredBindings), _cx') -> do
          putStrLn $ "Type inference complete. Generating " ++ show (length moduleTestPairs) ++ " module(s)..."

          -- Generate files one at a time, writing each immediately
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
    -- Core.module_ is required for schema types like Either, Maybe, etc.
    -- Lexical.module_ is required for hydra.lexical.emptyGraph
    extraModules = [Formatting.module_, Lexical.module_, Core.module_]

-- | Generate all test files using the provided test generator and write them
generateAllFiles :: TestGenerator a -> Graph -> FilePath -> [(Module, TestGroup)] -> ((FilePath, String) -> IO ()) -> IO (Either String Int)
generateAllFiles testGen g baseDir modulePairs writeFile = go 1 modulePairs
  where
    go _ [] = do
      -- Generate an aggregator file if the generator provides one
      case testGenAggregatorFile testGen of
        Just genAggregator -> do
          let aggregator = genAggregator baseDir (map fst modulePairs)
          writeFile aggregator
          return $ Right (length modulePairs + 1)
        Nothing -> return $ Right (length modulePairs)
    go idx ((sourceModule, testGroup):rest) = do
      let ns = moduleNamespace sourceModule
          generationModule = sourceModule {moduleNamespace = addGenerationPrefix ns}
      trace ("  Generating module " ++ show idx ++ ": " ++ show ns) $ return ()
      case testGenGenerateTestFile testGen generationModule testGroup g of
        Left err -> return $ Left err
        Right (filePath, content) -> do
          let fullPath = FP.combine baseDir filePath
          writeFile (fullPath, content)
          go (idx + 1) rest

-- | Generate an aggregator spec file that imports all generated test modules
generateAggregatorSpec :: FilePath -> [Module] -> (FilePath, String)
generateAggregatorSpec baseDir modules =
  let addSpecSuffix (Namespace ns) = Namespace (ns ++ "Spec")
      modulePaths = map (namespaceToModuleName . addSpecSuffix . addGenerationPrefix . moduleNamespace) modules
      imports = L.intercalate "\n" $ map (\m -> "import qualified " ++ m ++ " as " ++ sanitizeModuleName m) modulePaths
      specs = L.intercalate "\n    " $ map (\m -> sanitizeModuleName m ++ ".spec") modulePaths
      content = unlines [
        "-- Note: this is an automatically generated file. Do not edit.",
        "",
        "module Generation.Spec (spec) where",
        "",
        "import qualified Test.Hspec as H",
        imports,
        "",
        "spec :: H.Spec",
        "spec = do",
        "    " ++ specs
        ]
      filePath = FP.combine baseDir "Generation/Spec.hs"
  in (filePath, content)
  where
    sanitizeModuleName = map (\c -> if c == '.' then '_' else c)
    namespaceToModuleName (Namespace ns) =
      L.intercalate "." $ L.map capitalize (L.filter (not . null) $ Strings.splitOn "." ns)
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs
