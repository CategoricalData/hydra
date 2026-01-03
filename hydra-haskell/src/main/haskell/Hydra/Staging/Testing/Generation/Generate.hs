module Hydra.Staging.Testing.Generation.Generate where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Generation
import Hydra.Dsl.Bootstrap (bootstrapGraph)
import Hydra.Staging.Testing.Generation.Transform
import qualified Hydra.Inference as Inference
import Hydra.Sources.All

import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Lib.Strings as Strings

import qualified System.Directory as SD
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import Data.Char (isAlphaNum, isUpper, toLower, toUpper)
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)


-- | Language-agnostic test generator abstraction
-- Parameterized by the namespace/module name type (e.g., H.ModuleName for Haskell)
data TestGenerator a = TestGenerator {
  -- | Build namespaces for a module, resolving all imports and primitives
  testGenNamespacesForModule :: Module -> Flow Graph (Namespaces a),

  -- | Create a test codec from resolved namespaces
  testGenCreateCodec :: Namespaces a -> TestCodec,

  -- | Generate a complete test file for a module and test group
  testGenGenerateTestFile :: Module -> TestGroup -> Flow Graph (FilePath, String),

  -- | Generate an aggregator file (e.g., Spec.hs for Haskell, conftest.py for Python)
  -- Takes base directory and list of modules, returns (filepath, content) or Nothing if not needed
  testGenAggregatorFile :: Maybe (FilePath -> [Module] -> (FilePath, String))
}


-- | Build namespaces for test group by creating a module with test terms and using the generator's namespacesForModule
buildNamespacesForTestGroup :: TestGenerator a -> Module -> TestGroup -> Flow Graph (Namespaces a)
buildNamespacesForTestGroup testGen testModule testGroup = do
    -- Extract all test case terms
    let testCases = collectTestCases testGroup
        testTerms = concatMap extractTestTerms testCases

        -- Create bindings from test terms so they can be analyzed for dependencies
        testBindings = zipWith (\i term -> Binding (Name $ "_test_" ++ show i) term Nothing) ([0..] :: [Integer]) testTerms

        -- Create a temporary module with test terms as bindings
        tempModule = testModule { moduleElements = testBindings }

    -- Use the language-specific namespacesForModule which handles primitives correctly
    testGenNamespacesForModule testGen tempModule
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
    -- e.g., "hydra.test.monads" -> "hydra.monads"
    -- e.g., "hydra.test.etaExpansion" -> "eta expansion"
    -- e.g., "hydra.test.json.coder" -> "JSON coder"
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
generateGenerationTestSuite :: TestGenerator a -> FilePath -> [Module] -> (Namespace -> Maybe TestGroup) -> IO ()
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
    then putStrLn "No generation tests to generate"
    else do
      putStrLn $ "Found " ++ show (length moduleTestPairs) ++ " module(s) with generation tests, generating to " ++ outDir

      let graph = modulesToGraph (mainModules ++ testModules) $ modules ++ extraModules
--      let graph = bootstrapGraph

--      putStrLn $ "graph elements: {" ++ (L.intercalate ", " $ fmap (unName . bindingName) (M.elems $ graphElements graph)) ++ "}"

      -- Generate using the provided test generator, writing files incrementally
      result <- runFlowWithGraph graph $ generateAllModuleTestsIncremental testGen outDir moduleTestPairs writeFilePair

      case result of
        Left trace -> putStrLn $ "✗ Generation failed: " ++ traceSummary trace
        Right count -> do
          putStrLn $ "✓ Successfully generated " ++ show count ++ " test file(s)"
  where
    writeFilePair (fullPath, content) = do
      SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
      writeFile fullPath content
      putStrLn $ "  Generated: " ++ fullPath
    -- Core.module_ is required for schema types like Either, Maybe, etc.
    -- Monads.module_ is required for primitives like hydra.monads.pure
    extraModules = [Formatting.module_, Monads.module_, Core.module_]

-- Note: collectModules removed since Module dependencies are now Namespaces.
-- Callers should provide explicit module lists instead of relying on traversal.

-- | Run a Flow action with the given graph, returning Maybe
runFlowWithGraph :: s -> Flow s a -> IO (Either Trace a)
runFlowWithGraph s f = do
    return $ case v of
      Nothing -> Left trace
      Just value -> Right value
  where
    FlowState v _ trace = unFlow f s emptyTrace

-- | Generate all test files using the provided test generator
-- Returns a list of (FilePath, content) pairs
generateAllModuleTests :: TestGenerator a -> FilePath -> [(Module, TestGroup)] -> Flow Graph [(FilePath, String)]
generateAllModuleTests testGen baseDir modulePairs = do
  -- Perform type inference ONCE upfront for the entire graph
  -- This is critical for performance: inferGraphTypes is expensive and should not be called per-module
  g0 <- getState
  trace ("Starting type inference...") $ return ()
  g <- Inference.inferGraphTypes g0
  trace ("Type inference complete. Generating " ++ show (length modulePairs) ++ " module(s)...") $ return ()
  putState g

  files <- mapM (generateModuleTestWithProgress testGen baseDir) (zip [1..] modulePairs)
  -- Generate an aggregator file if the generator provides one
  case testGenAggregatorFile testGen of
    Just genAggregator -> return (genAggregator baseDir (map fst modulePairs) : files)
    Nothing -> return files

-- | Generate all test files incrementally, writing each file immediately after generation
-- This reduces peak memory usage by not accumulating all file contents in memory
-- Uses unsafePerformIO to write files from within Flow - this is safe because:
-- 1. The IO is idempotent (writing the same file twice produces the same result)
-- 2. The ordering doesn't affect the final result
-- Returns the count of files generated
generateAllModuleTestsIncremental :: TestGenerator a -> FilePath -> [(Module, TestGroup)] -> ((FilePath, String) -> IO ()) -> Flow Graph Int
generateAllModuleTestsIncremental testGen baseDir modulePairs writeFile = do
  -- Perform type inference ONCE upfront for the entire graph
  g0 <- getState
  trace ("Starting type inference...") $ return ()
  g <- Inference.inferGraphTypes g0
  trace ("Type inference complete. Generating " ++ show (length modulePairs) ++ " module(s)...") $ return ()
  putState g

  -- Generate files one at a time, writing each immediately
  -- Continue on failures so that one failing module doesn't block others
  successCount <- generateModulesWithContinueOnError testGen baseDir writeFile (zip [1..] modulePairs) 0

  -- Generate an aggregator file if the generator provides one
  case testGenAggregatorFile testGen of
    Just genAggregator -> do
      let aggregator = genAggregator baseDir (map fst modulePairs)
      unsafePerformIO (writeFile aggregator) `seq` return (successCount + 1)
    Nothing -> return successCount

-- | Generate modules one at a time, continuing on errors
-- Returns the count of successfully generated modules
generateModulesWithContinueOnError :: TestGenerator a -> FilePath -> ((FilePath, String) -> IO ()) -> [(Int, (Module, TestGroup))] -> Int -> Flow Graph Int
generateModulesWithContinueOnError _ _ _ [] count = return count
generateModulesWithContinueOnError testGen baseDir writeFile ((idx, pair):rest) count = do
  let (sourceModule, _) = pair
      ns = moduleNamespace sourceModule
  trace ("  Generating module " ++ show idx ++ ": " ++ show ns) $ return ()

  -- Try to generate this module using tryFlow, which catches errors and returns Maybe
  mresult <- tryFlow (generateModuleTest testGen baseDir pair)

  -- Handle result
  newCount <- case mresult of
    Just result -> do
      -- Success: write the file
      unsafePerformIO (writeFile result) `seq` return ()
      return (count + 1)
    Nothing -> do
      -- Failure: log and continue
      trace ("  ✗ Generation failed for " ++ show ns ++ " (continuing...)") $ return ()
      return count

  -- Continue with remaining modules
  generateModulesWithContinueOnError testGen baseDir writeFile rest newCount

-- | Try to run a Flow, returning Nothing if it fails instead of propagating the error
tryFlow :: Flow s a -> Flow s (Maybe a)
tryFlow f = Flow $ \s t ->
  let FlowState mval s' t' = unFlow f s t
  in FlowState (Just mval) s' t'

-- | Generate a single module test and write it immediately
generateAndWriteModule :: TestGenerator a -> FilePath -> ((FilePath, String) -> IO ()) -> (Int, (Module, TestGroup)) -> Flow Graph ()
generateAndWriteModule testGen baseDir writeFile (idx, pair) = do
  let (sourceModule, _) = pair
      ns = moduleNamespace sourceModule
  trace ("  Generating module " ++ show idx ++ ": " ++ show ns) $ return ()
  result <- generateModuleTest testGen baseDir pair
  -- Use unsafePerformIO to write the file immediately, then discard the result
  -- This allows GC to reclaim the generated content
  unsafePerformIO (writeFile result) `seq` return ()

-- | Generate a single test file for a module and its test group (with progress)
generateModuleTestWithProgress :: TestGenerator a -> FilePath -> (Int, (Module, TestGroup)) -> Flow Graph (FilePath, String)
generateModuleTestWithProgress testGen baseDir (idx, pair) = do
  let (sourceModule, _) = pair
  trace ("  Generating module " ++ show idx ++ ": " ++ show (moduleNamespace sourceModule)) $ return ()
  generateModuleTest testGen baseDir pair

-- | Generate a single test file for a module and its test group
generateModuleTest :: TestGenerator a -> FilePath -> (Module, TestGroup) -> Flow Graph (FilePath, String)
generateModuleTest testGen baseDir (sourceModule, testGroup) = do
  -- Use the test generator's file generation function
  (filePath, content) <- testGenGenerateTestFile testGen generationModule testGroup

  let fullPath = FP.combine baseDir filePath
  return (fullPath, content)
  where
    -- Use the source module's namespace with "generation." prefix
    -- Keep the same dependencies so imports are generated correctly
    generationModule = sourceModule {moduleNamespace = addGenerationPrefix (moduleNamespace sourceModule)}

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
