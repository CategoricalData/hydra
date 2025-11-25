module Hydra.Staging.Testing.Generation.Generate where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Generation
import Hydra.Dsl.Bootstrap (bootstrapGraph)
import Hydra.Staging.Testing.Generation.Transform

import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Lib.Strings as Strings

import qualified System.Directory as SD
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import Data.Char (isAlphaNum, toLower, toUpper)


-- | Language-agnostic test generator abstraction
-- Parameterized by the namespace/module name type (e.g., H.ModuleName for Haskell)
data TestGenerator a = TestGenerator {
  -- | Build namespaces for a module, resolving all imports and primitives
  testGenNamespacesForModule :: Module -> Flow Graph (Namespaces a),

  -- | Create a test codec from resolved namespaces
  testGenCreateCodec :: Namespaces a -> TestCodec,

  -- | Generate a complete test file for a module and test group
  testGenGenerateTestFile :: Module -> TestGroup -> Flow Graph (FilePath, String)
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

-- | Build a mapping from module namespaces to test groups by directly mapping submodules to subgroups
-- Pairs up modules with test groups up to the shorter of the two lists
buildTestGroupMap :: Module -> TestGroup -> M.Map Namespace TestGroup
buildTestGroupMap rootModule rootTestGroup =
  let subModules = moduleTermDependencies rootModule
      subGroups = testGroupSubgroups rootTestGroup
      -- Zip up to the minimum length (handles cases where some modules don't have test groups yet)
      pairs = zip (fmap moduleNamespace subModules) subGroups
  in M.fromList pairs

-- | Create a lookup function from a test group hierarchy
-- This walks the test group and module hierarchies to build the mapping
createTestGroupLookup :: Module -> TestGroup -> (Namespace -> Maybe TestGroup)
createTestGroupLookup rootModule rootTestGroup =
  let testGroupMap = buildTestGroupMap rootModule rootTestGroup
  in \ns -> M.lookup ns testGroupMap

-- | Main entry point: generate generation test suite from test modules
-- Takes a test generator, output directory, root test suite module and a lookup function for test groups
-- Automatically extracts all submodules and generates tests for each
generateGenerationTestSuite :: TestGenerator a -> FilePath -> Module -> (Namespace -> Maybe TestGroup) -> IO ()
generateGenerationTestSuite testGen outDir testSuiteModule lookupTestGroup = do
  putStrLn "Extracting test modules..."

  -- Extract all test modules from the module hierarchy (excluding the root)
  -- The root module is just a container and doesn't have test cases of its own
  let testModules = concatMap collectModules (moduleTermDependencies testSuiteModule)

  putStrLn $ "Found " ++ show (length testModules) ++ " test module(s)"
  putStrLn "Transforming test suite to generation tests..."

  -- Match modules with their test groups and transform
  let moduleTestPairs = [(mod, transformed) |
        mod <- testModules,
        Just testGroup <- [lookupTestGroup (moduleNamespace mod)],
        Just transformed <- [transformToCompiledTests testGroup]]

  if null moduleTestPairs
    then putStrLn "No generation tests to generate"
    else do
      putStrLn $ "Found " ++ show (length moduleTestPairs) ++ " module(s) with generation tests, generating to " ++ outDir

      let graph = modulesToGraph $ testModules ++ extraModules
--      let graph = bootstrapGraph

--      putStrLn $ "graph elements: {" ++ (L.intercalate ", " $ fmap (unName . bindingName) (M.elems $ graphElements graph)) ++ "}"

      -- Generate using the provided test generator
      result <- runFlowWithGraph graph $ generateAllModuleTests testGen outDir moduleTestPairs

      case result of
        Left trace -> putStrLn $ "✗ Generation failed: " ++ traceSummary trace
        Right files -> do
          -- Write all generated files
          mapM_ writeFilePair files
          putStrLn $ "✓ Successfully generated " ++ show (length files) ++ " test file(s)"
  where
    writeFilePair (fullPath, content) = do
      SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
      writeFile fullPath content
      putStrLn $ "  Generated: " ++ fullPath
    extraModules = [Formatting.module_]

-- | Collect all modules from a module hierarchy (including the root and all submodules)
collectModules :: Module -> [Module]
collectModules mod = mod : concatMap collectModules (moduleTermDependencies mod)

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
  files <- mapM (generateModuleTest testGen baseDir) modulePairs
  -- Also generate an aggregator spec file
  let aggregatorFile = generateAggregatorSpec baseDir (map fst modulePairs)
  return (aggregatorFile : files)

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

