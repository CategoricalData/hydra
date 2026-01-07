-- | Python test code generation codec, similar to HaskellCodec but for pytest
module Hydra.Ext.Staging.Python.TestCodec (
  pythonTestGenerator,
  generatePythonGenerationTests,
) where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Coders (LanguageName(..))
import Hydra.Staging.Testing.Generation.Transform (collectTestCases)
import Hydra.Staging.Testing.Generation.Generate (TestGenerator(..), createTestGroupLookup, generateGenerationTestSuite)
import Hydra.Ext.Staging.Python.Coder (encodeTermInline, encodeType, PyGraph(..), PythonModuleMetadata(..))
import qualified Hydra.Ext.Staging.Python.Names as PyNames
import Hydra.Ext.Staging.Python.Names (PythonEnvironment(..), encodeNamespace, findNamespaces)
import Hydra.Ext.Staging.Python.Utils
import qualified Hydra.Ext.Staging.Python.Serde as PySer
import Hydra.Serialization (printExpr, parenthesize)
import qualified Hydra.Ext.Python.Syntax as Py
import qualified Hydra.Names as Names
import qualified Hydra.Util as Util
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Lib.Lists as Lists

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Maybe as Y
import qualified Data.Set as S
import Data.Char (toLower, isAlphaNum)
import qualified Hydra.Typing as Typing
import Hydra.Typing (TypeContext(..), InferenceContext(..))
import qualified Hydra.Schemas as Schemas


-- | Initial empty metadata for running encoding in PyGraph
emptyPythonModuleMetadata :: Namespace -> PythonModuleMetadata
emptyPythonModuleMetadata ns = PythonModuleMetadata {
  pythonModuleMetadataNamespaces = Namespaces (ns, encodeNamespace ns) M.empty,
  pythonModuleMetadataTypeVariables = S.empty,
  pythonModuleMetadataUsesAnnotated = False,
  pythonModuleMetadataUsesCallable = False,
  pythonModuleMetadataUsesCast = False,
  pythonModuleMetadataUsesTypeAlias = False,
  pythonModuleMetadataUsesDataclass = False,
  pythonModuleMetadataUsesDecimal = False,
  pythonModuleMetadataUsesEither = False,
  pythonModuleMetadataUsesEnum = False,
  pythonModuleMetadataUsesFrozenDict = False,
  pythonModuleMetadataUsesFrozenList = False,
  pythonModuleMetadataUsesGeneric = False,
  pythonModuleMetadataUsesJust = False,
  pythonModuleMetadataUsesLeft = False,
  pythonModuleMetadataUsesMaybe = False,
  pythonModuleMetadataUsesName = False,
  pythonModuleMetadataUsesNode = False,
  pythonModuleMetadataUsesNothing = False,
  pythonModuleMetadataUsesRight = False,
  pythonModuleMetadataUsesTypeVar = False}

-- | Convert a Hydra term to a Python expression string
-- Takes a pre-built TypeContext to avoid rebuilding it for every term
-- The skipCasts parameter controls whether to generate cast() calls (False = generate casts, True = skip)
termToPythonWithContext :: Namespaces Py.DottedName -> TypeContext -> Bool -> Term -> Flow Graph String
termToPythonWithContext namespaces tcontext skipCasts term = do
  g <- getState
  let env = PythonEnvironment {
        pythonEnvironmentNamespaces = namespaces,
        pythonEnvironmentBoundTypeVariables = ([], M.empty),
        pythonEnvironmentTypeContext = tcontext,
        pythonEnvironmentNUllaryBindings = S.empty,
        pythonEnvironmentVersion = PyNames.targetPythonVersion,
        pythonEnvironmentSkipCasts = skipCasts
      }
      -- Create initial PyGraph state
      ns = fst $ namespacesFocus namespaces
      meta = emptyPythonModuleMetadata ns
      pyGraph = PyGraph g meta

  -- Run in PyGraph context and extract result
  -- Note: We use the original term directly. Type inference happens at the graph level
  -- via inferGraphTypes in generateAllModuleTests, which enriches schema types.
  -- Per-term inference is not needed since Python is dynamically typed.
  let flow = encodeTermInline env False term
      FlowState mresult _ trace = unFlow flow pyGraph emptyTrace
  case mresult of
    Just pyExpr -> return $ printExpr $ PySer.encodeExpression pyExpr
    Nothing -> fail $ "Failed to encode term to Python: " ++ traceSummary trace

-- | Legacy wrapper that builds TypeContext on each call (inefficient, for backwards compat)
termToPython :: Namespaces Py.DottedName -> Term -> Flow Graph String
termToPython namespaces term = do
  g <- getState
  tcontext <- Schemas.graphToTypeContext g
  termToPythonWithContext namespaces tcontext False term

-- | Convert a Hydra type to a Python type expression string
-- Note: This function expects the graph to already have inferred types (for performance,
-- inference should be done once per test file, not per term)
typeToPython :: Namespaces Py.DottedName -> Type -> Flow Graph String
typeToPython namespaces typ = do
  g <- getState
  -- Build a type context with schema types from the graph
  tcontext <- Schemas.graphToTypeContext g
  let env = PythonEnvironment {
        pythonEnvironmentNamespaces = namespaces,
        pythonEnvironmentBoundTypeVariables = ([], M.empty),
        pythonEnvironmentTypeContext = tcontext,
        pythonEnvironmentNUllaryBindings = S.empty,
        pythonEnvironmentVersion = PyNames.targetPythonVersion,
        pythonEnvironmentSkipCasts = False  -- Types don't use casts anyway
      }
      ns = fst $ namespacesFocus namespaces
      meta = emptyPythonModuleMetadata ns
      pyGraph = PyGraph g meta

  let flow = encodeType env typ
      FlowState mresult _ trace = unFlow flow pyGraph emptyTrace
  case mresult of
    Just pyType -> return $ printExpr $ PySer.encodeExpression pyType
    Nothing -> fail $ "Failed to encode type to Python: " ++ traceSummary trace

-- | Create a Python TestCodec that uses the Python coder
-- Note: For efficiency, prefer pythonTestCodecWithContext which takes a pre-built TypeContext
pythonTestCodec :: Namespaces Py.DottedName -> TestCodec
pythonTestCodec namespaces = TestCodec {
    testCodecLanguage = LanguageName "python",
    testCodecFileExtension = FileExtension "py",
    testCodecEncodeTerm = termToPython namespaces,
    testCodecEncodeType = typeToPython namespaces,
    testCodecFormatTestName = formatPythonTestName,
    testCodecFormatModuleName = namespaceToPythonModuleName,
    testCodecTestCaseTemplate = pythonTestCaseTemplate,
    testCodecTestGroupTemplate = pythonTestGroupTemplate,
    testCodecModuleTemplate = pythonModuleTemplate,
    testCodecImportTemplate = pythonImportTemplate,
    testCodecFindImports = findPythonImports namespaces}

-- | Create an efficient Python TestCodec with a pre-built TypeContext
-- This avoids rebuilding the TypeContext for every term encoded.
-- Skips cast() generation for better performance during test generation.
pythonTestCodecWithContext :: Namespaces Py.DottedName -> TypeContext -> TestCodec
pythonTestCodecWithContext namespaces tcontext = TestCodec {
    testCodecLanguage = LanguageName "python",
    testCodecFileExtension = FileExtension "py",
    -- Skip casts for test generation - reduces memory usage dramatically
    testCodecEncodeTerm = termToPythonWithContext namespaces tcontext True,
    testCodecEncodeType = typeToPython namespaces,  -- types are rarely encoded, ok to be less efficient
    testCodecFormatTestName = formatPythonTestName,
    testCodecFormatModuleName = namespaceToPythonModuleName,
    testCodecTestCaseTemplate = pythonTestCaseTemplate,
    testCodecTestGroupTemplate = pythonTestGroupTemplate,
    testCodecModuleTemplate = pythonModuleTemplate,
    testCodecImportTemplate = pythonImportTemplate,
    testCodecFindImports = findPythonImports namespaces}

-- | Format a test name for Python (snake_case, valid identifier)
formatPythonTestName :: String -> String
formatPythonTestName name = "test_" ++ L.map toSnakeChar name
  where
    toSnakeChar c
      | isAlphaNum c = toLower c
      | c == ' ' = '_'
      | c == '-' = '_'
      | otherwise = '_'

-- Templates for Python test generation (pytest style)
pythonTestCaseTemplate :: String
pythonTestCaseTemplate = unlines [
  "def {name}():",
  "    assert ({input}) == ({output})"]

pythonTestGroupTemplate :: String
pythonTestGroupTemplate = "# {groupName}"

pythonModuleTemplate :: String
pythonModuleTemplate = unlines [
  "# " ++ warningAutoGeneratedFile,
  "",
  "{imports}",
  "",
  "{testGroup}",
  "",
  "{testCases}"]

pythonImportTemplate :: String
pythonImportTemplate = "import {namespace}"

-- | Find necessary imports for Python based on referenced namespaces
findPythonImports :: Namespaces Py.DottedName -> S.Set Name -> [String]
findPythonImports namespaces names = L.map makeImport (M.toList filteredMapping)
  where
    -- Filter out test module namespaces (hydra.test.*) which aren't in main library
    isTestNamespace (Namespace ns) = "hydra.test." `L.isPrefixOf` ns
    filteredMapping = M.filterWithKey (\ns _ -> not (isTestNamespace ns)) (namespacesMapping namespaces)
    makeImport (ns, _) = "import " ++ nsToModuleName ns
    nsToModuleName (Namespace ns) = ns

-- | Build namespaces for Python module
-- For test generation, we create a simple namespace mapping without needing full definitions
namespacesForPythonModule :: Module -> Flow Graph (Namespaces Py.DottedName)
namespacesForPythonModule mod = do
    graph <- getState
    -- Convert Bindings to Definitions for findNamespaces
    -- We only need term definitions for namespace discovery
    let bindings = M.elems $ graphElements graph
        defs = Y.mapMaybe bindingToDefinition bindings
        ns = moduleNamespace mod
    return $ findNamespaces ns defs
  where
    bindingToDefinition :: Binding -> Maybe Definition
    bindingToDefinition (Binding name term mts) = case mts of
      Just ts -> Just $ DefinitionTerm $ TermDefinition name term ts
      Nothing -> Nothing  -- Skip bindings without type info

-- | Convert namespace to Python module name
-- Python modules use dot-separated lowercase names
namespaceToPythonModuleName :: Namespace -> String
namespaceToPythonModuleName (Namespace ns) = ns

-- | Generate test hierarchy for Python
-- Note: Python doesn't need InferenceContext since it's dynamically typed (no type annotations needed)
-- The groupPath parameter accumulates parent group names to create unique test function names
generatePythonTestGroupHierarchy :: Namespaces Py.DottedName -> TestCodec -> [String] -> TestGroup -> Flow Graph String
generatePythonTestGroupHierarchy namespaces codec groupPath testGroup = do
  -- Generate test cases at the current level
  testCaseLinesRaw <- mapM (generatePythonTestCase namespaces codec groupPath) (testGroupCases testGroup)
  let testCaseLines = concat testCaseLinesRaw
      testCasesStr = L.intercalate "\n\n" testCaseLines

  -- Generate sections for each subgroup, passing the subgroup name in the path
  subgroupStrs <- mapM generateSubgroupBlock (testGroupSubgroups testGroup)
  let subgroupsStr = L.intercalate "\n\n" subgroupStrs

  -- Combine test cases and subgroups
  return $ testCasesStr ++ (if null testCasesStr || null subgroupsStr then "" else "\n\n") ++ subgroupsStr
  where
    generateSubgroupBlock :: TestGroup -> Flow Graph String
    generateSubgroupBlock subgroup = do
      let groupName = testGroupName subgroup
      subgroupContent <- generatePythonTestGroupHierarchy namespaces codec (groupPath ++ [groupName]) subgroup
      let header = "# " ++ groupName
      return $ header ++ "\n\n" ++ subgroupContent

-- | Generate a single test case for Python/pytest
-- Note: Python doesn't need InferenceContext since it's dynamically typed
-- The groupPath is used to create unique test names by prefixing with parent group names
generatePythonTestCase :: Namespaces Py.DottedName -> TestCodec -> [String] -> TestCaseWithMetadata -> Flow Graph [String]
generatePythonTestCase namespaces codec groupPath (TestCaseWithMetadata name tcase _ _) = case tcase of
  TestCaseDelegatedEvaluation (DelegatedEvaluationTestCase input output) -> do
    inputCode <- testCodecEncodeTerm codec input
    outputCode <- testCodecEncodeTerm codec output

    -- Include group path in test name to avoid collisions
    -- e.g., groupPath=["bind"], name="empty list" -> "test_bind__empty_list"
    let fullName = if L.null groupPath
          then name
          else L.intercalate "__" (groupPath ++ [name])
        formattedName = testCodecFormatTestName codec fullName
    return [
      "def " ++ formattedName ++ "():",
      "    assert (" ++ inputCode ++ ") == (" ++ outputCode ++ ")"]

  _ -> return []  -- Skip non-delegated tests

-- | Generate test file using Python codec
generateTestFileWithPythonCodec :: TestCodec -> Module -> TestGroup -> Namespaces Py.DottedName -> Flow Graph (FilePath, String)
generateTestFileWithPythonCodec codec testModule testGroup namespaces = do
  -- Note: Type inference is now performed ONCE upfront in generateAllModuleTests (Generate.hs)
  -- This is critical for performance: inferGraphTypes is expensive and should not be called per-module

  -- Generate test hierarchy directly (Python doesn't need InferenceContext for type annotations)
  testBody <- generatePythonTestGroupHierarchy namespaces codec [] testGroup

  -- Build the complete test module
  let testModuleContent = buildPythonTestModule codec testModule testGroup testBody namespaces

  -- Generate file path: namespace -> path with test_ prefix
  let Namespace ns = moduleNamespace testModule
      -- Convert dots to slashes and add test_ prefix to filename
      parts = Strings.splitOn "." ns
      dirParts = L.init parts
      fileName = "test_" ++ L.last parts ++ ".py"
      filePath = L.intercalate "/" dirParts ++ "/" ++ fileName

  return (filePath, testModuleContent)

-- | Build complete Python test module
buildPythonTestModule :: TestCodec -> Module -> TestGroup -> String -> Namespaces Py.DottedName -> String
buildPythonTestModule codec testModule testGroup testBody namespaces = header ++ testBody ++ "\n"
  where
    groupName = testGroupName testGroup

    -- Use the codec's findImports to determine necessary imports
    domainImports = testCodecFindImports codec S.empty

    -- Standard imports for Python tests
    standardImports = [
      "from __future__ import annotations",
      "from typing import cast",
      "from decimal import Decimal",
      "from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing"]

    allImports = standardImports ++ domainImports

    header = unlines [
        "# " ++ warningAutoGeneratedFile,
        "# " ++ groupName,
        ""
      ] ++ L.intercalate "\n" allImports ++ "\n\n"

-- | Generate Python test file for a test group
generatePythonTestFile :: Module -> TestGroup -> Flow Graph (FilePath, String)
generatePythonTestFile testModule testGroup = do
  -- Build proper namespaces that include all primitives
  namespaces <- buildNamespacesForTestGroup testModule testGroup

  -- Build TypeContext ONCE per test file (critical for performance)
  g <- getState
  tcontext <- Schemas.graphToTypeContext g

  -- Generate test file using the efficient codec with pre-built TypeContext
  generateTestFileWithPythonCodec (pythonTestCodecWithContext namespaces tcontext) testModule testGroup namespaces
  where
    buildNamespacesForTestGroup mod tgroup = do
      let testCases = collectTestCases tgroup
          testTerms = concatMap extractTestTerms testCases
          testBindings = zipWith (\i term -> Binding (Name $ "_test_" ++ show i) term Nothing) ([0..] :: [Integer]) testTerms
          tempModule = mod { moduleElements = testBindings }
      namespacesForPythonModule tempModule
    extractTestTerms (TestCaseWithMetadata _ tcase _ _) = case tcase of
      TestCaseDelegatedEvaluation (DelegatedEvaluationTestCase input output) -> [input, output]
      _ -> []

-- | Python-specific test generator
-- Provides the complete Python implementation of the TestGenerator abstraction
pythonTestGenerator :: TestGenerator Py.DottedName
pythonTestGenerator = TestGenerator {
  testGenNamespacesForModule = namespacesForPythonModule,
  testGenCreateCodec = pythonTestCodec,
  testGenGenerateTestFile = generatePythonTestFile,
  testGenAggregatorFile = Nothing  -- pytest auto-discovers tests via test_ prefix
}

-- | Main entry point for generating Python generation tests
generatePythonGenerationTests :: FilePath -> [Module] -> (Namespace -> Maybe TestGroup) -> IO Bool
generatePythonGenerationTests = generateGenerationTestSuite pythonTestGenerator
