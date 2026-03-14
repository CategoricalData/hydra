-- | Python test code generation codec, similar to TestCodec in hydra-haskell but for pytest
module Hydra.Ext.Staging.Python.TestCodec (
  pythonTestGenerator,
  generatePythonGenerationTests,
) where

import Hydra.Kernel hiding (map)
import qualified Hydra.Show.Error as ShowError
import Hydra.Testing
import Hydra.Coders (LanguageName(..))
import Hydra.Test.Transform (collectTestCases)
import Hydra.Staging.Testing.Generation.Generate (TestGenerator(..), createTestGroupLookup, generateGenerationTestSuite)
import Hydra.Ext.Python.Coder (encodeTermInline, encodeType)
import Hydra.Ext.Python.Helpers (PythonModuleMetadata(..))
import qualified Hydra.Ext.Python.Names as PyNames
import Hydra.Ext.Python.Names (encodeNamespace)
import Hydra.Ext.Python.Helpers (PythonEnvironment(..))
import Hydra.Ext.Python.Utils
import qualified Hydra.Ext.Python.Serde as PySer
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
import Hydra.Typing (InferenceResult(..))
import qualified Hydra.Inference as Inference


-- | Initial empty metadata for running encoding
emptyPythonModuleMetadata :: Namespace -> PythonModuleMetadata
emptyPythonModuleMetadata ns = PythonModuleMetadata {
  pythonModuleMetadataNamespaces = Namespaces (ns, encodeNamespace ns) M.empty,
  pythonModuleMetadataTypeVariables = S.empty,
  pythonModuleMetadataUsesAnnotated = False,
  pythonModuleMetadataUsesCallable = False,
  pythonModuleMetadataUsesCast = False,
  pythonModuleMetadataUsesLruCache = False,
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
-- Takes a pre-built Graph to avoid rebuilding it for every term
-- The skipCasts parameter controls whether to generate cast() calls (False = generate casts, True = skip)
termToPythonWithContext :: Namespaces Py.DottedName -> Graph -> Bool -> Term -> Graph -> Either String String
termToPythonWithContext namespaces graph0 skipCasts term _g =
  let env = PythonEnvironment {
        pythonEnvironmentNamespaces = namespaces,
        pythonEnvironmentBoundTypeVariables = ([], M.empty),
        pythonEnvironmentGraph = graph0,
        pythonEnvironmentNullaryBindings = S.empty,
        pythonEnvironmentVersion = targetPythonVersion,
        pythonEnvironmentSkipCasts = skipCasts,
        pythonEnvironmentInlineVariables = S.empty
      }
      result = encodeTermInline emptyContext env skipCasts term
  in case result of
    Right pyExpr -> Right $ printExpr $ PySer.encodeExpression pyExpr
    Left ic -> Left $ "Failed to encode term to Python: " ++ ShowError.error (inContextObject ic)

-- | Legacy wrapper that gets Graph on each call (for backwards compat)
termToPython :: Namespaces Py.DottedName -> Term -> Graph -> Either String String
termToPython namespaces term g = termToPythonWithContext namespaces g False term g

-- | Convert a Hydra type to a Python type expression string
-- Note: This function expects the graph to already have inferred types (for performance,
-- inference should be done once per test file, not per term)
typeToPython :: Namespaces Py.DottedName -> Type -> Graph -> Either String String
typeToPython namespaces typ g =
  let env = PythonEnvironment {
        pythonEnvironmentNamespaces = namespaces,
        pythonEnvironmentBoundTypeVariables = ([], M.empty),
        pythonEnvironmentGraph = g,
        pythonEnvironmentNullaryBindings = S.empty,
        pythonEnvironmentVersion = targetPythonVersion,
        pythonEnvironmentSkipCasts = False,  -- Types don't use casts anyway
        pythonEnvironmentInlineVariables = S.empty
      }
      result = encodeType env typ
  in case result of
    Right pyType -> Right $ printExpr $ PySer.encodeExpression pyType
    Left _e -> Left $ "Failed to encode type to Python"

-- | Create a Python TestCodec that uses the Python coder
-- Note: For efficiency, prefer pythonTestCodecWithContext which takes a pre-built Graph
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

-- | Create an efficient Python TestCodec with a pre-built Graph
-- This avoids rebuilding the Graph for every term encoded.
-- Skips cast() generation for better performance during test generation.
pythonTestCodecWithContext :: Namespaces Py.DottedName -> Graph -> TestCodec
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
namespacesForPythonModule :: Module -> Graph -> Either String (Namespaces Py.DottedName)
namespacesForPythonModule mod graph = do
    -- Convert Bindings to Definitions for findNamespaces
    -- We only need term definitions for namespace discovery
    let bindings = graphToBindings graph
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
generatePythonTestGroupHierarchy :: Graph -> Namespaces Py.DottedName -> TestCodec -> [String] -> TestGroup -> Either String String
generatePythonTestGroupHierarchy g namespaces codec groupPath testGroup = do
  -- Generate test cases at the current level
  testCaseLinesRaw <- mapM (generatePythonTestCase g namespaces codec groupPath) (testGroupCases testGroup)
  let testCaseLines = concat testCaseLinesRaw
      testCasesStr = L.intercalate "\n\n" testCaseLines

  -- Generate sections for each subgroup, passing the subgroup name in the path
  subgroupStrs <- mapM generateSubgroupBlock (testGroupSubgroups testGroup)
  let subgroupsStr = L.intercalate "\n\n" subgroupStrs

  -- Combine test cases and subgroups
  return $ testCasesStr ++ (if null testCasesStr || null subgroupsStr then "" else "\n\n") ++ subgroupsStr
  where
    generateSubgroupBlock :: TestGroup -> Either String String
    generateSubgroupBlock subgroup = do
      let groupName = testGroupName subgroup
      subgroupContent <- generatePythonTestGroupHierarchy g namespaces codec (groupPath ++ [groupName]) subgroup
      let header = "# " ++ groupName
      return $ header ++ "\n\n" ++ subgroupContent

-- | Generate a single test case for Python/pytest
-- Note: Python doesn't need InferenceContext since it's dynamically typed
-- The groupPath is used to create unique test names by prefixing with parent group names
generatePythonTestCase :: Graph -> Namespaces Py.DottedName -> TestCodec -> [String] -> TestCaseWithMetadata -> Either String [String]
generatePythonTestCase g namespaces codec groupPath (TestCaseWithMetadata name tcase _ _) = case tcase of
  TestCaseDelegatedEvaluation (DelegatedEvaluationTestCase input output) -> do
    inputCode <- testCodecEncodeTerm codec input g
    outputCode <- testCodecEncodeTerm codec output g

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
generateTestFileWithPythonCodec :: TestCodec -> Module -> TestGroup -> Namespaces Py.DottedName -> Graph -> Either String (FilePath, String)
generateTestFileWithPythonCodec codec testModule testGroup namespaces g = do
  -- Generate test hierarchy directly (Python doesn't need InferenceContext for type annotations)
  testBody <- generatePythonTestGroupHierarchy g namespaces codec [] testGroup

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
generatePythonTestFile :: Module -> TestGroup -> Graph -> Either String (FilePath, String)
generatePythonTestFile testModule testGroup g = do
  -- Run type inference on all test terms to ensure lambdas have domain types
  inferredTestGroup <- inferTestGroupTerms g testGroup

  -- Build proper namespaces that include all primitives
  namespaces <- buildNamespacesForTestGroup testModule inferredTestGroup g

  -- Generate test file using the efficient codec with pre-built Graph
  generateTestFileWithPythonCodec (pythonTestCodecWithContext namespaces g) testModule inferredTestGroup namespaces g
  where
    buildNamespacesForTestGroup mod tgroup graph = do
      let testCases = collectTestCases tgroup
          testTerms = concatMap extractTestTerms testCases
          testBindings = zipWith (\i term -> Binding (Name $ "_test_" ++ show i) term Nothing) ([0..] :: [Integer]) testTerms
          tempModule = mod { moduleElements = testBindings }
      namespacesForPythonModule tempModule graph
    extractTestTerms (TestCaseWithMetadata _ tcase _ _) = case tcase of
      TestCaseDelegatedEvaluation (DelegatedEvaluationTestCase input output) -> [input, output]
      _ -> []

-- | Run type inference on all terms in a TestGroup
inferTestGroupTerms :: Graph -> TestGroup -> Either String TestGroup
inferTestGroupTerms g (TestGroup name desc subgroups cases) = do
  inferredSubgroups <- mapM (inferTestGroupTerms g) subgroups
  inferredCases <- mapM (inferTestCase g) cases
  return $ TestGroup name desc inferredSubgroups inferredCases

-- | Run type inference on the terms in a test case
inferTestCase :: Graph -> TestCaseWithMetadata -> Either String TestCaseWithMetadata
inferTestCase g (TestCaseWithMetadata name tcase desc tags) = do
  inferredTcase <- case tcase of
    TestCaseDelegatedEvaluation (DelegatedEvaluationTestCase input output) -> do
      inferredInput <- inferTerm g input
      inferredOutput <- inferTerm g output
      return $ TestCaseDelegatedEvaluation $ DelegatedEvaluationTestCase inferredInput inferredOutput
    other -> return other
  return $ TestCaseWithMetadata name inferredTcase desc tags

-- | Run type inference on a single term
inferTerm :: Graph -> Term -> Either String Term
inferTerm g term = case Inference.inferInGraphContext emptyContext g term of
    Left ic -> Left (ShowError.error (inContextObject ic))
    Right result -> Right (inferenceResultTerm result)

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
