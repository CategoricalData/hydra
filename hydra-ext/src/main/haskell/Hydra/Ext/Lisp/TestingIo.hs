-- | Lisp test code generation codec for all four Lisp dialects

module Hydra.Ext.Lisp.TestingIo (
  lispTestGenerator,
  generateClojureGenerationTests,
  generateSchemeGenerationTests,
  generateCommonLispGenerationTests,
  generateEmacsLispGenerationTests,
) where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Generation (TestGenerator(..), generateGenerationTestSuite)
import qualified Hydra.Ext.Lisp.Syntax as Syntax
import qualified Hydra.Lib.Strings as Strings

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char (toLower, isAlphaNum, isUpper)


-- | Get the language name for a dialect
dialectName :: Syntax.Dialect -> String
dialectName Syntax.DialectClojure = "clojure"
dialectName Syntax.DialectScheme = "scheme"
dialectName Syntax.DialectCommonLisp = "commonLisp"
dialectName Syntax.DialectEmacsLisp = "emacsLisp"

-- | Get the file extension for a dialect
dialectExtension :: Syntax.Dialect -> String
dialectExtension Syntax.DialectClojure = "clj"
dialectExtension Syntax.DialectScheme = "scm"
dialectExtension Syntax.DialectCommonLisp = "lisp"
dialectExtension Syntax.DialectEmacsLisp = "el"

-- | Format a test name for a dialect
dialectFormatTestName :: Syntax.Dialect -> String -> String
dialectFormatTestName Syntax.DialectClojure = formatClojureTestName
dialectFormatTestName _ = formatKebabTestName

-- | Format a test name in camelCase for Clojure (matching deftest naming)
formatClojureTestName :: String -> String
formatClojureTestName name = "test-" ++ toKebabCase (preprocessName name)

-- | Format a test name in kebab-case for Scheme, Common Lisp, Emacs Lisp
formatKebabTestName :: String -> String
formatKebabTestName name = "test-" ++ toKebabCase (preprocessName name)

-- | Preprocess a name to handle special characters
preprocessName :: String -> String
preprocessName = concatMap charToWord
  where
    charToWord c
      | c == '-' = "-neg"
      | c == '.' = "-dot"
      | c == '+' = "-plus"
      | c == '/' = "-div"
      | c == '*' = "-mul"
      | c == '#' = "-num"
      | otherwise = [c]

-- | Convert a string to kebab-case
toKebabCase :: String -> String
toKebabCase s = map toLower $ L.intercalate "-" $ words $ map toSep s
  where
    toSep c
      | isAlphaNum c = c
      | c == '-' = '-'
      | otherwise = ' '

-- | Generate test hierarchy for Lisp
generateLispTestGroupHierarchy :: Syntax.Dialect -> [String] -> TestGroup -> Either String String
generateLispTestGroupHierarchy dialect groupPath testGroup = do
  -- Generate test cases at the current level
  testCaseLinesRaw <- mapM (generateLispTestCase dialect groupPath) (testGroupCases testGroup)
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
      subgroupContent <- generateLispTestGroupHierarchy dialect (groupPath ++ [groupName]) subgroup
      let header = ";; " ++ groupName
      return $ header ++ "\n\n" ++ subgroupContent

-- | Generate a single test case for Lisp
generateLispTestCase :: Syntax.Dialect -> [String] -> TestCaseWithMetadata -> Either String [String]
generateLispTestCase dialect groupPath (TestCaseWithMetadata name tcase _ _) = case tcase of
  TestCaseUniversal (UniversalTestCase actual expected) -> do
    -- Include group path in test name to avoid collisions
    let fullName = if L.null groupPath
          then name
          else L.intercalate "-" (groupPath ++ [name])
        formattedName = dialectFormatTestName dialect fullName

    return $ case dialect of
      Syntax.DialectClojure -> [
        "(deftest " ++ formattedName,
        "  (is (= " ++ expected,
        "         " ++ actual ++ ")))"]
      Syntax.DialectScheme -> [
        "(define (" ++ formattedName ++ ")",
        "  (assert (equal? " ++ expected ++ " " ++ actual ++ ")))"]
      Syntax.DialectCommonLisp -> [
        "(defun " ++ formattedName ++ " ()",
        "  (assert (equal " ++ expected ++ " " ++ actual ++ ")))"]
      Syntax.DialectEmacsLisp -> [
        "(ert-deftest " ++ formattedName ++ " ()",
        "  (should (equal " ++ expected ++ " " ++ actual ++ ")))"]

-- | Generate test file for a dialect
generateLispTestFile :: Syntax.Dialect -> Module -> TestGroup -> Graph -> Either String (FilePath, String)
generateLispTestFile dialect testModule testGroup _g = do
  -- Generate test hierarchy. Seed group path with module name to avoid cross-module name collisions.
  let Namespace ns0 = moduleNamespace testModule
      moduleSuffix = L.last (Strings.splitOn "." ns0)
  testBody <- generateLispTestGroupHierarchy dialect [moduleSuffix] testGroup

  -- Build the complete test module
  let testModuleContent = buildLispTestModule dialect testModule testGroup testBody

  -- Generate file path
  let Namespace ns = moduleNamespace testModule
      parts = Strings.splitOn "." ns
      dirParts = drop 1 (L.init parts)  -- drop "generation" prefix
      lastPart = toKebabCase (L.last parts)
      -- Clojure requires underscores in filenames (dashes in namespaces map to underscores in files)
      baseName = case dialect of
        Syntax.DialectClojure -> map (\c -> if c == '-' then '_' else c) lastPart ++ "_test"
        _                    -> lastPart ++ "-test"
      fileName = baseName ++ "." ++ dialectExtension dialect
      filePath = L.intercalate "/" (map (\p -> case dialect of
        Syntax.DialectClojure -> map (\c -> if c == '-' then '_' else c) (toKebabCase p)
        _                    -> toKebabCase p) dirParts) ++ "/" ++ fileName

  return (filePath, testModuleContent)

-- | Build complete Lisp test module
buildLispTestModule :: Syntax.Dialect -> Module -> TestGroup -> String -> String
buildLispTestModule dialect _testModule testGroup testBody = case dialect of
    Syntax.DialectClojure -> clojureModule
    Syntax.DialectScheme -> schemeModule
    Syntax.DialectCommonLisp -> commonLispModule
    Syntax.DialectEmacsLisp -> emacsLispModule
  where
    groupName = testGroupName testGroup

    clojureModule = unlines [
        ";; " ++ warningAutoGeneratedFile,
        ";; " ++ groupName,
        "",
        "(ns test-ns",
        "  (:require [clojure.test :refer :all]))",
        ""
      ] ++ testBody ++ "\n"

    schemeModule = unlines [
        ";; " ++ warningAutoGeneratedFile,
        ";; " ++ groupName,
        "",
        "(import (scheme base))",
        ""
      ] ++ testBody ++ "\n"

    commonLispModule = unlines [
        ";; " ++ warningAutoGeneratedFile,
        ";; " ++ groupName,
        ""
      ] ++ testBody ++ "\n"

    emacsLispModule = unlines [
        ";;; " ++ warningAutoGeneratedFile ++ " -*- lexical-binding: t; coding: utf-8 -*-",
        ";;; " ++ groupName,
        "",
        "(require 'ert)",
        ""
      ] ++ testBody ++ "\n"

-- | Lisp test generator parameterized by dialect
lispTestGenerator :: Syntax.Dialect -> TestGenerator ()
lispTestGenerator dialect = TestGenerator {
  testGeneratorNamespacesForModule = \_ _ -> Right $ Namespaces (Namespace "", ()) M.empty,
  testGeneratorGenerateTestFile = generateLispTestFile dialect,
  testGeneratorAggregatorFile = Nothing
}

-- | Generate Clojure generation tests
generateClojureGenerationTests :: FilePath -> [Module] -> (Namespace -> Maybe TestGroup) -> IO Bool
generateClojureGenerationTests = generateGenerationTestSuite (lispTestGenerator Syntax.DialectClojure)

-- | Generate Scheme generation tests
generateSchemeGenerationTests :: FilePath -> [Module] -> (Namespace -> Maybe TestGroup) -> IO Bool
generateSchemeGenerationTests = generateGenerationTestSuite (lispTestGenerator Syntax.DialectScheme)

-- | Generate Common Lisp generation tests
generateCommonLispGenerationTests :: FilePath -> [Module] -> (Namespace -> Maybe TestGroup) -> IO Bool
generateCommonLispGenerationTests = generateGenerationTestSuite (lispTestGenerator Syntax.DialectCommonLisp)

-- | Generate Emacs Lisp generation tests
generateEmacsLispGenerationTests :: FilePath -> [Module] -> (Namespace -> Maybe TestGroup) -> IO Bool
generateEmacsLispGenerationTests = generateGenerationTestSuite (lispTestGenerator Syntax.DialectEmacsLisp)
