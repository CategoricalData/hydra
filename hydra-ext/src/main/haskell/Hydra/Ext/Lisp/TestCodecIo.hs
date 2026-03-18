-- | Lisp test code generation codec for all four Lisp dialects

module Hydra.Ext.Lisp.TestCodecIo (
  lispTestGenerator,
  generateClojureGenerationTests,
  generateSchemeGenerationTests,
  generateCommonLispGenerationTests,
  generateEmacsLispGenerationTests,
) where

import Hydra.Kernel hiding (map)
import qualified Hydra.Show.Error as ShowError
import Hydra.Testing
import Hydra.Coders (LanguageName(..))
import Hydra.Generation (TestGenerator(..), generateGenerationTestSuite)
import Hydra.Ext.Lisp.Coder (encodeTerm)
import Hydra.Ext.Lisp.Serde (expressionToExpr)
import Hydra.Serialization (printExpr, parenthesize)
import qualified Hydra.Ext.Lisp.Syntax as Syntax
import qualified Hydra.Lib.Strings as Strings
import Hydra.Typing (InferenceResult(..))
import qualified Hydra.Inference as Inference

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Char (toLower, isAlphaNum, isUpper)


-- | Convert a Hydra term to a Lisp expression string for a given dialect.
--   Handles runtime representation differences: maps are alists, sets are sorted lists,
--   Maybe is (:just val)/(:nothing), pairs are (list a b).
termToLisp :: Syntax.Dialect -> Term -> Graph -> Either String String
termToLisp dialect term _g = encodeForRuntime term
  where
    encode t = case encodeTerm dialect () () t of
      Left _ -> Left "Lisp encoding failed"
      Right expr -> Right $ printExpr $ parenthesize $ expressionToExpr dialect expr

    encodeForRuntime t = case t of
      -- Maybe: (list :just val) or (list :nothing)
      -- Matches the hand-written lib representation
      TermMaybe Nothing -> Right "(list :nothing)"
      TermMaybe (Just v) -> do
        vs <- encodeForRuntime v
        Right $ "(list :just " ++ vs ++ ")"

      -- Maps: alist ((k1 v1) (k2 v2) ...)
      TermMap m | M.null m -> Right "()"
      TermMap m -> do
        let entries = M.toAscList m
        pairs <- mapM (\(k, v) -> do
          ks <- encodeForRuntime k
          vs <- encodeForRuntime v
          Right $ "(list " ++ ks ++ " " ++ vs ++ ")") entries
        Right $ "(list " ++ L.intercalate " " pairs ++ ")"

      -- Sets: sorted list
      TermSet s | S.null s -> Right "()"
      TermSet s -> do
        let elems = L.sort $ S.toList s
        strs <- mapM encodeForRuntime elems
        Right $ "(list " ++ L.intercalate " " strs ++ ")"

      -- Pairs: (list a b)
      TermPair (a, b) -> do
        as <- encodeForRuntime a
        bs <- encodeForRuntime b
        Right $ "(list " ++ as ++ " " ++ bs ++ ")"

      -- Annotated: encode with annotation map as alist
      TermAnnotated (AnnotatedTerm body ann)
        | M.null ann -> encodeForRuntime body
        | otherwise -> do
            bs <- encodeForRuntime body
            annStr <- encodeAnnotationMap ann
            Right $ "(list :annotated (->hydra_core_annotated_term " ++ bs ++ " " ++ annStr ++ "))"

      -- Lists: encode each element
      TermList ts -> do
        strs <- mapM encodeForRuntime ts
        Right $ "(list " ++ L.intercalate " " strs ++ ")"

      -- Type applications/lambdas (added by inference): strip and recurse
      TermTypeApplication (TypeApplicationTerm body _) -> encodeForRuntime body
      TermTypeLambda (TypeLambda _ body) -> encodeForRuntime body

      -- Applications: recurse into function and argument
      TermApplication (Application f a) -> do
        fs <- encodeForRuntime f
        as <- encodeForRuntime a
        Right $ "(" ++ fs ++ " " ++ as ++ ")"

      -- Functions: use the coder (lambdas, primitives, etc.)
      TermFunction _ -> encode t

      -- Unions: encode the field value as (list :field_name value)
      TermUnion (Injection _tn (Field fn_ ft)) -> do
        fts <- encodeForRuntime ft
        let fieldName = toSnakeCase (unName fn_)
        Right $ "(list :" ++ fieldName ++ " " ++ fts ++ ")"

      -- Records: encode constructor call with runtime-encoded field values
      TermRecord (Record tn fields) -> do
        fieldStrs <- mapM (\(Field _fn ft) -> encodeForRuntime ft) fields
        let ctorName = "->" ++ toSnakeCase (map (\c -> if c == '.' then '_' else c) (unName tn))
        Right $ "(" ++ ctorName ++ " " ++ L.intercalate " " fieldStrs ++ ")"

      -- Wraps: recurse into body
      TermWrap (WrappedTerm _ body) -> encodeForRuntime body

      -- Eithers: encode as (list :left val) or (list :right val)
      TermEither (Left l) -> do
        ls <- encodeForRuntime l
        Right $ "(list :left " ++ ls ++ ")"
      TermEither (Right r) -> do
        rs <- encodeForRuntime r
        Right $ "(list :right " ++ rs ++ ")"

      -- Let bindings
      TermLet (Let bindings body) -> do
        -- Simplify: just encode the body (let bindings are already evaluated in delegated tests)
        encodeForRuntime body

      -- For everything else (literals, variables, unit, etc.), use the coder
      _ -> encode t

    encodeAnnotationMap :: M.Map Name Term -> Either String String
    encodeAnnotationMap ann
      | M.null ann = Right "()"
      | otherwise = do
          let entries = M.toAscList ann
          pairs <- mapM (\(Name k, v) -> do
            vs <- encodeForRuntime v
            Right $ "(list \"" ++ k ++ "\" " ++ vs ++ ")") entries
          Right $ "(list " ++ L.intercalate " " pairs ++ ")"

-- | Convert a Hydra type to a Lisp type expression string (always returns a comment)
typeToLisp :: Type -> Graph -> Either String String
typeToLisp _ _ = Right "Object"

-- | Create a Lisp TestCodec for a given dialect
lispTestCodec :: Syntax.Dialect -> TestCodec
lispTestCodec dialect = TestCodec {
    testCodecLanguage = LanguageName (dialectName dialect),
    testCodecFileExtension = FileExtension (dialectExtension dialect),
    testCodecEncodeTerm = termToLisp dialect,
    testCodecEncodeType = typeToLisp,
    testCodecFormatTestName = dialectFormatTestName dialect,
    testCodecFormatModuleName = dialectFormatModuleName dialect,
    testCodecTestCaseTemplate = dialectTestCaseTemplate dialect,
    testCodecTestGroupTemplate = dialectTestGroupTemplate dialect,
    testCodecModuleTemplate = dialectModuleTemplate dialect,
    testCodecImportTemplate = dialectImportTemplate dialect,
    testCodecFindImports = \_ -> dialectFindImports dialect}

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

-- | Convert camelCase to snake_case (avoids double underscores)
toSnakeCase :: String -> String
toSnakeCase [] = []
toSnakeCase (c:cs) = toLower c : go False cs
  where
    go _ [] = []
    go afterUnderscore (x:xs)
      | isUpper x && not afterUnderscore = '_' : toLower x : go False xs
      | isUpper x = toLower x : go False xs
      | x == '_'  = x : go True xs
      | otherwise = x : go False xs

-- | Format a module name for a dialect
dialectFormatModuleName :: Syntax.Dialect -> Namespace -> String
dialectFormatModuleName _ (Namespace ns) = ns

-- | Test case template for each dialect
dialectTestCaseTemplate :: Syntax.Dialect -> String
dialectTestCaseTemplate Syntax.DialectClojure = unlines [
  "(deftest {name}",
  "  (is (= {output}",
  "         {input})))"]
dialectTestCaseTemplate Syntax.DialectScheme = unlines [
  "(define (test-{name})",
  "  (assert (equal? {output} {input})))"]
dialectTestCaseTemplate Syntax.DialectCommonLisp = unlines [
  "(defun {name} ()",
  "  (assert (equal {output} {input})))"]
dialectTestCaseTemplate Syntax.DialectEmacsLisp = unlines [
  "(ert-deftest {name} ()",
  "  (should (equal {output} {input})))"]

-- | Test group template for each dialect
dialectTestGroupTemplate :: Syntax.Dialect -> String
dialectTestGroupTemplate _ = ";; {groupName}"

-- | Module template for each dialect
dialectModuleTemplate :: Syntax.Dialect -> String
dialectModuleTemplate Syntax.DialectClojure = unlines [
  ";; " ++ warningAutoGeneratedFile,
  "",
  "(ns {package}",
  "  (:require [clojure.test :refer :all]))",
  "",
  "{testCases}"]
dialectModuleTemplate Syntax.DialectScheme = unlines [
  ";; " ++ warningAutoGeneratedFile,
  "",
  "(import (scheme base))",
  "",
  "{testCases}"]
dialectModuleTemplate Syntax.DialectCommonLisp = unlines [
  ";; " ++ warningAutoGeneratedFile,
  "",
  "{testCases}"]
dialectModuleTemplate Syntax.DialectEmacsLisp = unlines [
  ";;; " ++ warningAutoGeneratedFile,
  "",
  "(require 'ert)",
  "",
  "{testCases}"]

-- | Import template for each dialect
dialectImportTemplate :: Syntax.Dialect -> String
dialectImportTemplate Syntax.DialectClojure = "(:require [{namespace}])"
dialectImportTemplate Syntax.DialectScheme = "(import ({namespace}))"
dialectImportTemplate Syntax.DialectCommonLisp = "(load \"{namespace}\")"
dialectImportTemplate Syntax.DialectEmacsLisp = "(require '{namespace})"

-- | Find necessary imports for a dialect
dialectFindImports :: Syntax.Dialect -> [String]
dialectFindImports Syntax.DialectClojure = []
dialectFindImports Syntax.DialectScheme = []
dialectFindImports Syntax.DialectCommonLisp = []
dialectFindImports Syntax.DialectEmacsLisp = []

-- | Generate test hierarchy for Lisp
generateLispTestGroupHierarchy :: Syntax.Dialect -> Graph -> TestCodec -> [String] -> TestGroup -> Either String String
generateLispTestGroupHierarchy dialect g codec groupPath testGroup = do
  -- Generate test cases at the current level
  testCaseLinesRaw <- mapM (generateLispTestCase dialect g codec groupPath) (testGroupCases testGroup)
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
      subgroupContent <- generateLispTestGroupHierarchy dialect g codec (groupPath ++ [groupName]) subgroup
      let header = ";; " ++ groupName
      return $ header ++ "\n\n" ++ subgroupContent

-- | Generate a single test case for Lisp
generateLispTestCase :: Syntax.Dialect -> Graph -> TestCodec -> [String] -> TestCaseWithMetadata -> Either String [String]
generateLispTestCase dialect g codec groupPath (TestCaseWithMetadata name tcase _ _) = case tcase of
  TestCaseDelegatedEvaluation (DelegatedEvaluationTestCase input output) -> do
    inputCode <- testCodecEncodeTerm codec input g
    outputCode <- testCodecEncodeTerm codec output g

    -- Include group path in test name to avoid collisions
    let fullName = if L.null groupPath
          then name
          else L.intercalate "-" (groupPath ++ [name])
        formattedName = testCodecFormatTestName codec fullName

    return $ case dialect of
      Syntax.DialectClojure -> [
        "(deftest " ++ formattedName,
        "  (is (= " ++ outputCode,
        "         " ++ inputCode ++ ")))"]
      Syntax.DialectScheme -> [
        "(define (" ++ formattedName ++ ")",
        "  (assert (equal? " ++ outputCode ++ " " ++ inputCode ++ ")))"]
      Syntax.DialectCommonLisp -> [
        "(defun " ++ formattedName ++ " ()",
        "  (assert (equal " ++ outputCode ++ " " ++ inputCode ++ ")))"]
      Syntax.DialectEmacsLisp -> [
        "(ert-deftest " ++ formattedName ++ " ()",
        "  (should (equal " ++ outputCode ++ " " ++ inputCode ++ ")))"]

  _ -> return []  -- Skip non-delegated tests

-- | Generate test file for a dialect
generateTestFileWithLispCodec :: Syntax.Dialect -> TestCodec -> Module -> TestGroup -> Graph -> Either String (FilePath, String)
generateTestFileWithLispCodec dialect codec testModule testGroup g = do
  -- Generate test hierarchy
  testBody <- generateLispTestGroupHierarchy dialect g codec [] testGroup

  -- Build the complete test module
  let testModuleContent = buildLispTestModule dialect codec testModule testGroup testBody

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
buildLispTestModule :: Syntax.Dialect -> TestCodec -> Module -> TestGroup -> String -> String
buildLispTestModule dialect codec testModule testGroup testBody = case dialect of
    Syntax.DialectClojure -> clojureModule
    Syntax.DialectScheme -> schemeModule
    Syntax.DialectCommonLisp -> commonLispModule
    Syntax.DialectEmacsLisp -> emacsLispModule
  where
    Namespace ns = moduleNamespace testModule
    parts = Strings.splitOn "." ns
    -- Build test namespace: drop leading "generation." if present, add "-test" suffix to last part
    testNs = L.intercalate "." (L.init parts ++ [L.last parts ++ "-test"])
    groupName = testGroupName testGroup

    clojureModule = unlines [
        ";; " ++ warningAutoGeneratedFile,
        ";; " ++ groupName,
        "",
        "(ns " ++ testNs,
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
        ";;; " ++ warningAutoGeneratedFile,
        ";;; " ++ groupName,
        "",
        "(require 'ert)",
        ""
      ] ++ testBody ++ "\n"

-- | Generate Lisp test file for a test group (with type inference)
generateLispTestFile :: Syntax.Dialect -> Module -> TestGroup -> Graph -> Either String (FilePath, String)
generateLispTestFile dialect testModule testGroup g = do
  -- Skip type inference for Lisp — the coder is dynamically typed
  -- and inference adds TermTypeApplication/TermAnnotated wrappers that interfere
  generateTestFileWithLispCodec dialect (lispTestCodec dialect) testModule testGroup g

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

-- | Lisp test generator parameterized by dialect
lispTestGenerator :: Syntax.Dialect -> TestGenerator ()
lispTestGenerator dialect = TestGenerator {
  testGeneratorNamespacesForModule = \_ _ -> Right $ Namespaces (Namespace "", ()) M.empty,
  testGeneratorCreateCodec = \_ -> lispTestCodec dialect,
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
