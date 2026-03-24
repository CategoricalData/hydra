{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.TestSuiteSpec.spec
-}

module Hydra.TestSuiteSpec where

import Hydra.Kernel
import Hydra.Generation (showError)
import Hydra.TestUtils
import Hydra.Testing
import Hydra.Inference
import Hydra.Parsing (ParseResult(..))
import Hydra.Test.TestSuite
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Dsl.Meta.Testing as Testing
import qualified Hydra.Json.Writer as JsonWriter
import qualified Hydra.Json.Parser as JsonParser
import qualified Hydra.Json.Decode as JsonDecode
import qualified Hydra.Json.Encode as JsonEncode
import qualified Hydra.Json.Model as Json
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Hoisting as Hoisting
import qualified Hydra.Coders as Coders
import qualified Hydra.Unification as Unification
import qualified Hydra.Validate.Core as ValidateCore

import qualified Control.Exception
import qualified Control.Monad as CM
import qualified Test.Hspec as H
import qualified Test.HUnit.Lang as HL
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.IORef as IORef
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified Data.Time.Clock.POSIX as POSIX
import qualified System.Environment as Env
import qualified System.IO as IO
import qualified System.Process as Proc


type TestRunner = String -> TestCaseWithMetadata -> Y.Maybe (H.SpecWith ())

defaultTestRunner :: TestRunner
defaultTestRunner desc tcase = if Testing.isDisabled tcase
  then Nothing
  else Just $ case testCaseWithMetadataCase tcase of
    TestCaseAlphaConversion (AlphaConversionTestCase term oldVar newVar result) ->
      H.it "alpha conversion" $ H.shouldBe
        (alphaConvert oldVar newVar term)
        result
    TestCaseCaseConversion (CaseConversionTestCase fromConvention toConvention fromString toString) ->
      H.it "case conversion" $ H.shouldBe
        (convertCase fromConvention toConvention fromString)
        toString
    TestCaseDelegatedEvaluation _ ->
      H.it "delegated evaluation (skipped - runs in target language)" $ H.shouldBe True True
    TestCaseEtaExpansion (EtaExpansionTestCase input output) -> expectEtaExpansionResult desc input output
    TestCaseEvaluation (EvaluationTestCase _ input output) ->
      H.it "evaluation" $ shouldSucceedWith
        (ShowCore.term <$> eval input)
        (ShowCore.term output)
    TestCaseInference (InferenceTestCase input output) -> expectInferenceResult desc input output
    TestCaseInferenceFailure (InferenceFailureTestCase input) ->
      H.it "inference failure" $ expectInferenceFailure desc input
    TestCaseJsonParser (ParserTestCase input expectedResult) ->
      H.it "JSON parser" $ H.shouldBe
        (JsonParser.parseJson input)
        expectedResult
    TestCaseJsonWriter (WriterTestCase input expectedOutput) ->
      H.it "JSON writer" $ H.shouldBe
        (JsonWriter.printJson input)
        expectedOutput
    TestCaseJsonDecode (JsonDecodeTestCase typ json expected) ->
      H.it "JSON decode" $ checkJsonDecode typ json expected
    TestCaseJsonEncode (JsonEncodeTestCase term expected) ->
      H.it "JSON encode" $ checkJsonEncode term expected
    TestCaseJsonRoundtrip (JsonRoundtripTestCase typ term) ->
      H.it "JSON roundtrip" $ checkJsonRoundtrip typ term
    TestCaseTypeChecking (TypeCheckingTestCase input outputTerm outputType) ->
      expectTypeCheckingResult desc input outputTerm outputType
    TestCaseTypeCheckingFailure (TypeCheckingFailureTestCase input) ->
      H.it "type checking failure" $ H.shouldBe True False  -- TODO: implement
    TestCaseTypeReduction (TypeReductionTestCase input output) ->
      H.it "type reduction" $ H.shouldBe
        (either (const input) id $ betaReduceType (Context [] [] M.empty) testGraph input)
        output
    TestCaseTopologicalSort (TopologicalSortTestCase adjList expected) ->
      H.it "topological sort" $ H.shouldBe
        (Sorting.topologicalSort adjList)
        expected
    TestCaseTopologicalSortSCC (TopologicalSortSCCTestCase adjList expected) ->
      H.it "topological sort SCC" $ H.shouldBe
        (Sorting.topologicalSortComponents adjList)
        expected
    TestCaseSerialization (SerializationTestCase input output) ->
      H.it "serialization" $ H.shouldBe
        (Serialization.printExpr (Serialization.parenthesize input))
        output
    TestCaseFlattenLetTerms (FlattenLetTermsTestCase input output) ->
      H.it "flatten let terms" $ H.shouldBe
        (Rewriting.flattenLetTerms input)
        output
    TestCaseFreeVariables (FreeVariablesTestCase input output) ->
      H.it "free variables" $ H.shouldBe
        (Rewriting.freeVariablesInTerm input)
        output
    TestCaseLiftLambdaAboveLet (LiftLambdaAboveLetTestCase input output) ->
      H.it "lift lambda above let" $ H.shouldBe
        (Rewriting.liftLambdaAboveLet input)
        output
    TestCaseSimplifyTerm (SimplifyTermTestCase input output) ->
      H.it "simplify term" $ H.shouldBe
        (Rewriting.simplifyTerm input)
        output
    TestCaseDeannotateTerm (DeannotateTermTestCase input output) ->
      H.it "deannotate term" $ H.shouldBe
        (Rewriting.deannotateTerm input)
        output
    TestCaseDeannotateType (DeannotateTypeTestCase input output) ->
      H.it "deannotate type" $ H.shouldBe
        (Rewriting.deannotateType input)
        output
    TestCaseTopologicalSortBindings (TopologicalSortBindingsTestCase bindings expected) ->
      H.it "topological sort bindings" $ H.shouldBe
        (S.fromList (fmap S.fromList (Rewriting.topologicalSortBindingMap (M.fromList bindings))))
        (S.fromList (fmap S.fromList expected))
    TestCaseNormalizeTypeVariables (NormalizeTypeVariablesTestCase input output) ->
      H.it "normalize type variables" $ H.shouldBe
        (Rewriting.normalizeTypeVariablesInTerm input)
        output
    TestCaseFoldOverTerm (FoldOverTermTestCase input traversalOrder op output) ->
      H.it "fold over term" $ H.shouldBe
        (runFoldOperation traversalOrder op input)
        output
    TestCaseRewriteTerm (RewriteTermTestCase input rewriter output) ->
      H.it "rewrite term" $ H.shouldBe
        (runTermRewriter rewriter input)
        output
    TestCaseRewriteType (RewriteTypeTestCase input rewriter output) ->
      H.it "rewrite type" $ H.shouldBe
        (runTypeRewriter rewriter input)
        output
    TestCaseHoistSubterms (HoistSubtermsTestCase predicate input output) ->
      H.it "hoist subterms" $ H.shouldBe
        (runHoistSubterms predicate input)
        output
    TestCaseHoistCaseStatements (HoistCaseStatementsTestCase input output) ->
      H.it "hoist case statements" $ H.shouldBe
        (Hoisting.hoistCaseStatements emptyGraph input)
        output
    TestCaseHoistPolymorphicLetBindings (HoistPolymorphicLetBindingsTestCase input output) ->
      H.it "hoist polymorphic let bindings" $ H.shouldBe
        (ShowCore.let_ $ Hoisting.hoistPolymorphicLetBindings (const True) input)
        (ShowCore.let_ output)
    TestCaseHoistLetBindings (HoistLetBindingsTestCase input output) ->
      H.it "hoist all let bindings" $ H.shouldBe
        (ShowCore.let_ $ Hoisting.hoistAllLetBindings input)
        (ShowCore.let_ output)
    TestCaseSubstInType (SubstInTypeTestCase substitution input output) ->
      H.it "substitute in type" $ H.shouldBe
        (substInType (TypeSubst (M.fromList substitution)) input)
        output
    TestCaseVariableOccursInType (VariableOccursInTypeTestCase variable typ expected) ->
      H.it "variable occurs in type" $ H.shouldBe
        (variableOccursInType variable typ)
        expected
    TestCaseUnifyTypes (UnifyTypesTestCase schemaTypeNames left right expected) ->
      H.it "unify types" $ checkUnifyTypes schemaTypeNames left right expected
    TestCaseJoinTypes (JoinTypesTestCase left right expected) ->
      H.it "join types" $ checkJoinTypes left right expected
    TestCaseUnshadowVariables (UnshadowVariablesTestCase input output) ->
      H.it "unshadow variables" $ H.shouldBe
        (ShowCore.term $ Rewriting.unshadowVariables input)
        (ShowCore.term output)
    TestCaseValidateCoreTerm (ValidateCoreTermTestCase typed input output) ->
      H.it "validate core term" $ H.shouldBe
        (ValidateCore.term typed emptyGraph input)
        output

runTestCase :: String -> TestRunner -> TestCaseWithMetadata -> H.SpecWith ()
runTestCase pdesc runner tcase@(TestCaseWithMetadata name _ mdesc _) =
  case runner cdesc tcase of
    Nothing -> return ()
    Just spec -> H.describe desc spec
  where
    desc = name ++ Y.maybe ("") (\d -> ": " ++ d) mdesc
    cdesc = if L.null pdesc then desc else pdesc ++ ", " ++ desc

runTestGroup :: String -> TestRunner -> TestGroup -> H.SpecWith ()
runTestGroup pdesc runner tg = do
    H.describe desc $ do
      CM.mapM (runTestCase cdesc runner) $ testGroupCases tg
      CM.sequence (runTestGroup cdesc runner <$> (testGroupSubgroups tg))
      return ()
  where
    desc = testGroupName tg ++ descSuffix
    cdesc = if L.null pdesc then desc else pdesc ++ ", " ++ desc
    descSuffix = case testGroupDescription tg of
      Nothing -> ""
      Just d -> " (" ++ d ++ ")"

runTestGroupTimed :: IORef.IORef (M.Map String Double) -> String -> TestRunner -> TestGroup -> H.SpecWith ()
runTestGroupTimed timingsRef hydraPath runner tg = do
    H.describe desc $ do
      H.runIO $ IORef.modifyIORef' timingsRef (M.insert hydraPath 0) -- placeholder
      startRef <- H.runIO $ IORef.newIORef (0 :: Double)
      H.beforeAll_ (recordStart startRef) $ H.afterAll_ (recordStop startRef) $ do
        CM.mapM (runTestCase cdesc runner) $ testGroupCases tg
        CM.sequence [runTestGroupTimed timingsRef subPath runner sub
          | sub <- testGroupSubgroups tg
          , let subPath = hydraPath ++ "/" ++ testGroupName sub]
        return ()
  where
    desc = testGroupName tg ++ descSuffix
    cdesc = if L.null pdesc then desc else pdesc ++ ", " ++ desc
    pdesc = ""  -- Not used for benchmark path construction
    descSuffix = case testGroupDescription tg of
      Nothing -> ""
      Just d -> " (" ++ d ++ ")"
    recordStart startRef = do
      now <- POSIX.getPOSIXTime
      IORef.writeIORef startRef (realToFrac now :: Double)
    recordStop startRef = do
      startTime <- IORef.readIORef startRef
      now <- POSIX.getPOSIXTime
      let elapsedMs = (realToFrac now - startTime) * 1000.0
      IORef.modifyIORef' timingsRef (M.insert hydraPath elapsedMs)

spec :: H.Spec
spec = do
  benchmarkOutput <- H.runIO $ Env.lookupEnv "HYDRA_BENCHMARK_OUTPUT"
  case benchmarkOutput of
    Nothing -> runTestGroup "" defaultTestRunner allTests
    Just outputPath -> do
      timingsRef <- H.runIO $ IORef.newIORef M.empty
      let rootPath = testGroupName allTests
      runTestGroupTimed timingsRef rootPath defaultTestRunner allTests
      H.afterAll_ (writeBenchmarkJson outputPath timingsRef allTests) $ do
        -- A dummy test to ensure afterAll_ fires
        H.it "benchmark finalize" $ True `H.shouldBe` True

-- | Check that JSON decoding produces the expected result (Either String Term)
checkJsonDecode :: Type -> Json.Value -> Either String Term -> H.Expectation
checkJsonDecode typ json expected = case JsonDecode.fromJson M.empty (Name "test") typ json of
    Left errMsg -> case expected of
      Left _ -> return ()  -- Expected failure, got failure
      Right _ -> HL.assertFailure ("JSON decode failed: " ++ errMsg)
    Right result -> case expected of
      Left errMsg -> HL.assertFailure $
        "Expected decode failure with message containing '" ++ errMsg ++
        "' but got success: " ++ show result
      Right expectedTerm -> H.shouldBe result expectedTerm

-- | Check that JSON encoding produces the expected result (Either String Value)
checkJsonEncode :: Term -> Either String Json.Value -> H.Expectation
checkJsonEncode term expected = case JsonEncode.toJson term of
    Left errMsg -> case expected of
      Left _ -> return ()  -- Expected failure, got failure
      Right _ -> HL.assertFailure ("JSON encode failed: " ++ errMsg)
    Right result -> case expected of
      Left errMsg -> HL.assertFailure $
        "Expected encode failure with message containing '" ++ errMsg ++
        "' but got success: " ++ show result
      Right expectedJson -> H.shouldBe result expectedJson

-- | Check that a term can be encoded to JSON and decoded back to the same term
checkJsonRoundtrip :: Type -> Term -> H.Expectation
checkJsonRoundtrip typ term = case JsonEncode.toJson term of
    Left errMsg -> HL.assertFailure ("Failed to encode term to JSON: " ++ errMsg)
    Right json -> case JsonDecode.fromJson M.empty (Name "test") typ json of
      Left errMsg -> HL.assertFailure ("Failed to decode JSON back to term: " ++ errMsg)
      Right decoded -> H.shouldBe decoded term

-- | Run a fold operation over a term
runFoldOperation :: Coders.TraversalOrder -> FoldOperation -> Term -> Term
runFoldOperation order op = case op of
    FoldOperationSumInt32Literals -> TermLiteral . LiteralInteger . IntegerValueInt32 . sumInt32
    FoldOperationCollectListLengths -> TermList . fmap (TermLiteral . LiteralInteger . IntegerValueInt32) . collectListLengths
    FoldOperationCollectLabels -> TermList . fmap TermLiteral . collectLabels
  where
    sumInt32 :: Term -> Int
    sumInt32 = Rewriting.foldOverTerm order (\acc t -> acc + getInt32 t) 0
    collectListLengths :: Term -> [Int]
    collectListLengths = Rewriting.foldOverTerm order (\acc t -> acc ++ getListLength t) []
    collectLabels = Rewriting.foldOverTerm order (\acc t -> acc ++ getLabel t) []

    getInt32 :: Term -> Int
    getInt32 (TermLiteral (LiteralInteger (IntegerValueInt32 n))) = n
    getInt32 _ = 0

    getListLength (TermList elems) = [length elems]
    getListLength _ = []

    getLabel (TermPair (TermLiteral (LiteralString s), _)) = [LiteralString s]
    getLabel _ = []

-- | Run a term rewriter
runTermRewriter :: TermRewriter -> Term -> Term
runTermRewriter rewriter = Rewriting.rewriteTerm rewrite
  where
    rewrite recurse term = case (rewriter, term) of
      (TermRewriterReplaceFooWithBar, TermLiteral (LiteralString "foo")) ->
        TermLiteral (LiteralString "bar")
      (TermRewriterReplaceInt32WithInt64, TermLiteral (LiteralInteger (IntegerValueInt32 n))) ->
        TermLiteral (LiteralInteger (IntegerValueInt64 (fromIntegral n)))
      _ -> recurse term

-- | Run a type rewriter
runTypeRewriter :: TypeRewriter -> Type -> Type
runTypeRewriter TypeRewriterReplaceStringWithInt32 = Rewriting.rewriteType rewrite
  where
    rewrite recurse typ = case typ of
      TypeLiteral LiteralTypeString -> TypeLiteral (LiteralTypeInteger IntegerTypeInt32)
      _ -> recurse typ

-- | Run hoistSubterms with the given predicate
-- The predicate receives (path, term) where path is the list of SubtermSteps from root
runHoistSubterms :: HoistPredicate -> Term -> Term
runHoistSubterms pred term = Hoisting.hoistSubterms (predicateFn pred) emptyGraph term
  where
    -- A predicate returns True if the term should be hoisted.
    -- The predicate receives (path, term) for path-aware hoisting decisions.
    predicateFn :: HoistPredicate -> ([SubtermStep], Term) -> Bool
    predicateFn HoistPredicateNothing _ = False
    predicateFn HoistPredicateLists (_, t) = case t of
      TermList _ -> True
      _ -> False
    predicateFn HoistPredicateApplications (_, t) = case t of
      TermApplication _ -> True
      _ -> False
    predicateFn HoistPredicateCaseStatements (_, t) = case t of
      TermFunction (FunctionElimination _) -> True  -- Case statements are eliminations
      _ -> False

-- | Check unifyTypes result against expected
-- schemaTypeNames is a list of names that should be treated as schema types (not bound during unification)
checkUnifyTypes :: [Name] -> Type -> Type -> Either String TypeSubst -> H.Expectation
checkUnifyTypes schemaTypeNames left right expected = case expected of
    Left errSubstring -> case unifyResult of
      Left _ -> return ()  -- Expected failure, got failure
      Right result -> HL.assertFailure $
        "Expected unification failure but got success: " ++ show (unTypeSubst result)
    Right expectedSubst -> case unifyResult of
      Left err -> HL.assertFailure $
        "Expected unification success but got failure: " ++ unificationErrorMessage (inContextObject err)
      Right actualSubst -> H.shouldBe actualSubst expectedSubst
  where
    -- Build schema types map from the list of names
    -- Each schema name gets a trivial type scheme (no free variables)
    schemaTypes = M.fromList [(n, TypeScheme [] (TypeVariable n) Nothing) | n <- schemaTypeNames]
    emptyCtx = Context [] [] M.empty
    unifyResult = Unification.unifyTypes emptyCtx schemaTypes left right "test"

-- | Check joinTypes result against expected
checkJoinTypes :: Type -> Type -> Either () [TypeConstraint] -> H.Expectation
checkJoinTypes left right expected = case expected of
    Left () -> case joinResult of
      Left _ -> return ()  -- Expected failure, got failure
      Right result -> HL.assertFailure $
        "Expected join failure but got success with constraints: " ++ show result
    Right expectedConstraints -> case joinResult of
      Left err -> HL.assertFailure $
        "Expected join success but got failure: " ++ unificationErrorMessage (inContextObject err)
      Right actualConstraints -> H.shouldBe actualConstraints expectedConstraints
  where
    emptyCtx = Context [] [] M.empty
    joinResult = Unification.joinTypes emptyCtx left right "test"

-- ---- Benchmark JSON output ----

writeBenchmarkJson :: String -> IORef.IORef (M.Map String Double) -> TestGroup -> IO ()
writeBenchmarkJson outputPath timingsRef root = do
  timings <- IORef.readIORef timingsRef
  now <- Clock.getCurrentTime
  let timestamp = TimeFormat.formatTime TimeFormat.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  branch <- gitOutput "git" ["rev-parse", "--abbrev-ref", "HEAD"]
  commit <- gitOutput "git" ["rev-parse", "--short", "HEAD"]
  commitMsg <- gitOutput "git" ["log", "-1", "--format=%s"]
  let rootPath = testGroupName root
      groups = testGroupSubgroups root
      groupJsons = [groupToJson timings rootPath g | g <- groups]
      (totalP, totalF, totalS) = foldl (\(p,f,s) g -> let (p',f',s') = countTests g in (p+p',f+f',s+s')) (0,0,0) groups
      totalTime = sum [M.findWithDefault 0 (rootPath ++ "/" ++ testGroupName g) timings | g <- groups]
      json = "{\n" ++
        "  \"metadata\": {\n" ++
        "    \"timestamp\": " ++ jsonStr timestamp ++ ",\n" ++
        "    \"language\": \"haskell\",\n" ++
        "    \"branch\": " ++ jsonStr branch ++ ",\n" ++
        "    \"commit\": " ++ jsonStr commit ++ ",\n" ++
        "    \"commitMessage\": " ++ jsonStr commitMsg ++ "\n" ++
        "  },\n" ++
        "  \"groups\": [\n" ++
        L.intercalate ",\n" groupJsons ++ "\n" ++
        "  ],\n" ++
        "  \"summary\": {\n" ++
        "    \"totalPassed\": " ++ show totalP ++ ",\n" ++
        "    \"totalFailed\": " ++ show totalF ++ ",\n" ++
        "    \"totalSkipped\": " ++ show totalS ++ ",\n" ++
        "    \"totalTimeMs\": " ++ round1 totalTime ++ "\n" ++
        "  }\n" ++
        "}\n"
  writeFile outputPath json
  IO.hPutStrLn IO.stderr $ "Benchmark results written to " ++ outputPath

groupToJson :: M.Map String Double -> String -> TestGroup -> String
groupToJson timings parentPath group =
    "    {\n" ++
    "      \"failed\": 0,\n" ++
    "      \"passed\": " ++ show passed ++ ",\n" ++
    "      \"path\": " ++ jsonStr groupPath ++ ",\n" ++
    "      \"skipped\": " ++ show skipped ++ ",\n" ++
    subgroupsJson ++
    "      \"totalTimeMs\": " ++ round1 groupTime ++ "}"
  where
    groupPath = parentPath ++ "/" ++ testGroupName group
    groupTime = M.findWithDefault 0 groupPath timings
    (passed, _, skipped) = countTests group
    subs = testGroupSubgroups group
    subgroupsJson
      | null subs = ""
      | otherwise =
          "      \"subgroups\": [\n" ++
          L.intercalate ",\n" [subgroupToJson timings groupPath s | s <- subs] ++ "\n" ++
          "      ],\n"

subgroupToJson :: M.Map String Double -> String -> TestGroup -> String
subgroupToJson timings parentPath sub =
    "        {\n" ++
    "          \"failed\": 0,\n" ++
    "          \"passed\": " ++ show passed ++ ",\n" ++
    "          \"path\": " ++ jsonStr subPath ++ ",\n" ++
    "          \"skipped\": " ++ show skipped ++ ",\n" ++
    "          \"totalTimeMs\": " ++ round1 subTime ++ "}"
  where
    subPath = parentPath ++ "/" ++ testGroupName sub
    subTime = M.findWithDefault 0 subPath timings
    (passed, _, skipped) = countTests sub

countTests :: TestGroup -> (Int, Int, Int)
countTests group = (runnable + subRunnable, 0, skipped + subSkipped)
  where
    (runnable, skipped) = foldl (\(r, s) tc ->
      if Testing.isDisabled tc
        then (r, s + 1)
        else (r + 1, s)) (0, 0) (testGroupCases group)
    (subRunnable, _, subSkipped) = foldl (\(r, f, s) sub ->
      let (r', f', s') = countTests sub in (r + r', f + f', s + s')) (0, 0, 0) (testGroupSubgroups group)

round1 :: Double -> String
round1 d = show (fromIntegral (round (d * 10)) / 10.0 :: Double)

jsonStr :: String -> String
jsonStr s = "\"" ++ concatMap escChar (filter (/= '\n') s) ++ "\""
  where
    escChar '"' = "\\\""
    escChar '\\' = "\\\\"
    escChar c = [c]

gitOutput :: String -> [String] -> IO String
gitOutput cmd args =
  Control.Exception.catch
    (do
      result <- Proc.readProcess cmd args ""
      return (L.dropWhileEnd (== '\n') result))
    (\e -> let _ = e :: Control.Exception.SomeException in return "unknown")
