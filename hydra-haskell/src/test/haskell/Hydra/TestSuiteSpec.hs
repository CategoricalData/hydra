{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.TestSuiteSpec.spec
-}

module Hydra.TestSuiteSpec where

import Hydra.Testing
import Hydra.Test.TestSuite
import qualified Hydra.Dsl.Meta.Testing as Testing

import qualified Control.Monad as CM
import qualified Test.Hspec as H
import qualified Data.IORef as IORef
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Time.Clock.POSIX as POSIX
import qualified System.Environment as Env


type TestRunner = String -> TestCaseWithMetadata -> Y.Maybe (H.SpecWith ())

defaultTestRunner :: TestRunner
defaultTestRunner desc tcase = if Testing.isDisabled tcase
  then Nothing
  else Just $ case testCaseWithMetadataCase tcase of
    TestCaseUniversal (UniversalTestCase actual expected) ->
      H.it "universal" $ H.shouldBe actual expected

runTestCase :: String -> TestRunner -> TestCaseWithMetadata -> H.SpecWith ()
runTestCase pdesc runner tcase@(TestCaseWithMetadata name _ mdesc _) =
  case runner cdesc tcase of
    Nothing -> return ()
    Just spec -> H.describe desc spec
  where
    desc = name ++ Y.maybe ("") (\d -> ": " ++ d) mdesc
    cdesc = if L.null pdesc then desc else pdesc ++ ", " ++ desc

runTestGroup :: String -> TestRunner -> TestGroup -> H.SpecWith ()
runTestGroup pdesc runner tg = H.describe desc $ do
    CM.mapM (runTestCase cdesc runner) $ testGroupCases tg
    CM.sequence [runTestGroup cdesc runner sub | sub <- testGroupSubgroups tg]
    return ()
  where
    desc = testGroupName tg ++ descSuffix
    cdesc = if L.null pdesc then desc else pdesc ++ ", " ++ desc
    descSuffix = case testGroupDescription tg of
      Nothing -> ""
      Just d -> " (" ++ d ++ ")"

-- | Timed variant for benchmark output
runTestGroupTimed :: IORef.IORef (M.Map String Double) -> String -> TestRunner -> TestGroup -> H.SpecWith ()
runTestGroupTimed timingsRef hydraPath runner tg = do
    H.describe desc $ do
      H.runIO $ IORef.modifyIORef' timingsRef (M.insert hydraPath 0)
      startRef <- H.runIO $ IORef.newIORef (0 :: Double)
      H.beforeAll_ (recordStart startRef) $ H.afterAll_ (recordStop startRef) $ do
        CM.mapM (runTestCase cdesc runner) $ testGroupCases tg
        CM.sequence [runTestGroupTimed timingsRef subPath runner sub
          | sub <- testGroupSubgroups tg
          , let subPath = hydraPath ++ "/" ++ testGroupName sub]
        return ()
  where
    desc = testGroupName tg ++ descSuffix
    cdesc = ""
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

-- | Test runner that uses eval-mode primitives.
-- Only runs evaluation test cases; skips all other test types.
evalTestRunner :: TestRunner
evalTestRunner desc tcase = if Testing.isDisabled tcase
  then Nothing
  else case testCaseWithMetadataCase tcase of
    TestCaseEvaluation (EvaluationTestCase _ input output) ->
      Just $ H.it "eval-mode evaluation" $ shouldSucceedWith
        (ShowCore.term <$> evalEval input)
        (ShowCore.term output)
    _ -> Nothing  -- Only test evaluation cases in eval mode

-- | Filter a test group to only include lib primitive tests (hydra.lib.*)
filterLibTests :: TestGroup -> TestGroup
filterLibTests tg = tg {
  testGroupSubgroups = filterLibTests <$> testGroupSubgroups tg,
  testGroupCases = testGroupCases tg}

spec :: H.Spec
spec = do
  benchmarkOutput <- H.runIO $ Env.lookupEnv "HYDRA_BENCHMARK_OUTPUT"
  case benchmarkOutput of
    Nothing -> do
      runTestGroup "" defaultTestRunner allTests
      H.describe "eval-mode primitives" $
        runTestGroup "" evalTestRunner allTests
    Just outputPath -> do
      timingsRef <- H.runIO $ IORef.newIORef M.empty
      let rootPath = testGroupName allTests
      runTestGroupTimed timingsRef rootPath defaultTestRunner allTests
      H.afterAll_ (writeBenchmarkJson outputPath timingsRef allTests) $ do
        H.it "benchmark finalize" $ True `H.shouldBe` True

-- | Write benchmark JSON
writeBenchmarkJson :: String -> IORef.IORef (M.Map String Double) -> TestGroup -> IO ()
writeBenchmarkJson outputPath timingsRef tg = do
    timings <- IORef.readIORef timingsRef
    let json = renderBenchmarkJson timings tg
    writeFile outputPath json

renderBenchmarkJson :: M.Map String Double -> TestGroup -> String
renderBenchmarkJson timings tg =
    "{\n" ++
    "  \"metadata\": {\n" ++
    "    \"language\": \"haskell\"\n" ++
    "  },\n" ++
    "  \"groups\": [\n" ++
    renderGroup "    " (testGroupName tg) tg ++ "\n" ++
    "  ],\n" ++
    "  \"summary\": {\n" ++
    "    \"totalPassed\": " ++ show totalPassed ++ ",\n" ++
    "    \"totalFailed\": 0,\n" ++
    "    \"totalSkipped\": " ++ show totalSkipped ++ ",\n" ++
    "    \"totalTimeMs\": " ++ show totalTime ++ "\n" ++
    "  }\n" ++
    "}"
  where
    totalTime = Y.fromMaybe 0 $ M.lookup (testGroupName tg) timings
    totalPassed = countCases False tg
    totalSkipped = countCases True tg
    countCases wantSkipped g =
      length [() | c <- testGroupCases g, Testing.isDisabled c == wantSkipped] +
      sum [countCases wantSkipped sub | sub <- testGroupSubgroups g]
    renderGroup indent path g =
      indent ++ "{\n" ++
      indent ++ "  \"name\": " ++ show (testGroupName g) ++ ",\n" ++
      indent ++ "  \"time_ms\": " ++ show (Y.fromMaybe 0 $ M.lookup path timings) ++ ",\n" ++
      indent ++ "  \"subgroups\": [" ++
      (if null (testGroupSubgroups g) then "]" else
        "\n" ++ L.intercalate ",\n" [renderGroup (indent ++ "    ") (path ++ "/" ++ testGroupName sub) sub | sub <- testGroupSubgroups g] ++
        "\n" ++ indent ++ "  ]") ++
      "\n" ++ indent ++ "}"
