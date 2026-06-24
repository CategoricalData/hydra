{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.TestSuiteSpec.spec
-}

module Hydra.TestSuiteSpec where

import Hydra.Testing
import Hydra.Test.TestSuite
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing as Testing

import qualified Control.Monad as CM
import qualified Test.Hspec as H
import qualified Data.IORef as IORef
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Time.Clock.POSIX as POSIX
import qualified System.Environment as Env
import qualified System.Directory as Dir

-- | Canonical root directory for effectful (file I/O) test cases. Must match the testDir constant
-- in Hydra.Sources.Test.Lib.Files. Hard-coded *nix path for now (configurable later, #494).
effectfulTestDir :: String
effectfulTestDir = "/tmp/hydra-testing"

-- | Prepare a guaranteed-empty canonical temp directory before an effectful test case that needs it.
-- Currently prepares unconditionally for every effectful case (never for universal cases). A future
-- refinement (#494) is to skip preparation for pure-effect cases whose term references no
-- hydra.lib.files primitive; that scan happens at test-generation time, not here in compiled code.
prepareEffectfulTempDir :: TestCaseWithMetadata -> IO ()
prepareEffectfulTempDir _ = do
  exists <- Dir.doesDirectoryExist effectfulTestDir
  CM.when exists $ Dir.removeDirectoryRecursive effectfulTestDir
  Dir.createDirectoryIfMissing True effectfulTestDir


type TestRunner = String -> TestCaseWithMetadata -> Y.Maybe (H.SpecWith ())

defaultTestRunner :: TestRunner
defaultTestRunner desc tcase = if Testing.isDisabled tcase
  then Nothing
  else Just $ case testCaseWithMetadataCase tcase of
    TestCaseUniversal (UniversalTestCase actual expected) ->
      H.it "universal" $ H.shouldBe (actual ()) (expected ())
    -- Effectful cases: 'actual' is a thunk producing an effect (mapped to IO String by the
    -- Haskell coder). Prepare the canonical temp directory iff the effect references a
    -- hydra.lib.files primitive, then execute the effect and compare to 'expected'. See
    -- docs/test-suite-architecture.md and Hydra.Effects.Testing.
    TestCaseEffectful (EffectfulTestCase actual expected) ->
      H.it "effectful" $ do
        prepareEffectfulTempDir tcase
        result <- actual ()
        H.shouldBe result (expected ())

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
