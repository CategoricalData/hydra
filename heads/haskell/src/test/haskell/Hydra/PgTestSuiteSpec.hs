{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.PgTestSuiteSpec.spec
-}

-- | Package-scoped counterpart to Hydra.TestSuiteSpec (hydra-kernel) and Hydra.BuildTestSuiteSpec
-- (hydra-build); runs hydra-pg's own test groups. hspec-discover picks this up automatically
-- alongside the kernel and build specs, so no composition/registration step is needed.
--
-- All of hydra-pg's test cases (as of #738) are universal (pure string comparison), so this spec
-- only needs the universal branch of TestRunner, mirroring Hydra.BuildTestSuiteSpec.

module Hydra.PgTestSuiteSpec where

import Hydra.Core.Testing
import Hydra.Pg.Test.TestSuite
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing as Testing

import qualified Control.Monad as CM
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Maybe as Y

defaultTestRunner :: String -> TestCaseWithMetadata -> Y.Maybe (H.SpecWith ())
defaultTestRunner _ tcase = if Testing.isDisabled tcase
  then Nothing
  else Just $ case testCaseWithMetadataCase tcase of
    TestCaseUniversal (UniversalTestCase actual expected) ->
      H.it "universal" $ H.shouldBe (actual ()) (expected ())

runTestCase :: String -> (String -> TestCaseWithMetadata -> Y.Maybe (H.SpecWith ())) -> TestCaseWithMetadata -> H.SpecWith ()
runTestCase pdesc runner tcase@(TestCaseWithMetadata name _ mdesc _ _) =
  case runner cdesc tcase of
    Nothing -> return ()
    Just spec_ -> H.describe desc spec_
  where
    desc = name ++ Y.maybe ("") (\d -> ": " ++ d) mdesc
    cdesc = if L.null pdesc then desc else pdesc ++ ", " ++ desc

runTestGroup :: String -> (String -> TestCaseWithMetadata -> Y.Maybe (H.SpecWith ())) -> TestGroup -> H.SpecWith ()
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

spec :: H.Spec
spec = runTestGroup "" defaultTestRunner allTests
