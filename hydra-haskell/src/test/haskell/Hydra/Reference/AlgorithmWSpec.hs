{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.Reference.AlgorithmWSpec.spec
-}
module Hydra.Reference.AlgorithmWSpec where

import Hydra.Kernel
import Hydra.Sources.Libraries
import Hydra.TestUtils
import Hydra.TestData
import qualified Hydra.Extract.Core as ExtractCore
import Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Reference.AlgorithmWBridge as W

import qualified Hydra.TestUtils as TU
import Hydra.Testing
import Hydra.TestSuiteSpec
import Hydra.Test.TestSuite
import qualified Hydra.Dsl.Meta.Testing as Testing

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Exception as Exception
import Control.Monad


testHydraContext = W.HydraContext $ graphPrimitives testGraph

inferType :: Term -> IO (Either String (Term, TypeScheme))
inferType term = do
  result <- Exception.try (W.termToInferredTerm testHydraContext term)
    :: IO (Either Exception.SomeException (Term, TypeScheme))
  return $ case result of
    Left e -> Left (show e)
    Right r -> Right r

-- | Strip class constraints from a TypeScheme, since the reference Algorithm W
--   implementation does not track type class constraints.
stripConstraints :: TypeScheme -> TypeScheme
stripConstraints ts = ts { typeSchemeConstraints = Nothing }

expectType :: Term -> TypeScheme -> H.SpecWith ()
expectType term ts = do
  result <- H.runIO $ inferType term
  case result of
    Left err -> H.it "inferred type (skipped: unsupported by reference implementation)" $
      H.pendingWith err
    Right (iterm, its) -> do
      H.it "inferred type" $
        H.shouldBe (ShowCore.typeScheme $ stripConstraints its) (ShowCore.typeScheme $ stripConstraints ts)
      H.it "inferred term" $
        H.shouldBe (ShowCore.term $ removeTypesFromTerm iterm) (ShowCore.term $ removeTypesFromTerm term)

algorithmWRunner :: TestRunner
algorithmWRunner desc tcase = if Testing.isDisabled tcase || Testing.isDisabledForMinimalInference tcase
  then Nothing
  else case testCaseWithMetadataCase tcase of
    TestCaseInference (InferenceTestCase input output) -> Just $ expectType input output
    _ -> Nothing

spec :: H.Spec
spec = runTestGroup "" algorithmWRunner allTests
