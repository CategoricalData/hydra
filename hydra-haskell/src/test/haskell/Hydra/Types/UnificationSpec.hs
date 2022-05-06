module Hydra.Types.UnificationSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.Basics
import Hydra.Types.Unification
import Hydra.TestUtils
import Hydra.Impl.Haskell.Sources.Adapters.Utils
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.Map as M
import qualified Data.Set as S


expectUnified :: [Constraint Meta] -> [(TypeVariable, Type Meta)] -> H.Expectation
expectUnified constraints subst = H.shouldBe
  (solveConstraints testContext constraints)
  (Right $ M.fromList subst)

checkIndividualConstraints :: H.SpecWith ()
checkIndividualConstraints = do
  H.describe "Check a few hand-crafted constraints" $ do

    H.it "Unify nothing" $
      expectUnified [] []

    H.it "Unify variable with variable" $
      expectUnified
        [(Types.variable "a", Types.variable "b")]
        [("a", Types.variable "b")]

    H.it "Unify variable with literal type" $
      expectUnified
        [(Types.variable "a" :: Type Meta, Types.string)]
        [("a", Types.string)]

spec :: H.Spec
spec = do
  checkIndividualConstraints
