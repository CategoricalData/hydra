module Hydra.Types.UnificationSpec where

import Hydra.Core
import Hydra.Types.Unification
import Hydra.TestUtils
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Test.Hspec as H
import qualified Data.Map as M


expectUnified :: [Constraint Meta] -> [(VariableType, Type Meta)] -> H.Expectation
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
        [(VariableType "a", Types.variable "b")]

    H.it "Unify variable with literal type" $
      expectUnified
        [(Types.variable "a" :: Type Meta, Types.string)]
        [(VariableType "a", Types.string)]

spec :: H.Spec
spec = do
  checkIndividualConstraints
