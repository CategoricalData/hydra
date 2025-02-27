module Hydra.Inference.UnificationSpec where

import Hydra.Kernel
import Hydra.Staging.Inference.Unification
import Hydra.TestUtils
import qualified Hydra.Dsl.Types as Types

import qualified Test.Hspec as H
import qualified Data.Map as M


expectUnified :: [TypeConstraint] -> [(Name, Type)] -> H.Expectation
expectUnified constraints subst = shouldSucceedWith
  (solveConstraints constraints)
  (M.fromList subst)

checkIndividualConstraints :: H.SpecWith ()
checkIndividualConstraints = do
  H.describe "Check a few hand-crafted constraints" $ do

    H.it "Unify nothing" $
      expectUnified [] []

    H.it "Unify variable with variable" $
      expectUnified
        [TypeConstraint (Types.var "a") (Types.var "b") Nothing]
        [(Name "b", Types.var "a")]

    H.it "Unify variable with literal type" $
      expectUnified
        [TypeConstraint (Types.var "a") Types.string Nothing]
        [(Name "a", Types.string)]

spec :: H.Spec
spec = do
  checkIndividualConstraints
