module Hydra.UnificationSpec where

import Hydra.Kernel
import Hydra.Unification
import Hydra.TestUtils
import qualified Hydra.Dsl.Types as Types

import qualified Test.Hspec as H
import qualified Data.Map as M


expectUnified :: [Constraint Kv] -> [(Name, Type Kv)] -> H.Expectation
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
        [(Types.variable "a", Types.variable "b")]
        [(Name "a", Types.variable "b")]

    H.it "Unify variable with literal type" $
      expectUnified
        [(Types.variable "a" :: Type Kv, Types.string)]
        [(Name "a", Types.string)]

spec :: H.Spec
spec = do
  checkIndividualConstraints
