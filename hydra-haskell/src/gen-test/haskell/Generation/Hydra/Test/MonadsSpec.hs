-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.monads"},ModuleName {unModuleName = "Monads"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.compute"},ModuleName {unModuleName = "Compute"}),(Namespace {unNamespace = "hydra.lexical"},ModuleName {unModuleName = "Lexical"}),(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.monads"},ModuleName {unModuleName = "Monads"})]

module Generation.Hydra.Test.MonadsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Compute as Compute
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Monads as Monads

spec :: H.Spec
spec = H.describe "monads" $ do
  H.describe "pure" $ do
    H.it "integer" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.pure 42) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just 42)
    H.it "string" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.pure "hello") Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just "hello")
  H.describe "map" $ do
    H.it "map negate" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.map Math.negate (Monads.pure 5)) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just (-5))
    H.it "map absolute" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.map Math.abs (Monads.pure (-3))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just 3)
  H.describe "bind" $ do
    H.it "bind add" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure 10) (\n -> Monads.pure (Math.add n 5))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just 15)
    H.it "bind multiply" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure 3) (\n -> Monads.pure (Math.mul n 4))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just 12)
  H.describe "error traces" $ do
    H.it "Error traces are in the right order" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.withTrace "one" (Monads.withTrace "two" (Monads.fail "oops"))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Nothing :: Maybe Int)
