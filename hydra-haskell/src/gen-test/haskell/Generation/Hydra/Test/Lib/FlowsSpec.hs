-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.flows"},ModuleName {unModuleName = "Flows"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.compute"},ModuleName {unModuleName = "Compute"}),(Namespace {unNamespace = "hydra.lexical"},ModuleName {unModuleName = "Lexical"}),(Namespace {unNamespace = "hydra.lib.flows"},ModuleName {unModuleName = "Flows"}),(Namespace {unNamespace = "hydra.lib.maps"},ModuleName {unModuleName = "Maps"}),(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.lib.sets"},ModuleName {unModuleName = "Sets"}),(Namespace {unNamespace = "hydra.monads"},ModuleName {unModuleName = "Monads"})]

module Generation.Hydra.Test.Lib.FlowsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Compute as Compute
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Monads as Monads

spec :: H.Spec
spec = H.describe "hydra.lib.flows primitives" $ do
  H.describe "apply" $ do
    H.it "apply add" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure (Math.add 3)) (\f -> Monads.bind (Monads.pure 5) (\x -> Monads.pure (f x)))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just 8)
  H.describe "bind" $ do
    H.it "bind add" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure 5) (\n -> Monads.pure (Math.add n 5))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just 10)
    H.it "bind multiply" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure 3) (\n -> Monads.pure (Math.mul n 4))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just 12)
  H.describe "fail" $ do
    H.it "fail with message" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.fail "test error message") Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Nothing :: Maybe Int)
  H.describe "foldl" $ do
    H.it "foldl sum" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure 0) (\a0 -> Monads.bind (Monads.pure (Math.add a0 1)) (\a1 -> Monads.bind (Monads.pure (Math.add a1 2)) (\a2 -> Monads.pure (Math.add a2 3))))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just 6)
  H.describe "map" $ do
    H.it "map negate" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Flows.map Math.negate (Monads.pure 5)) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just (-5))
    H.it "map abs" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Flows.map Math.abs (Monads.pure (-3))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just 3)
  H.describe "mapElems" $ do
    H.it "mapElems add one" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure (Math.add 1 1)) (\v1 -> Monads.bind (Monads.pure (Math.add 2 1)) (\v2 -> Monads.pure (Maps.fromList [
          ("a", v1),
          ("b", v2)])))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just (M.fromList [
          ("a", 2),
          ("b", 3)]))
  H.describe "mapKeys" $ do
    H.it "mapKeys add one" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure (Math.add 1 1)) (\k1 -> Monads.bind (Monads.pure (Math.add 2 1)) (\k2 -> Monads.pure (Maps.fromList [
          (k1, "a"),
          (k2, "b")])))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just (M.fromList [
          (2, "a"),
          (3, "b")]))
  H.describe "mapList" $ do
    H.it "mapList add one" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure (Math.add 1 1)) (\y1 -> Monads.bind (Monads.pure (Math.add 2 1)) (\y2 -> Monads.bind (Monads.pure (Math.add 3 1)) (\y3 -> Monads.pure [
          y1,
          y2,
          y3])))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just [
          2,
          3,
          4])
  H.describe "mapMaybe" $ do
    H.it "mapMaybe just" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure (Math.add 5 1)) (\y -> Monads.pure (Just y))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just (Just 6))
    H.it "mapMaybe nothing" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.pure Nothing) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just Nothing :: Maybe (Maybe Int))
  H.describe "mapSet" $ do
    H.it "mapSet add one" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure (Math.add 1 1)) (\y1 -> Monads.bind (Monads.pure (Math.add 2 1)) (\y2 -> Monads.bind (Monads.pure (Math.add 3 1)) (\y3 -> Monads.pure (Sets.fromList [
          y1,
          y2,
          y3]))))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just (S.fromList [
          2,
          3,
          4]))
  H.describe "pure" $ do
    H.it "pure integer" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.pure 42) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just 42)
    H.it "pure zero" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.pure 0) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just 0)
    H.it "pure negative" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.pure (-5)) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just (-5))
    H.it "pure string" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.pure "hello") Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just "hello")
  H.describe "sequence" $ do
    H.it "sequence pure list" $ H.shouldBe
      (Compute.flowStateValue (Compute.unFlow (Monads.bind (Monads.pure 1) (\x1 -> Monads.bind (Monads.pure 2) (\x2 -> Monads.bind (Monads.pure 3) (\x3 -> Monads.pure [
          x1,
          x2,
          x3])))) Lexical.emptyGraph (Compute.Trace {
          Compute.traceStack = [],
          Compute.traceMessages = [],
          Compute.traceOther = M.empty})))
      (Just [
          1,
          2,
          3])
