-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.maybes"},ModuleName {unModuleName = "Maybes"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.equality"},ModuleName {unModuleName = "Equality"}),(Namespace {unNamespace = "hydra.lib.logic"},ModuleName {unModuleName = "Logic"}),(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.lib.maybes"},ModuleName {unModuleName = "Maybes"})]

module Generation.Hydra.Test.Lib.MaybesSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes

spec :: H.Spec
spec = H.describe "hydra.lib.maybes primitives" $ do
  H.describe "apply" $ do
    H.it "both just" $ H.shouldBe
      (Maybes.apply (Just (Math.add 3)) (Just 5))
      (Just 8)
    H.it "nothing function" $ H.shouldBe
      (Maybes.apply Nothing (Just 5))
      (Nothing :: Maybe Int)
    H.it "nothing value" $ H.shouldBe
      (Maybes.apply (Just (Math.add 3)) Nothing)
      (Nothing)
  H.describe "bind" $ do
    H.it "just to just" $ H.shouldBe
      (Maybes.bind (Just 5) (\x -> Just (Math.mul x 2)))
      (Just 10)
    H.it "nothing to nothing" $ H.shouldBe
      (Maybes.bind Nothing (\x -> Just (Math.mul x 2)))
      (Nothing)
  H.describe "cases" $ do
    H.it "just applies function" $ H.shouldBe
      (Maybes.cases (Just 5) 0 (\x -> Math.mul x 2))
      (10)
    H.it "nothing returns default" $ H.shouldBe
      (Maybes.cases Nothing 99 (\x -> Math.mul x 2))
      (99)
  H.describe "cat" $ do
    H.it "filters nothings" $ H.shouldBe
      (Maybes.cat [
          Just 1,
          Nothing,
          (Just 2)])
      ([
          1,
          2])
    H.it "all justs" $ H.shouldBe
      (Maybes.cat [
          Just 1,
          (Just 2)])
      ([
          1,
          2])
    H.it "all nothings" $ H.shouldBe
      (Maybes.cat [
          Nothing,
          Nothing])
      ([] :: [Int])
    H.it "empty list" $ H.shouldBe
      (Maybes.cat [])
      ([] :: [Int])
  H.describe "compose" $ do
    H.it "both succeed" $ H.shouldBe
      (Maybes.compose (\x -> Logic.ifElse (Equality.lte x 5) (Just (Math.add x 1)) Nothing) (\y -> Logic.ifElse (Equality.gte y 5) (Just (Math.mul y 2)) Nothing) 5)
      (Just 12)
    H.it "first fails" $ H.shouldBe
      (Maybes.compose (\x -> Logic.ifElse (Equality.lte x 5) (Just (Math.add x 1)) Nothing) (\y -> Logic.ifElse (Equality.gte y 5) (Just (Math.mul y 2)) Nothing) 10)
      (Nothing)
    H.it "second fails" $ H.shouldBe
      (Maybes.compose (\x -> Logic.ifElse (Equality.lte x 5) (Just (Math.add x 1)) Nothing) (\y -> Logic.ifElse (Equality.gte y 5) (Just (Math.mul y 2)) Nothing) 3)
      (Nothing)
  H.describe "fromJust" $ do
    H.it "extract from just" $ H.shouldBe
      (Maybes.fromJust (Just 42))
      (42)
  H.describe "fromMaybe" $ do
    H.it "just value" $ H.shouldBe
      (Maybes.fromMaybe 0 (Just 42))
      (42)
    H.it "nothing with default" $ H.shouldBe
      (Maybes.fromMaybe 99 Nothing)
      (99)
  H.describe "isJust" $ do
    H.it "just value" $ H.shouldBe
      (Maybes.isJust (Just 42))
      (True)
    H.it "nothing" $ H.shouldBe
      (Maybes.isJust Nothing)
      (False)
  H.describe "isNothing" $ do
    H.it "just value" $ H.shouldBe
      (Maybes.isNothing (Just 42))
      (False)
    H.it "nothing" $ H.shouldBe
      (Maybes.isNothing Nothing)
      (True)
  H.describe "map" $ do
    H.it "maps just value" $ H.shouldBe
      (Maybes.map (\x -> Math.mul x 2) (Just 5))
      (Just 10)
    H.it "nothing unchanged" $ H.shouldBe
      (Maybes.map (\x -> Math.mul x 2) Nothing)
      (Nothing)
  H.describe "mapMaybe" $ do
    H.it "filter and transform" $ H.shouldBe
      (Maybes.mapMaybe (\x -> Logic.ifElse (Equality.gt x 2) (Just (Math.mul x 2)) Nothing) [
          1,
          2,
          3,
          4,
          5])
      ([
          6,
          8,
          10])
    H.it "empty result" $ H.shouldBe
      (Maybes.mapMaybe (\x -> Logic.ifElse (Equality.gt x 2) (Just (Math.mul x 2)) Nothing) [
          1,
          2])
      ([])
    H.it "empty input" $ H.shouldBe
      (Maybes.mapMaybe (\x -> Logic.ifElse (Equality.gt x 2) (Just (Math.mul x 2)) Nothing) [])
      ([])
  H.describe "maybe" $ do
    H.it "just value applies function" $ H.shouldBe
      (Maybes.maybe 0 (\x -> Math.mul x 2) (Just 5))
      (10)
    H.it "nothing returns default" $ H.shouldBe
      (Maybes.maybe 99 (\x -> Math.mul x 2) Nothing)
      (99)
  H.describe "pure" $ do
    H.it "wraps integer" $ H.shouldBe
      (Maybes.pure 42)
      (Just 42)
    H.it "wraps string" $ H.shouldBe
      (Maybes.pure "hello")
      (Just "hello")
