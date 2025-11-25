-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.maybes"},ModuleName {unModuleName = "Maybes"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.lib.maybes"},ModuleName {unModuleName = "Maybes"})]

module Generation.Hydra.Test.Lib.MaybesSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes

spec :: H.Spec
spec = H.describe "hydra.lib.maybes primitives" $ do
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
  H.describe "fromMaybe" $ do
    H.it "just value" $ H.shouldBe
      (Maybes.fromMaybe 0 (Just 42))
      (42)
    H.it "nothing with default" $ H.shouldBe
      (Maybes.fromMaybe 99 Nothing)
      (99)
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
      ([] :: [String])
    H.it "empty list" $ H.shouldBe
      (Maybes.cat [])
      ([] :: [String])
  H.describe "map" $ do
    H.it "maps just value" $ H.shouldBe
      (Maybes.map (\x -> Math.mul x 2) (Just 5))
      (Just 10)
    H.it "nothing unchanged" $ H.shouldBe
      (Maybes.map (\x -> Math.mul x 2) Nothing)
      (Nothing)
