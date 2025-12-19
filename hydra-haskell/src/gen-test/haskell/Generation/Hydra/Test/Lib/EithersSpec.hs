-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.eithers"},ModuleName {unModuleName = "Eithers"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.eithers"},ModuleName {unModuleName = "Eithers"}),(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.lib.strings"},ModuleName {unModuleName = "Strings"})]

module Generation.Hydra.Test.Lib.EithersSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Strings as Strings

spec :: H.Spec
spec = H.describe "hydra.lib.eithers primitives" $ do
  H.describe "bimap" $ do
    H.it "map left value" $ H.shouldBe
      (Eithers.bimap (\x -> Math.mul x 2) (\s -> Strings.length s) (Left 5))
      (Left 10)
    H.it "map right value" $ H.shouldBe
      (Eithers.bimap (\x -> Math.mul x 2) (\s -> Strings.length s) (Right "ab"))
      (Right 2)
  H.describe "isLeft" $ do
    H.it "left value" $ H.shouldBe
      (Eithers.isLeft (Left 42))
      (True)
    H.it "right value" $ H.shouldBe
      (Eithers.isLeft (Right "test"))
      (False)
  H.describe "isRight" $ do
    H.it "right value" $ H.shouldBe
      (Eithers.isRight (Right "test"))
      (True)
    H.it "left value" $ H.shouldBe
      (Eithers.isRight (Left 42))
      (False)
  H.describe "fromLeft" $ do
    H.it "extract left" $ H.shouldBe
      (Eithers.fromLeft 99 (Left 42))
      (42)
    H.it "use default for right" $ H.shouldBe
      (Eithers.fromLeft 99 (Right "test"))
      (99)
  H.describe "fromRight" $ do
    H.it "extract right" $ H.shouldBe
      (Eithers.fromRight "default" (Right "test"))
      ("test")
    H.it "use default for left" $ H.shouldBe
      (Eithers.fromRight "default" (Left 42))
      ("default")
  H.describe "either" $ do
    H.it "apply left function" $ H.shouldBe
      (Eithers.either (\x -> Math.mul x 2) (\s -> Strings.length s) (Left 5))
      (10)
    H.it "apply right function" $ H.shouldBe
      (Eithers.either (\x -> Math.mul x 2) (\s -> Strings.length s) (Right "ab"))
      (2)
  H.describe "lefts" $ do
    H.it "filter left values" $ H.shouldBe
      (Eithers.lefts [
          Left 1,
          Right "a",
          Left 2,
          (Right "b")])
      ([
          1,
          2])
    H.it "all lefts" $ H.shouldBe
      (Eithers.lefts [
          Left 1,
          (Left 2)])
      ([
          1,
          2])
    H.it "all rights" $ H.shouldBe
      (Eithers.lefts [
          Right "a",
          (Right "b")])
      ([] :: [Int])
    H.it "empty list" $ H.shouldBe
      (Eithers.lefts [])
      ([] :: [Int])
  H.describe "rights" $ do
    H.it "filter right values" $ H.shouldBe
      (Eithers.rights [
          Left 1,
          Right "a",
          Left 2,
          (Right "b")])
      ([
          "a",
          "b"])
    H.it "all rights" $ H.shouldBe
      (Eithers.rights [
          Right "a",
          (Right "b")])
      ([
          "a",
          "b"])
    H.it "all lefts" $ H.shouldBe
      (Eithers.rights [
          Left 1,
          (Left 2)])
      ([] :: [Int])
    H.it "empty list" $ H.shouldBe
      (Eithers.rights [])
      ([] :: [Int])
  H.describe "partitionEithers" $ do
    H.it "partition mixed" $ H.shouldBe
      (Eithers.partitionEithers [
          Left 1,
          Right "a",
          Left 2,
          (Right "b")])
      (([
          1,
          2], [
          "a",
          "b"]))
    H.it "all lefts" $ H.shouldBe
      (Eithers.partitionEithers [
          Left 1,
          (Left 2)])
      (([
          1,
          2], []) :: ([Int], [Int]))
    H.it "all rights" $ H.shouldBe
      (Eithers.partitionEithers [
          Right "a",
          (Right "b")])
      (([], [
          "a",
          "b"]) :: ([Int], [String]))
    H.it "empty list" $ H.shouldBe
      (Eithers.partitionEithers [])
      (([], []) :: ([Int], [Int]))
