-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.eithers"},ModuleName {unModuleName = "Eithers"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lexical"},ModuleName {unModuleName = "Lexical"}),(Namespace {unNamespace = "hydra.lib.eithers"},ModuleName {unModuleName = "Eithers"}),(Namespace {unNamespace = "hydra.lib.equality"},ModuleName {unModuleName = "Equality"}),(Namespace {unNamespace = "hydra.lib.logic"},ModuleName {unModuleName = "Logic"}),(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.lib.strings"},ModuleName {unModuleName = "Strings"})]

module Generation.Hydra.Test.Lib.EithersSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Strings as Strings

spec :: H.Spec
spec = H.describe "hydra.lib.eithers primitives" $ do
  H.describe "bind" $ do
    H.it "bind Right with success" $ H.shouldBe
      (Eithers.bind (Right "ab") (\s -> Logic.ifElse (Strings.null s) (Left 0) (Right (Strings.length s))))
      (Right 2 :: Either Int Int)
    H.it "bind Right with failure" $ H.shouldBe
      (Eithers.bind (Right "") (\s -> Logic.ifElse (Strings.null s) (Left 0) (Right (Strings.length s))))
      (Left 0 :: Either Int Int)
    H.it "bind Left returns Left unchanged" $ H.shouldBe
      (Eithers.bind (Left 42) (\s -> Logic.ifElse (Strings.null s) (Left 0) (Right (Strings.length s))))
      (Left 42 :: Either Int Int)
  H.describe "bimap" $ do
    H.it "map left value" $ H.shouldBe
      (Eithers.bimap (\x -> Math.mul x 2) (\s -> Strings.length s) (Left 5))
      (Left 10 :: Either Int Int)
    H.it "map right value" $ H.shouldBe
      (Eithers.bimap (\x -> Math.mul x 2) (\s -> Strings.length s) (Right "ab"))
      (Right 2 :: Either Int Int)
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
  H.describe "map" $ do
    H.it "map right value" $ H.shouldBe
      (Eithers.map (\x -> Math.mul x 2) (Right 5))
      (Right 10 :: Either Int Int)
    H.it "preserve left" $ H.shouldBe
      (Eithers.map (\x -> Math.mul x 2) (Left 99))
      (Left 99 :: Either Int Int)
  H.describe "mapList" $ do
    H.it "all succeed" $ H.shouldBe
      (Eithers.mapList (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) [
          1,
          2,
          3])
      (Right [
          2,
          4,
          6] :: Either String [Int])
    H.it "first fails" $ H.shouldBe
      (Eithers.mapList (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) [
          1,
          0,
          3])
      (Left "zero" :: Either String [Int])
    H.it "empty list" $ H.shouldBe
      (Eithers.mapList (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) [])
      (Right [] :: Either String [Int])
  H.describe "mapMaybe" $ do
    H.it "just succeeds" $ H.shouldBe
      (Eithers.mapMaybe (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) (Just 5))
      (Right (Just 10) :: Either String (Maybe Int))
    H.it "just fails" $ H.shouldBe
      (Eithers.mapMaybe (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) (Just 0))
      (Left "zero" :: Either String (Maybe Int))
    H.it "nothing" $ H.shouldBe
      (Eithers.mapMaybe (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) Nothing)
      (Right Nothing :: Either String (Maybe Int))
