-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.pairs"},ModuleName {unModuleName = "Pairs"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.lib.sets"},ModuleName {unModuleName = "Sets"})]

module Generation.Hydra.Test.Lib.PairsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Sets as Sets

spec :: H.Spec
spec = H.describe "hydra.lib.sets primitives" $ do
  H.describe "empty" $ do
    H.it "empty set" $ H.shouldBe
      (Sets.empty)
      (S.empty :: S.Set String)
  H.describe "singleton" $ do
    H.it "single element" $ H.shouldBe
      (Sets.singleton 42)
      (S.fromList [
          42])
  H.describe "fromList" $ do
    H.it "create from list" $ H.shouldBe
      (Sets.fromList [
          1,
          2,
          3])
      (S.fromList [
          1,
          2,
          3])
    H.it "duplicates removed" $ H.shouldBe
      (Sets.fromList [
          1,
          2,
          1,
          3])
      (S.fromList [
          1,
          2,
          3])
    H.it "empty list" $ H.shouldBe
      (Sets.fromList [])
      (S.empty :: S.Set String)
  H.describe "toList" $ do
    H.it "convert to list" $ H.shouldBe
      (Sets.toList (S.fromList [
          1,
          2,
          3]))
      ([
          1,
          2,
          3])
    H.it "empty set" $ H.shouldBe
      (Sets.toList S.empty)
      ([] :: [String])
  H.describe "insert" $ do
    H.it "insert new element" $ H.shouldBe
      (Sets.insert 4 (S.fromList [
          1,
          2,
          3]))
      (S.fromList [
          1,
          2,
          3,
          4])
    H.it "insert existing element" $ H.shouldBe
      (Sets.insert 2 (S.fromList [
          1,
          2,
          3]))
      (S.fromList [
          1,
          2,
          3])
    H.it "insert into empty" $ H.shouldBe
      (Sets.insert 1 S.empty)
      (S.fromList [
          1])
  H.describe "delete" $ do
    H.it "delete existing" $ H.shouldBe
      (Sets.delete 2 (S.fromList [
          1,
          2,
          3]))
      (S.fromList [
          1,
          3])
    H.it "delete non-existing" $ H.shouldBe
      (Sets.delete 4 (S.fromList [
          1,
          2,
          3]))
      (S.fromList [
          1,
          2,
          3])
    H.it "delete from empty" $ H.shouldBe
      (Sets.delete 1 S.empty)
      (S.empty)
  H.describe "member" $ do
    H.it "element exists" $ H.shouldBe
      (Sets.member 2 (S.fromList [
          1,
          2,
          3]))
      (True)
    H.it "element missing" $ H.shouldBe
      (Sets.member 4 (S.fromList [
          1,
          2,
          3]))
      (False)
    H.it "empty set" $ H.shouldBe
      (Sets.member 1 S.empty)
      (False)
  H.describe "size" $ do
    H.it "three elements" $ H.shouldBe
      (Sets.size (S.fromList [
          1,
          2,
          3]))
      (3)
    H.it "single element" $ H.shouldBe
      (Sets.size (S.fromList [
          42]))
      (1)
    H.it "empty set" $ H.shouldBe
      (Sets.size S.empty)
      (0)
  H.describe "null" $ do
    H.it "empty set" $ H.shouldBe
      (Sets.null S.empty)
      (True)
    H.it "non-empty set" $ H.shouldBe
      (Sets.null (S.fromList [
          1,
          2]))
      (False)
  H.describe "union" $ do
    H.it "union two sets" $ H.shouldBe
      (Sets.union (S.fromList [
          1,
          2]) (S.fromList [
          2,
          3]))
      (S.fromList [
          1,
          2,
          3])
    H.it "union with empty" $ H.shouldBe
      (Sets.union (S.fromList [
          1,
          2]) S.empty)
      (S.fromList [
          1,
          2])
    H.it "empty with non-empty" $ H.shouldBe
      (Sets.union S.empty (S.fromList [
          1,
          2]))
      (S.fromList [
          1,
          2])
  H.describe "intersection" $ do
    H.it "common elements" $ H.shouldBe
      (Sets.intersection (S.fromList [
          1,
          2,
          3]) (S.fromList [
          2,
          3,
          4]))
      (S.fromList [
          2,
          3])
    H.it "no common elements" $ H.shouldBe
      (Sets.intersection (S.fromList [
          1,
          2]) (S.fromList [
          3,
          4]))
      (S.empty)
    H.it "intersection with empty" $ H.shouldBe
      (Sets.intersection (S.fromList [
          1,
          2]) S.empty)
      (S.empty)
  H.describe "difference" $ do
    H.it "remove elements" $ H.shouldBe
      (Sets.difference (S.fromList [
          1,
          2,
          3]) (S.fromList [
          2,
          4]))
      (S.fromList [
          1,
          3])
    H.it "no overlap" $ H.shouldBe
      (Sets.difference (S.fromList [
          1,
          2]) (S.fromList [
          3,
          4]))
      (S.fromList [
          1,
          2])
    H.it "difference with empty" $ H.shouldBe
      (Sets.difference (S.fromList [
          1,
          2]) S.empty)
      (S.fromList [
          1,
          2])
  H.describe "map" $ do
    H.it "map function" $ H.shouldBe
      (Sets.map (\x -> Math.mul x 2) (S.fromList [
          1,
          2,
          3]))
      (S.fromList [
          2,
          4,
          6])
    H.it "map on empty" $ H.shouldBe
      (Sets.map (\x -> Math.mul x 2) S.empty)
      (S.empty)
