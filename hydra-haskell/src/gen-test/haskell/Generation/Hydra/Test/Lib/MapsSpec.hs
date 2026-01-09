-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.maps"},ModuleName {unModuleName = "Maps"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lexical"},ModuleName {unModuleName = "Lexical"}),(Namespace {unNamespace = "hydra.lib.equality"},ModuleName {unModuleName = "Equality"}),(Namespace {unNamespace = "hydra.lib.maps"},ModuleName {unModuleName = "Maps"}),(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.lib.strings"},ModuleName {unModuleName = "Strings"})]

module Generation.Hydra.Test.Lib.MapsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Strings as Strings

spec :: H.Spec
spec = H.describe "hydra.lib.maps primitives" $ do
  H.describe "alter" $ do
    H.it "insert new key" $ H.shouldBe
      (Maps.alter (\opt -> Just "new") 3 (M.fromList [
          (1, "a"),
          (2, "b")]))
      (M.fromList [
          (1, "a"),
          (2, "b"),
          (3, "new")])
    H.it "update existing key" $ H.shouldBe
      (Maps.alter (\opt -> Just "updated") 2 (M.fromList [
          (1, "a"),
          (2, "b")]))
      (M.fromList [
          (1, "a"),
          (2, "updated")])
    H.it "delete key" $ H.shouldBe
      (Maps.alter (\opt -> Nothing) 2 (M.fromList [
          (1, "a"),
          (2, "b")]))
      (M.fromList [
          (1, "a")])
  H.describe "bimap" $ do
    H.it "transform both" $ H.shouldBe
      (Maps.bimap (\k -> Math.mul k 2) (\v -> Strings.toUpper v) (M.fromList [
          (1, "a"),
          (2, "b")]))
      (M.fromList [
          (2, "A"),
          (4, "B")])
    H.it "empty map" $ H.shouldBe
      (Maps.bimap (\k -> Math.mul k 2) (\v -> Strings.toUpper v) M.empty)
      (M.empty)
  H.describe "elems" $ do
    H.it "get all elements" $ H.shouldBe
      (Maps.elems (M.fromList [
          (1, "a"),
          (2, "b")]))
      ([
          "a",
          "b"])
    H.it "empty map" $ H.shouldBe
      (Maps.elems M.empty)
      ([] :: [Int])
  H.describe "empty" $ do
    H.it "empty map" $ H.shouldBe
      (Maps.empty)
      (M.empty :: M.Map Int Int)
  H.describe "filter" $ do
    H.it "filter values starting with a" $ H.shouldBe
      (Maps.filter (\v -> Equality.equal (Strings.charAt 0 v) 97) (M.fromList [
          (1, "a"),
          (2, "b"),
          (3, "ab")]))
      (M.fromList [
          (1, "a"),
          (3, "ab")])
    H.it "filter all" $ H.shouldBe
      (Maps.filter (\v -> Equality.equal (Strings.charAt 0 v) 97) (M.fromList [
          (1, "b"),
          (2, "c")]))
      (M.empty)
    H.it "empty map" $ H.shouldBe
      (Maps.filter (\v -> Equality.equal (Strings.charAt 0 v) 97) M.empty)
      (M.empty :: M.Map Int String)
  H.describe "filterWithKey" $ do
    H.it "filter by key > 1" $ H.shouldBe
      (Maps.filterWithKey (\k -> \v -> Equality.gt k 1) (M.fromList [
          (1, "a"),
          (2, "b"),
          (3, "c")]))
      (M.fromList [
          (2, "b"),
          (3, "c")])
    H.it "filter all" $ H.shouldBe
      (Maps.filterWithKey (\k -> \v -> Equality.gt k 1) (M.fromList [
          (1, "a")]))
      (M.empty)
    H.it "empty map" $ H.shouldBe
      (Maps.filterWithKey (\k -> \v -> Equality.gt k 1) M.empty)
      (M.empty :: M.Map Int Int)
  H.describe "findWithDefault" $ do
    H.it "find existing" $ H.shouldBe
      (Maps.findWithDefault "default" 2 (M.fromList [
          (1, "a"),
          (2, "b")]))
      ("b")
    H.it "use default" $ H.shouldBe
      (Maps.findWithDefault "default" 3 (M.fromList [
          (1, "a"),
          (2, "b")]))
      ("default")
  H.describe "fromList" $ do
    H.it "create from pairs" $ H.shouldBe
      (Maps.fromList [
          (1, "a"),
          (2, "b")])
      (M.fromList [
          (1, "a"),
          (2, "b")])
    H.it "duplicate keys" $ H.shouldBe
      (Maps.fromList [
          (1, "a"),
          (1, "b")])
      (M.fromList [
          (1, "b")])
    H.it "empty list" $ H.shouldBe
      (Maps.fromList [])
      (M.empty :: M.Map Int Int)
  H.describe "insert" $ do
    H.it "insert new key" $ H.shouldBe
      (Maps.insert 3 "c" (M.fromList [
          (1, "a"),
          (2, "b")]))
      (M.fromList [
          (1, "a"),
          (2, "b"),
          (3, "c")])
    H.it "update existing" $ H.shouldBe
      (Maps.insert 2 "updated" (M.fromList [
          (1, "a"),
          (2, "b")]))
      (M.fromList [
          (1, "a"),
          (2, "updated")])
    H.it "insert into empty" $ H.shouldBe
      (Maps.insert 1 "x" M.empty)
      (M.fromList [
          (1, "x")])
  H.describe "keys" $ do
    H.it "get all keys" $ H.shouldBe
      (Maps.keys (M.fromList [
          (1, "a"),
          (2, "b"),
          (3, "c")]))
      ([
          1,
          2,
          3])
    H.it "empty map" $ H.shouldBe
      (Maps.keys M.empty)
      ([] :: [Int])
  H.describe "lookup" $ do
    H.it "find existing key" $ H.shouldBe
      (Maps.lookup 2 (M.fromList [
          (1, "a"),
          (2, "b")]))
      (Just "b")
    H.it "key not found" $ H.shouldBe
      (Maps.lookup 3 (M.fromList [
          (1, "a"),
          (2, "b")]))
      (Nothing)
    H.it "lookup in empty" $ H.shouldBe
      (Maps.lookup 1 M.empty)
      (Nothing :: Maybe Int)
  H.describe "map" $ do
    H.it "map over values" $ H.shouldBe
      (Maps.map (\s -> Strings.toUpper s) (M.fromList [
          (1, "a"),
          (2, "b")]))
      (M.fromList [
          (1, "A"),
          (2, "B")])
    H.it "map empty" $ H.shouldBe
      (Maps.map (\s -> Strings.toUpper s) M.empty)
      (M.empty :: M.Map Int String)
  H.describe "mapKeys" $ do
    H.it "double keys" $ H.shouldBe
      (Maps.mapKeys (\k -> Math.mul k 2) (M.fromList [
          (1, "a"),
          (2, "b")]))
      (M.fromList [
          (2, "a"),
          (4, "b")])
    H.it "empty map" $ H.shouldBe
      (Maps.mapKeys (\k -> Math.mul k 2) M.empty)
      (M.empty :: M.Map Int Int)
  H.describe "member" $ do
    H.it "key exists" $ H.shouldBe
      (Maps.member 2 (M.fromList [
          (1, "a"),
          (2, "b")]))
      (True)
    H.it "key missing" $ H.shouldBe
      (Maps.member 3 (M.fromList [
          (1, "a"),
          (2, "b")]))
      (False)
    H.it "empty map" $ H.shouldBe
      (Maps.member 1 M.empty)
      (False)
  H.describe "null" $ do
    H.it "empty map" $ H.shouldBe
      (Maps.null M.empty)
      (True)
    H.it "non-empty map" $ H.shouldBe
      (Maps.null (M.fromList [
          (1, "a")]))
      (False)
  H.describe "remove" $ do
    H.it "remove existing" $ H.shouldBe
      (Maps.delete 2 (M.fromList [
          (1, "a"),
          (2, "b"),
          (3, "c")]))
      (M.fromList [
          (1, "a"),
          (3, "c")])
    H.it "remove non-existing" $ H.shouldBe
      (Maps.delete 4 (M.fromList [
          (1, "a"),
          (2, "b")]))
      (M.fromList [
          (1, "a"),
          (2, "b")])
    H.it "remove from empty" $ H.shouldBe
      (Maps.delete 1 M.empty)
      (M.empty :: M.Map Int Int)
  H.describe "singleton" $ do
    H.it "single entry" $ H.shouldBe
      (Maps.singleton 42 "hello")
      (M.fromList [
          (42, "hello")])
  H.describe "size" $ do
    H.it "three entries" $ H.shouldBe
      (Maps.size (M.fromList [
          (1, "a"),
          (2, "b"),
          (3, "c")]))
      (3)
    H.it "single entry" $ H.shouldBe
      (Maps.size (M.fromList [
          (42, "test")]))
      (1)
    H.it "empty map" $ H.shouldBe
      (Maps.size M.empty)
      (0)
  H.describe "toList" $ do
    H.it "convert to pairs" $ H.shouldBe
      (Maps.toList (M.fromList [
          (1, "a"),
          (2, "b")]))
      ([
          (1, "a"),
          (2, "b")])
    H.it "empty map" $ H.shouldBe
      (Maps.toList M.empty)
      ([] :: [(Int, Int)])
  H.describe "union" $ do
    H.it "union two maps" $ H.shouldBe
      (Maps.union (M.fromList [
          (1, "a"),
          (2, "b")]) (M.fromList [
          (2, "x"),
          (3, "c")]))
      (M.fromList [
          (1, "a"),
          (2, "b"),
          (3, "c")])
    H.it "union with empty" $ H.shouldBe
      (Maps.union (M.fromList [
          (1, "a")]) M.empty)
      (M.fromList [
          (1, "a")])
    H.it "empty with map" $ H.shouldBe
      (Maps.union M.empty (M.fromList [
          (1, "a")]))
      (M.fromList [
          (1, "a")])
