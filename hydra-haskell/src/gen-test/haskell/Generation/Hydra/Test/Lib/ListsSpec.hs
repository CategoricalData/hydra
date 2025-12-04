-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.lists"},ModuleName {unModuleName = "Lists"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.equality"},ModuleName {unModuleName = "Equality"}),(Namespace {unNamespace = "hydra.lib.lists"},ModuleName {unModuleName = "Lists"}),(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.lib.strings"},ModuleName {unModuleName = "Strings"})]

module Generation.Hydra.Test.Lib.ListsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Strings as Strings

spec :: H.Spec
spec = H.describe "hydra.lib.lists primitives" $ do
  H.describe "apply" $ do
    H.describe "string transformations" $ do
      H.it "string transformations" $ H.shouldBe
        (Lists.apply [
            Strings.toUpper,
            Strings.toLower] [
            "One",
            "Two",
            "Three"])
        ([
            "ONE",
            "TWO",
            "THREE",
            "one",
            "two",
            "three"])
    H.describe "edge cases" $ do
      H.it "empty function list" $ H.shouldBe
        (Lists.apply [] [
            "a",
            "b"])
        ([] :: [Int])
      H.it "empty input list" $ H.shouldBe
        (Lists.apply [
            Strings.toUpper] [])
        ([])
      H.it "single function" $ H.shouldBe
        (Lists.apply [
            Strings.toUpper] [
            "hello"])
        ([
            "HELLO"])
      H.it "single input" $ H.shouldBe
        (Lists.apply [
            Strings.toUpper,
            Strings.toLower] [
            "Test"])
        ([
            "TEST",
            "test"])
  H.describe "at" $ do
    H.it "first element" $ H.shouldBe
      (Lists.at 0 [
          1,
          2,
          3])
      (1)
    H.it "middle element" $ H.shouldBe
      (Lists.at 1 [
          1,
          2,
          3])
      (2)
    H.it "last element" $ H.shouldBe
      (Lists.at 2 [
          1,
          2,
          3])
      (3)
    H.it "single element list" $ H.shouldBe
      (Lists.at 0 [
          42])
      (42)
    H.it "string list access" $ H.shouldBe
      (Lists.at 1 [
          "hello",
          "world"])
      ("world")
  H.describe "bind" $ do
    H.it "negation function" $ H.shouldBe
      (Lists.bind [
          1,
          2,
          3,
          4] (\x -> Lists.pure (Math.negate x)))
      ([
          (-1),
          (-2),
          (-3),
          (-4)])
    H.it "empty list" $ H.shouldBe
      (Lists.bind [] (\x -> Lists.pure (Math.negate x)))
      ([])
    H.it "single element" $ H.shouldBe
      (Lists.bind [
          5] (\x -> Lists.pure (Math.negate x)))
      ([
          (-5)])
    H.it "duplicate elements" $ H.shouldBe
      (Lists.bind [
          1,
          1,
          2] (\x -> Lists.pure (Math.negate x)))
      ([
          (-1),
          (-1),
          (-2)])
  H.describe "concat" $ do
    H.it "multiple non-empty lists" $ H.shouldBe
      (Lists.concat [
          [
            1,
            2,
            3],
          [
            4,
            5],
          [
            6,
            7,
            8]])
      ([
          1,
          2,
          3,
          4,
          5,
          6,
          7,
          8])
    H.it "empty lists included" $ H.shouldBe
      (Lists.concat [
          [],
          [
            1,
            2],
          [],
          [
            3]])
      ([
          1,
          2,
          3])
    H.it "single list" $ H.shouldBe
      (Lists.concat [
          [
            1,
            2,
            3]])
      ([
          1,
          2,
          3])
    H.it "all empty lists" $ H.shouldBe
      (Lists.concat [
          [],
          [],
          []])
      ([] :: [Int])
    H.it "empty list of lists" $ H.shouldBe
      (Lists.concat [])
      ([] :: [Int])
  H.describe "concat2" $ do
    H.it "two non-empty lists" $ H.shouldBe
      (Lists.concat2 [
          1,
          2] [
          3,
          4])
      ([
          1,
          2,
          3,
          4])
    H.it "first list empty" $ H.shouldBe
      (Lists.concat2 [] [
          1,
          2])
      ([
          1,
          2])
    H.it "second list empty" $ H.shouldBe
      (Lists.concat2 [
          1,
          2] [])
      ([
          1,
          2])
    H.it "both lists empty" $ H.shouldBe
      (Lists.concat2 [] [])
      ([] :: [Int])
    H.it "single elements" $ H.shouldBe
      (Lists.concat2 [
          1] [
          2])
      ([
          1,
          2])
    H.it "string lists" $ H.shouldBe
      (Lists.concat2 [
          "a",
          "b"] [
          "c",
          "d"])
      ([
          "a",
          "b",
          "c",
          "d"])
  H.describe "cons" $ do
    H.it "cons to non-empty list" $ H.shouldBe
      (Lists.cons 1 [
          2,
          3])
      ([
          1,
          2,
          3])
    H.it "cons to empty list" $ H.shouldBe
      (Lists.cons 1 [])
      ([
          1])
    H.it "cons negative number" $ H.shouldBe
      (Lists.cons (-1) [
          2,
          3])
      ([
          (-1),
          2,
          3])
    H.it "cons string" $ H.shouldBe
      (Lists.cons "hello" [
          "world"])
      ([
          "hello",
          "world"])
  H.describe "drop" $ do
    H.it "drop from beginning" $ H.shouldBe
      (Lists.drop 2 [
          1,
          2,
          3,
          4,
          5])
      ([
          3,
          4,
          5])
    H.it "drop zero elements" $ H.shouldBe
      (Lists.drop 0 [
          1,
          2,
          3])
      ([
          1,
          2,
          3])
    H.it "drop all elements" $ H.shouldBe
      (Lists.drop 3 [
          1,
          2,
          3])
      ([])
    H.it "drop more than length" $ H.shouldBe
      (Lists.drop 5 [
          1,
          2])
      ([])
    H.it "drop from empty list" $ H.shouldBe
      (Lists.drop 3 [])
      ([] :: [Int])
    H.it "drop negative amount" $ H.shouldBe
      (Lists.drop (-1) [
          1,
          2,
          3])
      ([
          1,
          2,
          3])
  H.describe "dropWhile" $ do
    H.it "drop while less than 3" $ H.shouldBe
      (Lists.dropWhile (\x -> Equality.lt x 3) [
          1,
          2,
          3,
          2,
          1])
      ([
          3,
          2,
          1])
    H.it "drop all elements" $ H.shouldBe
      (Lists.dropWhile (\x -> Equality.lt x 10) [
          1,
          2,
          3])
      ([])
    H.it "drop no elements" $ H.shouldBe
      (Lists.dropWhile (\x -> Equality.lt x 0) [
          1,
          2,
          3])
      ([
          1,
          2,
          3])
    H.it "empty list" $ H.shouldBe
      (Lists.dropWhile (\x -> Equality.lt x 5) [])
      ([])
  H.describe "elem" $ do
    H.it "element present" $ H.shouldBe
      (Lists.elem 2 [
          1,
          2,
          3])
      (True)
    H.it "element not present" $ H.shouldBe
      (Lists.elem 4 [
          1,
          2,
          3])
      (False)
    H.it "empty list" $ H.shouldBe
      (Lists.elem 1 [])
      (False)
    H.it "single element present" $ H.shouldBe
      (Lists.elem 1 [
          1])
      (True)
    H.it "single element not present" $ H.shouldBe
      (Lists.elem 2 [
          1])
      (False)
    H.it "duplicate elements" $ H.shouldBe
      (Lists.elem 2 [
          1,
          2,
          2,
          3])
      (True)
    H.it "string element present" $ H.shouldBe
      (Lists.elem "hello" [
          "world",
          "hello",
          "test"])
      (True)
    H.it "string element not present" $ H.shouldBe
      (Lists.elem "missing" [
          "world",
          "hello"])
      (False)
  H.describe "filter" $ do
    H.it "filter positive numbers" $ H.shouldBe
      (Lists.filter (\x -> Equality.gt x 0) [
          (-1),
          2,
          (-3),
          4,
          5])
      ([
          2,
          4,
          5])
    H.it "filter all elements" $ H.shouldBe
      (Lists.filter (\x -> Equality.lt x 10) [
          1,
          2,
          3])
      ([
          1,
          2,
          3])
    H.it "filter no elements" $ H.shouldBe
      (Lists.filter (\x -> Equality.gt x 10) [
          1,
          2,
          3])
      ([])
    H.it "empty list" $ H.shouldBe
      (Lists.filter (\x -> Equality.gt x 0) [])
      ([])
  H.describe "foldl" $ do
    H.it "sum with addition" $ H.shouldBe
      (Lists.foldl Math.add 0 [
          1,
          2,
          3,
          4])
      (10)
    H.it "product with multiplication" $ H.shouldBe
      (Lists.foldl Math.mul 1 [
          2,
          3,
          4])
      (24)
    H.it "empty list" $ H.shouldBe
      (Lists.foldl Math.add 5 [])
      (5)
    H.it "single element" $ H.shouldBe
      (Lists.foldl Math.add 10 [
          5])
      (15)
    H.it "subtraction fold" $ H.shouldBe
      (Lists.foldl Math.sub 10 [
          1,
          2,
          3])
      (4)
  H.describe "group" $ do
    H.it "consecutive duplicates" $ H.shouldBe
      (Lists.group [
          1,
          1,
          2,
          2,
          2,
          3,
          1])
      ([
          [
            1,
            1],
          [
            2,
            2,
            2],
          [
            3],
          [
            1]])
    H.it "no duplicates" $ H.shouldBe
      (Lists.group [
          1,
          2,
          3])
      ([
          [
            1],
          [
            2],
          [
            3]])
    H.it "all same" $ H.shouldBe
      (Lists.group [
          1,
          1,
          1])
      ([
          [
            1,
            1,
            1]])
    H.it "empty list" $ H.shouldBe
      (Lists.group [])
      ([] :: [[Int]])
    H.it "single element" $ H.shouldBe
      (Lists.group [
          1])
      ([
          [
            1]])
  H.describe "head" $ do
    H.it "three element list" $ H.shouldBe
      (Lists.head [
          1,
          2,
          3])
      (1)
    H.it "single element list" $ H.shouldBe
      (Lists.head [
          42])
      (42)
    H.it "negative numbers" $ H.shouldBe
      (Lists.head [
          (-1),
          (-2),
          (-3)])
      ((-1))
    H.it "string list" $ H.shouldBe
      (Lists.head [
          "hello",
          "world"])
      ("hello")
  H.describe "init" $ do
    H.it "multiple elements" $ H.shouldBe
      (Lists.init [
          1,
          2,
          3,
          4])
      ([
          1,
          2,
          3])
    H.it "two elements" $ H.shouldBe
      (Lists.init [
          1,
          2])
      ([
          1])
    H.it "single element" $ H.shouldBe
      (Lists.init [
          1])
      ([])
    H.it "string list" $ H.shouldBe
      (Lists.init [
          "a",
          "b",
          "c"])
      ([
          "a",
          "b"])
  H.describe "intercalate" $ do
    H.it "double zero separator" $ H.shouldBe
      (Lists.intercalate [
          0,
          0] [
          [
            1,
            2,
            3],
          [
            4,
            5],
          [
            6,
            7,
            8]])
      ([
          1,
          2,
          3,
          0,
          0,
          4,
          5,
          0,
          0,
          6,
          7,
          8])
    H.it "empty separator" $ H.shouldBe
      (Lists.intercalate [] [
          [
            1,
            2],
          [
            3,
            4]])
      ([
          1,
          2,
          3,
          4])
    H.it "single element separator" $ H.shouldBe
      (Lists.intercalate [
          99] [
          [
            1],
          [
            2],
          [
            3]])
      ([
          1,
          99,
          2,
          99,
          3])
    H.it "empty list of lists" $ H.shouldBe
      (Lists.intercalate [
          0] [])
      ([])
    H.it "single list" $ H.shouldBe
      (Lists.intercalate [
          0] [
          [
            1,
            2,
            3]])
      ([
          1,
          2,
          3])
    H.it "lists with empty lists" $ H.shouldBe
      (Lists.intercalate [
          0] [
          [],
          [
            1],
          []])
      ([
          0,
          1,
          0])
  H.describe "intersperse" $ do
    H.it "string interspersion" $ H.shouldBe
      (Lists.intersperse "and" [
          "one",
          "two",
          "three"])
      ([
          "one",
          "and",
          "two",
          "and",
          "three"])
    H.it "single element" $ H.shouldBe
      (Lists.intersperse "x" [
          "only"])
      ([
          "only"])
    H.it "empty list" $ H.shouldBe
      (Lists.intersperse "x" [])
      ([])
    H.it "two elements" $ H.shouldBe
      (Lists.intersperse "+" [
          "a",
          "b"])
      ([
          "a",
          "+",
          "b"])
    H.it "number interspersion" $ H.shouldBe
      (Lists.intersperse 0 [
          1,
          2,
          3])
      ([
          1,
          0,
          2,
          0,
          3])
  H.describe "last" $ do
    H.it "three element list" $ H.shouldBe
      (Lists.last [
          1,
          2,
          3])
      (3)
    H.it "single element list" $ H.shouldBe
      (Lists.last [
          42])
      (42)
    H.it "negative numbers" $ H.shouldBe
      (Lists.last [
          (-1),
          (-2),
          (-3)])
      ((-3))
    H.it "string list" $ H.shouldBe
      (Lists.last [
          "hello",
          "world"])
      ("world")
  H.describe "length" $ do
    H.it "three elements" $ H.shouldBe
      (Lists.length [
          1,
          2,
          3])
      (3)
    H.it "empty list" $ H.shouldBe
      (Lists.length [])
      (0)
    H.it "single element" $ H.shouldBe
      (Lists.length [
          42])
      (1)
    H.it "many elements" $ H.shouldBe
      (Lists.length [
          1,
          2,
          3,
          4,
          5,
          6,
          7,
          8,
          9,
          10])
      (10)
    H.it "string list" $ H.shouldBe
      (Lists.length [
          "a",
          "b",
          "c"])
      (3)
  H.describe "map" $ do
    H.it "string to uppercase" $ H.shouldBe
      (Lists.map Strings.toUpper [
          "one",
          "two"])
      ([
          "ONE",
          "TWO"])
    H.it "empty list" $ H.shouldBe
      (Lists.map Strings.toUpper [])
      ([])
    H.it "single element" $ H.shouldBe
      (Lists.map Strings.toUpper [
          "hello"])
      ([
          "HELLO"])
    H.it "number negation" $ H.shouldBe
      (Lists.map Math.negate [
          1,
          2,
          3])
      ([
          (-1),
          (-2),
          (-3)])
    H.it "identity function" $ H.shouldBe
      (Lists.map Equality.identity [
          1,
          2,
          3])
      ([
          1,
          2,
          3])
  H.describe "nub" $ do
    H.it "remove duplicates" $ H.shouldBe
      (Lists.nub [
          1,
          2,
          1,
          3,
          2,
          4])
      ([
          1,
          2,
          3,
          4])
    H.it "no duplicates" $ H.shouldBe
      (Lists.nub [
          1,
          2,
          3])
      ([
          1,
          2,
          3])
    H.it "all duplicates" $ H.shouldBe
      (Lists.nub [
          1,
          1,
          1])
      ([
          1])
    H.it "empty list" $ H.shouldBe
      (Lists.nub [])
      ([] :: [Int])
    H.it "single element" $ H.shouldBe
      (Lists.nub [
          1])
      ([
          1])
    H.it "string duplicates" $ H.shouldBe
      (Lists.nub [
          "a",
          "b",
          "a",
          "c"])
      ([
          "a",
          "b",
          "c"])
  H.describe "null" $ do
    H.it "empty int list" $ H.shouldBe
      (Lists.null [])
      (True)
    H.it "single element" $ H.shouldBe
      (Lists.null [
          1])
      (False)
    H.it "multiple elements" $ H.shouldBe
      (Lists.null [
          1,
          2,
          3])
      (False)
    H.it "empty string list" $ H.shouldBe
      (Lists.null [])
      (True)
    H.it "non-empty string list" $ H.shouldBe
      (Lists.null [
          "a"])
      (False)
  H.describe "pure" $ do
    H.it "string element" $ H.shouldBe
      (Lists.pure "one")
      ([
          "one"])
    H.it "empty string" $ H.shouldBe
      (Lists.pure "")
      ([
          ""])
    H.it "number element" $ H.shouldBe
      (Lists.pure 42)
      ([
          42])
    H.it "negative number" $ H.shouldBe
      (Lists.pure (-5))
      ([
          (-5)])
  H.describe "replicate" $ do
    H.it "replicate three times" $ H.shouldBe
      (Lists.replicate 3 42)
      ([
          42,
          42,
          42])
    H.it "replicate zero times" $ H.shouldBe
      (Lists.replicate 0 1)
      ([])
    H.it "replicate once" $ H.shouldBe
      (Lists.replicate 1 99)
      ([
          99])
    H.it "replicate string" $ H.shouldBe
      (Lists.replicate 2 "hello")
      ([
          "hello",
          "hello"])
  H.describe "reverse" $ do
    H.it "multiple elements" $ H.shouldBe
      (Lists.reverse [
          1,
          2,
          3,
          4])
      ([
          4,
          3,
          2,
          1])
    H.it "single element" $ H.shouldBe
      (Lists.reverse [
          1])
      ([
          1])
    H.it "empty list" $ H.shouldBe
      (Lists.reverse [])
      ([] :: [Int])
    H.it "two elements" $ H.shouldBe
      (Lists.reverse [
          1,
          2])
      ([
          2,
          1])
    H.it "string list" $ H.shouldBe
      (Lists.reverse [
          "a",
          "b",
          "c"])
      ([
          "c",
          "b",
          "a"])
  H.describe "safeHead" $ do
    H.it "non-empty int list" $ H.shouldBe
      (Lists.safeHead [
          1,
          2,
          3])
      (Just 1)
    H.it "empty int list" $ H.shouldBe
      (Lists.safeHead [])
      (Nothing :: Maybe Int)
    H.it "single element" $ H.shouldBe
      (Lists.safeHead [
          42])
      (Just 42)
    H.it "non-empty string list" $ H.shouldBe
      (Lists.safeHead [
          "hello",
          "world"])
      (Just "hello")
    H.it "empty string list" $ H.shouldBe
      (Lists.safeHead [])
      (Nothing :: Maybe Int)
  H.describe "singleton" $ do
    H.it "number element" $ H.shouldBe
      (Lists.singleton 42)
      ([
          42])
    H.it "negative number" $ H.shouldBe
      (Lists.singleton (-1))
      ([
          (-1)])
    H.it "zero" $ H.shouldBe
      (Lists.singleton 0)
      ([
          0])
    H.it "string element" $ H.shouldBe
      (Lists.singleton "hello")
      ([
          "hello"])
  H.describe "sort" $ do
    H.it "unsorted numbers" $ H.shouldBe
      (Lists.sort [
          3,
          1,
          4,
          1,
          5])
      ([
          1,
          1,
          3,
          4,
          5])
    H.it "already sorted" $ H.shouldBe
      (Lists.sort [
          1,
          2,
          3])
      ([
          1,
          2,
          3])
    H.it "reverse sorted" $ H.shouldBe
      (Lists.sort [
          3,
          2,
          1])
      ([
          1,
          2,
          3])
    H.it "single element" $ H.shouldBe
      (Lists.sort [
          1])
      ([
          1])
    H.it "empty list" $ H.shouldBe
      (Lists.sort [])
      ([] :: [Int])
    H.it "duplicates" $ H.shouldBe
      (Lists.sort [
          2,
          1,
          2,
          3,
          1])
      ([
          1,
          1,
          2,
          2,
          3])
    H.it "string sort" $ H.shouldBe
      (Lists.sort [
          "zebra",
          "apple",
          "banana"])
      ([
          "apple",
          "banana",
          "zebra"])
  H.describe "sortOn" $ do
    H.it "sort by string length" $ H.shouldBe
      (Lists.sortOn Strings.length [
          "hello",
          "hi",
          "world"])
      ([
          "hi",
          "hello",
          "world"])
    H.it "empty string list" $ H.shouldBe
      (Lists.sortOn Strings.length [])
      ([])
    H.it "single string element" $ H.shouldBe
      (Lists.sortOn Strings.length [
          "test"])
      ([
          "test"])
    H.it "sort by negation" $ H.shouldBe
      (Lists.sortOn Math.negate [
          1,
          3,
          2])
      ([
          3,
          2,
          1])
    H.it "sort by absolute value" $ H.shouldBe
      (Lists.sortOn Math.abs [
          (-1),
          (-3),
          2])
      ([
          (-1),
          2,
          (-3)])
  H.describe "span" $ do
    H.it "span less than 3" $ H.shouldBe
      (Lists.span (\x -> Equality.lt x 3) [
          1,
          2,
          3,
          1,
          2])
      (([
          1,
          2], [
          3,
          1,
          2]))
    H.it "span all elements" $ H.shouldBe
      (Lists.span (\x -> Equality.lt x 10) [
          1,
          2,
          3])
      (([
          1,
          2,
          3], []))
    H.it "span no elements" $ H.shouldBe
      (Lists.span (\x -> Equality.gt x 10) [
          1,
          2,
          3])
      (([], [
          1,
          2,
          3]))
    H.it "empty list" $ H.shouldBe
      (Lists.span (\x -> Equality.lt x 5) [])
      (([], []))
  H.describe "tail" $ do
    H.it "multiple elements" $ H.shouldBe
      (Lists.tail [
          1,
          2,
          3,
          4])
      ([
          2,
          3,
          4])
    H.it "two elements" $ H.shouldBe
      (Lists.tail [
          1,
          2])
      ([
          2])
    H.it "single element" $ H.shouldBe
      (Lists.tail [
          1])
      ([])
    H.it "string list" $ H.shouldBe
      (Lists.tail [
          "a",
          "b",
          "c"])
      ([
          "b",
          "c"])
  H.describe "take" $ do
    H.it "take from beginning" $ H.shouldBe
      (Lists.take 2 [
          1,
          2,
          3,
          4,
          5])
      ([
          1,
          2])
    H.it "take zero elements" $ H.shouldBe
      (Lists.take 0 [
          1,
          2,
          3])
      ([])
    H.it "take all elements" $ H.shouldBe
      (Lists.take 3 [
          1,
          2,
          3])
      ([
          1,
          2,
          3])
    H.it "take more than length" $ H.shouldBe
      (Lists.take 5 [
          1,
          2])
      ([
          1,
          2])
    H.it "take from empty list" $ H.shouldBe
      (Lists.take 3 [])
      ([] :: [Int])
    H.it "take negative amount" $ H.shouldBe
      (Lists.take (-1) [
          1,
          2,
          3])
      ([])
  H.describe "transpose" $ do
    H.it "square matrix" $ H.shouldBe
      (Lists.transpose [
          [
            1,
            2,
            3],
          [
            4,
            5,
            6]])
      ([
          [
            1,
            4],
          [
            2,
            5],
          [
            3,
            6]])
    H.it "empty lists" $ H.shouldBe
      (Lists.transpose [])
      ([] :: [[Int]])
    H.it "single row" $ H.shouldBe
      (Lists.transpose [
          [
            1,
            2,
            3]])
      ([
          [
            1],
          [
            2],
          [
            3]])
    H.it "single column" $ H.shouldBe
      (Lists.transpose [
          [
            1],
          [
            2],
          [
            3]])
      ([
          [
            1,
            2,
            3]])
    H.it "ragged matrix" $ H.shouldBe
      (Lists.transpose [
          [
            1,
            2],
          [
            3],
          [
            4,
            5,
            6]])
      ([
          [
            1,
            3,
            4],
          [
            2,
            5],
          [
            6]])
  H.describe "zip" $ do
    H.it "equal length lists" $ H.shouldBe
      (Lists.zip [
          1,
          2,
          3] [
          "a",
          "b",
          "c"])
      ([
          (1, "a"),
          (2, "b"),
          (3, "c")])
    H.it "first list shorter" $ H.shouldBe
      (Lists.zip [
          1,
          2] [
          "a",
          "b",
          "c"])
      ([
          (1, "a"),
          (2, "b")])
    H.it "second list shorter" $ H.shouldBe
      (Lists.zip [
          1,
          2,
          3] [
          "a",
          "b"])
      ([
          (1, "a"),
          (2, "b")])
    H.it "empty first list" $ H.shouldBe
      (Lists.zip [] [
          "a",
          "b"])
      ([] :: [(Int, String)])
    H.it "empty second list" $ H.shouldBe
      (Lists.zip [
          1,
          2] [])
      ([] :: [(Int, Int)])
    H.it "both empty lists" $ H.shouldBe
      (Lists.zip [] [])
      ([] :: [(Int, Int)])
  H.describe "zipWith" $ do
    H.it "addition" $ H.shouldBe
      (Lists.zipWith Math.add [
          1,
          2,
          3] [
          4,
          5,
          6])
      ([
          5,
          7,
          9])
    H.it "first list shorter" $ H.shouldBe
      (Lists.zipWith Math.add [
          1,
          2] [
          4,
          5,
          6])
      ([
          5,
          7])
    H.it "second list shorter" $ H.shouldBe
      (Lists.zipWith Math.add [
          1,
          2,
          3] [
          4,
          5])
      ([
          5,
          7])
    H.it "empty first list" $ H.shouldBe
      (Lists.zipWith Math.add [] [
          1,
          2,
          3])
      ([])
    H.it "empty second list" $ H.shouldBe
      (Lists.zipWith Math.add [
          1,
          2,
          3] [])
      ([])
    H.it "string concatenation" $ H.shouldBe
      (Lists.zipWith Strings.cat2 [
          "a",
          "b"] [
          "1",
          "2"])
      ([
          "a1",
          "b2"])
