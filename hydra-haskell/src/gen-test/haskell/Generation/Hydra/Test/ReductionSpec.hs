-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.reduction"},ModuleName {unModuleName = "Reduction"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lexical"},ModuleName {unModuleName = "Lexical"}),(Namespace {unNamespace = "hydra.lib.lists"},ModuleName {unModuleName = "Lists"}),(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.lib.sets"},ModuleName {unModuleName = "Sets"}),(Namespace {unNamespace = "hydra.lib.strings"},ModuleName {unModuleName = "Strings"})]

module Generation.Hydra.Test.ReductionSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings

spec :: H.Spec
spec = H.describe "reduction" $ do
  H.describe "beta reduction" $ do
    H.it "identity function applied to literal" $ H.shouldBe
      ((\x -> x) 42)
      (42)
    H.it "constant function" $ H.shouldBe
      ((\x -> 1) 42)
      (1)
    H.it "nested application" $ H.shouldBe
      ((\x -> \y -> x) 1 2)
      (1)
  H.describe "monomorphic primitives" $ do
    H.it "toUpper on lowercase" $ H.shouldBe
      (Strings.toUpper "hello")
      ("HELLO")
    H.it "toUpper on mixed case" $ H.shouldBe
      (Strings.toUpper "Hello World")
      ("HELLO WORLD")
    H.it "toUpper on empty string" $ H.shouldBe
      (Strings.toUpper "")
      ("")
    H.it "toLower on uppercase" $ H.shouldBe
      (Strings.toLower "HELLO")
      ("hello")
    H.it "string length" $ H.shouldBe
      (Strings.length "hello")
      (5)
    H.it "string length of empty" $ H.shouldBe
      (Strings.length "")
      (0)
    H.it "add two positive integers" $ H.shouldBe
      (Math.add 3 5)
      (8)
    H.it "add negative and positive" $ H.shouldBe
      (Math.add (-10) 3)
      ((-7))
    H.it "add with zero" $ H.shouldBe
      (Math.add 0 42)
      (42)
    H.it "subtract integers" $ H.shouldBe
      (Math.sub 10 3)
      (7)
    H.it "multiply integers" $ H.shouldBe
      (Math.mul 6 7)
      (42)
    H.it "multiply by zero" $ H.shouldBe
      (Math.mul 100 0)
      (0)
    H.it "divide integers" $ H.shouldBe
      (Math.div 20 4)
      (5)
    H.it "modulo" $ H.shouldBe
      (Math.mod 17 5)
      (2)
    H.it "splitOn basic" $ H.shouldBe
      (Strings.splitOn "," "a,b,c")
      ([
          "a",
          "b",
          "c"])
    H.it "cat2 strings" $ H.shouldBe
      (Strings.cat2 "hello" "world")
      ("helloworld")
  H.describe "polymorphic primitives" $ do
    H.it "length of integer list" $ H.shouldBe
      (Lists.length [
          1,
          2,
          3])
      (3)
    H.it "length of string list" $ H.shouldBe
      (Lists.length [
          "a",
          "b"])
      (2)
    H.it "length of empty list" $ H.shouldBe
      (Lists.length [])
      (0)
    H.it "length of single element list" $ H.shouldBe
      (Lists.length [
          True])
      (1)
    H.it "head of integer list" $ H.shouldBe
      (Lists.head [
          10,
          20,
          30])
      (10)
    H.it "head of string list" $ H.shouldBe
      (Lists.head [
          "first",
          "second"])
      ("first")
    H.it "last of integer list" $ H.shouldBe
      (Lists.last [
          10,
          20,
          30])
      (30)
    H.it "concat two integer lists" $ H.shouldBe
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
    H.it "concat with empty list" $ H.shouldBe
      (Lists.concat2 [] [
          1,
          2])
      ([
          1,
          2])
    H.it "reverse integer list" $ H.shouldBe
      (Lists.reverse [
          1,
          2,
          3])
      ([
          3,
          2,
          1])
    H.it "reverse empty list" $ H.shouldBe
      (Lists.reverse [])
      ([] :: [Int])
  H.describe "nullary primitives" $ do
    H.it "empty set has size zero" $ H.shouldBe
      (Sets.size Sets.empty)
      (0)
  H.describe "literals as values" $ do
    H.it "integer literal is a value" $ H.shouldBe
      (42)
      (42)
    H.it "negative integer literal" $ H.shouldBe
      ((-17))
      ((-17))
    H.it "zero integer literal" $ H.shouldBe
      (0)
      (0)
    H.it "string literal is a value" $ H.shouldBe
      ("hello")
      ("hello")
    H.it "empty string literal" $ H.shouldBe
      ("")
      ("")
    H.it "string with special characters" $ H.shouldBe
      ("hello\nworld\ttab")
      ("hello\nworld\ttab")
    H.it "boolean true is a value" $ H.shouldBe
      (True)
      (True)
    H.it "boolean false is a value" $ H.shouldBe
      (False)
      (False)
    H.it "float literal is a value" $ H.shouldBe
      (3.14)
      (3.14)
    H.it "negative float literal" $ H.shouldBe
      ((-2.718))
      ((-2.718))
    H.it "zero float literal" $ H.shouldBe
      (0.0)
      (0.0)
  H.describe "list reduction" $ do
    H.it "empty list is a value" $ H.shouldBe
      ([])
      ([] :: [Int])
    H.it "list of literals is a value" $ H.shouldBe
      ([
          1,
          2,
          3])
      ([
          1,
          2,
          3])
    H.it "list with reducible element" $ H.shouldBe
      ([
          (\x -> x) 42])
      ([
          42])
  H.describe "optional reduction" $ do
    H.it "nothing is a value" $ H.shouldBe
      (Nothing)
      (Nothing :: Maybe Int)
    H.it "just literal is a value" $ H.shouldBe
      (Just 42)
      (Just 42)
    H.it "just with reducible content" $ H.shouldBe
      (Just ((\x -> x) 42))
      (Just 42)
