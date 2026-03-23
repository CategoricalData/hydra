-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (see generated module)
-- DEBUG: Namespace mappings: (see generated module)

module Generation.Hydra.Test.Lib.RegexSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lib.Regex as Regex

spec :: H.Spec
spec = H.describe "hydra.lib.regex primitives" $ do
  H.describe "matches" $ do
    H.it "exact match" $ H.shouldBe
      (Regex.matches "hello" "hello")
      (True)
    H.it "pattern match" $ H.shouldBe
      (Regex.matches "[a-z]+" "hello")
      (True)
    H.it "no match" $ H.shouldBe
      (Regex.matches "[0-9]+" "hello")
      (False)
    H.it "partial content does not match" $ H.shouldBe
      (Regex.matches "[a-z]+" "hello123")
      (False)
    H.it "digit pattern" $ H.shouldBe
      (Regex.matches "[0-9]+" "12345")
      (True)
    H.it "mixed pattern" $ H.shouldBe
      (Regex.matches "[a-z]+[0-9]+" "hello123")
      (True)
    H.it "empty pattern matches empty" $ H.shouldBe
      (Regex.matches "" "")
      (True)
    H.it "empty pattern does not match non-empty" $ H.shouldBe
      (Regex.matches "" "hello")
      (False)
    H.it "star matches empty" $ H.shouldBe
      (Regex.matches "a*" "")
      (True)
    H.it "alternation" $ H.shouldBe
      (Regex.matches "cat|dog" "cat")
      (True)
    H.it "alternation second" $ H.shouldBe
      (Regex.matches "cat|dog" "dog")
      (True)
    H.it "alternation no match" $ H.shouldBe
      (Regex.matches "cat|dog" "bird")
      (False)
    H.it "quantifier" $ H.shouldBe
      (Regex.matches "ab?c" "ac")
      (True)
    H.it "quantifier with optional" $ H.shouldBe
      (Regex.matches "ab?c" "abc")
      (True)
  H.describe "find" $ do
    H.it "simple find" $ H.shouldBe
      (Regex.find "[0-9]+" "abc123def")
      (Just "123")
    H.it "no match" $ H.shouldBe
      (Regex.find "[0-9]+" "abcdef")
      (Nothing)
    H.it "find first" $ H.shouldBe
      (Regex.find "[a-z]+" "123abc456def")
      (Just "abc")
    H.it "empty input" $ H.shouldBe
      (Regex.find "[0-9]+" "")
      (Nothing)
    H.it "full match" $ H.shouldBe
      (Regex.find ".*" "hello")
      (Just "hello")
  H.describe "findAll" $ do
    H.it "multiple matches" $ H.shouldBe
      (Regex.findAll "[0-9]+" "a1b2c3")
      ([
          "1",
          "2",
          "3"])
    H.it "no matches" $ H.shouldBe
      (Regex.findAll "[0-9]+" "abc")
      ([])
    H.it "overlapping words" $ H.shouldBe
      (Regex.findAll "[a-z]+" "abc def ghi")
      ([
          "abc",
          "def",
          "ghi"])
    H.it "single match" $ H.shouldBe
      (Regex.findAll "hello" "say hello world")
      ([
          "hello"])
  H.describe "replace" $ do
    H.it "basic replace" $ H.shouldBe
      (Regex.replace "[0-9]+" "X" "abc123def456")
      ("abcXdef456")
    H.it "no match" $ H.shouldBe
      (Regex.replace "[0-9]+" "X" "abcdef")
      ("abcdef")
    H.it "replace at start" $ H.shouldBe
      (Regex.replace "^[a-z]+" "X" "abc123")
      ("X123")
    H.it "empty replacement" $ H.shouldBe
      (Regex.replace "[0-9]+" "" "abc123def")
      ("abcdef")
  H.describe "replaceAll" $ do
    H.it "replace all digits" $ H.shouldBe
      (Regex.replaceAll "[0-9]+" "X" "a1b2c3")
      ("aXbXcX")
    H.it "no match" $ H.shouldBe
      (Regex.replaceAll "[0-9]+" "X" "abc")
      ("abc")
    H.it "replace all words" $ H.shouldBe
      (Regex.replaceAll "[a-z]+" "X" "abc 123 def")
      ("X 123 X")
    H.it "empty replacement" $ H.shouldBe
      (Regex.replaceAll "[0-9]+" "" "a1b2c3")
      ("abc")
  H.describe "split" $ do
    H.it "split on comma" $ H.shouldBe
      (Regex.split "," "a,b,c")
      ([
          "a",
          "b",
          "c"])
    H.it "split on spaces" $ H.shouldBe
      (Regex.split " +" "a b  c")
      ([
          "a",
          "b",
          "c"])
    H.it "no match" $ H.shouldBe
      (Regex.split "," "abc")
      ([
          "abc"])
    H.it "split on digits" $ H.shouldBe
      (Regex.split "[0-9]+" "a1b2c")
      ([
          "a",
          "b",
          "c"])
    H.it "trailing delimiter" $ H.shouldBe
      (Regex.split "," "a,b,")
      ([
          "a",
          "b",
          ""])
