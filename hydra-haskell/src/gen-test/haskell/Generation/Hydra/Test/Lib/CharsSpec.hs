-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.chars"},ModuleName {unModuleName = "Chars"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lexical"},ModuleName {unModuleName = "Lexical"}),(Namespace {unNamespace = "hydra.lib.chars"},ModuleName {unModuleName = "Chars"})]

module Generation.Hydra.Test.Lib.CharsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Chars as Chars

spec :: H.Spec
spec = H.describe "hydra.lib.chars primitives" $ do
  H.describe "isAlphaNum" $ do
    H.it "letter" $ H.shouldBe
      (Chars.isAlphaNum 97)
      (True)
    H.it "digit" $ H.shouldBe
      (Chars.isAlphaNum 53)
      (True)
    H.it "space" $ H.shouldBe
      (Chars.isAlphaNum 32)
      (False)
    H.it "punctuation" $ H.shouldBe
      (Chars.isAlphaNum 46)
      (False)
  H.describe "isLower" $ do
    H.it "lowercase" $ H.shouldBe
      (Chars.isLower 97)
      (True)
    H.it "uppercase" $ H.shouldBe
      (Chars.isLower 65)
      (False)
    H.it "digit" $ H.shouldBe
      (Chars.isLower 53)
      (False)
  H.describe "isSpace" $ do
    H.it "space" $ H.shouldBe
      (Chars.isSpace 32)
      (True)
    H.it "tab" $ H.shouldBe
      (Chars.isSpace 9)
      (True)
    H.it "newline" $ H.shouldBe
      (Chars.isSpace 10)
      (True)
    H.it "letter" $ H.shouldBe
      (Chars.isSpace 97)
      (False)
  H.describe "isUpper" $ do
    H.it "uppercase" $ H.shouldBe
      (Chars.isUpper 65)
      (True)
    H.it "lowercase" $ H.shouldBe
      (Chars.isUpper 97)
      (False)
    H.it "digit" $ H.shouldBe
      (Chars.isUpper 53)
      (False)
  H.describe "toLower" $ do
    H.it "uppercase" $ H.shouldBe
      (Chars.toLower 65)
      (97)
    H.it "lowercase" $ H.shouldBe
      (Chars.toLower 97)
      (97)
    H.it "digit" $ H.shouldBe
      (Chars.toLower 53)
      (53)
  H.describe "toUpper" $ do
    H.it "lowercase" $ H.shouldBe
      (Chars.toUpper 97)
      (65)
    H.it "uppercase" $ H.shouldBe
      (Chars.toUpper 65)
      (65)
    H.it "digit" $ H.shouldBe
      (Chars.toUpper 53)
      (53)
