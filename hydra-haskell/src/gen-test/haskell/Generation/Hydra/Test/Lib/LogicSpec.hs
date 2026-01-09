-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.logic"},ModuleName {unModuleName = "Logic"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lexical"},ModuleName {unModuleName = "Lexical"}),(Namespace {unNamespace = "hydra.lib.logic"},ModuleName {unModuleName = "Logic"})]

module Generation.Hydra.Test.Lib.LogicSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Logic as Logic

spec :: H.Spec
spec = H.describe "hydra.lib.logic primitives" $ do
  H.describe "and" $ do
    H.it "true and true" $ H.shouldBe
      (Logic.and True True)
      (True)
    H.it "true and false" $ H.shouldBe
      (Logic.and True False)
      (False)
    H.it "false and true" $ H.shouldBe
      (Logic.and False True)
      (False)
    H.it "false and false" $ H.shouldBe
      (Logic.and False False)
      (False)
  H.describe "ifElse" $ do
    H.describe "boolean values" $ do
      H.it "true condition returns then" $ H.shouldBe
        (Logic.ifElse True True False)
        (True)
      H.it "false condition returns else" $ H.shouldBe
        (Logic.ifElse False True False)
        (False)
    H.describe "integer values" $ do
      H.it "true selects first int" $ H.shouldBe
        (Logic.ifElse True 42 0)
        (42)
      H.it "false selects second int" $ H.shouldBe
        (Logic.ifElse False 42 0)
        (0)
    H.describe "string values" $ do
      H.it "true selects first string" $ H.shouldBe
        (Logic.ifElse True "yes" "no")
        ("yes")
      H.it "false selects second string" $ H.shouldBe
        (Logic.ifElse False "yes" "no")
        ("no")
  H.describe "not" $ do
    H.it "not true" $ H.shouldBe
      (Logic.not True)
      (False)
    H.it "not false" $ H.shouldBe
      (Logic.not False)
      (True)
  H.describe "or" $ do
    H.it "true or true" $ H.shouldBe
      (Logic.or True True)
      (True)
    H.it "true or false" $ H.shouldBe
      (Logic.or True False)
      (True)
    H.it "false or true" $ H.shouldBe
      (Logic.or False True)
      (True)
    H.it "false or false" $ H.shouldBe
      (Logic.or False False)
      (False)
