-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.formatting"},ModuleName {unModuleName = "Formatting"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.formatting"},ModuleName {unModuleName = "Formatting"}),(Namespace {unNamespace = "hydra.lexical"},ModuleName {unModuleName = "Lexical"}),(Namespace {unNamespace = "hydra.util"},ModuleName {unModuleName = "Util"})]

module Generation.Hydra.Test.FormattingSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Util as Util

spec :: H.Spec
spec = H.describe "formatting" $ do
  H.describe "case conversion" $ do
    H.it "#1 (lower_snake_case -> UPPER_SNAKE_CASE)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionLowerSnake Util.CaseConventionUpperSnake "a_hello_world_42_a42_42a_b")
      ("A_HELLO_WORLD_42_A42_42A_B")
    H.it "#2 (lower_snake_case -> camelCase)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionLowerSnake Util.CaseConventionCamel "a_hello_world_42_a42_42a_b")
      ("aHelloWorld42A4242aB")
    H.it "#3 (lower_snake_case -> PascalCase)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionLowerSnake Util.CaseConventionPascal "a_hello_world_42_a42_42a_b")
      ("AHelloWorld42A4242aB")
    H.it "#4 (lower_snake_case -> lower_snake_case)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionLowerSnake Util.CaseConventionLowerSnake "a_hello_world_42_a42_42a_b")
      ("a_hello_world_42_a42_42a_b")
    H.it "#5 (UPPER_SNAKE_CASE -> lower_snake_case)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionUpperSnake Util.CaseConventionLowerSnake "A_HELLO_WORLD_42_A42_42A_B")
      ("a_hello_world_42_a42_42a_b")
    H.it "#6 (UPPER_SNAKE_CASE -> camelCase)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionUpperSnake Util.CaseConventionCamel "A_HELLO_WORLD_42_A42_42A_B")
      ("aHelloWorld42A4242aB")
    H.it "#7 (UPPER_SNAKE_CASE -> PascalCase)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionUpperSnake Util.CaseConventionPascal "A_HELLO_WORLD_42_A42_42A_B")
      ("AHelloWorld42A4242aB")
    H.it "#8 (UPPER_SNAKE_CASE -> UPPER_SNAKE_CASE)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionUpperSnake Util.CaseConventionUpperSnake "A_HELLO_WORLD_42_A42_42A_B")
      ("A_HELLO_WORLD_42_A42_42A_B")
    H.it "#9 (camelCase -> lower_snake_case)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionLowerSnake "aHelloWorld42A4242aB")
      ("a_hello_world42_a4242a_b")
    H.it "#10 (camelCase -> UPPER_SNAKE_CASE)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionUpperSnake "aHelloWorld42A4242aB")
      ("A_HELLO_WORLD42_A4242A_B")
    H.it "#11 (camelCase -> PascalCase)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionPascal "aHelloWorld42A4242aB")
      ("AHelloWorld42A4242aB")
    H.it "#12 (camelCase -> camelCase)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionCamel "aHelloWorld42A4242aB")
      ("aHelloWorld42A4242aB")
    H.it "#13 (PascalCase -> lower_snake_case)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionLowerSnake "AHelloWorld42A4242aB")
      ("a_hello_world42_a4242a_b")
    H.it "#14 (PascalCase -> UPPER_SNAKE_CASE)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionUpperSnake "AHelloWorld42A4242aB")
      ("A_HELLO_WORLD42_A4242A_B")
    H.it "#15 (PascalCase -> camelCase)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionCamel "AHelloWorld42A4242aB")
      ("aHelloWorld42A4242aB")
    H.it "#16 (PascalCase -> PascalCase)" $ H.shouldBe
      (Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionPascal "AHelloWorld42A4242aB")
      ("AHelloWorld42A4242aB")
