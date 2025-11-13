module Hydra.Sources.Test.Formatting (formattingTests) where

import Hydra.Kernel
import Hydra.Dsl.Tests
import Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types


formattingTests :: TestGroup
formattingTests = TestGroup "formatting tests" Nothing [] cases
  where
    cases = [
      -- from lower_snake_case
      testCase 1 CaseConventionLowerSnake CaseConventionUpperSnake "a_hello_world_42_a42_42a_b" "A_HELLO_WORLD_42_A42_42A_B",
      testCase 2 CaseConventionLowerSnake CaseConventionCamel "a_hello_world_42_a42_42a_b" "aHelloWorld42A4242aB",
      testCase 3 CaseConventionLowerSnake CaseConventionPascal "a_hello_world_42_a42_42a_b" "AHelloWorld42A4242aB",
      testCase 4 CaseConventionLowerSnake CaseConventionLowerSnake "a_hello_world_42_a42_42a_b" "a_hello_world_42_a42_42a_b",

      -- from UPPER_SNAKE_CASE
      testCase 5 CaseConventionUpperSnake CaseConventionLowerSnake "A_HELLO_WORLD_42_A42_42A_B" "a_hello_world_42_a42_42a_b",
      testCase 6 CaseConventionUpperSnake CaseConventionCamel "A_HELLO_WORLD_42_A42_42A_B" "aHelloWorld42A4242aB",
      testCase 7 CaseConventionUpperSnake CaseConventionPascal "A_HELLO_WORLD_42_A42_42A_B" "AHelloWorld42A4242aB",
      testCase 8 CaseConventionUpperSnake CaseConventionUpperSnake "A_HELLO_WORLD_42_A42_42A_B" "A_HELLO_WORLD_42_A42_42A_B",

      -- from camelCase
      testCase 9 CaseConventionCamel CaseConventionLowerSnake "aHelloWorld42A4242aB" "a_hello_world42_a4242a_b",
      testCase 10 CaseConventionCamel CaseConventionUpperSnake "aHelloWorld42A4242aB" "A_HELLO_WORLD42_A4242A_B",
      testCase 11 CaseConventionCamel CaseConventionPascal "aHelloWorld42A4242aB" "AHelloWorld42A4242aB",
      testCase 12 CaseConventionCamel CaseConventionCamel "aHelloWorld42A4242aB" "aHelloWorld42A4242aB",

      -- from PascalCase
      testCase 13 CaseConventionPascal CaseConventionLowerSnake "AHelloWorld42A4242aB" "a_hello_world42_a4242a_b",
      testCase 14 CaseConventionPascal CaseConventionUpperSnake "AHelloWorld42A4242aB" "A_HELLO_WORLD42_A4242A_B",
      testCase 15 CaseConventionPascal CaseConventionCamel "AHelloWorld42A4242aB" "aHelloWorld42A4242aB",
      testCase 16 CaseConventionPascal CaseConventionPascal "AHelloWorld42A4242aB" "AHelloWorld42A4242aB"]

    testCase i fromConvention toConvention fromString toString = TestCaseWithMetadata name tcase Nothing []
      where
        tcase = TestCaseCaseConversion $ CaseConversionTestCase fromConvention toConvention fromString toString
        name = "#" ++ show i ++ " (" ++ showConvention fromConvention ++ " -> " ++ showConvention toConvention ++ ")"

    showConvention c = case c of
      CaseConventionLowerSnake -> "lower_snake_case"
      CaseConventionUpperSnake -> "UPPER_SNAKE_CASE"
      CaseConventionCamel -> "camelCase"
      CaseConventionPascal -> "PascalCase"
