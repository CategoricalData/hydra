module Hydra.Sources.Test.Formatting where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import qualified Hydra.Dsl.Meta.Util as Util
import qualified Hydra.Show.Util as ShowUtil


ns :: Namespace
ns = Namespace "hydra.test.formatting"

module_ :: Module
module_ = Module ns elements
    [TestGraph.ns]
    kernelTypesNamespaces
    (Just "Test cases for string formatting and case conversion")
  where
    elements = [
      Phantoms.toBinding allTests,
      Phantoms.toBinding caseConversionTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for hydra.formatting" $
    Testing.testGroup (Phantoms.string "formatting") Phantoms.nothing (Phantoms.list subgroups) (Phantoms.list ([] :: [TTerm TestCaseWithMetadata]))
  where
    subgroups = [
      caseConversionTests]

caseConversionTests :: TBinding TestGroup
caseConversionTests = define "caseConversionTests" $
  Phantoms.doc "Test cases for case conversion" $
  Testing.testGroup (Phantoms.string "case conversion") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list cases)
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

-- Helpers

testCase :: Int -> CaseConvention -> CaseConvention -> String -> String -> TTerm TestCaseWithMetadata
testCase i fromConvention toConvention fromString toString = Testing.testCaseWithMetadata name tcase Phantoms.nothing (Phantoms.list ([] :: [TTerm Tag]))
  where
    tcase = Testing.testCaseCaseConversion $ Testing.caseConversionTestCase
      (metaConv fromConvention)
      (metaConv toConvention)
      (Phantoms.string fromString)
      (Phantoms.string toString)
    name = Phantoms.string $ "#" ++ show i ++ " (" ++ ShowUtil.caseConvention fromConvention ++ " -> " ++ ShowUtil.caseConvention toConvention ++ ")"
    metaConv conv = case conv of
      CaseConventionLowerSnake -> Util.caseConventionLowerSnake
      CaseConventionUpperSnake -> Util.caseConventionUpperSnake
      CaseConventionCamel -> Util.caseConventionCamel
      CaseConventionPascal -> Util.caseConventionPascal
