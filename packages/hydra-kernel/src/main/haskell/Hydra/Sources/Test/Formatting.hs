module Hydra.Sources.Test.Formatting where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import           Hydra.Dsl.Meta.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import qualified Hydra.Dsl.Util as Util
import qualified Hydra.Show.Util as ShowUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting as FormattingModule

import Hydra.Testing


ns :: Namespace
ns = Namespace "hydra.test.formatting"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [TestGraph.ns, FormattingModule.ns],
            moduleTypeDependencies = kernelTypesNamespaces,
            moduleDescription = (Just "Test cases for string formatting and case conversion")}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition caseConversionTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for hydra.formatting" $
    Testing.testGroup (string "formatting") nothing (list subgroups) (list ([] :: [TTerm TestCaseWithMetadata]))
  where
    subgroups = [
      caseConversionTests]

caseConversionTests :: TTermDefinition TestGroup
caseConversionTests = define "caseConversionTests" $
  doc "Test cases for case conversion" $
  Testing.testGroup (string "case conversion") nothing (list ([] :: [TTerm TestGroup])) (list cases)
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
testCase i fromConvention toConvention fromString toString =
    universalCase name actual expected
  where
    actual = FormattingModule.convertCase @@ metaConv fromConvention
      @@ metaConv toConvention @@ string fromString
    expected = string toString
    name = "#" ++ show i ++ " (" ++ ShowUtil.caseConvention fromConvention ++ " -> " ++ ShowUtil.caseConvention toConvention ++ ")"
    metaConv conv = case conv of
      CaseConventionLowerSnake -> Util.caseConventionLowerSnake
      CaseConventionUpperSnake -> Util.caseConventionUpperSnake
      CaseConventionCamel -> Util.caseConventionCamel
      CaseConventionPascal -> Util.caseConventionPascal
