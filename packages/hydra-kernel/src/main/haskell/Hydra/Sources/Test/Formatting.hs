module Hydra.Sources.Test.Formatting where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import qualified Hydra.Dsl.Util as Util
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names as Names

import Hydra.Testing


ns :: ModuleName
ns = ModuleName "hydra.test.formatting"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, Formatting.ns, Names.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for string formatting and case conversion"))}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition caseConversionTests,
      Phantoms.toDefinition nameToUpperDashedTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for hydra.formatting" $
    Testing.testGroup (string "formatting") nothing (list subgroups) (list ([] :: [TypedTerm TestCaseWithMetadata]))
  where
    subgroups = [
      caseConversionTests,
      nameToUpperDashedTests]

caseConversionTests :: TypedTermDefinition TestGroup
caseConversionTests = define "caseConversionTests" $
  doc "Test cases for case conversion" $
  Testing.testGroup (string "case conversion") nothing (list ([] :: [TypedTerm TestGroup])) (list match)
  where
    match = [
      -- from lower_snake_case
      testCase 1 CaseConventionLowerSnake CaseConventionUpperSnake "a_hello_world_42_a42_42a_b" "A_HELLO_WORLD_42_A42_42A_B",
      testCase 2 CaseConventionLowerSnake CaseConventionCamel "a_hello_world_42_a42_42a_b" "aHelloWorld42A4242aB",
      testCase 3 CaseConventionLowerSnake CaseConventionPascal "a_hello_world_42_a42_42a_b" "AHelloWorld42A4242aB",
      testCase 4 CaseConventionLowerSnake CaseConventionLowerSnake "a_hello_world_42_a42_42a_b" "a_hello_world_42_a42_42a_b",
      testCase 17 CaseConventionLowerSnake CaseConventionUpperDashed "a_hello_world_42_a42_42a_b" "A-HELLO-WORLD-42-A42-42A-B",

      -- from UPPER_SNAKE_CASE
      testCase 5 CaseConventionUpperSnake CaseConventionLowerSnake "A_HELLO_WORLD_42_A42_42A_B" "a_hello_world_42_a42_42a_b",
      testCase 6 CaseConventionUpperSnake CaseConventionCamel "A_HELLO_WORLD_42_A42_42A_B" "aHelloWorld42A4242aB",
      testCase 7 CaseConventionUpperSnake CaseConventionPascal "A_HELLO_WORLD_42_A42_42A_B" "AHelloWorld42A4242aB",
      testCase 8 CaseConventionUpperSnake CaseConventionUpperSnake "A_HELLO_WORLD_42_A42_42A_B" "A_HELLO_WORLD_42_A42_42A_B",
      testCase 18 CaseConventionUpperSnake CaseConventionUpperDashed "A_HELLO_WORLD_42_A42_42A_B" "A-HELLO-WORLD-42-A42-42A-B",

      -- from camelCase
      testCase 9 CaseConventionCamel CaseConventionLowerSnake "aHelloWorld42A4242aB" "a_hello_world42_a4242a_b",
      testCase 10 CaseConventionCamel CaseConventionUpperSnake "aHelloWorld42A4242aB" "A_HELLO_WORLD42_A4242A_B",
      testCase 11 CaseConventionCamel CaseConventionPascal "aHelloWorld42A4242aB" "AHelloWorld42A4242aB",
      testCase 12 CaseConventionCamel CaseConventionCamel "aHelloWorld42A4242aB" "aHelloWorld42A4242aB",
      testCase 19 CaseConventionCamel CaseConventionUpperDashed "aHelloWorld42A4242aB" "A-HELLO-WORLD42-A4242A-B",

      -- from PascalCase
      testCase 13 CaseConventionPascal CaseConventionLowerSnake "AHelloWorld42A4242aB" "a_hello_world42_a4242a_b",
      testCase 14 CaseConventionPascal CaseConventionUpperSnake "AHelloWorld42A4242aB" "A_HELLO_WORLD42_A4242A_B",
      testCase 15 CaseConventionPascal CaseConventionCamel "AHelloWorld42A4242aB" "aHelloWorld42A4242aB",
      testCase 16 CaseConventionPascal CaseConventionPascal "AHelloWorld42A4242aB" "AHelloWorld42A4242aB",
      testCase 20 CaseConventionPascal CaseConventionUpperDashed "AHelloWorld42A4242aB" "A-HELLO-WORLD42-A4242A-B",

      -- from UPPER-DASHED-CASE
      testCase 21 CaseConventionUpperDashed CaseConventionLowerSnake "A-HELLO-WORLD-42-A42-42A-B" "a_hello_world_42_a42_42a_b",
      testCase 22 CaseConventionUpperDashed CaseConventionUpperSnake "A-HELLO-WORLD-42-A42-42A-B" "A_HELLO_WORLD_42_A42_42A_B",
      testCase 23 CaseConventionUpperDashed CaseConventionCamel "A-HELLO-WORLD-42-A42-42A-B" "aHelloWorld42A4242aB",
      testCase 24 CaseConventionUpperDashed CaseConventionPascal "A-HELLO-WORLD-42-A42-42A-B" "AHelloWorld42A4242aB",
      testCase 25 CaseConventionUpperDashed CaseConventionUpperDashed "A-HELLO-WORLD-42-A42-42A-B" "A-HELLO-WORLD-42-A42-42A-B"]

nameToUpperDashedTests :: TypedTermDefinition TestGroup
nameToUpperDashedTests = define "nameToUpperDashedTests" $
  doc "Test cases for rendering a Name in UPPER-DASHED display form" $
  Testing.testGroup (string "name to upper-dashed") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    nameCase 1 "hydra.lib.lists.concat.emptyLists" "HYDRA-LIB-LISTS-CONCAT-EMPTY-LISTS",
    nameCase 2 "hydra.core.Name" "HYDRA-CORE-NAME",
    nameCase 3 "singlePart" "SINGLE-PART"])
  where
    nameCase i input expected = universalCase name actual (string expected)
      where
        actual = Names.nameToUpperDashed @@ (Core.name $ string input)
        name = "#" ++ show i ++ " (" ++ input ++ ")"

-- Helpers

testCase :: Int -> CaseConvention -> CaseConvention -> String -> String -> TypedTerm TestCaseWithMetadata
testCase i fromConvention toConvention fromString toString =
    universalCase name actual expected
  where
    actual = Formatting.convertCase @@ metaConv fromConvention
      @@ metaConv toConvention @@ string fromString
    expected = string toString
    name = "#" ++ show i ++ " (" ++ showCaseConvention fromConvention ++ " -> " ++ showCaseConvention toConvention ++ ")"
    metaConv conv = case conv of
      CaseConventionLowerSnake -> Util.caseConventionLowerSnake
      CaseConventionUpperSnake -> Util.caseConventionUpperSnake
      CaseConventionCamel -> Util.caseConventionCamel
      CaseConventionPascal -> Util.caseConventionPascal
      CaseConventionUpperDashed -> Util.caseConventionUpperDashed

-- Local mirror of hydra.print.util.caseConvention, for readable test names only.
-- Deliberately not calling the generated Hydra.Print.Util here: this DSL source is
-- compiled against BOTH the local (renamed) and #376-cold-seeded (published, pre-rename)
-- kernels, and a single import can't satisfy both. See #497 plan for the general issue.
showCaseConvention :: CaseConvention -> String
showCaseConvention conv = case conv of
  CaseConventionLowerSnake -> "lower_snake_case"
  CaseConventionUpperSnake -> "UPPER_SNAKE_CASE"
  CaseConventionCamel -> "camelCase"
  CaseConventionPascal -> "PascalCase"
  CaseConventionUpperDashed -> "UPPER-DASHED-CASE"
