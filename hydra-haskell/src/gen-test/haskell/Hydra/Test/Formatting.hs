-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for string formatting and case conversion

module Hydra.Test.Formatting where

import qualified Hydra.Testing as Testing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for hydra.formatting
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "formatting",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    caseConversionTests],
  Testing.testGroupCases = []}

-- | Test cases for case conversion
caseConversionTests :: Testing.TestGroup
caseConversionTests = Testing.TestGroup {
  Testing.testGroupName = "case conversion",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#1 (lower_snake_case -> UPPER_SNAKE_CASE)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseFromString = "a_hello_world_42_a42_42a_b",
        Testing.caseConversionTestCaseToString = "A_HELLO_WORLD_42_A42_42A_B"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#2 (lower_snake_case -> camelCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionCamel,
        Testing.caseConversionTestCaseFromString = "a_hello_world_42_a42_42a_b",
        Testing.caseConversionTestCaseToString = "aHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#3 (lower_snake_case -> PascalCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionPascal,
        Testing.caseConversionTestCaseFromString = "a_hello_world_42_a42_42a_b",
        Testing.caseConversionTestCaseToString = "AHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#4 (lower_snake_case -> lower_snake_case)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseFromString = "a_hello_world_42_a42_42a_b",
        Testing.caseConversionTestCaseToString = "a_hello_world_42_a42_42a_b"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#5 (UPPER_SNAKE_CASE -> lower_snake_case)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseFromString = "A_HELLO_WORLD_42_A42_42A_B",
        Testing.caseConversionTestCaseToString = "a_hello_world_42_a42_42a_b"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#6 (UPPER_SNAKE_CASE -> camelCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionCamel,
        Testing.caseConversionTestCaseFromString = "A_HELLO_WORLD_42_A42_42A_B",
        Testing.caseConversionTestCaseToString = "aHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#7 (UPPER_SNAKE_CASE -> PascalCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionPascal,
        Testing.caseConversionTestCaseFromString = "A_HELLO_WORLD_42_A42_42A_B",
        Testing.caseConversionTestCaseToString = "AHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#8 (UPPER_SNAKE_CASE -> UPPER_SNAKE_CASE)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseFromString = "A_HELLO_WORLD_42_A42_42A_B",
        Testing.caseConversionTestCaseToString = "A_HELLO_WORLD_42_A42_42A_B"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#9 (camelCase -> lower_snake_case)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionCamel,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseFromString = "aHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "a_hello_world42_a4242a_b"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#10 (camelCase -> UPPER_SNAKE_CASE)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionCamel,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseFromString = "aHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "A_HELLO_WORLD42_A4242A_B"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#11 (camelCase -> PascalCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionCamel,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionPascal,
        Testing.caseConversionTestCaseFromString = "aHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "AHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#12 (camelCase -> camelCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionCamel,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionCamel,
        Testing.caseConversionTestCaseFromString = "aHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "aHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#13 (PascalCase -> lower_snake_case)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionPascal,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseFromString = "AHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "a_hello_world42_a4242a_b"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#14 (PascalCase -> UPPER_SNAKE_CASE)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionPascal,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseFromString = "AHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "A_HELLO_WORLD42_A4242A_B"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#15 (PascalCase -> camelCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionPascal,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionCamel,
        Testing.caseConversionTestCaseFromString = "AHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "aHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#16 (PascalCase -> PascalCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Util.CaseConventionPascal,
        Testing.caseConversionTestCaseToConvention = Util.CaseConventionPascal,
        Testing.caseConversionTestCaseFromString = "AHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "AHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}
