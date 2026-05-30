-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for string formatting and case conversion

module Hydra.Test.Formatting where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Test cases for hydra.formatting
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "formatting",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        caseConversionTests],
      Testing.testGroupCases = []}
-- | Test cases for case conversion
caseConversionTests :: Testing.TestGroup
caseConversionTests =
    Testing.TestGroup {
      Testing.testGroupName = "case conversion",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1 (lower_snake_case -> UPPER_SNAKE_CASE)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionLowerSnake Util.CaseConventionUpperSnake "a_hello_world_42_a42_42a_b"),
            Testing.universalTestCaseExpected = (\_ -> "A_HELLO_WORLD_42_A42_42A_B")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2 (lower_snake_case -> camelCase)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionLowerSnake Util.CaseConventionCamel "a_hello_world_42_a42_42a_b"),
            Testing.universalTestCaseExpected = (\_ -> "aHelloWorld42A4242aB")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3 (lower_snake_case -> PascalCase)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionLowerSnake Util.CaseConventionPascal "a_hello_world_42_a42_42a_b"),
            Testing.universalTestCaseExpected = (\_ -> "AHelloWorld42A4242aB")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4 (lower_snake_case -> lower_snake_case)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionLowerSnake Util.CaseConventionLowerSnake "a_hello_world_42_a42_42a_b"),
            Testing.universalTestCaseExpected = (\_ -> "a_hello_world_42_a42_42a_b")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#5 (UPPER_SNAKE_CASE -> lower_snake_case)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionUpperSnake Util.CaseConventionLowerSnake "A_HELLO_WORLD_42_A42_42A_B"),
            Testing.universalTestCaseExpected = (\_ -> "a_hello_world_42_a42_42a_b")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#6 (UPPER_SNAKE_CASE -> camelCase)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionUpperSnake Util.CaseConventionCamel "A_HELLO_WORLD_42_A42_42A_B"),
            Testing.universalTestCaseExpected = (\_ -> "aHelloWorld42A4242aB")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#7 (UPPER_SNAKE_CASE -> PascalCase)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionUpperSnake Util.CaseConventionPascal "A_HELLO_WORLD_42_A42_42A_B"),
            Testing.universalTestCaseExpected = (\_ -> "AHelloWorld42A4242aB")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#8 (UPPER_SNAKE_CASE -> UPPER_SNAKE_CASE)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionUpperSnake Util.CaseConventionUpperSnake "A_HELLO_WORLD_42_A42_42A_B"),
            Testing.universalTestCaseExpected = (\_ -> "A_HELLO_WORLD_42_A42_42A_B")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#9 (camelCase -> lower_snake_case)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionLowerSnake "aHelloWorld42A4242aB"),
            Testing.universalTestCaseExpected = (\_ -> "a_hello_world42_a4242a_b")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#10 (camelCase -> UPPER_SNAKE_CASE)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionUpperSnake "aHelloWorld42A4242aB"),
            Testing.universalTestCaseExpected = (\_ -> "A_HELLO_WORLD42_A4242A_B")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#11 (camelCase -> PascalCase)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionPascal "aHelloWorld42A4242aB"),
            Testing.universalTestCaseExpected = (\_ -> "AHelloWorld42A4242aB")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#12 (camelCase -> camelCase)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionCamel "aHelloWorld42A4242aB"),
            Testing.universalTestCaseExpected = (\_ -> "aHelloWorld42A4242aB")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#13 (PascalCase -> lower_snake_case)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionLowerSnake "AHelloWorld42A4242aB"),
            Testing.universalTestCaseExpected = (\_ -> "a_hello_world42_a4242a_b")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#14 (PascalCase -> UPPER_SNAKE_CASE)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionUpperSnake "AHelloWorld42A4242aB"),
            Testing.universalTestCaseExpected = (\_ -> "A_HELLO_WORLD42_A4242A_B")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#15 (PascalCase -> camelCase)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionCamel "AHelloWorld42A4242aB"),
            Testing.universalTestCaseExpected = (\_ -> "aHelloWorld42A4242aB")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#16 (PascalCase -> PascalCase)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionPascal "AHelloWorld42A4242aB"),
            Testing.universalTestCaseExpected = (\_ -> "AHelloWorld42A4242aB")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
