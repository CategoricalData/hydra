-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for JSON serialization

module Hydra.Test.Json.Writer where

import qualified Hydra.Json.Model as Model
import qualified Hydra.Json.Writer as Writer
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Hydra.Lib.Literals as Literals

-- | Test cases for JSON serialization (writer)
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "JSON serialization",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "primitives",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "null",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson Model.ValueNull),
                Testing.universalTestCaseExpected = "null"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueBoolean True)),
                Testing.universalTestCaseExpected = "true"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueBoolean False)),
                Testing.universalTestCaseExpected = "false"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "zero",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "0.0"))),
                Testing.universalTestCaseExpected = "0"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "positive integer",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "42.0"))),
                Testing.universalTestCaseExpected = "42"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative integer",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "-17.0"))),
                Testing.universalTestCaseExpected = "-17"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "large integer",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "1000000.0"))),
                Testing.universalTestCaseExpected = "1000000"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "decimal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "3.14"))),
                Testing.universalTestCaseExpected = "3.14"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative decimal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "-2.5"))),
                Testing.universalTestCaseExpected = "-2.5"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "small decimal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "1.0e-3"))),
                Testing.universalTestCaseExpected = "1.0e-3"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "decimal precision",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "large integer exact",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "1.00000000000000000001e20"))),
                Testing.universalTestCaseExpected = "100000000000000000001"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "large negative integer exact",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "-1.00000000000000000001e20"))),
                Testing.universalTestCaseExpected = "-100000000000000000001"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "tiny exponent",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "1.0e-20"))),
                Testing.universalTestCaseExpected = "1.0e-20"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "huge exponent",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "1.0e20"))),
                Testing.universalTestCaseExpected = "1.0e20"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "strings",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueString "")),
                Testing.universalTestCaseExpected = "\"\""})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "simple string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueString "hello")),
                Testing.universalTestCaseExpected = "\"hello\""})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string with spaces",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueString "hello world")),
                Testing.universalTestCaseExpected = "\"hello world\""})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string with double quote",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueString "say \"hi\"")),
                Testing.universalTestCaseExpected = "\"say \\\"hi\\\"\""})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string with backslash",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueString "path\\to\\file")),
                Testing.universalTestCaseExpected = "\"path\\\\to\\\\file\""})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string with newline",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueString "line1\nline2")),
                Testing.universalTestCaseExpected = "\"line1\\nline2\""})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string with carriage return",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueString "line1\rline2")),
                Testing.universalTestCaseExpected = "\"line1\\rline2\""})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string with tab",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueString "col1\tcol2")),
                Testing.universalTestCaseExpected = "\"col1\\tcol2\""})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string with mixed escapes",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueString "a\"b\\c\nd")),
                Testing.universalTestCaseExpected = "\"a\\\"b\\\\c\\nd\""})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "arrays",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty array",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueArray [])),
                Testing.universalTestCaseExpected = "[]"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueArray [
                  Model.ValueNumber (Literals.stringToDecimal "1.0")])),
                Testing.universalTestCaseExpected = "[1]"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple numbers",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueArray [
                  Model.ValueNumber (Literals.stringToDecimal "1.0"),
                  (Model.ValueNumber (Literals.stringToDecimal "2.0")),
                  (Model.ValueNumber (Literals.stringToDecimal "3.0"))])),
                Testing.universalTestCaseExpected = "[1, 2, 3]"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple strings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueArray [
                  Model.ValueString "a",
                  (Model.ValueString "b")])),
                Testing.universalTestCaseExpected = "[\"a\", \"b\"]"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueArray [
                  Model.ValueNumber (Literals.stringToDecimal "1.0"),
                  (Model.ValueString "two"),
                  (Model.ValueBoolean True),
                  Model.ValueNull])),
                Testing.universalTestCaseExpected = "[1, \"two\", true, null]"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "objects",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty object",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueObject M.empty)),
                Testing.universalTestCaseExpected = "{}"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single key-value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueObject (M.fromList [
                  ("name", (Model.ValueString "Alice"))]))),
                Testing.universalTestCaseExpected = "{\"name\": \"Alice\"}"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple keys",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueObject (M.fromList [
                  ("a", (Model.ValueNumber (Literals.stringToDecimal "1.0"))),
                  ("b", (Model.ValueNumber (Literals.stringToDecimal "2.0")))]))),
                Testing.universalTestCaseExpected = "{\"a\": 1, \"b\": 2}"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed value types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueObject (M.fromList [
                  ("active", (Model.ValueBoolean True)),
                  ("count", (Model.ValueNumber (Literals.stringToDecimal "42.0"))),
                  ("name", (Model.ValueString "test"))]))),
                Testing.universalTestCaseExpected = "{\"active\": true, \"count\": 42, \"name\": \"test\"}"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "nested structures",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested arrays",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueArray [
                  Model.ValueArray [
                    Model.ValueNumber (Literals.stringToDecimal "1.0"),
                    (Model.ValueNumber (Literals.stringToDecimal "2.0"))],
                  (Model.ValueArray [
                    Model.ValueNumber (Literals.stringToDecimal "3.0"),
                    (Model.ValueNumber (Literals.stringToDecimal "4.0"))])])),
                Testing.universalTestCaseExpected = "[[1, 2], [3, 4]]"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "object with array",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueObject (M.fromList [
                  ("items", (Model.ValueArray [
                    Model.ValueNumber (Literals.stringToDecimal "1.0"),
                    (Model.ValueNumber (Literals.stringToDecimal "2.0"))]))]))),
                Testing.universalTestCaseExpected = "{\"items\": [1, 2]}"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "array of objects",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueArray [
                  Model.ValueObject (M.fromList [
                    ("id", (Model.ValueNumber (Literals.stringToDecimal "1.0")))]),
                  (Model.ValueObject (M.fromList [
                    ("id", (Model.ValueNumber (Literals.stringToDecimal "2.0")))]))])),
                Testing.universalTestCaseExpected = "[{\"id\": 1}, {\"id\": 2}]"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested object",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Writer.printJson (Model.ValueObject (M.fromList [
                  ("user", (Model.ValueObject (M.fromList [
                    ("name", (Model.ValueString "Bob"))])))]))),
                Testing.universalTestCaseExpected = "{\"user\": {\"name\": \"Bob\"}}"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
