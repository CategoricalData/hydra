-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for JSON parsing

module Hydra.Test.Json.Parser where

import qualified Hydra.Json as Json
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for JSON parsing
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "JSON parsing",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "null",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "null",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = Json.ValueNull,
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "true",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueBoolean True),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "false",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "false",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueBoolean False),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "0",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueNumber 0.0),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive integer",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "42",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueNumber 42.0),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative integer",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "-17",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueNumber (-17.0)),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "large integer",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "1000000",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueNumber 1000000.0),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "decimal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "3.14",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueNumber 3.14),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative decimal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "-2.5",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueNumber (-2.5)),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "scientific notation",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "1e3",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueNumber 1000.0),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "scientific with decimal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "1.5e2",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueNumber 150.0),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative exponent",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "1e-2",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueNumber 1.0e-2),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "strings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "\"\"",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueString ""),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "simple string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "\"hello\"",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueString "hello"),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "\"hello world\"",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueString "hello world"),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "escaped double quote",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "\"say \\\"hi\\\"\"",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueString "say \"hi\""),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "escaped backslash",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "\"path\\\\to\\\\file\"",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueString "path\\to\\file"),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "escaped newline",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "\"line1\\nline2\"",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueString "line1\nline2"),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "escaped carriage return",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "\"line1\\rline2\"",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueString "line1\rline2"),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "escaped tab",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "\"col1\\tcol2\"",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueString "col1\tcol2"),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "escaped forward slash",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "\"a\\/b\"",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueString "a/b"),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "arrays",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty array",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "[]",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueArray []),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "[1]",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueArray [
                Json.ValueNumber 1.0]),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple numbers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "[1, 2, 3]",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueArray [
                Json.ValueNumber 1.0,
                Json.ValueNumber 2.0,
                (Json.ValueNumber 3.0)]),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "[\"a\", \"b\"]",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueArray [
                Json.ValueString "a",
                (Json.ValueString "b")]),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mixed types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "[1, \"two\", true, null]",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueArray [
                Json.ValueNumber 1.0,
                Json.ValueString "two",
                Json.ValueBoolean True,
                Json.ValueNull]),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "objects",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty object",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "{}",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueObject M.empty),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single key-value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "{\"name\": \"Alice\"}",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueObject (M.fromList [
                ("name", (Json.ValueString "Alice"))])),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple keys",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "{\"a\": 1, \"b\": 2}",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueObject (M.fromList [
                ("a", (Json.ValueNumber 1.0)),
                ("b", (Json.ValueNumber 2.0))])),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mixed value types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "{\"active\": true, \"count\": 42, \"name\": \"test\"}",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueObject (M.fromList [
                ("active", (Json.ValueBoolean True)),
                ("count", (Json.ValueNumber 42.0)),
                ("name", (Json.ValueString "test"))])),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "nested structures",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested arrays",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "[[1, 2], [3, 4]]",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueArray [
                Json.ValueArray [
                  Json.ValueNumber 1.0,
                  (Json.ValueNumber 2.0)],
                (Json.ValueArray [
                  Json.ValueNumber 3.0,
                  (Json.ValueNumber 4.0)])]),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "object with array",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "{\"items\": [1, 2]}",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueObject (M.fromList [
                ("items", (Json.ValueArray [
                  Json.ValueNumber 1.0,
                  (Json.ValueNumber 2.0)]))])),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "array of objects",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "[{\"id\": 1}, {\"id\": 2}]",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueArray [
                Json.ValueObject (M.fromList [
                  ("id", (Json.ValueNumber 1.0))]),
                (Json.ValueObject (M.fromList [
                  ("id", (Json.ValueNumber 2.0))]))]),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested object",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "{\"user\": {\"name\": \"Bob\"}}",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueObject (M.fromList [
                ("user", (Json.ValueObject (M.fromList [
                  ("name", (Json.ValueString "Bob"))])))])),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "whitespace handling",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "leading whitespace",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "  null",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = Json.ValueNull,
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "trailing whitespace",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "null  ",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = Json.ValueNull,
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "both whitespace",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "  null  ",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = Json.ValueNull,
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "array with whitespace",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "[ 1 , 2 , 3 ]",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueArray [
                Json.ValueNumber 1.0,
                Json.ValueNumber 2.0,
                (Json.ValueNumber 3.0)]),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "object with whitespace",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "{ \"a\" : 1 }",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueObject (M.fromList [
                ("a", (Json.ValueNumber 1.0))])),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiline array",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonParser (Testing.ParserTestCase {
            Testing.parserTestCaseInput = "[\n  1,\n  2\n]",
            Testing.parserTestCaseOutput = (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Json.ValueArray [
                Json.ValueNumber 1.0,
                (Json.ValueNumber 2.0)]),
              Parsing.parseSuccessRemainder = ""}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
