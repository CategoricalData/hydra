-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for JSON serialization

module Hydra.Test.Json.Writer where

import qualified Hydra.Json as Json
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for JSON serialization (writer)
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
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
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = Json.ValueNull,
            Testing.writerTestCaseOutput = "null"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueBoolean True),
            Testing.writerTestCaseOutput = "true"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "false",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueBoolean False),
            Testing.writerTestCaseOutput = "false"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueNumber 0.0),
            Testing.writerTestCaseOutput = "0.0"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive integer",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueNumber 42.0),
            Testing.writerTestCaseOutput = "42.0"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative integer",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueNumber (-17.0)),
            Testing.writerTestCaseOutput = "-17.0"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "large integer",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueNumber 1000000.0),
            Testing.writerTestCaseOutput = "1000000.0"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "decimal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueNumber 3.14),
            Testing.writerTestCaseOutput = "3.14"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative decimal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueNumber (-2.5)),
            Testing.writerTestCaseOutput = "-2.5"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "small decimal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueNumber 1.0e-3),
            Testing.writerTestCaseOutput = "1.0e-3"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "strings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueString ""),
            Testing.writerTestCaseOutput = "\"\""})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "simple string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueString "hello"),
            Testing.writerTestCaseOutput = "\"hello\""})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueString "hello world"),
            Testing.writerTestCaseOutput = "\"hello world\""})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with double quote",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueString "say \"hi\""),
            Testing.writerTestCaseOutput = "\"say \\\"hi\\\"\""})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with backslash",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueString "path\\to\\file"),
            Testing.writerTestCaseOutput = "\"path\\\\to\\\\file\""})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with newline",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueString "line1\nline2"),
            Testing.writerTestCaseOutput = "\"line1\\nline2\""})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with carriage return",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueString "line1\rline2"),
            Testing.writerTestCaseOutput = "\"line1\\rline2\""})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with tab",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueString "col1\tcol2"),
            Testing.writerTestCaseOutput = "\"col1\\tcol2\""})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with mixed escapes",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueString "a\"b\\c\nd"),
            Testing.writerTestCaseOutput = "\"a\\\"b\\\\c\\nd\""})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "arrays",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty array",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueArray []),
            Testing.writerTestCaseOutput = "[]"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueArray [
              Json.ValueNumber 1.0]),
            Testing.writerTestCaseOutput = "[1.0]"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple numbers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueArray [
              Json.ValueNumber 1.0,
              Json.ValueNumber 2.0,
              (Json.ValueNumber 3.0)]),
            Testing.writerTestCaseOutput = "[1.0, 2.0, 3.0]"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueArray [
              Json.ValueString "a",
              (Json.ValueString "b")]),
            Testing.writerTestCaseOutput = "[\"a\", \"b\"]"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mixed types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueArray [
              Json.ValueNumber 1.0,
              Json.ValueString "two",
              Json.ValueBoolean True,
              Json.ValueNull]),
            Testing.writerTestCaseOutput = "[1.0, \"two\", true, null]"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "objects",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty object",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueObject M.empty),
            Testing.writerTestCaseOutput = "{}"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single key-value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueObject (M.fromList [
              ("name", (Json.ValueString "Alice"))])),
            Testing.writerTestCaseOutput = "{\"name\": \"Alice\"}"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple keys",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueObject (M.fromList [
              ("a", (Json.ValueNumber 1.0)),
              ("b", (Json.ValueNumber 2.0))])),
            Testing.writerTestCaseOutput = "{\"a\": 1.0, \"b\": 2.0}"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mixed value types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueObject (M.fromList [
              ("active", (Json.ValueBoolean True)),
              ("count", (Json.ValueNumber 42.0)),
              ("name", (Json.ValueString "test"))])),
            Testing.writerTestCaseOutput = "{\"active\": true, \"count\": 42.0, \"name\": \"test\"}"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "nested structures",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested arrays",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueArray [
              Json.ValueArray [
                Json.ValueNumber 1.0,
                (Json.ValueNumber 2.0)],
              (Json.ValueArray [
                Json.ValueNumber 3.0,
                (Json.ValueNumber 4.0)])]),
            Testing.writerTestCaseOutput = "[[1.0, 2.0], [3.0, 4.0]]"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "object with array",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueObject (M.fromList [
              ("items", (Json.ValueArray [
                Json.ValueNumber 1.0,
                (Json.ValueNumber 2.0)]))])),
            Testing.writerTestCaseOutput = "{\"items\": [1.0, 2.0]}"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "array of objects",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueArray [
              Json.ValueObject (M.fromList [
                ("id", (Json.ValueNumber 1.0))]),
              (Json.ValueObject (M.fromList [
                ("id", (Json.ValueNumber 2.0))]))]),
            Testing.writerTestCaseOutput = "[{\"id\": 1.0}, {\"id\": 2.0}]"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested object",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonWriter (Testing.WriterTestCase {
            Testing.writerTestCaseInput = (Json.ValueObject (M.fromList [
              ("user", (Json.ValueObject (M.fromList [
                ("name", (Json.ValueString "Bob"))])))])),
            Testing.writerTestCaseOutput = "{\"user\": {\"name\": \"Bob\"}}"})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
