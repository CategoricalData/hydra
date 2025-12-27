-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for the type-directed JSON coder (Hydra Term <-> JSON Value)

module Hydra.Test.Json.Coder where

import qualified Hydra.Core as Core
import qualified Hydra.Json as Json
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for the type-directed JSON coder
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "JSON coder",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "literal types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "boolean true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralBoolean True)),
            Testing.jsonCoderTestCaseJson = (Json.ValueBoolean True)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "boolean false",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralBoolean False)),
            Testing.jsonCoderTestCaseJson = (Json.ValueBoolean False)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int32 positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Testing.jsonCoderTestCaseJson = (Json.ValueNumber 42.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int32 negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-17)))),
            Testing.jsonCoderTestCaseJson = (Json.ValueNumber (-17.0))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int32 zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
            Testing.jsonCoderTestCaseJson = (Json.ValueNumber 0.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "float32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.5))),
            Testing.jsonCoderTestCaseJson = (Json.ValueNumber 1.5)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "float64",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.718))),
            Testing.jsonCoderTestCaseJson = (Json.ValueNumber 2.718)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string simple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralString "hello")),
            Testing.jsonCoderTestCaseJson = (Json.ValueString "hello")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string empty",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralString "")),
            Testing.jsonCoderTestCaseJson = (Json.ValueString "")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralString "hello world")),
            Testing.jsonCoderTestCaseJson = (Json.ValueString "hello world")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "collection types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list of integers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.jsonCoderTestCaseTerm = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]),
            Testing.jsonCoderTestCaseJson = (Json.ValueArray [
              Json.ValueNumber 1.0,
              Json.ValueNumber 2.0,
              (Json.ValueNumber 3.0)])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list of strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.jsonCoderTestCaseTerm = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              (Core.TermLiteral (Core.LiteralString "b"))]),
            Testing.jsonCoderTestCaseJson = (Json.ValueArray [
              Json.ValueString "a",
              (Json.ValueString "b")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.jsonCoderTestCaseTerm = (Core.TermList []),
            Testing.jsonCoderTestCaseJson = (Json.ValueArray [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "optional types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optional string with value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.jsonCoderTestCaseTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))),
            Testing.jsonCoderTestCaseJson = (Json.ValueString "hello")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optional string nothing",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.jsonCoderTestCaseTerm = (Core.TermMaybe Nothing),
            Testing.jsonCoderTestCaseJson = Json.ValueNull})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optional int with value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.jsonCoderTestCaseTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
            Testing.jsonCoderTestCaseJson = (Json.ValueNumber 42.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optional int nothing",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.jsonCoderTestCaseTerm = (Core.TermMaybe Nothing),
            Testing.jsonCoderTestCaseJson = Json.ValueNull})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "record types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = []}],
  Testing.testGroupCases = []}
