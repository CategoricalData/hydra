-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for the type-directed JSON coder (Hydra Term <-> JSON Value)

module Hydra.Test.Json.Coder where

import qualified Hydra.Core as Core
import qualified Hydra.Json.Model as Model
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
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
            Testing.jsonCoderTestCaseJson = (Model.ValueBoolean True)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "boolean false",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralBoolean False)),
            Testing.jsonCoderTestCaseJson = (Model.ValueBoolean False)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int32 positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 42.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int32 negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-17)))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber (-17.0))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int32 zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 0.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "float32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.5))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 1.5)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "float64",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.718))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 2.718)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string simple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralString "hello")),
            Testing.jsonCoderTestCaseJson = (Model.ValueString "hello")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string empty",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralString "")),
            Testing.jsonCoderTestCaseJson = (Model.ValueString "")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralString "hello world")),
            Testing.jsonCoderTestCaseJson = (Model.ValueString "hello world")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "integer types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int8 positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 127))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 127.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int8 negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-128)))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber (-128.0))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int16 positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 1000))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 1000.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int16 negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 (-1000)))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber (-1000.0))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint8 max",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint8)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 255))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 255.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint8 zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint8)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 0))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 0.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint16 positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 60000))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 60000.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint32 positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint32)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 4000000000))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 4.0e9)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint64 positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint64)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 1000000))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 1000000.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bigint positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 123456789012345))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 1.23456789012345e14)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bigint negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-999999999999)))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber (-9.99999999999e11))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "float types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bigfloat positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeBigfloat)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 3.14159265359))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 3.14159265359)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bigfloat negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeBigfloat)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat (-2.71828)))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber (-2.71828))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bigfloat zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeBigfloat)),
            Testing.jsonCoderTestCaseTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 0.0))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 0.0)})),
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
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]),
            Testing.jsonCoderTestCaseJson = (Model.ValueArray [
              Model.ValueNumber 1.0,
              (Model.ValueNumber 2.0),
              (Model.ValueNumber 3.0)])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list of strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.jsonCoderTestCaseTerm = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              (Core.TermLiteral (Core.LiteralString "b"))]),
            Testing.jsonCoderTestCaseJson = (Model.ValueArray [
              Model.ValueString "a",
              (Model.ValueString "b")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.jsonCoderTestCaseTerm = (Core.TermList []),
            Testing.jsonCoderTestCaseJson = (Model.ValueArray [])})),
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
            Testing.jsonCoderTestCaseJson = (Model.ValueString "hello")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optional string nothing",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.jsonCoderTestCaseTerm = (Core.TermMaybe Nothing),
            Testing.jsonCoderTestCaseJson = Model.ValueNull})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optional int with value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.jsonCoderTestCaseTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
            Testing.jsonCoderTestCaseJson = (Model.ValueNumber 42.0)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optional int nothing",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonCoder (Testing.JsonCoderTestCase {
            Testing.jsonCoderTestCaseType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.jsonCoderTestCaseTerm = (Core.TermMaybe Nothing),
            Testing.jsonCoderTestCaseJson = Model.ValueNull})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "record types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = []}],
  Testing.testGroupCases = []}
