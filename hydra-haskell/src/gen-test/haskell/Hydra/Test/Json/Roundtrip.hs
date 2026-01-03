-- Note: this is an automatically generated file. Do not edit.

-- | Round-trip test cases for JSON encoding and decoding

module Hydra.Test.Json.Roundtrip where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Round-trip test cases for JSON encoding and decoding
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "JSON round-trip",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "literal types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "boolean true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "boolean false",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int8 positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int8 negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-17))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int16",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 1000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint8",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint8)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 200)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint16",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 50000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int64",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 1000000000000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint32)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 4000000000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "float32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.5)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "float64",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14159)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string simple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralString "hello"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string empty",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermLiteral (Core.LiteralString "hello world"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "collection types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list of integers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list of strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              (Core.TermLiteral (Core.LiteralString "b"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeList (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
              (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set of strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermSet (S.fromList [
              Core.TermLiteral (Core.LiteralString "a"),
              (Core.TermLiteral (Core.LiteralString "b"))]))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty set",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermSet S.empty)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "optional types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optional string with value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello"))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optional string nothing",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optional int with value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJsonRoundtrip (Testing.JsonRoundtripTestCase {
            Testing.jsonRoundtripTestCaseType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.jsonRoundtripTestCaseTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
