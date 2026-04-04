-- Note: this is an automatically generated file. Do not edit.

-- | Round-trip test cases for JSON encoding and decoding

module Hydra.Test.Json.Roundtrip where

import qualified Hydra.Core as Core
import qualified Hydra.Json.Decode as Decode
import qualified Hydra.Json.Encode as Encode
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Set as S

-- | Round-trip test cases for JSON encoding and decoding
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
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
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeBoolean) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeBoolean) (Core.TermLiteral (Core.LiteralBoolean True)))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "boolean false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeBoolean) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeBoolean) (Core.TermLiteral (Core.LiteralBoolean False)))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int8 positive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 42))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int8 negative",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-17)))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-17)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int16",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 1000))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 1000))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100000))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100000))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "uint8",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint8)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint8)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 200))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 200))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "uint16",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 50000))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 50000))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int64",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 1000000000000))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 1000000000000))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "uint32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint32)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint32)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 4000000000))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 4000000000))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.5))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.5))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float64",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14159))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14159))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string simple",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) (Core.TermLiteral (Core.LiteralString "hello")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "hello")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) (Core.TermLiteral (Core.LiteralString "")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string with spaces",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) (Core.TermLiteral (Core.LiteralString "hello world")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "hello world")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "collection types",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list of integers",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list of strings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)) (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b"))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Core.TermList []))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))) (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "set of strings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeString)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeString)) (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b"))])))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b"))])))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Core.TermSet S.empty))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermSet S.empty))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "optional types",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "optional string with value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)) (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "optional string nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)) (Core.TermMaybe Nothing))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "optional int with value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested optional: nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) (Core.TermMaybe Nothing))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested optional: just nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) (Core.TermMaybe (Just (Core.TermMaybe Nothing))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermMaybe (Just (Core.TermMaybe Nothing))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested optional: just just value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> Core_.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) (Core.TermMaybe (Just (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermMaybe (Just (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
