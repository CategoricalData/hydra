-- Note: this is an automatically generated file. Do not edit.

-- | Round-trip test cases for JSON encoding and decoding

module Hydra.Test.Json.Roundtrip where

import qualified Hydra.Core as Core
import qualified Hydra.Json.Decode as Decode
import qualified Hydra.Json.Encode as Encode
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
import qualified Hydra.Lib.Literals as Literals

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
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeBoolean) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeBoolean) (Core.TermLiteral (Core.LiteralBoolean True)))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "boolean false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeBoolean) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeBoolean) (Core.TermLiteral (Core.LiteralBoolean False)))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int8 positive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 42))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int8 negative",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-17)))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-17)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int16",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 1000))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 1000))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100000))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100000))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "uint8",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint8)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint8)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 200))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 200))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "uint16",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 50000))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 50000))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int64",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 1000000000000))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 1000000000000))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "uint32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint32)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint32)) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 4000000000))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 4000000000))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.5))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.5))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float64",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14159))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14159))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float32 NaN",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (0/0)))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (0/0)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float32 positive infinity",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (1/0)))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (1/0)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float32 negative infinity",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (-(1/0))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (-(1/0))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float64 NaN",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (0/0)))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (0/0)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float64 positive infinity",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (1/0)))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (1/0)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float64 negative infinity",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-(1/0))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-(1/0))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string simple",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) (Core.TermLiteral (Core.LiteralString "hello")))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralString "hello")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) (Core.TermLiteral (Core.LiteralString "")))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralString "")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string with spaces",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeString) (Core.TermLiteral (Core.LiteralString "hello world")))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralString "hello world")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "decimal precision",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "decimal zero",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "0.0"))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "0.0"))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "decimal whole",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "42.0"))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "42.0"))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "decimal negative whole",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "-17.0"))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "-17.0"))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "decimal fraction",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "3.14"))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "3.14"))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "decimal negative fraction",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "-2.5"))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "-2.5"))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "decimal tiny exponent",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "1.0e-20"))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "1.0e-20"))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "decimal huge exponent",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeLiteral Core.LiteralTypeDecimal) (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "1.0e20"))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralDecimal (Literals.stringToDecimal "1.0e20"))))})),
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
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list of strings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)) (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b"))]))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Core.TermList []))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeList (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))) (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])]))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
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
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeString)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeString)) (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b"))])))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b"))])))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Core.TermSet S.empty))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermSet S.empty))})),
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
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)) (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "optional string nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)) (Core.TermMaybe Nothing))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "optional int with value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested optional: nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) (Core.TermMaybe Nothing))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested optional: just nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) (Core.TermMaybe (Just (Core.TermMaybe Nothing))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermMaybe Nothing))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested optional: just just value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))) (Core.TermMaybe (Just (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "record types",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "record with required fields",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeRecord [
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "name"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "age"),
                    Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}]) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeRecord [
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "name"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "age"),
                    Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}]) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "test"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "test"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "record with optional field present",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeRecord [
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "name"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "email"),
                    Core.fieldTypeType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))}]) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeRecord [
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "name"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "email"),
                    Core.fieldTypeType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))}]) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "test"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "email"),
                      Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "alice@example.com"))))}]})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "test"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "email"),
                      Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "alice@example.com"))))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "record with optional field absent",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeRecord [
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "name"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "email"),
                    Core.fieldTypeType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))}]) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeRecord [
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "name"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "email"),
                    Core.fieldTypeType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))}]) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "test"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "email"),
                      Core.fieldTerm = (Core.TermMaybe Nothing)}]})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "test"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "email"),
                      Core.fieldTerm = (Core.TermMaybe Nothing)}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "record with mixed optional fields",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeRecord [
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "name"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "email"),
                    Core.fieldTypeType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "age"),
                    Core.fieldTypeType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}]) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeRecord [
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "name"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "email"),
                    Core.fieldTypeType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "age"),
                    Core.fieldTypeType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}]) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "test"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "email"),
                      Core.fieldTerm = (Core.TermMaybe Nothing)},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))))}]})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "test"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "email"),
                      Core.fieldTerm = (Core.TermMaybe Nothing)},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "record with nested optional field",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\json -> Eithers.either (\e -> e) (\decoded -> ShowCore.term decoded) (Decode.fromJson Maps.empty (Core.Name "test") (Core.TypeRecord [
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "name"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "value"),
                    Core.fieldTypeType = (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))}]) json)) (Encode.toJson Maps.empty (Core.Name "test") (Core.TypeRecord [
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "name"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "value"),
                    Core.fieldTypeType = (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))}]) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "test"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "test"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "value"),
                      Core.fieldTerm = (Core.TermMaybe (Just (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))))}]})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "test"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "test"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "value"),
                      Core.fieldTerm = (Core.TermMaybe (Just (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
