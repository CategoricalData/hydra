-- Note: this is an automatically generated file. Do not edit.
-- | Round-trip test cases for the JSON<->YAML bridge, focused on decimal precision

module Hydra.Test.Json.Yaml where
import qualified Hydra.Json.Model as Model
import qualified Hydra.Json.Writer as Writer
import qualified Hydra.Json.Yaml.Decode as Decode
import qualified Hydra.Json.Yaml.Encode as Encode
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Hydra.Lib.Literals as Literals
-- | Round-trip test cases for the JSON<->YAML decimal bridge
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "JSON<->YAML bridge",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "decimal round-trip",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "zero",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (Model.ValueNumber (Literals.stringToDecimal "0.0"))))),
                Testing.universalTestCaseExpected = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "0.0")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "positive whole",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (Model.ValueNumber (Literals.stringToDecimal "42.0"))))),
                Testing.universalTestCaseExpected = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "42.0")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative whole",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (Model.ValueNumber (Literals.stringToDecimal "-17.0"))))),
                Testing.universalTestCaseExpected = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "-17.0")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "fraction",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (Model.ValueNumber (Literals.stringToDecimal "3.14"))))),
                Testing.universalTestCaseExpected = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "3.14")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative fraction",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (Model.ValueNumber (Literals.stringToDecimal "-2.5"))))),
                Testing.universalTestCaseExpected = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "-2.5")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "tiny exponent",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (Model.ValueNumber (Literals.stringToDecimal "1.0e-20"))))),
                Testing.universalTestCaseExpected = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "1.0e-20")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "huge exponent",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (Model.ValueNumber (Literals.stringToDecimal "1.0e20"))))),
                Testing.universalTestCaseExpected = (Writer.printJson (Model.ValueNumber (Literals.stringToDecimal "1.0e20")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "null",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml Model.ValueNull))),
                Testing.universalTestCaseExpected = (Writer.printJson Model.ValueNull)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (Model.ValueBoolean True)))),
                Testing.universalTestCaseExpected = (Writer.printJson (Model.ValueBoolean True))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (Model.ValueBoolean False)))),
                Testing.universalTestCaseExpected = (Writer.printJson (Model.ValueBoolean False))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "simple string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (Model.ValueString "hello")))),
                Testing.universalTestCaseExpected = (Writer.printJson (Model.ValueString "hello"))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed array",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (Model.ValueArray [
                  Model.ValueNumber (Literals.stringToDecimal "1.0"),
                  (Model.ValueNumber (Literals.stringToDecimal "1.0e-20")),
                  (Model.ValueString "note")])))),
                Testing.universalTestCaseExpected = (Writer.printJson (Model.ValueArray [
                  Model.ValueNumber (Literals.stringToDecimal "1.0"),
                  (Model.ValueNumber (Literals.stringToDecimal "1.0e-20")),
                  (Model.ValueString "note")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
