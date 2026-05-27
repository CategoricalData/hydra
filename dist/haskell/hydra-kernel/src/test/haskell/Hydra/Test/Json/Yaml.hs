-- Note: this is an automatically generated file. Do not edit.
-- | Round-trip test cases for the JSON<->YAML bridge, focused on decimal precision

module Hydra.Test.Json.Yaml where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Json.Writer as Writer
import qualified Hydra.Json.Yaml.Decode as Decode
import qualified Hydra.Json.Yaml.Encode as Encode
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import qualified Hydra.Yaml.Model as YamlModel
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Hydra.Haskell.Lib.Literals as Literals
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
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (JsonModel.ValueNumber (Literals.stringToDecimal "0.0"))))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson (JsonModel.ValueNumber (Literals.stringToDecimal "0.0")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "positive whole",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (JsonModel.ValueNumber (Literals.stringToDecimal "42.0"))))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson (JsonModel.ValueNumber (Literals.stringToDecimal "42.0")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative whole",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (JsonModel.ValueNumber (Literals.stringToDecimal "-17.0"))))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson (JsonModel.ValueNumber (Literals.stringToDecimal "-17.0")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "fraction",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (JsonModel.ValueNumber (Literals.stringToDecimal "3.14"))))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson (JsonModel.ValueNumber (Literals.stringToDecimal "3.14")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative fraction",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (JsonModel.ValueNumber (Literals.stringToDecimal "-2.5"))))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson (JsonModel.ValueNumber (Literals.stringToDecimal "-2.5")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "tiny exponent",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (JsonModel.ValueNumber (Literals.stringToDecimal "1.0e-20"))))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson (JsonModel.ValueNumber (Literals.stringToDecimal "1.0e-20")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "huge exponent",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (JsonModel.ValueNumber (Literals.stringToDecimal "1.0e20"))))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson (JsonModel.ValueNumber (Literals.stringToDecimal "1.0e20")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "null",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml JsonModel.ValueNull))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson JsonModel.ValueNull)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (JsonModel.ValueBoolean True)))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson (JsonModel.ValueBoolean True))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (JsonModel.ValueBoolean False)))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson (JsonModel.ValueBoolean False))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "simple string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (JsonModel.ValueString "hello")))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson (JsonModel.ValueString "hello"))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed array",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> e) (\back -> Writer.printJson back) (Decode.yamlToJson (Encode.jsonToYaml (JsonModel.ValueArray [
                  JsonModel.ValueNumber (Literals.stringToDecimal "1.0"),
                  (JsonModel.ValueNumber (Literals.stringToDecimal "1.0e-20")),
                  (JsonModel.ValueString "note")])))),
                Testing.universalTestCaseExpected = (\_ -> Writer.printJson (JsonModel.ValueArray [
                  JsonModel.ValueNumber (Literals.stringToDecimal "1.0"),
                  (JsonModel.ValueNumber (Literals.stringToDecimal "1.0e-20")),
                  (JsonModel.ValueString "note")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
