{-# LANGUAGE FlexibleContexts #-}

-- | Round-trip test cases for the JSON<->YAML bridge, focused on decimal precision.
--
-- These tests verify that JSON numbers (decimal-encoded by spec) survive the trip through
-- YAML via the dedicated decimal scalar variant in Hydra YAML.
module Hydra.Sources.Test.Json.Yaml where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Data.Scientific              as Sci

-- Additional imports specific to this module
import Hydra.Testing
import qualified Hydra.Dsl.Json.Model as Json
import qualified Hydra.Sources.Json.Writer as JsonWriter
import qualified Hydra.Sources.Json.Yaml.Encode as JsonYamlEncode
import qualified Hydra.Sources.Json.Yaml.Decode as JsonYamlDecode


ns :: Namespace
ns = Namespace "hydra.test.json.yaml"

module_ :: Module
module_ = Module ns definitions
    [Namespace "hydra.json.writer", Namespace "hydra.json.yaml.encode", Namespace "hydra.json.yaml.decode"]
    kernelTypesNamespaces
    (Just "Round-trip test cases for the JSON<->YAML bridge, focused on decimal precision")
  where
    definitions = [
        Phantoms.toDefinition allTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- Local alias for polymorphic application
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Round-trip test cases for the JSON<->YAML decimal bridge" $
    supergroup "JSON<->YAML bridge" [
      decimalBridgeGroup]

-- | Round-trip a JSON value through YAML and back, asserting the result prints identically.
-- JSON -> YAML -> JSON must preserve the full decimal value for any JSON number.
yamlBridgeCase :: String -> TTerm Json.Value -> TTerm TestCaseWithMetadata
yamlBridgeCase testName jsonValue = universalCase testName
  (Eithers.either_
    (Phantoms.lambda "e" $ Phantoms.var "e")
    (Phantoms.lambda "back" $ JsonWriter.printJson # Phantoms.var "back")
    (JsonYamlDecode.yamlToJson # (JsonYamlEncode.jsonToYaml # jsonValue)))
  (JsonWriter.printJson # jsonValue)

decimalBridgeGroup :: TTerm TestGroup
decimalBridgeGroup = subgroup "decimal round-trip" [
    yamlBridgeCase "zero" (Json.valueNumber $ Phantoms.decimal 0),
    yamlBridgeCase "positive whole" (Json.valueNumber $ Phantoms.decimal 42),
    yamlBridgeCase "negative whole" (Json.valueNumber $ Phantoms.decimal (-17)),
    yamlBridgeCase "fraction" (Json.valueNumber $ Phantoms.decimal 3.14),
    yamlBridgeCase "negative fraction" (Json.valueNumber $ Phantoms.decimal (-2.5)),
    yamlBridgeCase "large integer"
      (Json.valueNumber $ Phantoms.decimal (Sci.scientific 100000000000000000001 0)),
    yamlBridgeCase "tiny exponent"
      (Json.valueNumber $ Phantoms.decimal (Sci.scientific 1 (-20))),
    yamlBridgeCase "huge exponent"
      (Json.valueNumber $ Phantoms.decimal (Sci.scientific 1 20)),
    yamlBridgeCase "many significant digits"
      (Json.valueNumber $ Phantoms.decimal (Sci.scientific 314159265358979323846 (-20))),
    -- Non-number JSON values pass through unchanged
    yamlBridgeCase "null" Json.valueNull,
    yamlBridgeCase "true" (Json.valueBoolean $ Phantoms.boolean True),
    yamlBridgeCase "false" (Json.valueBoolean $ Phantoms.boolean False),
    yamlBridgeCase "simple string" (Json.valueString $ Phantoms.string "hello"),
    -- Array mixing decimal numbers and strings
    yamlBridgeCase "mixed array"
      (Json.valueArray $ Phantoms.list [
        Json.valueNumber $ Phantoms.decimal 1,
        Json.valueNumber $ Phantoms.decimal (Sci.scientific 1 (-20)),
        Json.valueString $ Phantoms.string "note"])]
