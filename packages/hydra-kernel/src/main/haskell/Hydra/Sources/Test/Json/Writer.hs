{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Test.Json.Writer where

-- Standard imports for tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                   as Terms hiding ((@@))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Literals      as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms                ((@@))
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Data.Scientific              as Sci

-- Additional imports specific to this module
import Hydra.Core.Json.Model (JsonWriterOptions, Value)
import Hydra.Core.Testing
import qualified Hydra.Core.Dsl.Json.Model as Json
import qualified Hydra.Sources.Json.Writer as JsonWriter


ns :: ModuleName
ns = ModuleName "hydra.core.test.json.writer"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([ModuleName "hydra.core.json.writer"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for JSON serialization"))}
  where
    definitions = [
        Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for JSON serialization (writer)" $
    supergroup "JSON serialization" [
      primitivesGroup,
      decimalPrecisionGroup,
      stringsGroup,
      arraysGroup,
      objectsGroup,
      nestedGroup,
      layoutOptionsGroup]

-- Local alias for polymorphic application

arraysGroup :: TypedTerm TestGroup
arraysGroup = subgroup "arrays" [
    -- Empty and single element
    writerCase "empty array" (Json.valueArray $ Phantoms.list ([] :: [TypedTerm Value])) "[]",
    writerCase "single element" (Json.valueArray $ Phantoms.list [Json.valueNumber $ Phantoms.decimal 1.0]) "[1]",

    -- Multiple elements
    writerCase "multiple numbers" (Json.valueArray $ Phantoms.list [
        Json.valueNumber $ Phantoms.decimal 1.0,
        Json.valueNumber $ Phantoms.decimal 2.0,
        Json.valueNumber $ Phantoms.decimal 3.0]) "[1, 2, 3]",

    writerCase "multiple strings" (Json.valueArray $ Phantoms.list [
        Json.valueString $ Phantoms.string "a",
        Json.valueString $ Phantoms.string "b"]) "[\"a\", \"b\"]",

    -- Mixed types
    writerCase "mixed types" (Json.valueArray $ Phantoms.list [
        Json.valueNumber $ Phantoms.decimal 1.0,
        Json.valueString $ Phantoms.string "two",
        Json.valueBoolean $ Phantoms.boolean True,
        Json.valueNull]) "[1, \"two\", true, null]"]

-- | Precision tests: values at the edges of what Scientific's default Show handles.
-- Full cross-host arbitrary-precision round trip is covered by decimalRoundtripGroup in
-- Test.Json.Roundtrip; this group exercises the writer's lexical choices.
-- Note: we do not include "more digits than Double" cases (e.g. 10^20 + 1) here because
-- several Hydra hosts emit decimal literals as host-native Double, which loses precision
-- before the writer even sees the value. Those cases are exercised only via the decimal
-- type coder, which goes through BigDecimal/Scientific without the lossy emission step.
decimalPrecisionGroup :: TypedTerm TestGroup
decimalPrecisionGroup = subgroup "decimal precision" [
    -- Tiny exponents (adjusted exponent a < -6) stay in scientific notation; huge
    -- exponents (a < 21) still print positionally (ECMAScript Number::toString / RFC 8785).
    writerCaseWithTags "tiny exponent" [tag_scaleDistinct]
      (Json.valueNumber $ Phantoms.decimal (Sci.scientific 1 (-20)))
      "1.0e-20",
    writerCaseWithTags "huge exponent" [tag_scaleDistinct]
      (Json.valueNumber $ Phantoms.decimal (Sci.scientific 1 20))
      "100000000000000000000"]

nestedGroup :: TypedTerm TestGroup
nestedGroup = subgroup "nested structures" [
    -- Array of arrays
    writerCase "nested arrays" (Json.valueArray $ Phantoms.list [
        Json.valueArray $ Phantoms.list [Json.valueNumber $ Phantoms.decimal 1.0, Json.valueNumber $ Phantoms.decimal 2.0],
        Json.valueArray $ Phantoms.list [Json.valueNumber $ Phantoms.decimal 3.0, Json.valueNumber $ Phantoms.decimal 4.0]]) "[[1, 2], [3, 4]]",

    -- Object with array
    writerCase "object with array" (Json.valueObject $ Phantoms.list [
        Phantoms.pair (Phantoms.string "items") (Json.valueArray $ Phantoms.list [
            Json.valueNumber $ Phantoms.decimal 1.0,
            Json.valueNumber $ Phantoms.decimal 2.0])]) "{\"items\": [1, 2]}",

    -- Array of objects
    writerCase "array of objects" (Json.valueArray $ Phantoms.list [
        Json.valueObject $ Phantoms.list [Phantoms.pair (Phantoms.string "id") (Json.valueNumber $ Phantoms.decimal 1.0)],
        Json.valueObject $ Phantoms.list [Phantoms.pair (Phantoms.string "id") (Json.valueNumber $ Phantoms.decimal 2.0)]]) "[{\"id\": 1}, {\"id\": 2}]",

    -- Nested object
    writerCase "nested object" (Json.valueObject $ Phantoms.list [
        Phantoms.pair (Phantoms.string "user") (Json.valueObject $ Phantoms.list [
            Phantoms.pair (Phantoms.string "name") (Json.valueString $ Phantoms.string "Bob")])]) "{\"user\": {\"name\": \"Bob\"}}"]

objectsGroup :: TypedTerm TestGroup
objectsGroup = subgroup "objects" [
    -- Empty and single key
    writerCase "empty object" (Json.valueObject $ Phantoms.list ([] :: [TypedTerm (String, Value)])) "{}",
    writerCase "single key-value" (Json.valueObject $ Phantoms.list [
        Phantoms.pair (Phantoms.string "name") (Json.valueString $ Phantoms.string "Alice")]) "{\"name\": \"Alice\"}",

    -- Multiple keys
    writerCase "multiple keys" (Json.valueObject $ Phantoms.list [
        Phantoms.pair (Phantoms.string "a") (Json.valueNumber $ Phantoms.decimal 1.0),
        Phantoms.pair (Phantoms.string "b") (Json.valueNumber $ Phantoms.decimal 2.0)]) "{\"a\": 1, \"b\": 2}",

    -- Mixed value types (the serializer now preserves the field order, rather than alphabetizing)
    writerCase "mixed value types" (Json.valueObject $ Phantoms.list [
        Phantoms.pair (Phantoms.string "count") (Json.valueNumber $ Phantoms.decimal 42.0),
        Phantoms.pair (Phantoms.string "name") (Json.valueString $ Phantoms.string "test"),
        Phantoms.pair (Phantoms.string "active") (Json.valueBoolean $ Phantoms.boolean True)]) "{\"count\": 42, \"name\": \"test\", \"active\": true}"]

primitivesGroup :: TypedTerm TestGroup
primitivesGroup = subgroup "primitives" [
    -- Null
    writerCase "null" Json.valueNull "null",

    -- Booleans
    writerCase "true" (Json.valueBoolean $ Phantoms.boolean True) "true",
    writerCase "false" (Json.valueBoolean $ Phantoms.boolean False) "false",

    -- Numbers - whole-valued decimals print in plain form when that's shorter than
    -- scientific notation; fractional values use Scientific's canonical form.
    writerCase "zero" (Json.valueNumber $ Phantoms.decimal 0.0) "0",
    writerCase "positive integer" (Json.valueNumber $ Phantoms.decimal 42.0) "42",
    writerCase "negative integer" (Json.valueNumber $ Phantoms.decimal (-17.0)) "-17",
    writerCase "large integer" (Json.valueNumber $ Phantoms.decimal 1000000.0) "1000000",

    -- Numbers - fractions. printDecimal is representation-faithful and uses positional
    -- form for adjusted exponent -6 <= a < 21 (ECMAScript Number::toString / RFC 8785),
    -- so 0.01 and 0.001 print plainly, not in scientific form.
    writerCase "decimal" (Json.valueNumber $ Phantoms.decimal 3.14) "3.14",
    writerCase "negative decimal" (Json.valueNumber $ Phantoms.decimal (-2.5)) "-2.5",
    writerCase "hundredth" (Json.valueNumber $ Phantoms.decimal 0.01) "0.01",
    writerCase "small decimal" (Json.valueNumber $ Phantoms.decimal 0.001) "0.001"]

stringsGroup :: TypedTerm TestGroup
stringsGroup = subgroup "strings" [
    -- Basic strings
    writerCase "empty string" (Json.valueString $ Phantoms.string "") "\"\"",
    writerCase "simple string" (Json.valueString $ Phantoms.string "hello") "\"hello\"",
    writerCase "string with spaces" (Json.valueString $ Phantoms.string "hello world") "\"hello world\"",

    -- Escape sequences
    writerCase "string with double quote" (Json.valueString $ Phantoms.string "say \"hi\"") "\"say \\\"hi\\\"\"",
    writerCase "string with backslash" (Json.valueString $ Phantoms.string "path\\to\\file") "\"path\\\\to\\\\file\"",
    writerCase "string with newline" (Json.valueString $ Phantoms.string "line1\nline2") "\"line1\\nline2\"",
    writerCase "string with carriage return" (Json.valueString $ Phantoms.string "line1\rline2") "\"line1\\rline2\"",
    writerCase "string with tab" (Json.valueString $ Phantoms.string "col1\tcol2") "\"col1\\tcol2\"",
    writerCase "string with mixed escapes" (Json.valueString $ Phantoms.string "a\"b\\c\nd") "\"a\\\"b\\\\c\\nd\""]

-- Helper for creating JSON writer test cases (universal)
writerCase :: String -> TypedTerm Value -> String -> TypedTerm TestCaseWithMetadata
writerCase name jsonValue expectedStr = writerCaseWithTags name [] jsonValue expectedStr

writerCaseWithTags :: String -> [Tag] -> TypedTerm Value -> String -> TypedTerm TestCaseWithMetadata
writerCaseWithTags name tags jsonValue expectedStr = universalCaseWithTags name tags
  (JsonWriter.printJson @@ jsonValue)
  (Phantoms.string expectedStr)

-- | A JSON writer test case which exercises printJsonWith against explicit layout options.
writerCaseWith :: String -> TypedTerm JsonWriterOptions -> TypedTerm Value -> String -> TypedTerm TestCaseWithMetadata
writerCaseWith name options jsonValue expectedStr = universalCaseWithTags name []
  (JsonWriter.printJsonWith @@ options @@ jsonValue)
  (Phantoms.string expectedStr)

compactOptions :: TypedTerm JsonWriterOptions
compactOptions = Json.jsonWriterOptions (Phantoms.boolean True) (Phantoms.string "  ") (Literals.int32 120) Phantoms.nothing

-- | A narrow width (forcing a block layout) paired with a four-space indent, so the custom
-- indent string is actually observable in the output.
fourSpaceOptions :: TypedTerm JsonWriterOptions
fourSpaceOptions = Json.jsonWriterOptions (Phantoms.boolean False) (Phantoms.string "    ") (Literals.int32 5) Phantoms.nothing

narrowWidthOptions :: TypedTerm JsonWriterOptions
narrowWidthOptions = Json.jsonWriterOptions (Phantoms.boolean False) (Phantoms.string "  ") (Literals.int32 5) Phantoms.nothing

-- | A narrow width (which would otherwise force a block layout) paired with
-- maxIndentDepth = 0, so the top-level value is forced inline regardless.
maxDepthZeroOptions :: TypedTerm JsonWriterOptions
maxDepthZeroOptions = Json.jsonWriterOptions (Phantoms.boolean False) (Phantoms.string "  ") (Literals.int32 5) (Phantoms.just $ Literals.int32 0)

maxDepthOneOptions :: TypedTerm JsonWriterOptions
maxDepthOneOptions = Json.jsonWriterOptions (Phantoms.boolean False) (Phantoms.string "  ") (Literals.int32 10) (Phantoms.just $ Literals.int32 1)

layoutOptionsGroup :: TypedTerm TestGroup
layoutOptionsGroup = subgroup "layout options" [
    -- printJsonWith with the default-reproducing options matches printJson exactly,
    -- across a variety of shapes (the key byte-identity regression check for #776).
    writerCaseWith "default options match printJson: nested object with array"
      (asTerm JsonWriter.defaultJsonWriterOptions)
      (Json.valueObject $ Phantoms.list [
        Phantoms.pair (Phantoms.string "items") (Json.valueArray $ Phantoms.list [
            Json.valueNumber $ Phantoms.decimal 1.0,
            Json.valueNumber $ Phantoms.decimal 2.0])])
      "{\"items\": [1, 2]}",

    -- Compact mode: no space after ':' or ',', no newlines, regardless of nesting.
    writerCaseWith "compact: empty array" compactOptions
      (Json.valueArray $ Phantoms.list ([] :: [TypedTerm Value])) "[]",
    writerCaseWith "compact: empty object" compactOptions
      (Json.valueObject $ Phantoms.list ([] :: [TypedTerm (String, Value)])) "{}",
    writerCaseWith "compact: array" compactOptions
      (Json.valueArray $ Phantoms.list [
        Json.valueNumber $ Phantoms.decimal 1.0,
        Json.valueNumber $ Phantoms.decimal 2.0,
        Json.valueNumber $ Phantoms.decimal 3.0]) "[1,2,3]",
    writerCaseWith "compact: object" compactOptions
      (Json.valueObject $ Phantoms.list [
        Phantoms.pair (Phantoms.string "a") (Json.valueNumber $ Phantoms.decimal 1.0),
        Phantoms.pair (Phantoms.string "b") (Json.valueNumber $ Phantoms.decimal 2.0)])
      "{\"a\":1,\"b\":2}",
    writerCaseWith "compact: nested" compactOptions
      (Json.valueObject $ Phantoms.list [
        Phantoms.pair (Phantoms.string "items") (Json.valueArray $ Phantoms.list [
            Json.valueNumber $ Phantoms.decimal 1.0,
            Json.valueNumber $ Phantoms.decimal 2.0])])
      "{\"items\":[1,2]}",

    -- Custom (narrow) line width forces a block layout that would stay inline by default.
    writerCaseWith "narrow line width forces block layout" narrowWidthOptions
      (Json.valueArray $ Phantoms.list [
        Json.valueNumber $ Phantoms.decimal 1.0,
        Json.valueNumber $ Phantoms.decimal 2.0,
        Json.valueNumber $ Phantoms.decimal 3.0])
      "[\n  1,\n  2,\n  3]",
    writerCaseWith "custom indent string, forced block" fourSpaceOptions
      (Json.valueObject $ Phantoms.list [
        Phantoms.pair (Phantoms.string "items") (Json.valueArray $ Phantoms.list [
            Json.valueNumber $ Phantoms.decimal 1.0,
            Json.valueNumber $ Phantoms.decimal 2.0])])
      "{\n    \"items\": [\n        1,\n        2]}",

    -- maxIndentDepth: depth 0 (the top-level value) forces the entire document inline,
    -- even though it would otherwise break under the narrow width alone (as the previous
    -- two cases demonstrate).
    writerCaseWith "maxIndentDepth 0 forces top-level inline" maxDepthZeroOptions
      (Json.valueObject $ Phantoms.list [
        Phantoms.pair (Phantoms.string "items") (Json.valueArray $ Phantoms.list [
            Json.valueNumber $ Phantoms.decimal 1.0,
            Json.valueNumber $ Phantoms.decimal 2.0])])
      "{\"items\": [1, 2]}",

    -- maxIndentDepth 1 with a narrow width: the top-level object still adapts (and here
    -- breaks, since it doesn't fit in 10 columns), but its child array (depth 1) is forced
    -- inline regardless of width.
    writerCaseWith "maxIndentDepth 1 forces children inline under a narrow width" maxDepthOneOptions
      (Json.valueObject $ Phantoms.list [
        Phantoms.pair (Phantoms.string "items") (Json.valueArray $ Phantoms.list [
            Json.valueNumber $ Phantoms.decimal 1.0,
            Json.valueNumber $ Phantoms.decimal 2.0])])
      "{\n  \"items\": [1, 2]}"]
