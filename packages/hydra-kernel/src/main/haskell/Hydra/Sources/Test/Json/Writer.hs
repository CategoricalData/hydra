{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Test.Json.Writer where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Data.Scientific              as Sci

-- Additional imports specific to this module
import Hydra.Json.Model (Value)
import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Json.Model as Json
import qualified Hydra.Sources.Json.Writer as WriterModule


ns :: Namespace
ns = Namespace "hydra.test.json.writer"

module_ :: Module
module_ = Module ns definitions
    [Namespace "hydra.json.writer"]
    kernelTypesNamespaces
    (Just "Test cases for JSON serialization")
  where
    definitions = [
        Phantoms.toDefinition allTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for JSON serialization (writer)" $
    supergroup "JSON serialization" [
      primitivesGroup,
      decimalPrecisionGroup,
      stringsGroup,
      arraysGroup,
      objectsGroup,
      nestedGroup]

-- Local alias for polymorphic application
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

-- Helper for creating JSON writer test cases (universal)
writerCase :: String -> TTerm Value -> String -> TTerm TestCaseWithMetadata
writerCase name jsonValue expectedStr = universalCase name
  (WriterModule.printJson # jsonValue)
  (Phantoms.string expectedStr)

primitivesGroup :: TTerm TestGroup
primitivesGroup = subgroup "primitives" [
    -- Null
    writerCase "null" Json.valueNull "null",

    -- Booleans
    writerCase "true" (Json.valueBoolean $ Phantoms.boolean True) "true",
    writerCase "false" (Json.valueBoolean $ Phantoms.boolean False) "false",

    -- Numbers - integers (whole numbers are formatted without decimal point)
    writerCase "zero" (Json.valueNumber $ Phantoms.decimal 0.0) "0",
    writerCase "positive integer" (Json.valueNumber $ Phantoms.decimal 42.0) "42",
    writerCase "negative integer" (Json.valueNumber $ Phantoms.decimal (-17.0)) "-17",
    writerCase "large integer" (Json.valueNumber $ Phantoms.decimal 1000000.0) "1000000",

    -- Numbers - decimals
    writerCase "decimal" (Json.valueNumber $ Phantoms.decimal 3.14) "3.14",
    writerCase "negative decimal" (Json.valueNumber $ Phantoms.decimal (-2.5)) "-2.5",
    writerCase "small decimal" (Json.valueNumber $ Phantoms.decimal 0.001) "1.0e-3"]

-- | Precision tests: values that bigfloat (Double) could not represent faithfully.
-- These assert that the writer preserves full decimal precision end-to-end;
-- the dedicated decimalRoundtripGroup in Test.Json.Roundtrip covers parse-then-write.
decimalPrecisionGroup :: TTerm TestGroup
decimalPrecisionGroup = subgroup "decimal precision" [
    -- Very large integer that a Double cannot represent exactly.
    -- Whole-number shortcut in the writer routes this to showBigint.
    writerCase "large integer exact"
      (Json.valueNumber $ Phantoms.decimal (Sci.scientific 100000000000000000001 0))
      "100000000000000000001",
    -- Negative large integer, same path.
    writerCase "large negative integer exact"
      (Json.valueNumber $ Phantoms.decimal (Sci.scientific (-100000000000000000001) 0))
      "-100000000000000000001",
    -- Tiny exponent value — Scientific's Show emits scientific notation (not a whole number).
    writerCase "tiny exponent"
      (Json.valueNumber $ Phantoms.decimal (Sci.scientific 1 (-20)))
      "1.0e-20",
    -- Large exponent value: the writer's whole-number shortcut detects this as an integer
    -- and prints it in plain form rather than scientific notation.
    writerCase "huge exponent"
      (Json.valueNumber $ Phantoms.decimal (Sci.scientific 1 20))
      "100000000000000000000"]

stringsGroup :: TTerm TestGroup
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

arraysGroup :: TTerm TestGroup
arraysGroup = subgroup "arrays" [
    -- Empty and single element
    writerCase "empty array" (Json.valueArray $ Phantoms.list ([] :: [TTerm Value])) "[]",
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

objectsGroup :: TTerm TestGroup
objectsGroup = subgroup "objects" [
    -- Empty and single key
    writerCase "empty object" (Json.valueObject $ Phantoms.map M.empty) "{}",
    writerCase "single key-value" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "name", Json.valueString $ Phantoms.string "Alice")]) "{\"name\": \"Alice\"}",

    -- Multiple keys
    writerCase "multiple keys" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "a", Json.valueNumber $ Phantoms.decimal 1.0),
        (Phantoms.string "b", Json.valueNumber $ Phantoms.decimal 2.0)]) "{\"a\": 1, \"b\": 2}",

    -- Mixed value types
    writerCase "mixed value types" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "count", Json.valueNumber $ Phantoms.decimal 42.0),
        (Phantoms.string "name", Json.valueString $ Phantoms.string "test"),
        (Phantoms.string "active", Json.valueBoolean $ Phantoms.boolean True)]) "{\"active\": true, \"count\": 42, \"name\": \"test\"}"]

nestedGroup :: TTerm TestGroup
nestedGroup = subgroup "nested structures" [
    -- Array of arrays
    writerCase "nested arrays" (Json.valueArray $ Phantoms.list [
        Json.valueArray $ Phantoms.list [Json.valueNumber $ Phantoms.decimal 1.0, Json.valueNumber $ Phantoms.decimal 2.0],
        Json.valueArray $ Phantoms.list [Json.valueNumber $ Phantoms.decimal 3.0, Json.valueNumber $ Phantoms.decimal 4.0]]) "[[1, 2], [3, 4]]",

    -- Object with array
    writerCase "object with array" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "items", Json.valueArray $ Phantoms.list [
            Json.valueNumber $ Phantoms.decimal 1.0,
            Json.valueNumber $ Phantoms.decimal 2.0])]) "{\"items\": [1, 2]}",

    -- Array of objects
    writerCase "array of objects" (Json.valueArray $ Phantoms.list [
        Json.valueObject $ Phantoms.map $ M.singleton (Phantoms.string "id") (Json.valueNumber $ Phantoms.decimal 1.0),
        Json.valueObject $ Phantoms.map $ M.singleton (Phantoms.string "id") (Json.valueNumber $ Phantoms.decimal 2.0)]) "[{\"id\": 1}, {\"id\": 2}]",

    -- Nested object
    writerCase "nested object" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "user", Json.valueObject $ Phantoms.map $ M.fromList [
            (Phantoms.string "name", Json.valueString $ Phantoms.string "Bob")])]) "{\"user\": {\"name\": \"Bob\"}}"]
