module Hydra.Sources.Test.Json.Writer where

import Hydra.Kernel
import Hydra.Json.Model (Value)
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Meta.Phantoms as Base
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Json as Json
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Types.Testing as TestingTypes

import qualified Data.Map as M


ns :: Namespace
ns = Namespace "hydra.test.json.writer"

module_ :: Module
module_ = Module ns elements
    []
    KernelTypes.kernelTypesNamespaces
    (Just "Test cases for JSON serialization")
  where
    elements = [
        Base.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Base.doc "Test cases for JSON serialization (writer)" $
    supergroup "JSON serialization" [
      primitivesGroup,
      stringsGroup,
      arraysGroup,
      objectsGroup,
      nestedGroup]

-- Helper for creating JSON writer test cases
writerCase :: String -> TTerm Value -> String -> TTerm TestCaseWithMetadata
writerCase name jsonValue expectedStr = testCaseWithMetadata (Base.string name)
  (testCaseJsonWriter $ jsonWriterTestCase jsonValue (Base.string expectedStr))
  Base.nothing (Base.list ([] :: [TTerm Tag]))

primitivesGroup :: TTerm TestGroup
primitivesGroup = subgroup "primitives" [
    -- Null
    writerCase "null" Json.valueNull "null",

    -- Booleans
    writerCase "true" (Json.valueBoolean $ Base.boolean True) "true",
    writerCase "false" (Json.valueBoolean $ Base.boolean False) "false",

    -- Numbers - integers (whole numbers are formatted without decimal point)
    writerCase "zero" (Json.valueNumber $ Base.bigfloat 0.0) "0",
    writerCase "positive integer" (Json.valueNumber $ Base.bigfloat 42.0) "42",
    writerCase "negative integer" (Json.valueNumber $ Base.bigfloat (-17.0)) "-17",
    writerCase "large integer" (Json.valueNumber $ Base.bigfloat 1000000.0) "1000000",

    -- Numbers - decimals
    writerCase "decimal" (Json.valueNumber $ Base.bigfloat 3.14) "3.14",
    writerCase "negative decimal" (Json.valueNumber $ Base.bigfloat (-2.5)) "-2.5",
    writerCase "small decimal" (Json.valueNumber $ Base.bigfloat 0.001) "1.0e-3"]

stringsGroup :: TTerm TestGroup
stringsGroup = subgroup "strings" [
    -- Basic strings
    writerCase "empty string" (Json.valueString $ Base.string "") "\"\"",
    writerCase "simple string" (Json.valueString $ Base.string "hello") "\"hello\"",
    writerCase "string with spaces" (Json.valueString $ Base.string "hello world") "\"hello world\"",

    -- Escape sequences
    writerCase "string with double quote" (Json.valueString $ Base.string "say \"hi\"") "\"say \\\"hi\\\"\"",
    writerCase "string with backslash" (Json.valueString $ Base.string "path\\to\\file") "\"path\\\\to\\\\file\"",
    writerCase "string with newline" (Json.valueString $ Base.string "line1\nline2") "\"line1\\nline2\"",
    writerCase "string with carriage return" (Json.valueString $ Base.string "line1\rline2") "\"line1\\rline2\"",
    writerCase "string with tab" (Json.valueString $ Base.string "col1\tcol2") "\"col1\\tcol2\"",
    writerCase "string with mixed escapes" (Json.valueString $ Base.string "a\"b\\c\nd") "\"a\\\"b\\\\c\\nd\""]

arraysGroup :: TTerm TestGroup
arraysGroup = subgroup "arrays" [
    -- Empty and single element
    writerCase "empty array" (Json.valueArray $ Base.list ([] :: [TTerm Value])) "[]",
    writerCase "single element" (Json.valueArray $ Base.list [Json.valueNumber $ Base.bigfloat 1.0]) "[1]",

    -- Multiple elements
    writerCase "multiple numbers" (Json.valueArray $ Base.list [
        Json.valueNumber $ Base.bigfloat 1.0,
        Json.valueNumber $ Base.bigfloat 2.0,
        Json.valueNumber $ Base.bigfloat 3.0]) "[1, 2, 3]",

    writerCase "multiple strings" (Json.valueArray $ Base.list [
        Json.valueString $ Base.string "a",
        Json.valueString $ Base.string "b"]) "[\"a\", \"b\"]",

    -- Mixed types
    writerCase "mixed types" (Json.valueArray $ Base.list [
        Json.valueNumber $ Base.bigfloat 1.0,
        Json.valueString $ Base.string "two",
        Json.valueBoolean $ Base.boolean True,
        Json.valueNull]) "[1, \"two\", true, null]"]

objectsGroup :: TTerm TestGroup
objectsGroup = subgroup "objects" [
    -- Empty and single key
    writerCase "empty object" (Json.valueObject $ Base.map M.empty) "{}",
    writerCase "single key-value" (Json.valueObject $ Base.map $ M.fromList [
        (Base.string "name", Json.valueString $ Base.string "Alice")]) "{\"name\": \"Alice\"}",

    -- Multiple keys
    writerCase "multiple keys" (Json.valueObject $ Base.map $ M.fromList [
        (Base.string "a", Json.valueNumber $ Base.bigfloat 1.0),
        (Base.string "b", Json.valueNumber $ Base.bigfloat 2.0)]) "{\"a\": 1, \"b\": 2}",

    -- Mixed value types
    writerCase "mixed value types" (Json.valueObject $ Base.map $ M.fromList [
        (Base.string "count", Json.valueNumber $ Base.bigfloat 42.0),
        (Base.string "name", Json.valueString $ Base.string "test"),
        (Base.string "active", Json.valueBoolean $ Base.boolean True)]) "{\"active\": true, \"count\": 42, \"name\": \"test\"}"]

nestedGroup :: TTerm TestGroup
nestedGroup = subgroup "nested structures" [
    -- Array of arrays
    writerCase "nested arrays" (Json.valueArray $ Base.list [
        Json.valueArray $ Base.list [Json.valueNumber $ Base.bigfloat 1.0, Json.valueNumber $ Base.bigfloat 2.0],
        Json.valueArray $ Base.list [Json.valueNumber $ Base.bigfloat 3.0, Json.valueNumber $ Base.bigfloat 4.0]]) "[[1, 2], [3, 4]]",

    -- Object with array
    writerCase "object with array" (Json.valueObject $ Base.map $ M.fromList [
        (Base.string "items", Json.valueArray $ Base.list [
            Json.valueNumber $ Base.bigfloat 1.0,
            Json.valueNumber $ Base.bigfloat 2.0])]) "{\"items\": [1, 2]}",

    -- Array of objects
    writerCase "array of objects" (Json.valueArray $ Base.list [
        Json.valueObject $ Base.map $ M.singleton (Base.string "id") (Json.valueNumber $ Base.bigfloat 1.0),
        Json.valueObject $ Base.map $ M.singleton (Base.string "id") (Json.valueNumber $ Base.bigfloat 2.0)]) "[{\"id\": 1}, {\"id\": 2}]",

    -- Nested object
    writerCase "nested object" (Json.valueObject $ Base.map $ M.fromList [
        (Base.string "user", Json.valueObject $ Base.map $ M.fromList [
            (Base.string "name", Json.valueString $ Base.string "Bob")])]) "{\"user\": {\"name\": \"Bob\"}}"]
