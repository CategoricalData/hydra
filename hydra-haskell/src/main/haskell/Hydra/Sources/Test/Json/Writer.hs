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

-- Additional imports specific to this module
import Hydra.Json.Model (Value)
import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Json as Json


ns :: Namespace
ns = Namespace "hydra.test.json.writer"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces
    (Just "Test cases for JSON serialization")
  where
    elements = [
        Phantoms.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for JSON serialization (writer)" $
    supergroup "JSON serialization" [
      primitivesGroup,
      stringsGroup,
      arraysGroup,
      objectsGroup,
      nestedGroup]

-- Helper for creating JSON writer test cases
writerCase :: String -> TTerm Value -> String -> TTerm TestCaseWithMetadata
writerCase name jsonValue expectedStr = testCaseWithMetadata (Phantoms.string name)
  (testCaseJsonWriter $ jsonWriterTestCase jsonValue (Phantoms.string expectedStr))
  Phantoms.nothing (Phantoms.list ([] :: [TTerm Tag]))

primitivesGroup :: TTerm TestGroup
primitivesGroup = subgroup "primitives" [
    -- Null
    writerCase "null" Json.valueNull "null",

    -- Booleans
    writerCase "true" (Json.valueBoolean $ Phantoms.boolean True) "true",
    writerCase "false" (Json.valueBoolean $ Phantoms.boolean False) "false",

    -- Numbers - integers (whole numbers are formatted without decimal point)
    writerCase "zero" (Json.valueNumber $ Phantoms.bigfloat 0.0) "0",
    writerCase "positive integer" (Json.valueNumber $ Phantoms.bigfloat 42.0) "42",
    writerCase "negative integer" (Json.valueNumber $ Phantoms.bigfloat (-17.0)) "-17",
    writerCase "large integer" (Json.valueNumber $ Phantoms.bigfloat 1000000.0) "1000000",

    -- Numbers - decimals
    writerCase "decimal" (Json.valueNumber $ Phantoms.bigfloat 3.14) "3.14",
    writerCase "negative decimal" (Json.valueNumber $ Phantoms.bigfloat (-2.5)) "-2.5",
    writerCase "small decimal" (Json.valueNumber $ Phantoms.bigfloat 0.001) "1.0e-3"]

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
    writerCase "single element" (Json.valueArray $ Phantoms.list [Json.valueNumber $ Phantoms.bigfloat 1.0]) "[1]",

    -- Multiple elements
    writerCase "multiple numbers" (Json.valueArray $ Phantoms.list [
        Json.valueNumber $ Phantoms.bigfloat 1.0,
        Json.valueNumber $ Phantoms.bigfloat 2.0,
        Json.valueNumber $ Phantoms.bigfloat 3.0]) "[1, 2, 3]",

    writerCase "multiple strings" (Json.valueArray $ Phantoms.list [
        Json.valueString $ Phantoms.string "a",
        Json.valueString $ Phantoms.string "b"]) "[\"a\", \"b\"]",

    -- Mixed types
    writerCase "mixed types" (Json.valueArray $ Phantoms.list [
        Json.valueNumber $ Phantoms.bigfloat 1.0,
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
        (Phantoms.string "a", Json.valueNumber $ Phantoms.bigfloat 1.0),
        (Phantoms.string "b", Json.valueNumber $ Phantoms.bigfloat 2.0)]) "{\"a\": 1, \"b\": 2}",

    -- Mixed value types
    writerCase "mixed value types" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "count", Json.valueNumber $ Phantoms.bigfloat 42.0),
        (Phantoms.string "name", Json.valueString $ Phantoms.string "test"),
        (Phantoms.string "active", Json.valueBoolean $ Phantoms.boolean True)]) "{\"active\": true, \"count\": 42, \"name\": \"test\"}"]

nestedGroup :: TTerm TestGroup
nestedGroup = subgroup "nested structures" [
    -- Array of arrays
    writerCase "nested arrays" (Json.valueArray $ Phantoms.list [
        Json.valueArray $ Phantoms.list [Json.valueNumber $ Phantoms.bigfloat 1.0, Json.valueNumber $ Phantoms.bigfloat 2.0],
        Json.valueArray $ Phantoms.list [Json.valueNumber $ Phantoms.bigfloat 3.0, Json.valueNumber $ Phantoms.bigfloat 4.0]]) "[[1, 2], [3, 4]]",

    -- Object with array
    writerCase "object with array" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "items", Json.valueArray $ Phantoms.list [
            Json.valueNumber $ Phantoms.bigfloat 1.0,
            Json.valueNumber $ Phantoms.bigfloat 2.0])]) "{\"items\": [1, 2]}",

    -- Array of objects
    writerCase "array of objects" (Json.valueArray $ Phantoms.list [
        Json.valueObject $ Phantoms.map $ M.singleton (Phantoms.string "id") (Json.valueNumber $ Phantoms.bigfloat 1.0),
        Json.valueObject $ Phantoms.map $ M.singleton (Phantoms.string "id") (Json.valueNumber $ Phantoms.bigfloat 2.0)]) "[{\"id\": 1}, {\"id\": 2}]",

    -- Nested object
    writerCase "nested object" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "user", Json.valueObject $ Phantoms.map $ M.fromList [
            (Phantoms.string "name", Json.valueString $ Phantoms.string "Bob")])]) "{\"user\": {\"name\": \"Bob\"}}"]
