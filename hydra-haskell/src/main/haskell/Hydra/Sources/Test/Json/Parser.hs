module Hydra.Sources.Test.Json.Parser where

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
import qualified Hydra.Dsl.Meta.Parsing as Parsing


ns :: Namespace
ns = Namespace "hydra.test.json.parser"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces
    (Just "Test cases for JSON parsing")
  where
    elements = [
        Phantoms.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for JSON parsing" $
    supergroup "JSON parsing" [
      primitivesGroup,
      stringsGroup,
      arraysGroup,
      objectsGroup,
      nestedGroup,
      whitespaceGroup]

-- Helper for creating successful JSON parser test cases
parserCase :: String -> String -> TTerm Value -> TTerm TestCaseWithMetadata
parserCase name input expectedValue = testCaseWithMetadata (Phantoms.string name)
  (testCaseJsonParser $ jsonParserTestCase (Phantoms.string input)
    (Parsing.parseResultSuccess $ Parsing.parseSuccess expectedValue (Phantoms.string "")))
  Phantoms.nothing (Phantoms.list ([] :: [TTerm Tag]))

primitivesGroup :: TTerm TestGroup
primitivesGroup = subgroup "primitives" [
    -- Null
    parserCase "null" "null" Json.valueNull,

    -- Booleans
    parserCase "true" "true" (Json.valueBoolean $ Phantoms.boolean True),
    parserCase "false" "false" (Json.valueBoolean $ Phantoms.boolean False),

    -- Numbers - integers
    parserCase "zero" "0" (Json.valueNumber $ Phantoms.bigfloat 0.0),
    parserCase "positive integer" "42" (Json.valueNumber $ Phantoms.bigfloat 42.0),
    parserCase "negative integer" "-17" (Json.valueNumber $ Phantoms.bigfloat (-17.0)),
    parserCase "large integer" "1000000" (Json.valueNumber $ Phantoms.bigfloat 1000000.0),

    -- Numbers - decimals
    parserCase "decimal" "3.14" (Json.valueNumber $ Phantoms.bigfloat 3.14),
    parserCase "negative decimal" "-2.5" (Json.valueNumber $ Phantoms.bigfloat (-2.5)),

    -- Numbers - scientific notation
    parserCase "scientific notation" "1e3" (Json.valueNumber $ Phantoms.bigfloat 1000.0),
    parserCase "scientific with decimal" "1.5e2" (Json.valueNumber $ Phantoms.bigfloat 150.0),
    parserCase "negative exponent" "1e-2" (Json.valueNumber $ Phantoms.bigfloat 0.01)]

stringsGroup :: TTerm TestGroup
stringsGroup = subgroup "strings" [
    -- Basic strings
    parserCase "empty string" "\"\"" (Json.valueString $ Phantoms.string ""),
    parserCase "simple string" "\"hello\"" (Json.valueString $ Phantoms.string "hello"),
    parserCase "string with spaces" "\"hello world\"" (Json.valueString $ Phantoms.string "hello world"),

    -- Escape sequences
    parserCase "escaped double quote" "\"say \\\"hi\\\"\"" (Json.valueString $ Phantoms.string "say \"hi\""),
    parserCase "escaped backslash" "\"path\\\\to\\\\file\"" (Json.valueString $ Phantoms.string "path\\to\\file"),
    parserCase "escaped newline" "\"line1\\nline2\"" (Json.valueString $ Phantoms.string "line1\nline2"),
    parserCase "escaped carriage return" "\"line1\\rline2\"" (Json.valueString $ Phantoms.string "line1\rline2"),
    parserCase "escaped tab" "\"col1\\tcol2\"" (Json.valueString $ Phantoms.string "col1\tcol2"),
    parserCase "escaped forward slash" "\"a\\/b\"" (Json.valueString $ Phantoms.string "a/b")]

arraysGroup :: TTerm TestGroup
arraysGroup = subgroup "arrays" [
    -- Empty and single element
    parserCase "empty array" "[]" (Json.valueArray $ Phantoms.list ([] :: [TTerm Value])),
    parserCase "single element" "[1]" (Json.valueArray $ Phantoms.list [Json.valueNumber $ Phantoms.bigfloat 1.0]),

    -- Multiple elements
    parserCase "multiple numbers" "[1, 2, 3]" (Json.valueArray $ Phantoms.list [
        Json.valueNumber $ Phantoms.bigfloat 1.0,
        Json.valueNumber $ Phantoms.bigfloat 2.0,
        Json.valueNumber $ Phantoms.bigfloat 3.0]),

    parserCase "multiple strings" "[\"a\", \"b\"]" (Json.valueArray $ Phantoms.list [
        Json.valueString $ Phantoms.string "a",
        Json.valueString $ Phantoms.string "b"]),

    -- Mixed types
    parserCase "mixed types" "[1, \"two\", true, null]" (Json.valueArray $ Phantoms.list [
        Json.valueNumber $ Phantoms.bigfloat 1.0,
        Json.valueString $ Phantoms.string "two",
        Json.valueBoolean $ Phantoms.boolean True,
        Json.valueNull])]

objectsGroup :: TTerm TestGroup
objectsGroup = subgroup "objects" [
    -- Empty and single key
    parserCase "empty object" "{}" (Json.valueObject $ Phantoms.map M.empty),
    parserCase "single key-value" "{\"name\": \"Alice\"}" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "name", Json.valueString $ Phantoms.string "Alice")]),

    -- Multiple keys
    parserCase "multiple keys" "{\"a\": 1, \"b\": 2}" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "a", Json.valueNumber $ Phantoms.bigfloat 1.0),
        (Phantoms.string "b", Json.valueNumber $ Phantoms.bigfloat 2.0)]),

    -- Mixed value types
    parserCase "mixed value types" "{\"active\": true, \"count\": 42, \"name\": \"test\"}" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "count", Json.valueNumber $ Phantoms.bigfloat 42.0),
        (Phantoms.string "name", Json.valueString $ Phantoms.string "test"),
        (Phantoms.string "active", Json.valueBoolean $ Phantoms.boolean True)])]

nestedGroup :: TTerm TestGroup
nestedGroup = subgroup "nested structures" [
    -- Array of arrays
    parserCase "nested arrays" "[[1, 2], [3, 4]]" (Json.valueArray $ Phantoms.list [
        Json.valueArray $ Phantoms.list [Json.valueNumber $ Phantoms.bigfloat 1.0, Json.valueNumber $ Phantoms.bigfloat 2.0],
        Json.valueArray $ Phantoms.list [Json.valueNumber $ Phantoms.bigfloat 3.0, Json.valueNumber $ Phantoms.bigfloat 4.0]]),

    -- Object with array
    parserCase "object with array" "{\"items\": [1, 2]}" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "items", Json.valueArray $ Phantoms.list [
            Json.valueNumber $ Phantoms.bigfloat 1.0,
            Json.valueNumber $ Phantoms.bigfloat 2.0])]),

    -- Array of objects
    parserCase "array of objects" "[{\"id\": 1}, {\"id\": 2}]" (Json.valueArray $ Phantoms.list [
        Json.valueObject $ Phantoms.map $ M.singleton (Phantoms.string "id") (Json.valueNumber $ Phantoms.bigfloat 1.0),
        Json.valueObject $ Phantoms.map $ M.singleton (Phantoms.string "id") (Json.valueNumber $ Phantoms.bigfloat 2.0)]),

    -- Nested object
    parserCase "nested object" "{\"user\": {\"name\": \"Bob\"}}" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "user", Json.valueObject $ Phantoms.map $ M.fromList [
            (Phantoms.string "name", Json.valueString $ Phantoms.string "Bob")])])]

whitespaceGroup :: TTerm TestGroup
whitespaceGroup = subgroup "whitespace handling" [
    -- Leading/trailing whitespace
    parserCase "leading whitespace" "  null" Json.valueNull,
    parserCase "trailing whitespace" "null  " Json.valueNull,
    parserCase "both whitespace" "  null  " Json.valueNull,

    -- Whitespace in arrays
    parserCase "array with whitespace" "[ 1 , 2 , 3 ]" (Json.valueArray $ Phantoms.list [
        Json.valueNumber $ Phantoms.bigfloat 1.0,
        Json.valueNumber $ Phantoms.bigfloat 2.0,
        Json.valueNumber $ Phantoms.bigfloat 3.0]),

    -- Whitespace in objects
    parserCase "object with whitespace" "{ \"a\" : 1 }" (Json.valueObject $ Phantoms.map $ M.fromList [
        (Phantoms.string "a", Json.valueNumber $ Phantoms.bigfloat 1.0)]),

    -- Newlines
    parserCase "multiline array" "[\n  1,\n  2\n]" (Json.valueArray $ Phantoms.list [
        Json.valueNumber $ Phantoms.bigfloat 1.0,
        Json.valueNumber $ Phantoms.bigfloat 2.0])]
