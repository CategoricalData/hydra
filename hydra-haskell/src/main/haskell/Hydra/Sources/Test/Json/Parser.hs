module Hydra.Sources.Test.Json.Parser where

import Hydra.Kernel
import Hydra.Json.Model (Value)
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Meta.Phantoms as Base
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Json as Json
import qualified Hydra.Dsl.Meta.Parsing as Parsing
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Types.Testing as TestingTypes

import qualified Data.Map as M


ns :: Namespace
ns = Namespace "hydra.test.json.parser"

module_ :: Module
module_ = Module ns elements
    []
    KernelTypes.kernelTypesNamespaces
    (Just "Test cases for JSON parsing")
  where
    elements = [
        Base.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Base.doc "Test cases for JSON parsing" $
    supergroup "JSON parsing" [
      primitivesGroup,
      stringsGroup,
      arraysGroup,
      objectsGroup,
      nestedGroup,
      whitespaceGroup]

-- Helper for creating successful JSON parser test cases
parserCase :: String -> String -> TTerm Value -> TTerm TestCaseWithMetadata
parserCase name input expectedValue = testCaseWithMetadata (Base.string name)
  (testCaseJsonParser $ jsonParserTestCase (Base.string input)
    (Parsing.parseResultSuccess $ Parsing.parseSuccess expectedValue (Base.string "")))
  Base.nothing (Base.list ([] :: [TTerm Tag]))

primitivesGroup :: TTerm TestGroup
primitivesGroup = subgroup "primitives" [
    -- Null
    parserCase "null" "null" Json.valueNull,

    -- Booleans
    parserCase "true" "true" (Json.valueBoolean $ Base.boolean True),
    parserCase "false" "false" (Json.valueBoolean $ Base.boolean False),

    -- Numbers - integers
    parserCase "zero" "0" (Json.valueNumber $ Base.bigfloat 0.0),
    parserCase "positive integer" "42" (Json.valueNumber $ Base.bigfloat 42.0),
    parserCase "negative integer" "-17" (Json.valueNumber $ Base.bigfloat (-17.0)),
    parserCase "large integer" "1000000" (Json.valueNumber $ Base.bigfloat 1000000.0),

    -- Numbers - decimals
    parserCase "decimal" "3.14" (Json.valueNumber $ Base.bigfloat 3.14),
    parserCase "negative decimal" "-2.5" (Json.valueNumber $ Base.bigfloat (-2.5)),

    -- Numbers - scientific notation
    parserCase "scientific notation" "1e3" (Json.valueNumber $ Base.bigfloat 1000.0),
    parserCase "scientific with decimal" "1.5e2" (Json.valueNumber $ Base.bigfloat 150.0),
    parserCase "negative exponent" "1e-2" (Json.valueNumber $ Base.bigfloat 0.01)]

stringsGroup :: TTerm TestGroup
stringsGroup = subgroup "strings" [
    -- Basic strings
    parserCase "empty string" "\"\"" (Json.valueString $ Base.string ""),
    parserCase "simple string" "\"hello\"" (Json.valueString $ Base.string "hello"),
    parserCase "string with spaces" "\"hello world\"" (Json.valueString $ Base.string "hello world"),

    -- Escape sequences
    parserCase "escaped double quote" "\"say \\\"hi\\\"\"" (Json.valueString $ Base.string "say \"hi\""),
    parserCase "escaped backslash" "\"path\\\\to\\\\file\"" (Json.valueString $ Base.string "path\\to\\file"),
    parserCase "escaped newline" "\"line1\\nline2\"" (Json.valueString $ Base.string "line1\nline2"),
    parserCase "escaped carriage return" "\"line1\\rline2\"" (Json.valueString $ Base.string "line1\rline2"),
    parserCase "escaped tab" "\"col1\\tcol2\"" (Json.valueString $ Base.string "col1\tcol2"),
    parserCase "escaped forward slash" "\"a\\/b\"" (Json.valueString $ Base.string "a/b")]

arraysGroup :: TTerm TestGroup
arraysGroup = subgroup "arrays" [
    -- Empty and single element
    parserCase "empty array" "[]" (Json.valueArray $ Base.list ([] :: [TTerm Value])),
    parserCase "single element" "[1]" (Json.valueArray $ Base.list [Json.valueNumber $ Base.bigfloat 1.0]),

    -- Multiple elements
    parserCase "multiple numbers" "[1, 2, 3]" (Json.valueArray $ Base.list [
        Json.valueNumber $ Base.bigfloat 1.0,
        Json.valueNumber $ Base.bigfloat 2.0,
        Json.valueNumber $ Base.bigfloat 3.0]),

    parserCase "multiple strings" "[\"a\", \"b\"]" (Json.valueArray $ Base.list [
        Json.valueString $ Base.string "a",
        Json.valueString $ Base.string "b"]),

    -- Mixed types
    parserCase "mixed types" "[1, \"two\", true, null]" (Json.valueArray $ Base.list [
        Json.valueNumber $ Base.bigfloat 1.0,
        Json.valueString $ Base.string "two",
        Json.valueBoolean $ Base.boolean True,
        Json.valueNull])]

objectsGroup :: TTerm TestGroup
objectsGroup = subgroup "objects" [
    -- Empty and single key
    parserCase "empty object" "{}" (Json.valueObject $ Base.map M.empty),
    parserCase "single key-value" "{\"name\": \"Alice\"}" (Json.valueObject $ Base.map $ M.fromList [
        (Base.string "name", Json.valueString $ Base.string "Alice")]),

    -- Multiple keys
    parserCase "multiple keys" "{\"a\": 1, \"b\": 2}" (Json.valueObject $ Base.map $ M.fromList [
        (Base.string "a", Json.valueNumber $ Base.bigfloat 1.0),
        (Base.string "b", Json.valueNumber $ Base.bigfloat 2.0)]),

    -- Mixed value types
    parserCase "mixed value types" "{\"active\": true, \"count\": 42, \"name\": \"test\"}" (Json.valueObject $ Base.map $ M.fromList [
        (Base.string "count", Json.valueNumber $ Base.bigfloat 42.0),
        (Base.string "name", Json.valueString $ Base.string "test"),
        (Base.string "active", Json.valueBoolean $ Base.boolean True)])]

nestedGroup :: TTerm TestGroup
nestedGroup = subgroup "nested structures" [
    -- Array of arrays
    parserCase "nested arrays" "[[1, 2], [3, 4]]" (Json.valueArray $ Base.list [
        Json.valueArray $ Base.list [Json.valueNumber $ Base.bigfloat 1.0, Json.valueNumber $ Base.bigfloat 2.0],
        Json.valueArray $ Base.list [Json.valueNumber $ Base.bigfloat 3.0, Json.valueNumber $ Base.bigfloat 4.0]]),

    -- Object with array
    parserCase "object with array" "{\"items\": [1, 2]}" (Json.valueObject $ Base.map $ M.fromList [
        (Base.string "items", Json.valueArray $ Base.list [
            Json.valueNumber $ Base.bigfloat 1.0,
            Json.valueNumber $ Base.bigfloat 2.0])]),

    -- Array of objects
    parserCase "array of objects" "[{\"id\": 1}, {\"id\": 2}]" (Json.valueArray $ Base.list [
        Json.valueObject $ Base.map $ M.singleton (Base.string "id") (Json.valueNumber $ Base.bigfloat 1.0),
        Json.valueObject $ Base.map $ M.singleton (Base.string "id") (Json.valueNumber $ Base.bigfloat 2.0)]),

    -- Nested object
    parserCase "nested object" "{\"user\": {\"name\": \"Bob\"}}" (Json.valueObject $ Base.map $ M.fromList [
        (Base.string "user", Json.valueObject $ Base.map $ M.fromList [
            (Base.string "name", Json.valueString $ Base.string "Bob")])])]

whitespaceGroup :: TTerm TestGroup
whitespaceGroup = subgroup "whitespace handling" [
    -- Leading/trailing whitespace
    parserCase "leading whitespace" "  null" Json.valueNull,
    parserCase "trailing whitespace" "null  " Json.valueNull,
    parserCase "both whitespace" "  null  " Json.valueNull,

    -- Whitespace in arrays
    parserCase "array with whitespace" "[ 1 , 2 , 3 ]" (Json.valueArray $ Base.list [
        Json.valueNumber $ Base.bigfloat 1.0,
        Json.valueNumber $ Base.bigfloat 2.0,
        Json.valueNumber $ Base.bigfloat 3.0]),

    -- Whitespace in objects
    parserCase "object with whitespace" "{ \"a\" : 1 }" (Json.valueObject $ Base.map $ M.fromList [
        (Base.string "a", Json.valueNumber $ Base.bigfloat 1.0)]),

    -- Newlines
    parserCase "multiline array" "[\n  1,\n  2\n]" (Json.valueArray $ Base.list [
        Json.valueNumber $ Base.bigfloat 1.0,
        Json.valueNumber $ Base.bigfloat 2.0])]
