-- | Test cases for the type-directed JSON coder
module Hydra.Sources.Test.Json.Coder where

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
ns = Namespace "hydra.test.json.coder"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces
    (Just "Test cases for the type-directed JSON coder (Hydra Term <-> JSON Value)")
  where
    elements = [
        Phantoms.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for the type-directed JSON coder" $
    supergroup "JSON coder" [
      literalCoderGroup,
      integerCoderGroup,
      floatCoderGroup,
      collectionCoderGroup,
      optionalCoderGroup,
      recordCoderGroup]

-- Helper for creating JSON coder test cases
coderTest :: String -> TTerm Type -> TTerm Term -> TTerm Value -> TTerm TestCaseWithMetadata
coderTest name typ term json = testCaseWithMetadata (Phantoms.string name)
  (testCaseJsonCoder $ jsonCoderTestCase typ term json)
  Phantoms.nothing (Phantoms.list ([] :: [TTerm Tag]))

-- | Test cases for literal type encoding/decoding
literalCoderGroup :: TTerm TestGroup
literalCoderGroup = subgroup "literal types" [
    -- Booleans
    coderTest "boolean true"
      T.boolean
      (Terms.boolean True)
      (Json.valueBoolean $ Phantoms.boolean True),
    coderTest "boolean false"
      T.boolean
      (Terms.boolean False)
      (Json.valueBoolean $ Phantoms.boolean False),

    -- Integers
    coderTest "int32 positive"
      T.int32
      (Terms.int32 42)
      (Json.valueNumber $ Phantoms.bigfloat 42.0),
    coderTest "int32 negative"
      T.int32
      (Terms.int32 (-17))
      (Json.valueNumber $ Phantoms.bigfloat (-17.0)),
    coderTest "int32 zero"
      T.int32
      (Terms.int32 0)
      (Json.valueNumber $ Phantoms.bigfloat 0.0),

    -- Floats (using exact binary fractions to avoid precision issues)
    coderTest "float32"
      T.float32
      (Terms.float32 1.5)
      (Json.valueNumber $ Phantoms.bigfloat 1.5),
    coderTest "float64"
      T.float64
      (Terms.float64 2.718)
      (Json.valueNumber $ Phantoms.bigfloat 2.718),

    -- Strings
    coderTest "string simple"
      T.string
      (Terms.string "hello")
      (Json.valueString $ Phantoms.string "hello"),
    coderTest "string empty"
      T.string
      (Terms.string "")
      (Json.valueString $ Phantoms.string ""),
    coderTest "string with spaces"
      T.string
      (Terms.string "hello world")
      (Json.valueString $ Phantoms.string "hello world")]

-- | Test cases for all integer types
integerCoderGroup :: TTerm TestGroup
integerCoderGroup = subgroup "integer types" [
    -- int8
    coderTest "int8 positive"
      T.int8
      (Terms.int8 127)
      (Json.valueNumber $ Phantoms.bigfloat 127.0),
    coderTest "int8 negative"
      T.int8
      (Terms.int8 (-128))
      (Json.valueNumber $ Phantoms.bigfloat (-128.0)),

    -- int16
    coderTest "int16 positive"
      T.int16
      (Terms.int16 1000)
      (Json.valueNumber $ Phantoms.bigfloat 1000.0),
    coderTest "int16 negative"
      T.int16
      (Terms.int16 (-1000))
      (Json.valueNumber $ Phantoms.bigfloat (-1000.0)),

    -- uint8
    coderTest "uint8 max"
      T.uint8
      (Terms.uint8 255)
      (Json.valueNumber $ Phantoms.bigfloat 255.0),
    coderTest "uint8 zero"
      T.uint8
      (Terms.uint8 0)
      (Json.valueNumber $ Phantoms.bigfloat 0.0),

    -- uint16
    coderTest "uint16 positive"
      T.uint16
      (Terms.uint16 60000)
      (Json.valueNumber $ Phantoms.bigfloat 60000.0),

    -- uint32
    coderTest "uint32 positive"
      T.uint32
      (Terms.uint32 4000000000)
      (Json.valueNumber $ Phantoms.bigfloat 4000000000.0),

    -- uint64
    coderTest "uint64 positive"
      T.uint64
      (Terms.uint64 1000000)
      (Json.valueNumber $ Phantoms.bigfloat 1000000.0),

    -- bigint (encoded as number, like other integer types)
    coderTest "bigint positive"
      T.bigint
      (Terms.bigint 123456789012345)
      (Json.valueNumber $ Phantoms.bigfloat 123456789012345.0),
    coderTest "bigint negative"
      T.bigint
      (Terms.bigint (-999999999999))
      (Json.valueNumber $ Phantoms.bigfloat (-999999999999.0))]

-- | Test cases for float types
floatCoderGroup :: TTerm TestGroup
floatCoderGroup = subgroup "float types" [
    -- bigfloat (note: like bigint, these may be encoded as strings for precision in some cases)
    coderTest "bigfloat positive"
      T.bigfloat
      (Terms.bigfloat 3.14159265359)
      (Json.valueNumber $ Phantoms.bigfloat 3.14159265359),
    coderTest "bigfloat negative"
      T.bigfloat
      (Terms.bigfloat (-2.71828))
      (Json.valueNumber $ Phantoms.bigfloat (-2.71828)),
    coderTest "bigfloat zero"
      T.bigfloat
      (Terms.bigfloat 0.0)
      (Json.valueNumber $ Phantoms.bigfloat 0.0)]

-- | Test cases for collection type encoding/decoding
collectionCoderGroup :: TTerm TestGroup
collectionCoderGroup = subgroup "collection types" [
    -- Lists
    coderTest "list of integers"
      (T.list T.int32)
      (Terms.list [Terms.int32 1, Terms.int32 2, Terms.int32 3])
      (Json.valueArray $ Phantoms.list [
          Json.valueNumber $ Phantoms.bigfloat 1.0,
          Json.valueNumber $ Phantoms.bigfloat 2.0,
          Json.valueNumber $ Phantoms.bigfloat 3.0]),
    coderTest "list of strings"
      (T.list T.string)
      (Terms.list [Terms.string "a", Terms.string "b"])
      (Json.valueArray $ Phantoms.list [
          Json.valueString $ Phantoms.string "a",
          Json.valueString $ Phantoms.string "b"]),
    coderTest "empty list"
      (T.list T.int32)
      (Terms.list [])
      (Json.valueArray $ Phantoms.list ([] :: [TTerm Value]))]
    -- Note: Map tests are more complex due to the map term representation

-- | Test cases for optional type encoding/decoding
optionalCoderGroup :: TTerm TestGroup
optionalCoderGroup = subgroup "optional types" [
    coderTest "optional string with value"
      (T.optional T.string)
      (Terms.optional $ Phantoms.just $ Terms.string "hello")
      (Json.valueString $ Phantoms.string "hello"),
    coderTest "optional string nothing"
      (T.optional T.string)
      (Terms.optional Phantoms.nothing)
      Json.valueNull,
    coderTest "optional int with value"
      (T.optional T.int32)
      (Terms.optional $ Phantoms.just $ Terms.int32 42)
      (Json.valueNumber $ Phantoms.bigfloat 42.0),
    coderTest "optional int nothing"
      (T.optional T.int32)
      (Terms.optional Phantoms.nothing)
      Json.valueNull]

-- | Test cases for record type encoding/decoding
recordCoderGroup :: TTerm TestGroup
recordCoderGroup = subgroup "record types" ([] :: [TTerm TestCaseWithMetadata])
