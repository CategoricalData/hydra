-- | Test cases for the type-directed JSON coder
module Hydra.Sources.Test.Json.Coder where

import Hydra.Kernel
import Hydra.Json.Model (Value)
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Meta.Phantoms as Base
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Types as MetaTypes
import qualified Hydra.Dsl.Meta.Json as Json
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Types.Testing as TestingTypes


ns :: Namespace
ns = Namespace "hydra.test.json.coder"

module_ :: Module
module_ = Module ns elements
    []
    KernelTypes.kernelTypesNamespaces
    (Just "Test cases for the type-directed JSON coder (Hydra Term <-> JSON Value)")
  where
    elements = [
        Base.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Base.doc "Test cases for the type-directed JSON coder" $
    supergroup "JSON coder" [
      literalCoderGroup,
      integerCoderGroup,
      floatCoderGroup,
      collectionCoderGroup,
      optionalCoderGroup,
      recordCoderGroup]

-- Helper for creating JSON coder test cases
coderTest :: String -> TTerm Type -> TTerm Term -> TTerm Value -> TTerm TestCaseWithMetadata
coderTest name typ term json = testCaseWithMetadata (Base.string name)
  (testCaseJsonCoder $ jsonCoderTestCase typ term json)
  Base.nothing (Base.list ([] :: [TTerm Tag]))

-- | Test cases for literal type encoding/decoding
literalCoderGroup :: TTerm TestGroup
literalCoderGroup = subgroup "literal types" [
    -- Booleans
    coderTest "boolean true"
      MetaTypes.boolean
      (MetaTerms.boolean True)
      (Json.valueBoolean $ Base.boolean True),
    coderTest "boolean false"
      MetaTypes.boolean
      (MetaTerms.boolean False)
      (Json.valueBoolean $ Base.boolean False),

    -- Integers
    coderTest "int32 positive"
      MetaTypes.int32
      (MetaTerms.int32 42)
      (Json.valueNumber $ Base.bigfloat 42.0),
    coderTest "int32 negative"
      MetaTypes.int32
      (MetaTerms.int32 (-17))
      (Json.valueNumber $ Base.bigfloat (-17.0)),
    coderTest "int32 zero"
      MetaTypes.int32
      (MetaTerms.int32 0)
      (Json.valueNumber $ Base.bigfloat 0.0),

    -- Floats (using exact binary fractions to avoid precision issues)
    coderTest "float32"
      MetaTypes.float32
      (MetaTerms.float32 1.5)
      (Json.valueNumber $ Base.bigfloat 1.5),
    coderTest "float64"
      MetaTypes.float64
      (MetaTerms.float64 2.718)
      (Json.valueNumber $ Base.bigfloat 2.718),

    -- Strings
    coderTest "string simple"
      MetaTypes.string
      (MetaTerms.string "hello")
      (Json.valueString $ Base.string "hello"),
    coderTest "string empty"
      MetaTypes.string
      (MetaTerms.string "")
      (Json.valueString $ Base.string ""),
    coderTest "string with spaces"
      MetaTypes.string
      (MetaTerms.string "hello world")
      (Json.valueString $ Base.string "hello world")]

-- | Test cases for all integer types
integerCoderGroup :: TTerm TestGroup
integerCoderGroup = subgroup "integer types" [
    -- int8
    coderTest "int8 positive"
      MetaTypes.int8
      (MetaTerms.int8 127)
      (Json.valueNumber $ Base.bigfloat 127.0),
    coderTest "int8 negative"
      MetaTypes.int8
      (MetaTerms.int8 (-128))
      (Json.valueNumber $ Base.bigfloat (-128.0)),

    -- int16
    coderTest "int16 positive"
      MetaTypes.int16
      (MetaTerms.int16 1000)
      (Json.valueNumber $ Base.bigfloat 1000.0),
    coderTest "int16 negative"
      MetaTypes.int16
      (MetaTerms.int16 (-1000))
      (Json.valueNumber $ Base.bigfloat (-1000.0)),

    -- uint8
    coderTest "uint8 max"
      MetaTypes.uint8
      (MetaTerms.uint8 255)
      (Json.valueNumber $ Base.bigfloat 255.0),
    coderTest "uint8 zero"
      MetaTypes.uint8
      (MetaTerms.uint8 0)
      (Json.valueNumber $ Base.bigfloat 0.0),

    -- uint16
    coderTest "uint16 positive"
      MetaTypes.uint16
      (MetaTerms.uint16 60000)
      (Json.valueNumber $ Base.bigfloat 60000.0),

    -- uint32
    coderTest "uint32 positive"
      MetaTypes.uint32
      (MetaTerms.uint32 4000000000)
      (Json.valueNumber $ Base.bigfloat 4000000000.0),

    -- uint64
    coderTest "uint64 positive"
      MetaTypes.uint64
      (MetaTerms.uint64 1000000)
      (Json.valueNumber $ Base.bigfloat 1000000.0),

    -- bigint (encoded as number, like other integer types)
    coderTest "bigint positive"
      MetaTypes.bigint
      (MetaTerms.bigint 123456789012345)
      (Json.valueNumber $ Base.bigfloat 123456789012345.0),
    coderTest "bigint negative"
      MetaTypes.bigint
      (MetaTerms.bigint (-999999999999))
      (Json.valueNumber $ Base.bigfloat (-999999999999.0))]

-- | Test cases for float types
floatCoderGroup :: TTerm TestGroup
floatCoderGroup = subgroup "float types" [
    -- bigfloat (note: like bigint, these may be encoded as strings for precision in some cases)
    coderTest "bigfloat positive"
      MetaTypes.bigfloat
      (MetaTerms.bigfloat 3.14159265359)
      (Json.valueNumber $ Base.bigfloat 3.14159265359),
    coderTest "bigfloat negative"
      MetaTypes.bigfloat
      (MetaTerms.bigfloat (-2.71828))
      (Json.valueNumber $ Base.bigfloat (-2.71828)),
    coderTest "bigfloat zero"
      MetaTypes.bigfloat
      (MetaTerms.bigfloat 0.0)
      (Json.valueNumber $ Base.bigfloat 0.0)]

-- | Test cases for collection type encoding/decoding
collectionCoderGroup :: TTerm TestGroup
collectionCoderGroup = subgroup "collection types" [
    -- Lists
    coderTest "list of integers"
      (MetaTypes.list MetaTypes.int32)
      (MetaTerms.list [MetaTerms.int32 1, MetaTerms.int32 2, MetaTerms.int32 3])
      (Json.valueArray $ Base.list [
          Json.valueNumber $ Base.bigfloat 1.0,
          Json.valueNumber $ Base.bigfloat 2.0,
          Json.valueNumber $ Base.bigfloat 3.0]),
    coderTest "list of strings"
      (MetaTypes.list MetaTypes.string)
      (MetaTerms.list [MetaTerms.string "a", MetaTerms.string "b"])
      (Json.valueArray $ Base.list [
          Json.valueString $ Base.string "a",
          Json.valueString $ Base.string "b"]),
    coderTest "empty list"
      (MetaTypes.list MetaTypes.int32)
      (MetaTerms.list [])
      (Json.valueArray $ Base.list ([] :: [TTerm Value]))]
    -- Note: Map tests are more complex due to the map term representation

-- | Test cases for optional type encoding/decoding
optionalCoderGroup :: TTerm TestGroup
optionalCoderGroup = subgroup "optional types" [
    coderTest "optional string with value"
      (MetaTypes.optional MetaTypes.string)
      (MetaTerms.optional $ Base.just $ MetaTerms.string "hello")
      (Json.valueString $ Base.string "hello"),
    coderTest "optional string nothing"
      (MetaTypes.optional MetaTypes.string)
      (MetaTerms.optional Base.nothing)
      Json.valueNull,
    coderTest "optional int with value"
      (MetaTypes.optional MetaTypes.int32)
      (MetaTerms.optional $ Base.just $ MetaTerms.int32 42)
      (Json.valueNumber $ Base.bigfloat 42.0),
    coderTest "optional int nothing"
      (MetaTypes.optional MetaTypes.int32)
      (MetaTerms.optional Base.nothing)
      Json.valueNull]

-- | Test cases for record type encoding/decoding
recordCoderGroup :: TTerm TestGroup
recordCoderGroup = subgroup "record types" ([] :: [TTerm TestCaseWithMetadata])
