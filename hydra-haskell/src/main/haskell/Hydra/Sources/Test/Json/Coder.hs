-- | Test cases for the type-directed JSON coder
--
-- Note: This module supersedes the Haskell-specific Hydra.Staging.Json.CoderSpec tests.
-- The following additional coverage should be added to this kernel test suite:
--   - Additional integer types: uint16, bigint
--   - Additional literal types: binary data, bigfloat
--   - Maps (Map String Int -> JSON objects)
--   - Sets (Set String -> JSON arrays)
--   - Nominal types / type aliases (dereferenced to underlying types)
--   - Union types encoded as single-attribute JSON objects
--   - Enum types encoded as objects with empty-object values
--   - Records with multiple fields (e.g., latlon with lat/lon fields)
--   - Functions (currently unsupported)
module Hydra.Sources.Test.Json.Coder where

import Hydra.Kernel
import Hydra.Json (Value)
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


module_ :: Module
module_ = Module (Namespace "hydra.test.json.coder") elements
    []
    KernelTypes.kernelTypesModules
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
