{-# LANGUAGE FlexibleContexts #-}

-- | Round-trip test cases for the JSON encoder and decoder
--
-- These tests verify that:
-- 1. Terms can be encoded to JSON and decoded back to the original term
-- 2. The encoder and decoder handle all supported term constructs correctly
module Hydra.Sources.Test.Json.Roundtrip where

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

-- Additional imports specific to this module
import Hydra.Testing
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Json.Encode as EncodeModule
import qualified Hydra.Sources.Json.Decode as DecodeModule


ns :: Namespace
ns = Namespace "hydra.test.json.roundtrip"

module_ :: Module
module_ = Module ns elements
    [ShowCore.ns, Namespace "hydra.json.encode", Namespace "hydra.json.decode"]
    kernelTypesNamespaces
    (Just "Round-trip test cases for JSON encoding and decoding")
  where
    elements = [
        Phantoms.toTermDefinition allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- Local alias for polymorphic application
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Phantoms.doc "Round-trip test cases for JSON encoding and decoding" $
    supergroup "JSON round-trip" [
      literalRoundtripGroup,
      collectionRoundtripGroup,
      optionalRoundtripGroup]

-- Helper for creating JSON round-trip test cases (universal)
-- Encodes term to JSON, decodes back, shows both and compares.
roundtripTest :: String -> TTerm Type -> TTerm Term -> TTerm TestCaseWithMetadata
roundtripTest testName typ term = universalCase testName
  (Eithers.either_
    (Phantoms.lambda "e" $ Phantoms.var "e")
    (Phantoms.lambda "json" $
      Eithers.either_
        (Phantoms.lambda "e" $ Phantoms.var "e")
        (Phantoms.lambda "decoded" $ ShowCore.term # Phantoms.var "decoded")
        (DecodeModule.fromJson # Maps.empty # Core.name (Phantoms.string "test") # typ # Phantoms.var "json"))
    (EncodeModule.toJson # term))
  (ShowCore.term # term)

----------------------------------------
-- Literal types
----------------------------------------

literalRoundtripGroup :: TTerm TestGroup
literalRoundtripGroup = subgroup "literal types" [
    -- Booleans
    roundtripTest "boolean true" T.boolean (boolean True),
    roundtripTest "boolean false" T.boolean (boolean False),

    -- Integers (native JSON numbers - fit within JSON precision)
    roundtripTest "int8 positive" T.int8 (int8 42),
    roundtripTest "int8 negative" T.int8 (int8 (-17)),
    roundtripTest "int16" T.int16 (int16 1000),
    roundtripTest "int32" T.int32 (int32 100000),
    roundtripTest "uint8" T.uint8 (uint8 200),
    roundtripTest "uint16" T.uint16 (uint16 50000),

    -- Larger integers (within JSON safe integer range ~2^53)
    roundtripTest "int64" T.int64 (int64 1000000000000),
    roundtripTest "uint32" T.uint32 (uint32 4000000000),

    -- Floats
    roundtripTest "float32" T.float32 (float32 1.5),
    roundtripTest "float64" T.float64 (float64 3.14159),

    -- Strings
    roundtripTest "string simple" T.string (string "hello"),
    roundtripTest "string empty" T.string (string ""),
    roundtripTest "string with spaces" T.string (string "hello world")]

----------------------------------------
-- Collection types
----------------------------------------

collectionRoundtripGroup :: TTerm TestGroup
collectionRoundtripGroup = subgroup "collection types" [
    -- Lists
    roundtripTest "list of integers"
      (T.list T.int32)
      (list [int32 1, int32 2, int32 3]),
    roundtripTest "list of strings"
      (T.list T.string)
      (list [string "a", string "b"]),
    roundtripTest "empty list"
      (T.list T.int32)
      (list []),
    roundtripTest "nested list"
      (T.list $ T.list T.int32)
      (list [list [int32 1, int32 2], list [int32 3]]),

    -- Sets (encoded as arrays)
    roundtripTest "set of strings"
      (T.set T.string)
      (set [string "a", string "b"]),
    roundtripTest "empty set"
      (T.set T.int32)
      (set [])]

----------------------------------------
-- Optional types
----------------------------------------

optionalRoundtripGroup :: TTerm TestGroup
optionalRoundtripGroup = subgroup "optional types" [
    roundtripTest "optional string with value"
      (T.optional T.string)
      (optional $ just $ string "hello"),
    roundtripTest "optional string nothing"
      (T.optional T.string)
      (optional nothing),
    roundtripTest "optional int with value"
      (T.optional T.int32)
      (optional $ just $ int32 42)]
