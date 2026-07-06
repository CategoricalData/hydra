{-# LANGUAGE FlexibleContexts #-}

-- | Round-trip test cases for the JSON encoder and decoder
--
-- These tests verify that:
-- 1. Terms can be encoded to JSON and decoded back to the original term
-- 2. The encoder and decoder handle all supported term constructs correctly
module Hydra.Sources.Test.Json.Roundtrip where

-- Standard imports for tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Terms                   as Terms hiding ((@@))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                ((@@))
import qualified Hydra.Dsl.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Data.Scientific              as Sci

-- Additional imports specific to this module
import Hydra.Testing
import Hydra.Json.Model (Value)
import qualified Hydra.Json.Model as Model
import qualified Hydra.Encode.Json.Model as EncodeJsonModel
import qualified Hydra.Typed as Typed
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Json.Encode as EncodeModule
import qualified Hydra.Sources.Json.Decode as JsonDecode
import qualified Hydra.Sources.Json.Writer as JsonWriter


ns :: ModuleName
ns = ModuleName "hydra.test.json.roundtrip"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([ShowCore.ns, ModuleName "hydra.json.encode", ModuleName "hydra.json.decode", ModuleName "hydra.json.writer"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Round-trip test cases for JSON encoding and decoding"))}
  where
    definitions = [
        Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- Local alias for polymorphic application

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Round-trip test cases for JSON encoding and decoding" $
    supergroup "JSON round-trip" [
      literalRoundtripGroup,
      decimalRoundtripGroup,
      collectionRoundtripGroup,
      optionalRoundtripGroup,
      recordRoundtripGroup,
      parametricTypeRoundtripGroup,
      unionRoundtripGroup,
      wireShapeGroup]

-- Helper for creating JSON round-trip test cases (universal)
-- Encodes term to JSON, decodes back, shows both and compares.
roundtripTest :: String -> TypedTerm Type -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
roundtripTest testName typ term = universalCase testName
  (Eithers.either
    (Phantoms.lambda "e" $ Phantoms.var "e")
    (Phantoms.lambda "json" $
      Eithers.either
        (Phantoms.lambda "e" $ Phantoms.var "e")
        (Phantoms.lambda "decoded" $ ShowCore.term @@ Phantoms.var "decoded")
        (JsonDecode.fromJson @@ Maps.empty @@ Core.name (Phantoms.string "test") @@ typ @@ Phantoms.var "json"))
    (EncodeModule.toJson @@ Maps.empty @@ Core.name (Phantoms.string "test") @@ typ @@ term))
  (ShowCore.term @@ term)

-- Helper that pins the encoder's exact wire shape: encodes a term to JSON, serializes
-- the resulting Value to a JSON string, and compares against the literal expected text.
-- Unlike roundtripTest (which compares decoded terms), this catches encoding asymmetries
-- such as a small integer being quoted as a string.
wireShapeTest :: String -> TypedTerm Type -> TypedTerm Term -> String -> TypedTerm TestCaseWithMetadata
wireShapeTest testName typ term expectedJson = universalCase testName
  (Eithers.either
    (Phantoms.lambda "e" $ Phantoms.var "e")
    (Phantoms.lambda "json" $ JsonWriter.printJson @@ Phantoms.var "json")
    (EncodeModule.toJson @@ Maps.empty @@ Core.name (Phantoms.string "test") @@ typ @@ term))
  (Phantoms.string expectedJson)

-- Like roundtripTest, but with a caller-supplied type lookup table instead of an empty
-- one. Needed to exercise Type.Variable / Type.Application / Type.Forall, which resolve
-- named types via the table.
roundtripTestWithTypes :: String -> TypedTerm (M.Map Name Type) -> TypedTerm Type -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
roundtripTestWithTypes testName types typ term = universalCase testName
  (Eithers.either
    (Phantoms.lambda "e" $ Phantoms.var "e")
    (Phantoms.lambda "json" $
      Eithers.either
        (Phantoms.lambda "e" $ Phantoms.var "e")
        (Phantoms.lambda "decoded" $ ShowCore.term @@ Phantoms.var "decoded")
        (JsonDecode.fromJson @@ types @@ Core.name (Phantoms.string "test") @@ typ @@ Phantoms.var "json"))
    (EncodeModule.toJson @@ types @@ Core.name (Phantoms.string "test") @@ typ @@ term))
  (ShowCore.term @@ term)

----------------------------------------
-- Literal types
----------------------------------------

literalRoundtripGroup :: TypedTerm TestGroup
literalRoundtripGroup = subgroup "literal types" [
    -- Booleans
    roundtripTest "boolean true" T.boolean (boolean True),
    roundtripTest "boolean false" T.boolean (boolean False),

    -- Integers (native JSON numbers - fit within JSON's 2^53-1 safe-integer range)
    roundtripTest "int8 positive" T.int8 (int8 42),
    roundtripTest "int8 negative" T.int8 (int8 (-17)),
    roundtripTest "int16" T.int16 (int16 1000),
    roundtripTest "int32" T.int32 (int32 100000),
    roundtripTest "uint8" T.uint8 (uint8 200),
    roundtripTest "uint16" T.uint16 (uint16 50000),
    roundtripTest "uint32" T.uint32 (uint32 4000000000),

    -- Large integers (may exceed 2^53-1; encoded as JSON strings to preserve precision)
    roundtripTest "int64" T.int64 (int64 1000000000000),

    -- Floats
    roundtripTest "float32" T.float32 (float32 1.5),
    roundtripTest "float64" T.float64 (float64 3.14159),

    -- Special floats (NaN, Infinity, -Infinity) encoded as JSON string sentinels.
    roundtripTest "float32 NaN" T.float32 (float32 (0/0)),
    roundtripTest "float32 positive infinity" T.float32 (float32 (1/0)),
    roundtripTest "float32 negative infinity" T.float32 (float32 (-1/0)),
    roundtripTest "float64 NaN" T.float64 (float64 (0/0)),
    roundtripTest "float64 positive infinity" T.float64 (float64 (1/0)),
    roundtripTest "float64 negative infinity" T.float64 (float64 (-1/0)),

    -- Strings
    roundtripTest "string simple" T.string (string "hello"),
    roundtripTest "string empty" T.string (string ""),
    roundtripTest "string with spaces" T.string (string "hello world")]

----------------------------------------
-- Decimal precision
----------------------------------------

-- | Decimal round-trips must preserve arbitrary precision. Covers values that Double
-- could express exactly (everyday decimals, modest exponents) — hosts with native
-- arbitrary-precision decimals (BigDecimal in Java/Scala/Clojure, Decimal in Python) also
-- preserve large-integer precision, but dialects like Scheme/Common Lisp/Emacs Lisp emit
-- decimal literals as Double and lose it before the round trip begins, so we don't test
-- values outside Double's exact range at this level.
decimalRoundtripGroup :: TypedTerm TestGroup
decimalRoundtripGroup = subgroup "decimal precision" [
    roundtripTest "decimal zero" T.decimal (decimal 0),
    roundtripTest "decimal whole" T.decimal (decimal 42),
    roundtripTest "decimal negative whole" T.decimal (decimal (-17)),
    roundtripTest "decimal fraction" T.decimal (decimal 3.14),
    roundtripTest "decimal negative fraction" T.decimal (decimal (-2.5)),
    -- Tiny and huge exponents (single-coefficient, representable as Double)
    roundtripTest "decimal tiny exponent"
      T.decimal
      (decimal (Sci.scientific 1 (-20))),
    roundtripTest "decimal huge exponent"
      T.decimal
      (decimal (Sci.scientific 1 20))]

----------------------------------------
-- Collection types
----------------------------------------

collectionRoundtripGroup :: TypedTerm TestGroup
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

optionalRoundtripGroup :: TypedTerm TestGroup
optionalRoundtripGroup = subgroup "optional types" [
    -- Simple Maybe (idiomatic encoding: null for Nothing, plain value for Just)
    roundtripTest "optional string with value"
      (T.optional T.string)
      (optional $ just $ string "hello"),
    roundtripTest "optional string nothing"
      (T.optional T.string)
      (optional nothing),
    roundtripTest "optional int with value"
      (T.optional T.int32)
      (optional $ just $ int32 42),

    -- Nested Maybe (array-wrapped encoding for round-trip fidelity)
    roundtripTest "nested optional: nothing"
      (T.optional $ T.optional T.string)
      (optional nothing),
    roundtripTest "nested optional: just nothing"
      (T.optional $ T.optional T.string)
      (optional $ just $ optional nothing),
    roundtripTest "nested optional: just just value"
      (T.optional $ T.optional T.string)
      (optional $ just $ optional $ just $ string "hello")]

----------------------------------------
-- Record types with optional fields
----------------------------------------

recordRoundtripGroup :: TypedTerm TestGroup
recordRoundtripGroup = subgroup "record types" [
    -- Record with all required fields
    roundtripTest "record with required fields"
      (T.record (name "test") ["name">: T.string, "age">: T.int32])
      (record (name "test") ["name">: string "Alice", "age">: int32 30]),

    -- Record with optional field present
    roundtripTest "record with optional field present"
      (T.record (name "test") ["name">: T.string, "email">: T.optional T.string])
      (record (name "test") ["name">: string "Alice", "email">: optional (just $ string "alice@example.com")]),

    -- Record with optional field absent (Nothing -> field omitted in JSON)
    roundtripTest "record with optional field absent"
      (T.record (name "test") ["name">: T.string, "email">: T.optional T.string])
      (record (name "test") ["name">: string "Alice", "email">: optional nothing]),

    -- Record with multiple optional fields, some present some absent
    roundtripTest "record with mixed optional fields"
      (T.record (name "test") [
        "name">: T.string,
        "email">: T.optional T.string,
        "age">: T.optional T.int32])
      (record (name "test") [
        "name">: string "Bob",
        "email">: optional nothing,
        "age">: optional (just $ int32 25)]),

    -- Record with nested Maybe field (uses array-wrapped encoding, field not omitted)
    roundtripTest "record with nested optional field"
      (T.record (name "test") [
        "name">: T.string,
        "value">: T.optional (T.optional T.int32)])
      (record (name "test") [
        "name">: string "test",
        "value">: optional (just $ optional (just $ int32 42))])]

----------------------------------------
-- Parametric types (Type.Application / Type.Forall) -- regression for #531
----------------------------------------

-- | A polymorphic record type: forall a. { target: a, note: string }
relationshipTypeName :: TypedTerm Name
relationshipTypeName = name "Relationship"

relationshipType :: TypedTerm Type
relationshipType = T.forAll "a" $ T.record (name "Relationship") [
  "target">: T.variable "a",
  "note">: T.string]

relationshipTypes :: TypedTerm (M.Map Name Type)
relationshipTypes = Maps.fromList $ Phantoms.list [
  Phantoms.pair relationshipTypeName relationshipType]

parametricTypeRoundtripGroup :: TypedTerm TestGroup
parametricTypeRoundtripGroup = subgroup "parametric types" [
    -- A field typed as a direct instantiation of a polymorphic type: (Relationship @ string).
    -- Note: like every other roundtripTest[WithTypes] case, the decoded record is tagged with
    -- the harness's fixed top-level type name ("test"), not the resolved type's own name.
    roundtripTestWithTypes "type application of a polymorphic record"
      relationshipTypes
      (T.apply (T.variable "Relationship") T.string)
      (record (name "test") [
        "target">: string "example.AtomId",
        "note">: string "authored by"]),

    -- The instantiation nested inside a list, matching the issue's repro shape
    -- (a list field whose element type is a type application)
    roundtripTestWithTypes "list of a type application"
      relationshipTypes
      (T.list $ T.apply (T.variable "Relationship") T.string)
      (list [
        record (name "test") [
          "target">: string "example.AtomId1",
          "note">: string "authored by"],
        record (name "test") [
          "target">: string "example.AtomId2",
          "note">: string "cites"]])]

----------------------------------------
-- Union types (compact string form for unit-valued variants)
----------------------------------------

-- | Decode a pre-built JSON Value to a Term and show the result.
-- Used for backward-compat tests: verify the old {"variant": {}} form still decodes.
decodeTest :: String -> TypedTerm Type -> TypedTerm Value -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
decodeTest testName typ jsonVal expectedTerm = universalCase testName
  (Eithers.either
    (Phantoms.lambda "e" $ Phantoms.var "e")
    (Phantoms.lambda "decoded" $ ShowCore.term @@ Phantoms.var "decoded")
    (JsonDecode.fromJson @@ Maps.empty @@ Core.name (Phantoms.string "test") @@ typ @@ jsonVal))
  (ShowCore.term @@ expectedTerm)

unionType :: TypedTerm Type
unionType = T.union (name "test") [
    "bool">: T.boolean,
    "string">: T.string,
    "unit">: T.unit]

unionRoundtripGroup :: TypedTerm TestGroup
unionRoundtripGroup = subgroup "union types" [
    -- Non-unit variant: uses single-key object encoding (unchanged behaviour)
    roundtripTest "union non-unit variant (string)"
      unionType
      (inject (name "test") "string" (string "hello")),

    roundtripTest "union non-unit variant (bool)"
      unionType
      (inject (name "test") "bool" (boolean True)),

    -- Unit-valued variant: compact string encoding ("unit" instead of {"unit": {}})
    roundtripTest "union unit variant round-trip"
      unionType
      (inject (name "test") "unit" unit),

    wireShapeTest "union unit variant wire shape"
      unionType
      (inject (name "test") "unit" unit)
      "\"unit\"",

    wireShapeTest "union non-unit variant wire shape"
      unionType
      (inject (name "test") "string" (string "hello"))
      "{\"string\": \"hello\"}",

    -- Backward compatibility: old {"unit": {}} form must still decode correctly
    decodeTest "union unit variant legacy object form"
      unionType
      (Typed.TypedTerm (EncodeJsonModel.value (Model.ValueObject [("unit", Model.ValueObject [])])))
      (inject (name "test") "unit" unit)]

----------------------------------------
-- Wire shape (encoder output)
----------------------------------------

-- | Pins the exact JSON the encoder emits for each integer precision class.
-- Small integers (including uint32, whose maximum 2^32-1 is well below JS's 2^53-1
-- safe-integer boundary) are emitted as JSON numbers; large integers that may exceed
-- 2^53-1 (int64, uint64, bigint) are emitted as quoted strings to preserve precision.
-- Numeric literals are chosen to avoid the writer's shorter-form scientific notation
-- (e.g. 4000000000 would serialize as "4.0e9"), so the unquoted-number shape is explicit.
wireShapeGroup :: TypedTerm TestGroup
wireShapeGroup = subgroup "wire shape" [
    -- Small integers: JSON numbers (unquoted)
    wireShapeTest "int8" T.int8 (int8 42) "42",
    wireShapeTest "int16" T.int16 (int16 1000) "1000",
    wireShapeTest "int32" T.int32 (int32 100003) "100003",
    wireShapeTest "uint8" T.uint8 (uint8 200) "200",
    wireShapeTest "uint16" T.uint16 (uint16 50003) "50003",
    wireShapeTest "uint32" T.uint32 (uint32 4000000001) "4000000001",

    -- Large integers: quoted strings
    wireShapeTest "int64" T.int64 (int64 1000000000007) "\"1000000000007\"",
    wireShapeTest "uint64" T.uint64 (uint64 1000000000007) "\"1000000000007\"",
    wireShapeTest "bigint" T.bigint (bigint 1000000000007) "\"1000000000007\""]
