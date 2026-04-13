module Hydra.Avro.CoderSpec where

import Hydra.Kernel
import qualified Hydra.Core as Core
import qualified Hydra.Avro.Schema as Avro
import qualified Hydra.Json.Model as Json
import qualified Hydra.Avro.Testing as T
import qualified Hydra.Avro.SchemaJson as SchemaJson
import qualified Hydra.Avro.Encoder as Encoder
import qualified Hydra.Avro.Environment as AvroEnv
import qualified Hydra.Util as Util
import Hydra.Avro.TestRunner
import qualified Hydra.Avro.Coder as AvroCoder
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Hydra.Json.Parser as JsonParser
import Hydra.Parsing (ParseResult(..), ParseSuccess(..), ParseError(..))

import qualified Test.Hspec as H
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map as M


spec :: H.Spec
spec = do
  runAvroTestCases "Type-level forward (Avro -> Hydra)" typeLevelForwardCases
  runAvroTestCases "Type-level reverse (Hydra -> Avro)" typeLevelReverseCases
  runAvroTestCases "Term-level forward (JSON -> Term)" termLevelForwardCases
  runAvroTestCases "Term-level reverse (Term -> JSON)" termLevelReverseCases
  runAvroTestCases "Term-level round-trip (JSON -> Term -> JSON)" termLevelRoundTripJsonCases
  runAvroTestCases "Term-level round-trip (Term -> JSON -> Term)" termLevelRoundTripTermCases
  runAvroTestCases "Type-level round-trip (Avro -> Hydra -> Avro)" typeLevelRoundTripAvroCases
  runAvroTestCases "Type-level round-trip (Hydra -> Avro -> Hydra)" typeLevelRoundTripHydraCases
  runAvroTestCases "Union encoding" unionCases
  runAvroTestCases "Name mapping" nameMappingCases
  runAvroTestCases "Lossiness and annotations" lossinessCases
  runAvroTestCases "Schema serialization" schemaSerializationCases
  schemaCodecRoundTripSpec
  avscFileSpec
  schemaStringCoderSpec
  endToEndSpec
  binaryTermSpec
  lossyIntegerFloatSpec
  wrapTypeSpec
  errorCaseSpec
  complexRealisticSpec
  kernelTypeSpec
  edgeCaseSpec


-- Avro schema helpers

avroPrim :: Avro.Primitive -> Avro.Schema
avroPrim = Avro.SchemaPrimitive

avroArray :: Avro.Schema -> Avro.Schema
avroArray s = Avro.SchemaArray (Avro.Array s)

avroMap :: Avro.Schema -> Avro.Schema
avroMap s = Avro.SchemaMap (Avro.Map s)

avroUnion :: [Avro.Schema] -> Avro.Schema
avroUnion ss = Avro.SchemaUnion (Avro.Union ss)

avroRecord :: String -> [Avro.Field] -> Avro.Schema
avroRecord name fields = Avro.SchemaNamed $ Avro.Named {
  Avro.namedName = name,
  Avro.namedNamespace = Nothing,
  Avro.namedAliases = Nothing,
  Avro.namedDoc = Nothing,
  Avro.namedType = Avro.NamedTypeRecord (Avro.Record fields),
  Avro.namedAnnotations = M.empty}

avroEnum :: String -> [String] -> Avro.Schema
avroEnum name symbols = Avro.SchemaNamed $ Avro.Named {
  Avro.namedName = name,
  Avro.namedNamespace = Nothing,
  Avro.namedAliases = Nothing,
  Avro.namedDoc = Nothing,
  Avro.namedType = Avro.NamedTypeEnum (Avro.Enum symbols Nothing),
  Avro.namedAnnotations = M.empty}

optionalField :: String -> Avro.Schema -> Avro.Field
optionalField name schema = Avro.Field {
  Avro.fieldName = name,
  Avro.fieldDoc = Nothing,
  Avro.fieldType = avroUnion [avroPrim Avro.PrimitiveNull, schema],
  Avro.fieldDefault = Just Json.ValueNull,
  Avro.fieldOrder = Nothing,
  Avro.fieldAliases = Nothing,
  Avro.fieldAnnotations = M.empty}

simpleField :: String -> Avro.Schema -> Avro.Field
simpleField name schema = Avro.Field {
  Avro.fieldName = name,
  Avro.fieldDoc = Nothing,
  Avro.fieldType = schema,
  Avro.fieldDefault = Nothing,
  Avro.fieldOrder = Nothing,
  Avro.fieldAliases = Nothing,
  Avro.fieldAnnotations = M.empty}


-- Hydra type helpers

hydraRecordType :: [(String, Core.Type)] -> Core.Type
hydraRecordType fields = Core.TypeRecord $
  [Core.FieldType (Core.Name n) t | (n, t) <- fields]

hydraUnionType :: [(String, Core.Type)] -> Core.Type
hydraUnionType fields = Core.TypeUnion $
  [Core.FieldType (Core.Name n) t | (n, t) <- fields]


-- Hydra term helpers

hydraRecord :: String -> [(String, Core.Term)] -> Core.Term
hydraRecord typeName fields = Core.TermRecord $ Core.Record {
  Core.recordTypeName = Core.Name typeName,
  Core.recordFields = [Core.Field (Core.Name n) t | (n, t) <- fields]}

hydraUnionTerm :: String -> String -> Core.Term -> Core.Term
hydraUnionTerm typeName fieldName value = Core.TermUnion $ Core.Injection {
  Core.injectionTypeName = Core.Name typeName,
  Core.injectionField = Core.Field (Core.Name fieldName) value}


-- ============================================================
-- Category 1: Type-level forward test cases
-- ============================================================

typeLevelForwardCases :: [T.AvroTestCase]
typeLevelForwardCases = map T.AvroTestCaseTypeLevelForward [
  -- Primitives
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "null -> unit",
    T.typeLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveNull,
    T.typeLevelForwardTestCaseType = Types.unit},
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "boolean -> boolean",
    T.typeLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveBoolean,
    T.typeLevelForwardTestCaseType = Types.boolean},
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "int -> int32",
    T.typeLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveInt,
    T.typeLevelForwardTestCaseType = Types.int32},
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "long -> int64",
    T.typeLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveLong,
    T.typeLevelForwardTestCaseType = Types.int64},
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "float -> float32",
    T.typeLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveFloat,
    T.typeLevelForwardTestCaseType = Types.float32},
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "double -> float64",
    T.typeLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveDouble,
    T.typeLevelForwardTestCaseType = Types.float64},
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "bytes -> binary",
    T.typeLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveBytes,
    T.typeLevelForwardTestCaseType = Types.binary},
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "string -> string",
    T.typeLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveString,
    T.typeLevelForwardTestCaseType = Types.string},

  -- Containers
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "array(string) -> list(string)",
    T.typeLevelForwardTestCaseSchema = avroArray (avroPrim Avro.PrimitiveString),
    T.typeLevelForwardTestCaseType = Types.list Types.string},
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "map(int) -> map(string, int32)",
    T.typeLevelForwardTestCaseSchema = avroMap (avroPrim Avro.PrimitiveInt),
    T.typeLevelForwardTestCaseType = Types.map Types.string Types.int32},

  -- Optional (union with null)
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "union[null, string] -> optional(string)",
    T.typeLevelForwardTestCaseSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveString],
    T.typeLevelForwardTestCaseType = Types.optional Types.string},

  -- Singleton union (unwrap)
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "union[int] -> int32",
    T.typeLevelForwardTestCaseSchema = avroUnion [avroPrim Avro.PrimitiveInt],
    T.typeLevelForwardTestCaseType = Types.int32},

  -- Enum
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "enum -> union of units",
    T.typeLevelForwardTestCaseSchema = avroEnum "Color" ["red", "green", "blue"],
    T.typeLevelForwardTestCaseType = hydraUnionType [("red", Types.unit), ("green", Types.unit), ("blue", Types.unit)]},

  -- Record
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "record -> record",
    T.typeLevelForwardTestCaseSchema = avroRecord "Point" [simpleField "x" (avroPrim Avro.PrimitiveInt), simpleField "y" (avroPrim Avro.PrimitiveInt)],
    T.typeLevelForwardTestCaseType = hydraRecordType [("x", Types.int32), ("y", Types.int32)]},

  -- Nested containers
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "array(array(int)) -> list(list(int32))",
    T.typeLevelForwardTestCaseSchema = avroArray (avroArray (avroPrim Avro.PrimitiveInt)),
    T.typeLevelForwardTestCaseType = Types.list (Types.list Types.int32)},
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "map(array(string)) -> map(string, list(string))",
    T.typeLevelForwardTestCaseSchema = avroMap (avroArray (avroPrim Avro.PrimitiveString)),
    T.typeLevelForwardTestCaseType = Types.map Types.string (Types.list Types.string)},

  -- Record with optional field (fields are ordered by adapter's map iteration, i.e. alphabetically)
  T.TypeLevelForwardTestCase {
    T.typeLevelForwardTestCaseDescription = "record with optional field",
    T.typeLevelForwardTestCaseSchema = avroRecord "Person" [
      simpleField "name" (avroPrim Avro.PrimitiveString),
      simpleField "age" (avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveInt])],
    T.typeLevelForwardTestCaseType = hydraRecordType [("age", Types.optional Types.int32), ("name", Types.string)]}
  ]


-- ============================================================
-- Category 2: Type-level reverse test cases (pending)
-- ============================================================

typeLevelReverseCases :: [T.AvroTestCase]
typeLevelReverseCases = map T.AvroTestCaseTypeLevelReverse [
  -- Primitives
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "unit -> null",
    T.typeLevelReverseTestCaseType = Types.unit,
    T.typeLevelReverseTestCaseSchema = avroPrim Avro.PrimitiveNull},
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "boolean -> boolean",
    T.typeLevelReverseTestCaseType = Types.boolean,
    T.typeLevelReverseTestCaseSchema = avroPrim Avro.PrimitiveBoolean},
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "int32 -> int",
    T.typeLevelReverseTestCaseType = Types.int32,
    T.typeLevelReverseTestCaseSchema = avroPrim Avro.PrimitiveInt},
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "int64 -> long",
    T.typeLevelReverseTestCaseType = Types.int64,
    T.typeLevelReverseTestCaseSchema = avroPrim Avro.PrimitiveLong},
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "float32 -> float",
    T.typeLevelReverseTestCaseType = Types.float32,
    T.typeLevelReverseTestCaseSchema = avroPrim Avro.PrimitiveFloat},
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "float64 -> double",
    T.typeLevelReverseTestCaseType = Types.float64,
    T.typeLevelReverseTestCaseSchema = avroPrim Avro.PrimitiveDouble},
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "binary -> bytes",
    T.typeLevelReverseTestCaseType = Types.binary,
    T.typeLevelReverseTestCaseSchema = avroPrim Avro.PrimitiveBytes},
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "string -> string",
    T.typeLevelReverseTestCaseType = Types.string,
    T.typeLevelReverseTestCaseSchema = avroPrim Avro.PrimitiveString},

  -- Containers
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "list(string) -> array(string)",
    T.typeLevelReverseTestCaseType = Types.list Types.string,
    T.typeLevelReverseTestCaseSchema = avroArray (avroPrim Avro.PrimitiveString)},
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "map(string, int32) -> map(int)",
    T.typeLevelReverseTestCaseType = Types.map Types.string Types.int32,
    T.typeLevelReverseTestCaseSchema = avroMap (avroPrim Avro.PrimitiveInt)},

  -- Optional
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "optional(string) -> union[null, string]",
    T.typeLevelReverseTestCaseType = Types.optional Types.string,
    T.typeLevelReverseTestCaseSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveString]},

  -- All-unit union -> enum (name is synthetic since Hydra types don't carry names)
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "all-unit union -> enum",
    T.typeLevelReverseTestCaseType = hydraUnionType [("red", Types.unit), ("green", Types.unit), ("blue", Types.unit)],
    T.typeLevelReverseTestCaseSchema = avroEnum "Union" ["red", "green", "blue"]},

  -- General union -> record with optional fields (with null defaults)
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "general union -> record with optional fields",
    T.typeLevelReverseTestCaseType = hydraUnionType [("a", Types.int32), ("b", Types.string)],
    T.typeLevelReverseTestCaseSchema = avroRecord "Union" [
      optionalField "a" (avroPrim Avro.PrimitiveInt),
      optionalField "b" (avroPrim Avro.PrimitiveString)]},

  -- Record (synthetic name since Hydra types don't carry names)
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "record -> Avro record (synthetic name)",
    T.typeLevelReverseTestCaseType = hydraRecordType [("x", Types.int32), ("y", Types.int32)],
    T.typeLevelReverseTestCaseSchema = avroRecord "Record" [simpleField "x" (avroPrim Avro.PrimitiveInt), simpleField "y" (avroPrim Avro.PrimitiveInt)]},

  -- Nested containers
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "list(list(int32)) -> array(array(int))",
    T.typeLevelReverseTestCaseType = Types.list (Types.list Types.int32),
    T.typeLevelReverseTestCaseSchema = avroArray (avroArray (avroPrim Avro.PrimitiveInt))},
  T.TypeLevelReverseTestCase {
    T.typeLevelReverseTestCaseDescription = "map(string, list(string)) -> map(array(string))",
    T.typeLevelReverseTestCaseType = Types.map Types.string (Types.list Types.string),
    T.typeLevelReverseTestCaseSchema = avroMap (avroArray (avroPrim Avro.PrimitiveString))}
  ]


-- ============================================================
-- Category 5: Term-level forward test cases
-- ============================================================

termLevelForwardCases :: [T.AvroTestCase]
termLevelForwardCases = map T.AvroTestCaseTermLevelForward [
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON boolean -> Hydra boolean term",
    T.termLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveBoolean,
    T.termLevelForwardTestCaseJson = Json.ValueBoolean True,
    T.termLevelForwardTestCaseTerm = Terms.boolean True},
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON number -> Hydra int32 term",
    T.termLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveInt,
    T.termLevelForwardTestCaseJson = Json.ValueNumber 42.0,
    T.termLevelForwardTestCaseTerm = Terms.int32 42},
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON number -> Hydra int64 term",
    T.termLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveLong,
    T.termLevelForwardTestCaseJson = Json.ValueNumber 1000000.0,
    T.termLevelForwardTestCaseTerm = Terms.int64 1000000},
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON number -> Hydra float32 term",
    T.termLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveFloat,
    T.termLevelForwardTestCaseJson = Json.ValueNumber 3.14,
    T.termLevelForwardTestCaseTerm = Terms.float32 (realToFrac (3.14 :: Double))},
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON number -> Hydra float64 term",
    T.termLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveDouble,
    T.termLevelForwardTestCaseJson = Json.ValueNumber 3.14,
    T.termLevelForwardTestCaseTerm = Terms.float64 3.14},
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON string -> Hydra string term",
    T.termLevelForwardTestCaseSchema = avroPrim Avro.PrimitiveString,
    T.termLevelForwardTestCaseJson = Json.ValueString "hello",
    T.termLevelForwardTestCaseTerm = Terms.string "hello"},
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON array -> Hydra list term",
    T.termLevelForwardTestCaseSchema = avroArray (avroPrim Avro.PrimitiveInt),
    T.termLevelForwardTestCaseJson = Json.ValueArray [Json.ValueNumber 1.0, Json.ValueNumber 2.0, Json.ValueNumber 3.0],
    T.termLevelForwardTestCaseTerm = Terms.list [Terms.int32 1, Terms.int32 2, Terms.int32 3]},
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON null -> Hydra optional nothing",
    T.termLevelForwardTestCaseSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveString],
    T.termLevelForwardTestCaseJson = Json.ValueNull,
    T.termLevelForwardTestCaseTerm = Core.TermMaybe Nothing},
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON string -> Hydra optional just",
    T.termLevelForwardTestCaseSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveString],
    T.termLevelForwardTestCaseJson = Json.ValueString "foo",
    T.termLevelForwardTestCaseTerm = Core.TermMaybe (Just (Terms.string "foo"))},
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON string -> Hydra enum term",
    T.termLevelForwardTestCaseSchema = avroEnum "Color" ["red", "green", "blue"],
    T.termLevelForwardTestCaseJson = Json.ValueString "red",
    T.termLevelForwardTestCaseTerm = hydraUnionTerm "Color" "red" Terms.unit},
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON object -> Hydra record term",
    T.termLevelForwardTestCaseSchema = avroRecord "Point" [simpleField "x" (avroPrim Avro.PrimitiveInt), simpleField "y" (avroPrim Avro.PrimitiveInt)],
    T.termLevelForwardTestCaseJson = Json.ValueObject (M.fromList [("x", Json.ValueNumber 1.0), ("y", Json.ValueNumber 2.0)]),
    T.termLevelForwardTestCaseTerm = hydraRecord "Point" [("x", Terms.int32 1), ("y", Terms.int32 2)]},

  -- Nested: array of arrays
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON nested array -> Hydra nested list",
    T.termLevelForwardTestCaseSchema = avroArray (avroArray (avroPrim Avro.PrimitiveInt)),
    T.termLevelForwardTestCaseJson = Json.ValueArray [
      Json.ValueArray [Json.ValueNumber 1.0, Json.ValueNumber 2.0],
      Json.ValueArray [Json.ValueNumber 3.0]],
    T.termLevelForwardTestCaseTerm = Terms.list [
      Terms.list [Terms.int32 1, Terms.int32 2],
      Terms.list [Terms.int32 3]]},

  -- Map
  T.TermLevelForwardTestCase {
    T.termLevelForwardTestCaseDescription = "JSON object -> Hydra map term",
    T.termLevelForwardTestCaseSchema = avroMap (avroPrim Avro.PrimitiveInt),
    T.termLevelForwardTestCaseJson = Json.ValueObject (M.fromList [("a", Json.ValueNumber 1.0), ("b", Json.ValueNumber 2.0)]),
    T.termLevelForwardTestCaseTerm = Core.TermMap (M.fromList [
      (Terms.string "a", Terms.int32 1),
      (Terms.string "b", Terms.int32 2)])}
  ]


-- ============================================================
-- Category 6: Term-level reverse test cases
-- ============================================================

termLevelReverseCases :: [T.AvroTestCase]
termLevelReverseCases = map T.AvroTestCaseTermLevelReverse [
  T.TermLevelReverseTestCase {
    T.termLevelReverseTestCaseDescription = "Hydra boolean -> JSON boolean",
    T.termLevelReverseTestCaseSchema = avroPrim Avro.PrimitiveBoolean,
    T.termLevelReverseTestCaseTerm = Terms.boolean True,
    T.termLevelReverseTestCaseJson = Json.ValueBoolean True},
  T.TermLevelReverseTestCase {
    T.termLevelReverseTestCaseDescription = "Hydra int32 -> JSON number",
    T.termLevelReverseTestCaseSchema = avroPrim Avro.PrimitiveInt,
    T.termLevelReverseTestCaseTerm = Terms.int32 42,
    T.termLevelReverseTestCaseJson = Json.ValueNumber 42.0},
  T.TermLevelReverseTestCase {
    T.termLevelReverseTestCaseDescription = "Hydra string -> JSON string",
    T.termLevelReverseTestCaseSchema = avroPrim Avro.PrimitiveString,
    T.termLevelReverseTestCaseTerm = Terms.string "hello",
    T.termLevelReverseTestCaseJson = Json.ValueString "hello"},
  T.TermLevelReverseTestCase {
    T.termLevelReverseTestCaseDescription = "Hydra optional nothing -> JSON null",
    T.termLevelReverseTestCaseSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveString],
    T.termLevelReverseTestCaseTerm = Core.TermMaybe Nothing,
    T.termLevelReverseTestCaseJson = Json.ValueNull},
  T.TermLevelReverseTestCase {
    T.termLevelReverseTestCaseDescription = "Hydra optional just -> JSON string",
    T.termLevelReverseTestCaseSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveString],
    T.termLevelReverseTestCaseTerm = Core.TermMaybe (Just (Terms.string "foo")),
    T.termLevelReverseTestCaseJson = Json.ValueString "foo"},
  T.TermLevelReverseTestCase {
    T.termLevelReverseTestCaseDescription = "Hydra enum term -> JSON string",
    T.termLevelReverseTestCaseSchema = avroEnum "Color" ["red", "green", "blue"],
    T.termLevelReverseTestCaseTerm = hydraUnionTerm "Color" "red" Terms.unit,
    T.termLevelReverseTestCaseJson = Json.ValueString "red"},
  T.TermLevelReverseTestCase {
    T.termLevelReverseTestCaseDescription = "Hydra record -> JSON object",
    T.termLevelReverseTestCaseSchema = avroRecord "Point" [simpleField "x" (avroPrim Avro.PrimitiveInt), simpleField "y" (avroPrim Avro.PrimitiveInt)],
    T.termLevelReverseTestCaseTerm = hydraRecord "Point" [("x", Terms.int32 1), ("y", Terms.int32 2)],
    T.termLevelReverseTestCaseJson = Json.ValueObject (M.fromList [("x", Json.ValueNumber 1.0), ("y", Json.ValueNumber 2.0)])},

  -- Map reverse
  T.TermLevelReverseTestCase {
    T.termLevelReverseTestCaseDescription = "Hydra map -> JSON object",
    T.termLevelReverseTestCaseSchema = avroMap (avroPrim Avro.PrimitiveInt),
    T.termLevelReverseTestCaseTerm = Core.TermMap (M.fromList [
      (Terms.string "a", Terms.int32 1),
      (Terms.string "b", Terms.int32 2)]),
    T.termLevelReverseTestCaseJson = Json.ValueObject (M.fromList [("a", Json.ValueNumber 1.0), ("b", Json.ValueNumber 2.0)])},

  -- List reverse
  T.TermLevelReverseTestCase {
    T.termLevelReverseTestCaseDescription = "Hydra list -> JSON array",
    T.termLevelReverseTestCaseSchema = avroArray (avroPrim Avro.PrimitiveString),
    T.termLevelReverseTestCaseTerm = Terms.list [Terms.string "x", Terms.string "y"],
    T.termLevelReverseTestCaseJson = Json.ValueArray [Json.ValueString "x", Json.ValueString "y"]}
  ]


-- ============================================================
-- Category 7: Term-level round-trip test cases (JSON -> Term -> JSON)
-- ============================================================

termLevelRoundTripJsonCases :: [T.AvroTestCase]
termLevelRoundTripJsonCases = map T.AvroTestCaseTermLevelRoundTripJson [
  T.TermLevelRoundTripJsonTestCase {
    T.termLevelRoundTripJsonTestCaseDescription = "boolean round-trip",
    T.termLevelRoundTripJsonTestCaseSchema = avroPrim Avro.PrimitiveBoolean,
    T.termLevelRoundTripJsonTestCaseJson = Json.ValueBoolean False,
    T.termLevelRoundTripJsonTestCaseExpectedJson = Json.ValueBoolean False},
  T.TermLevelRoundTripJsonTestCase {
    T.termLevelRoundTripJsonTestCaseDescription = "int round-trip",
    T.termLevelRoundTripJsonTestCaseSchema = avroPrim Avro.PrimitiveInt,
    T.termLevelRoundTripJsonTestCaseJson = Json.ValueNumber 99.0,
    T.termLevelRoundTripJsonTestCaseExpectedJson = Json.ValueNumber 99.0},
  T.TermLevelRoundTripJsonTestCase {
    T.termLevelRoundTripJsonTestCaseDescription = "string round-trip",
    T.termLevelRoundTripJsonTestCaseSchema = avroPrim Avro.PrimitiveString,
    T.termLevelRoundTripJsonTestCaseJson = Json.ValueString "test",
    T.termLevelRoundTripJsonTestCaseExpectedJson = Json.ValueString "test"},
  T.TermLevelRoundTripJsonTestCase {
    T.termLevelRoundTripJsonTestCaseDescription = "array round-trip",
    T.termLevelRoundTripJsonTestCaseSchema = avroArray (avroPrim Avro.PrimitiveString),
    T.termLevelRoundTripJsonTestCaseJson = Json.ValueArray [Json.ValueString "a", Json.ValueString "b"],
    T.termLevelRoundTripJsonTestCaseExpectedJson = Json.ValueArray [Json.ValueString "a", Json.ValueString "b"]},
  T.TermLevelRoundTripJsonTestCase {
    T.termLevelRoundTripJsonTestCaseDescription = "optional null round-trip",
    T.termLevelRoundTripJsonTestCaseSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveInt],
    T.termLevelRoundTripJsonTestCaseJson = Json.ValueNull,
    T.termLevelRoundTripJsonTestCaseExpectedJson = Json.ValueNull},
  T.TermLevelRoundTripJsonTestCase {
    T.termLevelRoundTripJsonTestCaseDescription = "optional value round-trip",
    T.termLevelRoundTripJsonTestCaseSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveInt],
    T.termLevelRoundTripJsonTestCaseJson = Json.ValueNumber 7.0,
    T.termLevelRoundTripJsonTestCaseExpectedJson = Json.ValueNumber 7.0},
  T.TermLevelRoundTripJsonTestCase {
    T.termLevelRoundTripJsonTestCaseDescription = "enum round-trip",
    T.termLevelRoundTripJsonTestCaseSchema = avroEnum "Color" ["red", "green", "blue"],
    T.termLevelRoundTripJsonTestCaseJson = Json.ValueString "green",
    T.termLevelRoundTripJsonTestCaseExpectedJson = Json.ValueString "green"},
  T.TermLevelRoundTripJsonTestCase {
    T.termLevelRoundTripJsonTestCaseDescription = "record round-trip",
    T.termLevelRoundTripJsonTestCaseSchema = avroRecord "Point" [simpleField "x" (avroPrim Avro.PrimitiveInt), simpleField "y" (avroPrim Avro.PrimitiveInt)],
    T.termLevelRoundTripJsonTestCaseJson = Json.ValueObject (M.fromList [("x", Json.ValueNumber 5.0), ("y", Json.ValueNumber 10.0)]),
    T.termLevelRoundTripJsonTestCaseExpectedJson = Json.ValueObject (M.fromList [("x", Json.ValueNumber 5.0), ("y", Json.ValueNumber 10.0)])},

  -- Map round-trip
  T.TermLevelRoundTripJsonTestCase {
    T.termLevelRoundTripJsonTestCaseDescription = "map round-trip",
    T.termLevelRoundTripJsonTestCaseSchema = avroMap (avroPrim Avro.PrimitiveString),
    T.termLevelRoundTripJsonTestCaseJson = Json.ValueObject (M.fromList [("k1", Json.ValueString "v1"), ("k2", Json.ValueString "v2")]),
    T.termLevelRoundTripJsonTestCaseExpectedJson = Json.ValueObject (M.fromList [("k1", Json.ValueString "v1"), ("k2", Json.ValueString "v2")])}
  ]


-- ============================================================
-- Category 10: Name mapping test cases
-- ============================================================

nameMappingCases :: [T.AvroTestCase]
nameMappingCases = map T.AvroTestCaseNameMapping [
  T.NameMappingTestCase {
    T.nameMappingTestCaseDescription = "simple name without namespace",
    T.nameMappingTestCaseHydraName = Core.Name "Point",
    T.nameMappingTestCaseAvroName = "Point",
    T.nameMappingTestCaseAvroNamespace = Nothing},
  T.NameMappingTestCase {
    T.nameMappingTestCaseDescription = "name with namespace",
    T.nameMappingTestCaseHydraName = Core.Name "com.example.Point",
    T.nameMappingTestCaseAvroName = "Point",
    T.nameMappingTestCaseAvroNamespace = Just "com.example"},
  T.NameMappingTestCase {
    T.nameMappingTestCaseDescription = "dotted name parsed as namespace.local",
    T.nameMappingTestCaseHydraName = Core.Name "org.apache.avro.Schema",
    T.nameMappingTestCaseAvroName = "org.apache.avro.Schema",
    T.nameMappingTestCaseAvroNamespace = Nothing}
  ]


-- ============================================================
-- Category 3: Type-level round-trip (Avro -> Hydra -> Avro)
-- ============================================================

typeLevelRoundTripAvroCases :: [T.AvroTestCase]
typeLevelRoundTripAvroCases = map T.AvroTestCaseTypeLevelRoundTripAvro [
  -- Primitives round-trip exactly
  T.TypeLevelRoundTripAvroTestCase {
    T.typeLevelRoundTripAvroTestCaseDescription = "int round-trips exactly",
    T.typeLevelRoundTripAvroTestCaseSchema = avroPrim Avro.PrimitiveInt,
    T.typeLevelRoundTripAvroTestCaseExpectedSchema = avroPrim Avro.PrimitiveInt},
  T.TypeLevelRoundTripAvroTestCase {
    T.typeLevelRoundTripAvroTestCaseDescription = "string round-trips exactly",
    T.typeLevelRoundTripAvroTestCaseSchema = avroPrim Avro.PrimitiveString,
    T.typeLevelRoundTripAvroTestCaseExpectedSchema = avroPrim Avro.PrimitiveString},

  -- Containers round-trip exactly
  T.TypeLevelRoundTripAvroTestCase {
    T.typeLevelRoundTripAvroTestCaseDescription = "array(string) round-trips exactly",
    T.typeLevelRoundTripAvroTestCaseSchema = avroArray (avroPrim Avro.PrimitiveString),
    T.typeLevelRoundTripAvroTestCaseExpectedSchema = avroArray (avroPrim Avro.PrimitiveString)},
  T.TypeLevelRoundTripAvroTestCase {
    T.typeLevelRoundTripAvroTestCaseDescription = "map(int) round-trips exactly",
    T.typeLevelRoundTripAvroTestCaseSchema = avroMap (avroPrim Avro.PrimitiveInt),
    T.typeLevelRoundTripAvroTestCaseExpectedSchema = avroMap (avroPrim Avro.PrimitiveInt)},

  -- Optional round-trips exactly
  T.TypeLevelRoundTripAvroTestCase {
    T.typeLevelRoundTripAvroTestCaseDescription = "union[null, string] round-trips exactly",
    T.typeLevelRoundTripAvroTestCaseSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveString],
    T.typeLevelRoundTripAvroTestCaseExpectedSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveString]},

  -- Enum round-trip: name is lost (Hydra types don't carry names)
  T.TypeLevelRoundTripAvroTestCase {
    T.typeLevelRoundTripAvroTestCaseDescription = "enum round-trips (name becomes synthetic)",
    T.typeLevelRoundTripAvroTestCaseSchema = avroEnum "Color" ["red", "green", "blue"],
    T.typeLevelRoundTripAvroTestCaseExpectedSchema = avroEnum "Union" ["red", "green", "blue"]},

  -- Record round-trip: name is lost
  T.TypeLevelRoundTripAvroTestCase {
    T.typeLevelRoundTripAvroTestCaseDescription = "record round-trips (name becomes synthetic)",
    T.typeLevelRoundTripAvroTestCaseSchema = avroRecord "Point" [simpleField "x" (avroPrim Avro.PrimitiveInt), simpleField "y" (avroPrim Avro.PrimitiveInt)],
    T.typeLevelRoundTripAvroTestCaseExpectedSchema = avroRecord "Record" [simpleField "x" (avroPrim Avro.PrimitiveInt), simpleField "y" (avroPrim Avro.PrimitiveInt)]},

  -- Fixed loses size info: fixed(16) -> binary -> bytes
  T.TypeLevelRoundTripAvroTestCase {
    T.typeLevelRoundTripAvroTestCaseDescription = "fixed normalizes to bytes (size info in annotation)",
    T.typeLevelRoundTripAvroTestCaseSchema = Avro.SchemaNamed $ Avro.Named {
      Avro.namedName = "Hash",
      Avro.namedNamespace = Nothing,
      Avro.namedAliases = Nothing,
      Avro.namedDoc = Nothing,
      Avro.namedType = Avro.NamedTypeFixed (Avro.Fixed 16),
      Avro.namedAnnotations = M.empty},
    T.typeLevelRoundTripAvroTestCaseExpectedSchema = avroPrim Avro.PrimitiveBytes}
  ]


-- ============================================================
-- Category 4: Type-level round-trip (Hydra -> Avro -> Hydra)
-- ============================================================

typeLevelRoundTripHydraCases :: [T.AvroTestCase]
typeLevelRoundTripHydraCases = map T.AvroTestCaseTypeLevelRoundTripHydra [
  -- Primitives round-trip exactly
  T.TypeLevelRoundTripHydraTestCase {
    T.typeLevelRoundTripHydraTestCaseDescription = "int32 round-trips exactly",
    T.typeLevelRoundTripHydraTestCaseType = Types.int32,
    T.typeLevelRoundTripHydraTestCaseExpectedType = Types.int32},
  T.TypeLevelRoundTripHydraTestCase {
    T.typeLevelRoundTripHydraTestCaseDescription = "string round-trips exactly",
    T.typeLevelRoundTripHydraTestCaseType = Types.string,
    T.typeLevelRoundTripHydraTestCaseExpectedType = Types.string},

  -- Containers round-trip exactly
  T.TypeLevelRoundTripHydraTestCase {
    T.typeLevelRoundTripHydraTestCaseDescription = "list(string) round-trips exactly",
    T.typeLevelRoundTripHydraTestCaseType = Types.list Types.string,
    T.typeLevelRoundTripHydraTestCaseExpectedType = Types.list Types.string},
  T.TypeLevelRoundTripHydraTestCase {
    T.typeLevelRoundTripHydraTestCaseDescription = "optional(int32) round-trips exactly",
    T.typeLevelRoundTripHydraTestCaseType = Types.optional Types.int32,
    T.typeLevelRoundTripHydraTestCaseExpectedType = Types.optional Types.int32},

  -- All-unit union round-trips exactly (via enum)
  T.TypeLevelRoundTripHydraTestCase {
    T.typeLevelRoundTripHydraTestCaseDescription = "all-unit union round-trips exactly via enum",
    T.typeLevelRoundTripHydraTestCaseType = hydraUnionType [("red", Types.unit), ("green", Types.unit), ("blue", Types.unit)],
    T.typeLevelRoundTripHydraTestCaseExpectedType = hydraUnionType [("red", Types.unit), ("green", Types.unit), ("blue", Types.unit)]},

  -- General union round-trips: union -> record-with-optionals -> record (not union)
  -- The round-trip is lossy: the Hydra type comes back as a record, not a union
  T.TypeLevelRoundTripHydraTestCase {
    T.typeLevelRoundTripHydraTestCaseDescription = "general union loses union structure in round-trip",
    T.typeLevelRoundTripHydraTestCaseType = hydraUnionType [("a", Types.int32), ("b", Types.string)],
    T.typeLevelRoundTripHydraTestCaseExpectedType = hydraRecordType [("a", Types.optional Types.int32), ("b", Types.optional Types.string)]}
  ]


-- ============================================================
-- Category 8: Term-level round-trip (Term -> JSON -> Term)
-- ============================================================

termLevelRoundTripTermCases :: [T.AvroTestCase]
termLevelRoundTripTermCases = map T.AvroTestCaseTermLevelRoundTripTerm [
  T.TermLevelRoundTripTermTestCase {
    T.termLevelRoundTripTermTestCaseDescription = "boolean term round-trips",
    T.termLevelRoundTripTermTestCaseType = Types.boolean,
    T.termLevelRoundTripTermTestCaseTerm = Terms.boolean True,
    T.termLevelRoundTripTermTestCaseExpectedTerm = Terms.boolean True},
  T.TermLevelRoundTripTermTestCase {
    T.termLevelRoundTripTermTestCaseDescription = "int32 term round-trips",
    T.termLevelRoundTripTermTestCaseType = Types.int32,
    T.termLevelRoundTripTermTestCaseTerm = Terms.int32 42,
    T.termLevelRoundTripTermTestCaseExpectedTerm = Terms.int32 42},
  T.TermLevelRoundTripTermTestCase {
    T.termLevelRoundTripTermTestCaseDescription = "string term round-trips",
    T.termLevelRoundTripTermTestCaseType = Types.string,
    T.termLevelRoundTripTermTestCaseTerm = Terms.string "hello",
    T.termLevelRoundTripTermTestCaseExpectedTerm = Terms.string "hello"},
  T.TermLevelRoundTripTermTestCase {
    T.termLevelRoundTripTermTestCaseDescription = "list term round-trips",
    T.termLevelRoundTripTermTestCaseType = Types.list Types.int32,
    T.termLevelRoundTripTermTestCaseTerm = Terms.list [Terms.int32 1, Terms.int32 2],
    T.termLevelRoundTripTermTestCaseExpectedTerm = Terms.list [Terms.int32 1, Terms.int32 2]},
  T.TermLevelRoundTripTermTestCase {
    T.termLevelRoundTripTermTestCaseDescription = "optional nothing round-trips",
    T.termLevelRoundTripTermTestCaseType = Types.optional Types.string,
    T.termLevelRoundTripTermTestCaseTerm = Core.TermMaybe Nothing,
    T.termLevelRoundTripTermTestCaseExpectedTerm = Core.TermMaybe Nothing},
  T.TermLevelRoundTripTermTestCase {
    T.termLevelRoundTripTermTestCaseDescription = "optional just round-trips",
    T.termLevelRoundTripTermTestCaseType = Types.optional Types.string,
    T.termLevelRoundTripTermTestCaseTerm = Core.TermMaybe (Just (Terms.string "foo")),
    T.termLevelRoundTripTermTestCaseExpectedTerm = Core.TermMaybe (Just (Terms.string "foo"))},

  -- Map round-trip via reverse adapter
  T.TermLevelRoundTripTermTestCase {
    T.termLevelRoundTripTermTestCaseDescription = "map term round-trips",
    T.termLevelRoundTripTermTestCaseType = Types.map Types.string Types.int32,
    T.termLevelRoundTripTermTestCaseTerm = Core.TermMap (M.fromList [
      (Terms.string "a", Terms.int32 1),
      (Terms.string "b", Terms.int32 2)]),
    T.termLevelRoundTripTermTestCaseExpectedTerm = Core.TermMap (M.fromList [
      (Terms.string "a", Terms.int32 1),
      (Terms.string "b", Terms.int32 2)])},

  -- Float64 round-trip via reverse adapter
  T.TermLevelRoundTripTermTestCase {
    T.termLevelRoundTripTermTestCaseDescription = "float64 term round-trips",
    T.termLevelRoundTripTermTestCaseType = Types.float64,
    T.termLevelRoundTripTermTestCaseTerm = Terms.float64 3.14,
    T.termLevelRoundTripTermTestCaseExpectedTerm = Terms.float64 3.14}
  ]


-- ============================================================
-- Category 9: Union-specific test cases
-- ============================================================

unionCases :: [T.AvroTestCase]
unionCases = map T.AvroTestCaseUnion [
  -- All-unit union encodes as enum
  T.UnionTestCase {
    T.unionTestCaseDescription = "all-unit union <-> enum",
    T.unionTestCaseHydraType = hydraUnionType [("red", Types.unit), ("green", Types.unit), ("blue", Types.unit)],
    T.unionTestCaseAvroSchema = avroEnum "Color" ["red", "green", "blue"],
    T.unionTestCaseTermPairs = [
      (hydraUnionTerm "Color" "red" Terms.unit, Json.ValueString "red"),
      (hydraUnionTerm "Color" "green" Terms.unit, Json.ValueString "green")]},

  -- General union encodes as record with optional fields
  T.UnionTestCase {
    T.unionTestCaseDescription = "general union <-> record with optional fields",
    T.unionTestCaseHydraType = hydraUnionType [("a", Types.int32), ("b", Types.string)],
    T.unionTestCaseAvroSchema = avroRecord "TaggedUnion" [
      simpleField "a" (avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveInt]),
      simpleField "b" (avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveString])],
    T.unionTestCaseTermPairs = [
      (hydraUnionTerm "TaggedUnion" "a" (Terms.int32 42),
        Json.ValueObject (M.fromList [("a", Json.ValueNumber 42.0), ("b", Json.ValueNull)])),
      (hydraUnionTerm "TaggedUnion" "b" (Terms.string "hello"),
        Json.ValueObject (M.fromList [("a", Json.ValueNull), ("b", Json.ValueString "hello")]))]},

  -- Optional is just union[null, T]
  T.UnionTestCase {
    T.unionTestCaseDescription = "optional <-> union[null, T]",
    T.unionTestCaseHydraType = Types.optional Types.int32,
    T.unionTestCaseAvroSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveInt],
    T.unionTestCaseTermPairs = [
      (Core.TermMaybe (Just (Terms.int32 7)), Json.ValueNumber 7.0),
      (Core.TermMaybe Nothing, Json.ValueNull)]}
  ]


-- ============================================================
-- Category 11: Lossiness and annotation test cases
-- ============================================================

lossinessCases :: [T.AvroTestCase]
lossinessCases = map T.AvroTestCaseLossiness [
  -- Fixed type loses size info
  T.LossinessTestCase {
    T.lossinessTestCaseDescription = "fixed loses size info (stashed in annotation)",
    T.lossinessTestCaseOriginalSchema = Avro.SchemaNamed $ Avro.Named {
      Avro.namedName = "Hash",
      Avro.namedNamespace = Nothing,
      Avro.namedAliases = Nothing,
      Avro.namedDoc = Nothing,
      Avro.namedType = Avro.NamedTypeFixed (Avro.Fixed 16),
      Avro.namedAnnotations = M.empty},
    T.lossinessTestCaseHydraType = Types.binary,
    T.lossinessTestCaseRecoveredSchema = avroPrim Avro.PrimitiveBytes,
    T.lossinessTestCaseIsLossy = False},

  -- General union -> record is lossy (structure changes)
  T.LossinessTestCase {
    T.lossinessTestCaseDescription = "general union -> record with optionals is lossy",
    T.lossinessTestCaseOriginalSchema = avroPrim Avro.PrimitiveNull, -- placeholder; the original is a Hydra union, not an Avro schema
    T.lossinessTestCaseHydraType = hydraUnionType [("a", Types.int32), ("b", Types.string)],
    T.lossinessTestCaseRecoveredSchema = avroRecord "Union" [
      optionalField "a" (avroPrim Avro.PrimitiveInt),
      optionalField "b" (avroPrim Avro.PrimitiveString)],
    T.lossinessTestCaseIsLossy = True}
  ]


-- ============================================================
-- Category 12: Schema serialization test cases
-- ============================================================

schemaSerializationCases :: [T.AvroTestCase]
schemaSerializationCases = map T.AvroTestCaseSchemaSerialization [
  T.SchemaSerializationTestCase {
    T.schemaSerializationTestCaseDescription = "primitive string schema -> JSON string",
    T.schemaSerializationTestCaseSchema = avroPrim Avro.PrimitiveString,
    T.schemaSerializationTestCaseJson = Json.ValueString "string"},
  T.SchemaSerializationTestCase {
    T.schemaSerializationTestCaseDescription = "primitive int schema -> JSON string",
    T.schemaSerializationTestCaseSchema = avroPrim Avro.PrimitiveInt,
    T.schemaSerializationTestCaseJson = Json.ValueString "int"},
  T.SchemaSerializationTestCase {
    T.schemaSerializationTestCaseDescription = "array schema -> JSON object",
    T.schemaSerializationTestCaseSchema = avroArray (avroPrim Avro.PrimitiveString),
    T.schemaSerializationTestCaseJson = Json.ValueObject (M.fromList [
      ("type", Json.ValueString "array"),
      ("items", Json.ValueString "string")])},
  T.SchemaSerializationTestCase {
    T.schemaSerializationTestCaseDescription = "enum schema -> JSON object",
    T.schemaSerializationTestCaseSchema = avroEnum "Color" ["red", "green", "blue"],
    T.schemaSerializationTestCaseJson = Json.ValueObject (M.fromList [
      ("type", Json.ValueString "enum"),
      ("name", Json.ValueString "Color"),
      ("symbols", Json.ValueArray [Json.ValueString "red", Json.ValueString "green", Json.ValueString "blue"])])},
  T.SchemaSerializationTestCase {
    T.schemaSerializationTestCaseDescription = "record schema -> JSON object",
    T.schemaSerializationTestCaseSchema = avroRecord "Point" [simpleField "x" (avroPrim Avro.PrimitiveInt), simpleField "y" (avroPrim Avro.PrimitiveInt)],
    T.schemaSerializationTestCaseJson = Json.ValueObject (M.fromList [
      ("type", Json.ValueString "record"),
      ("name", Json.ValueString "Point"),
      ("fields", Json.ValueArray [
        Json.ValueObject (M.fromList [("name", Json.ValueString "x"), ("type", Json.ValueString "int")]),
        Json.ValueObject (M.fromList [("name", Json.ValueString "y"), ("type", Json.ValueString "int")])])])},

  -- Union schema
  T.SchemaSerializationTestCase {
    T.schemaSerializationTestCaseDescription = "union schema -> JSON array",
    T.schemaSerializationTestCaseSchema = avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveString],
    T.schemaSerializationTestCaseJson = Json.ValueArray [Json.ValueString "null", Json.ValueString "string"]},

  -- Map schema
  T.SchemaSerializationTestCase {
    T.schemaSerializationTestCaseDescription = "map schema -> JSON object",
    T.schemaSerializationTestCaseSchema = avroMap (avroPrim Avro.PrimitiveInt),
    T.schemaSerializationTestCaseJson = Json.ValueObject (M.fromList [
      ("type", Json.ValueString "map"),
      ("values", Json.ValueString "int")])}
  ]


-- ============================================================
-- Schema encode/decode round-trip tests
-- ============================================================

schemaCodecRoundTripSpec :: H.Spec
schemaCodecRoundTripSpec = H.describe "Schema encode/decode round-trip" $ do
  let roundTrip label schema = H.it label $ do
        let json = SchemaJson.encodeSchema schema
        case SchemaJson.decodeSchema emptyContext json of
          Left e -> H.expectationFailure $ "decode failed: " ++ show e
          Right decoded -> decoded `H.shouldBe` schema

  roundTrip "null" (avroPrim Avro.PrimitiveNull)
  roundTrip "boolean" (avroPrim Avro.PrimitiveBoolean)
  roundTrip "int" (avroPrim Avro.PrimitiveInt)
  roundTrip "long" (avroPrim Avro.PrimitiveLong)
  roundTrip "float" (avroPrim Avro.PrimitiveFloat)
  roundTrip "double" (avroPrim Avro.PrimitiveDouble)
  roundTrip "bytes" (avroPrim Avro.PrimitiveBytes)
  roundTrip "string" (avroPrim Avro.PrimitiveString)
  roundTrip "array(string)" (avroArray (avroPrim Avro.PrimitiveString))
  roundTrip "map(int)" (avroMap (avroPrim Avro.PrimitiveInt))
  roundTrip "union[null, string]" (avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveString])
  roundTrip "enum" (avroEnum "Color" ["red", "green", "blue"])
  roundTrip "record" (avroRecord "Point" [simpleField "x" (avroPrim Avro.PrimitiveInt), simpleField "y" (avroPrim Avro.PrimitiveInt)])
  roundTrip "nested: array(map(string))" (avroArray (avroMap (avroPrim Avro.PrimitiveString)))
  roundTrip "record with optional field" (avroRecord "Person" [
    simpleField "name" (avroPrim Avro.PrimitiveString),
    simpleField "age" (avroUnion [avroPrim Avro.PrimitiveNull, avroPrim Avro.PrimitiveInt])])


-- ============================================================
-- .avsc file integration tests
-- ============================================================

avscFileSpec :: H.Spec
avscFileSpec = H.describe ".avsc file integration" $ do

  H.it "parse and round-trip Review.avsc schema" $ do
    json <- readAvscFile "src/test/avro/moviedemo/Review.avsc"
    case SchemaJson.decodeSchema emptyContext json of
      Left e -> H.expectationFailure $ "decode failed: " ++ show e
      Right schema -> do
        -- Verify it's a record named ReviewInfo
        case schema of
          Avro.SchemaNamed named -> do
            Avro.namedName named `H.shouldBe` "ReviewInfo"
            Avro.namedNamespace named `H.shouldBe` Just "com.example"
            case Avro.namedType named of
              Avro.NamedTypeRecord r ->
                length (Avro.recordFields r) `H.shouldBe` 3
              _ -> H.expectationFailure "expected record type"
          _ -> H.expectationFailure "expected named schema"
        -- Encode back and re-decode
        let json' = SchemaJson.encodeSchema schema
        case SchemaJson.decodeSchema emptyContext json' of
          Left e -> H.expectationFailure $ "re-decode failed: " ++ show e
          Right schema' -> schema' `H.shouldBe` schema

  H.it "parse and round-trip AirplaneInfo.avsc schema" $ do
    json <- readAvscFile "src/test/avro/aviationdemo/AirplaneInfo.avsc"
    case SchemaJson.decodeSchema emptyContext json of
      Left e -> H.expectationFailure $ "decode failed: " ++ show e
      Right schema -> do
        case schema of
          Avro.SchemaNamed named -> do
            Avro.namedName named `H.shouldBe` "AirplaneInfo"
            case Avro.namedType named of
              Avro.NamedTypeRecord r ->
                length (Avro.recordFields r) `H.shouldBe` 6
              _ -> H.expectationFailure "expected record type"
          _ -> H.expectationFailure "expected named schema"
        let json' = SchemaJson.encodeSchema schema
        case SchemaJson.decodeSchema emptyContext json' of
          Left e -> H.expectationFailure $ "re-decode failed: " ++ show e
          Right schema' -> schema' `H.shouldBe` schema

  H.it "parse Review.avsc and encode/decode example data" $ do
    schemaJson <- readAvscFile "src/test/avro/moviedemo/Review.avsc"
    dataJson <- readAvscFile "src/test/json/moviedemo/exampleReview.json"
    case SchemaJson.decodeSchema emptyContext schemaJson of
      Left e -> H.expectationFailure $ "schema decode failed: " ++ show e
      Right schema -> do
        case AvroCoder.avroHydraAdapter emptyContext schema AvroCoder.emptyAvroEnvironment of
          Left e -> H.expectationFailure $ "adapter creation failed: " ++ show e
          Right (adapter, _env) -> do
            -- Forward: JSON -> Term
            case Util.coderEncode (Util.adapterCoder adapter) emptyContext dataJson of
              Left e -> H.expectationFailure $ "encode failed: " ++ show e
              Right term -> do
                -- Reverse: Term -> JSON
                case Util.coderDecode (Util.adapterCoder adapter) emptyContext term of
                  Left e -> H.expectationFailure $ "decode failed: " ++ show e
                  Right roundTripped -> roundTripped `H.shouldBe` dataJson

  H.it "parse .avsc and run through forward adapter" $ do
    json <- readAvscFile "src/test/avro/moviedemo/Review.avsc"
    case SchemaJson.decodeSchema emptyContext json of
      Left e -> H.expectationFailure $ "decode failed: " ++ show e
      Right schema -> do
        case AvroCoder.avroHydraAdapter emptyContext schema AvroCoder.emptyAvroEnvironment of
          Left e -> H.expectationFailure $ "forward adapter failed: " ++ show e
          Right (adapter, _env) -> do
            -- Verify we got a record type (possibly wrapped in annotations)
            let isRecord t = case t of
                  Core.TypeRecord _ -> True
                  Core.TypeAnnotated (Core.AnnotatedType inner _) -> isRecord inner
                  _ -> False
            isRecord (Util.adapterTarget adapter) `H.shouldBe` True

-- | Read and parse a JSON file (used for .avsc files)
readAvscFile :: FilePath -> IO Json.Value
readAvscFile path = do
  contents <- readFile path
  case parseJsonString contents of
    Left e -> fail $ "failed to parse " ++ path ++ ": " ++ e
    Right v -> return v

parseJsonString :: String -> Either String Json.Value
parseJsonString s = case JsonParser.parseJson s of
  ParseResultSuccess success -> Right (parseSuccessValue success)
  ParseResultFailure err -> Left (parseErrorMessage err)


-- ============================================================
-- avroSchemaStringCoder tests
-- ============================================================

schemaStringCoderSpec :: H.Spec
schemaStringCoderSpec = H.describe "avroSchemaStringCoder" $ do
  let cx = emptyContext
  let coder = SchemaJson.avroSchemaStringCoder cx

  H.it "encode a record schema to JSON string" $ do
    let schema = avroRecord "Point" [simpleField "x" (avroPrim Avro.PrimitiveInt)]
    case Util.coderEncode coder cx schema of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right str -> do
        -- Should contain "record" and "Point"
        str `H.shouldSatisfy` \s -> "record" `L.isInfixOf` s && "Point" `L.isInfixOf` s

  H.it "decode a JSON string to a schema" $ do
    let jsonStr = "{\"type\":\"record\",\"name\":\"Point\",\"fields\":[{\"name\":\"x\",\"type\":\"int\"}]}"
    case Util.coderDecode coder cx jsonStr of
      Left e -> H.expectationFailure $ "decode failed: " ++ show e
      Right schema -> case schema of
        Avro.SchemaNamed named -> Avro.namedName named `H.shouldBe` "Point"
        _ -> H.expectationFailure "expected named schema"

  H.it "encode then decode round-trips" $ do
    let schema = avroEnum "Color" ["red", "green", "blue"]
    case Util.coderEncode coder cx schema of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right str -> do
        case Util.coderDecode coder cx str of
          Left e -> H.expectationFailure $ "decode failed: " ++ show e
          Right decoded -> decoded `H.shouldBe` schema


-- ============================================================
-- End-to-end integration tests
-- ============================================================

endToEndSpec :: H.Spec
endToEndSpec = H.describe "End-to-end pipeline" $ do

  H.it "forward then reverse: Avro record schema -> Hydra -> Avro schema -> JSON" $ do
    let schema = avroRecord "Employee" [
          simpleField "name" (avroPrim Avro.PrimitiveString),
          simpleField "age" (avroPrim Avro.PrimitiveInt),
          simpleField "active" (avroPrim Avro.PrimitiveBoolean)]
    -- Forward: Avro Schema -> Hydra Type
    case AvroCoder.avroHydraAdapter emptyContext schema AvroCoder.emptyAvroEnvironment of
      Left e -> H.expectationFailure $ "forward adapt failed: " ++ show e
      Right (adapter, _) -> do
        let hydraType = Util.adapterTarget adapter
        -- Reverse: Hydra Type -> Avro Schema
        case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
          Left e -> H.expectationFailure $ "reverse adapt failed: " ++ show e
          Right revAdapter -> do
            let avroSchema = Util.adapterTarget revAdapter
            -- Encode to JSON
            let json = SchemaJson.encodeSchema avroSchema
            -- Verify it's a valid record schema JSON
            case json of
              Json.ValueObject m -> do
                M.lookup "type" m `H.shouldBe` Just (Json.ValueString "record")
                case M.lookup "fields" m of
                  Just (Json.ValueArray fs) -> length fs `H.shouldBe` 3
                  _ -> H.expectationFailure "expected fields array"
              _ -> H.expectationFailure "expected JSON object"

  H.it "reverse adapter encodes and decodes terms correctly for a complex record" $ do
    let hydraType = hydraRecordType [
          ("name", Types.string),
          ("scores", Types.list Types.int32),
          ("metadata", Types.map Types.string Types.string)]
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter creation failed: " ++ show e
      Right adapter -> do
        let term = hydraRecord "Record" [
              ("name", Terms.string "Alice"),
              ("scores", Terms.list [Terms.int32 90, Terms.int32 85]),
              ("metadata", Core.TermMap (M.fromList [
                (Terms.string "role", Terms.string "engineer")]))]
        -- Encode term -> JSON
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> do
            -- Decode JSON -> term
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode failed: " ++ show e
              Right term' -> term' `H.shouldBe` term

  H.it "annotations round-trip through forward then reverse" $ do
    -- The Review.avsc has annotations like @edgeLabel, @vertexId, etc.
    -- Forward adapter preserves them as TypeAnnotated. Reverse should emit them.
    schemaJson <- readAvscFile "src/test/avro/moviedemo/Review.avsc"
    case SchemaJson.decodeSchema emptyContext schemaJson of
      Left e -> H.expectationFailure $ "schema decode failed: " ++ show e
      Right schema -> do
        -- Forward: Avro -> Hydra (preserves annotations in type)
        case AvroCoder.avroHydraAdapter emptyContext schema AvroCoder.emptyAvroEnvironment of
          Left e -> H.expectationFailure $ "forward adapter failed: " ++ show e
          Right (fwdAdapter, _env) -> do
            let hydraType = Util.adapterTarget fwdAdapter
            -- Reverse: Hydra -> Avro (should recover annotations)
            let reviewName = Core.Name "com.example.ReviewInfo"
            case Encoder.encodeType emptyContext (M.singleton reviewName hydraType) reviewName of
              Left e -> H.expectationFailure $ "reverse adapter failed: " ++ show e
              Right revAdapter -> do
                case Util.adapterTarget revAdapter of
                  Avro.SchemaNamed named -> do
                    -- The top-level @edgeLabel annotation should be preserved
                    M.member "edgeLabel" (Avro.namedAnnotations named) `H.shouldBe` True
                    Avro.namedName named `H.shouldBe` "ReviewInfo"
                    Avro.namedNamespace named `H.shouldBe` Just "com.example"
                  _ -> H.expectationFailure "expected named schema"

  H.it "environment tracks named types" $ do
    let typ = hydraRecordType [("x", Types.int32)]
    let name = Core.Name "com.example.Point"
    let typeMap = M.singleton name typ
    case Encoder.encodeTypeWithEnv emptyContext name (Encoder.emptyEncodeEnvironment typeMap) of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right (_, env) -> do
        M.member name (AvroEnv.encodeEnvironmentEmitted env) `H.shouldBe` True

  H.it "type references resolve from type map" $ do
    -- A type map with Point and Line, where Line references Point
    let pointName = Core.Name "com.example.Point"
    let lineName = Core.Name "com.example.Line"
    let pointType = hydraRecordType [("x", Types.int32), ("y", Types.int32)]
    let lineType = hydraRecordType [("start", Core.TypeVariable pointName), ("end", Core.TypeVariable pointName)]
    let typeMap = M.fromList [(pointName, pointType), (lineName, lineType)]
    case Encoder.encodeType emptyContext typeMap lineName of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right adapter -> do
        case Util.adapterTarget adapter of
          Avro.SchemaNamed named -> do
            Avro.namedName named `H.shouldBe` "Line"
            case Avro.namedType named of
              Avro.NamedTypeRecord (Avro.Record fields) -> do
                length fields `H.shouldBe` 2
                -- First field (start) should inline Point
                case Avro.fieldType (head fields) of
                  Avro.SchemaNamed innerNamed ->
                    Avro.namedName innerNamed `H.shouldBe` "Point"
                  other -> H.expectationFailure $ "expected named schema for start, got: " ++ show other
                -- Second field (end) should be a reference
                case Avro.fieldType (fields !! 1) of
                  Avro.SchemaReference ref ->
                    ref `H.shouldBe` "Point"
                  other -> H.expectationFailure $ "expected schema reference for end, got: " ++ show other
              _ -> H.expectationFailure "expected record type"
          _ -> H.expectationFailure "expected named schema"

  H.it "named reverse adapter preserves the given name" $ do
    let hydraType = hydraRecordType [("x", Types.int32), ("y", Types.int32)]
    let pointName = Core.Name "com.example.Point"
    case Encoder.encodeType emptyContext (M.singleton pointName hydraType) pointName of
      Left e -> H.expectationFailure $ "adapter creation failed: " ++ show e
      Right adapter -> do
        case Util.adapterTarget adapter of
          Avro.SchemaNamed named -> do
            Avro.namedName named `H.shouldBe` "Point"
            Avro.namedNamespace named `H.shouldBe` Just "com.example"
          other -> H.expectationFailure $ "expected named schema, got: " ++ show other

  H.it "bicoder can be constructed and used" $ do
    let cx = emptyContext
    -- Forward direction (Avro -> Hydra)
    case AvroCoder.avroHydraAdapter cx (avroPrim Avro.PrimitiveString) AvroCoder.emptyAvroEnvironment of
      Left e -> H.expectationFailure $ "forward adapter failed: " ++ show e
      Right (fwdAdapter, _) ->
        Util.adapterTarget fwdAdapter `H.shouldBe` Types.string
    -- Reverse direction (Hydra -> Avro)
    case Encoder.hydraAvroAdapter cx M.empty Types.int32 of
      Left e -> H.expectationFailure $ "reverse adapter failed: " ++ show e
      Right revAdapter ->
        Util.adapterTarget revAdapter `H.shouldBe` avroPrim Avro.PrimitiveInt


-- ============================================================
-- Binary (bytes) term-level tests
-- ============================================================

binaryTermSpec :: H.Spec
binaryTermSpec = H.describe "Binary (bytes) term encoding" $ do

  -- Note: Hydra's binaryToString/stringToBinary use base64 encoding
  H.it "forward: JSON base64 string -> Hydra binary term" $ do
    case AvroCoder.avroHydraAdapter emptyContext (avroPrim Avro.PrimitiveBytes) AvroCoder.emptyAvroEnvironment of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right (adapter, _) -> do
        -- "aGVsbG8=" is the base64 encoding of "hello"
        let result = Util.coderEncode (Util.adapterCoder adapter) emptyContext (Json.ValueString "aGVsbG8=")
        case result of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right term -> term `H.shouldBe` Core.TermLiteral (Core.LiteralBinary (B.pack [104,101,108,108,111]))

  H.it "reverse: Hydra binary term -> JSON base64 string" $ do
    case Encoder.hydraAvroAdapter emptyContext M.empty Types.binary of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        let term = Core.TermLiteral (Core.LiteralBinary (B.pack [104,101,108,108,111]))
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> json `H.shouldBe` Json.ValueString "aGVsbG8="

  H.it "binary round-trip: term -> JSON -> term" $ do
    case Encoder.hydraAvroAdapter emptyContext M.empty Types.binary of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        let term = Core.TermLiteral (Core.LiteralBinary (B.pack [0, 1, 255]))
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> do
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode failed: " ++ show e
              Right term' -> term' `H.shouldBe` term

  H.it "binary in a record field" $ do
    let hydraType = hydraRecordType [("data", Types.binary), ("label", Types.string)]
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        let term = hydraRecord "Record" [
              ("data", Core.TermLiteral (Core.LiteralBinary (B.pack [1, 2, 3]))),
              ("label", Terms.string "test")]
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> do
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode failed: " ++ show e
              Right term' -> term' `H.shouldBe` term


-- ============================================================
-- Lossy integer and float type tests
-- ============================================================

lossyIntegerFloatSpec :: H.Spec
lossyIntegerFloatSpec = H.describe "Lossy integer and float type mapping" $ do

  H.it "int8 -> long (lossy), term round-trips" $ do
    let hydraType = Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        Util.adapterIsLossy adapter `H.shouldBe` True
        case Util.adapterTarget adapter of
          Avro.SchemaPrimitive Avro.PrimitiveLong -> return ()
          other -> H.expectationFailure $ "expected long, got: " ++ show other

  H.it "int16 -> long (lossy)" $ do
    let hydraType = Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16)
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        Util.adapterIsLossy adapter `H.shouldBe` True
        case Util.adapterTarget adapter of
          Avro.SchemaPrimitive Avro.PrimitiveLong -> return ()
          other -> H.expectationFailure $ "expected long, got: " ++ show other

  H.it "uint16 -> long (lossy)" $ do
    let hydraType = Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16)
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> Util.adapterIsLossy adapter `H.shouldBe` True

  H.it "bigint -> long (lossy)" $ do
    let hydraType = Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> Util.adapterIsLossy adapter `H.shouldBe` True

  H.it "int32 -> int (not lossy)" $ do
    case Encoder.hydraAvroAdapter emptyContext M.empty Types.int32 of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        Util.adapterIsLossy adapter `H.shouldBe` False
        case Util.adapterTarget adapter of
          Avro.SchemaPrimitive Avro.PrimitiveInt -> return ()
          other -> H.expectationFailure $ "expected int, got: " ++ show other

  H.it "int64 -> long (not lossy)" $ do
    case Encoder.hydraAvroAdapter emptyContext M.empty Types.int64 of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        Util.adapterIsLossy adapter `H.shouldBe` False
        case Util.adapterTarget adapter of
          Avro.SchemaPrimitive Avro.PrimitiveLong -> return ()
          other -> H.expectationFailure $ "expected long, got: " ++ show other

  H.it "bigfloat -> double (lossy)" $ do
    let hydraType = Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeBigfloat)
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> Util.adapterIsLossy adapter `H.shouldBe` True

  H.it "float32 -> float (not lossy)" $ do
    case Encoder.hydraAvroAdapter emptyContext M.empty Types.float32 of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        Util.adapterIsLossy adapter `H.shouldBe` False
        case Util.adapterTarget adapter of
          Avro.SchemaPrimitive Avro.PrimitiveFloat -> return ()
          other -> H.expectationFailure $ "expected float, got: " ++ show other

  H.it "float64 -> double (not lossy)" $ do
    case Encoder.hydraAvroAdapter emptyContext M.empty Types.float64 of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        Util.adapterIsLossy adapter `H.shouldBe` False
        case Util.adapterTarget adapter of
          Avro.SchemaPrimitive Avro.PrimitiveDouble -> return ()
          other -> H.expectationFailure $ "expected double, got: " ++ show other

  H.it "lossy int8 term encoding round-trips through int64" $ do
    let hydraType = Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        let term = Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 42))
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> do
            json `H.shouldBe` Json.ValueNumber 42.0
            -- Decode comes back as int64 (lossy: original int8 precision lost)
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode failed: " ++ show e
              Right term' -> term' `H.shouldBe` Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 42))


-- ============================================================
-- TypeWrap (newtype) tests
-- ============================================================

wrapTypeSpec :: H.Spec
wrapTypeSpec = H.describe "TypeWrap (newtype) encoding" $ do

  H.it "wrapped string is encoded as Avro string" $ do
    -- Name is defined as a newtype over String in the Hydra kernel
    let hydraType = Core.TypeWrap Types.string
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        Util.adapterTarget adapter `H.shouldBe` avroPrim Avro.PrimitiveString
        Util.adapterIsLossy adapter `H.shouldBe` False

  H.it "wrapped int32 is encoded as Avro int" $ do
    let hydraType = Core.TypeWrap Types.int32
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter ->
        Util.adapterTarget adapter `H.shouldBe` avroPrim Avro.PrimitiveInt

  H.it "wrapped list(string) is encoded as Avro array(string)" $ do
    let hydraType = Core.TypeWrap (Types.list Types.string)
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter ->
        Util.adapterTarget adapter `H.shouldBe` avroArray (avroPrim Avro.PrimitiveString)

  H.it "nested wraps are unwrapped correctly" $ do
    let hydraType = Core.TypeWrap (Core.TypeWrap Types.boolean)
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter ->
        Util.adapterTarget adapter `H.shouldBe` avroPrim Avro.PrimitiveBoolean

  H.it "record with wrapped field types" $ do
    -- A record where one field is a wrapped string (like Hydra Name)
    let hydraType = hydraRecordType [
          ("id", Core.TypeWrap Types.string),
          ("count", Types.int32)]
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        case Util.adapterTarget adapter of
          Avro.SchemaNamed named -> do
            let Avro.NamedTypeRecord (Avro.Record fields) = Avro.namedType named
            length fields `H.shouldBe` 2
            -- Fields are in definition order: id (wrapped string), count (int32)
            Avro.fieldName (head fields) `H.shouldBe` "id"
            Avro.fieldType (head fields) `H.shouldBe` avroPrim Avro.PrimitiveString
            Avro.fieldName (fields !! 1) `H.shouldBe` "count"
            Avro.fieldType (fields !! 1) `H.shouldBe` avroPrim Avro.PrimitiveInt
          other -> H.expectationFailure $ "expected named schema, got: " ++ show other


-- ============================================================
-- Error case tests
-- ============================================================

errorCaseSpec :: H.Spec
errorCaseSpec = H.describe "Error cases" $ do

  H.it "non-string map keys are rejected" $ do
    let hydraType = Types.map Types.int32 Types.string
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left _ -> return ()  -- Expected to fail
      Right _ -> H.expectationFailure "should have rejected non-string map keys"

  H.it "unknown type variable is rejected" $ do
    -- Reference a type that doesn't exist in the type map
    let hydraType = Core.TypeVariable (Core.Name "com.example.NonExistent")
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left _ -> return ()
      Right _ -> H.expectationFailure "should have rejected unknown type variable"

  H.it "encodeType with unknown name is rejected" $ do
    let typeMap = M.empty :: M.Map Core.Name Core.Type
    case Encoder.encodeType emptyContext typeMap (Core.Name "com.example.Missing") of
      Left _ -> return ()
      Right _ -> H.expectationFailure "should have rejected unknown name"


-- ============================================================
-- Complex realistic types
-- ============================================================

complexRealisticSpec :: H.Spec
complexRealisticSpec = H.describe "Complex realistic Avro schemas" $ do

  -- Kafka-style schema: a message envelope with metadata and payload
  H.it "Kafka-style message envelope with nested records, optional fields, and containers" $ do
    let headerName = Core.Name "com.messaging.Header"
    let messageName = Core.Name "com.messaging.Message"
    let headerType = hydraRecordType [
          ("key", Types.string),
          ("value", Types.string)]
    let messageType = hydraRecordType [
          ("id", Types.string),
          ("timestamp", Types.int64),
          ("headers", Types.list (Core.TypeVariable headerName)),
          ("payload", Types.binary),
          ("metadata", Types.map Types.string Types.string),
          ("replyTo", Types.optional Types.string)]
    let typeMap = M.fromList [(headerName, headerType), (messageName, messageType)]
    case Encoder.encodeType emptyContext typeMap messageName of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right adapter -> do
        case Util.adapterTarget adapter of
          Avro.SchemaNamed named -> do
            Avro.namedName named `H.shouldBe` "Message"
            Avro.namedNamespace named `H.shouldBe` Just "com.messaging"
            case Avro.namedType named of
              Avro.NamedTypeRecord (Avro.Record fields) -> do
                length fields `H.shouldBe` 6
                -- The "headers" field should be array of inlined Header record
                let headersField = fields !! 2
                Avro.fieldName headersField `H.shouldBe` "headers"
                case Avro.fieldType headersField of
                  Avro.SchemaArray (Avro.Array items) -> case items of
                    Avro.SchemaNamed innerNamed -> Avro.namedName innerNamed `H.shouldBe` "Header"
                    other -> H.expectationFailure $ "expected named schema for array items, got: " ++ show other
                  other -> H.expectationFailure $ "expected array schema for headers, got: " ++ show other
                -- The "payload" field should be bytes
                Avro.fieldType (fields !! 3) `H.shouldBe` avroPrim Avro.PrimitiveBytes
                -- The "metadata" field should be map(string)
                case Avro.fieldType (fields !! 4) of
                  Avro.SchemaMap _ -> return ()
                  other -> H.expectationFailure $ "expected map schema for metadata, got: " ++ show other
                -- The "replyTo" field should be union[null, string]
                case Avro.fieldType (fields !! 5) of
                  Avro.SchemaUnion (Avro.Union schemas) -> do
                    length schemas `H.shouldBe` 2
                    head schemas `H.shouldBe` avroPrim Avro.PrimitiveNull
                  other -> H.expectationFailure $ "expected union for replyTo, got: " ++ show other
              _ -> H.expectationFailure "expected record type"
          _ -> H.expectationFailure "expected named schema"

  -- IoT sensor data schema: enum + nested records + arrays + maps
  H.it "IoT sensor data with enum, nested records, and mixed containers" $ do
    let sensorTypeName = Core.Name "iot.SensorType"
    let readingName = Core.Name "iot.Reading"
    let deviceName = Core.Name "iot.Device"
    let sensorType = hydraUnionType [("temperature", Types.unit), ("humidity", Types.unit), ("pressure", Types.unit)]
    let readingType = hydraRecordType [
          ("timestamp", Types.int64),
          ("value", Types.float64),
          ("quality", Types.optional Types.float32)]
    let deviceType = hydraRecordType [
          ("id", Types.string),
          ("sensorType", Core.TypeVariable sensorTypeName),
          ("readings", Types.list (Core.TypeVariable readingName)),
          ("tags", Types.map Types.string Types.string)]
    let typeMap = M.fromList [
          (sensorTypeName, sensorType),
          (readingName, readingType),
          (deviceName, deviceType)]
    case Encoder.encodeType emptyContext typeMap deviceName of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right adapter -> do
        case Util.adapterTarget adapter of
          Avro.SchemaNamed named -> do
            Avro.namedName named `H.shouldBe` "Device"
            case Avro.namedType named of
              Avro.NamedTypeRecord (Avro.Record fields) -> do
                length fields `H.shouldBe` 4
                -- sensorType should be inlined as an enum
                case Avro.fieldType (fields !! 1) of
                  Avro.SchemaNamed enumNamed -> case Avro.namedType enumNamed of
                    Avro.NamedTypeEnum (Avro.Enum symbols _) ->
                      symbols `H.shouldBe` ["temperature", "humidity", "pressure"]
                    other -> H.expectationFailure $ "expected enum, got: " ++ show other
                  other -> H.expectationFailure $ "expected named schema for sensorType, got: " ++ show other
                -- readings should be array of inlined Reading record
                case Avro.fieldType (fields !! 2) of
                  Avro.SchemaArray (Avro.Array items) -> case items of
                    Avro.SchemaNamed readingNamed -> do
                      Avro.namedName readingNamed `H.shouldBe` "Reading"
                      case Avro.namedType readingNamed of
                        Avro.NamedTypeRecord (Avro.Record rFields) -> length rFields `H.shouldBe` 3
                        _ -> H.expectationFailure "expected record for Reading"
                    other -> H.expectationFailure $ "expected named schema in array, got: " ++ show other
                  other -> H.expectationFailure $ "expected array for readings, got: " ++ show other
              _ -> H.expectationFailure "expected record type"
          _ -> H.expectationFailure "expected named schema"

  -- Term-level end-to-end with the Kafka message schema
  H.it "Kafka message: term-level encode and decode round-trip" $ do
    let headerName = Core.Name "com.messaging.Header"
    let messageName = Core.Name "com.messaging.Message"
    let headerType = hydraRecordType [("key", Types.string), ("value", Types.string)]
    let messageType = hydraRecordType [
          ("id", Types.string),
          ("timestamp", Types.int64),
          ("headers", Types.list (Core.TypeVariable headerName)),
          ("payload", Types.binary),
          ("metadata", Types.map Types.string Types.string),
          ("replyTo", Types.optional Types.string)]
    let typeMap = M.fromList [(headerName, headerType), (messageName, messageType)]
    case Encoder.encodeType emptyContext typeMap messageName of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right adapter -> do
        let headerTerm1 = hydraRecord "com.messaging.Header" [
              ("key", Terms.string "content-type"),
              ("value", Terms.string "application/json")]
        let headerTerm2 = hydraRecord "com.messaging.Header" [
              ("key", Terms.string "correlation-id"),
              ("value", Terms.string "abc-123")]
        let term = hydraRecord "com.messaging.Message" [
              ("id", Terms.string "msg-001"),
              ("timestamp", Terms.int64 1738136577459),
              ("headers", Terms.list [headerTerm1, headerTerm2]),
              ("payload", Core.TermLiteral (Core.LiteralBinary (B.pack [123, 125]))),
              ("metadata", Core.TermMap (M.fromList [
                (Terms.string "source", Terms.string "api"),
                (Terms.string "version", Terms.string "1.0")])),
              ("replyTo", Core.TermMaybe (Just (Terms.string "reply-queue")))]
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> do
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode failed: " ++ show e
              Right term' -> term' `H.shouldBe` term

  -- Full round-trip through AirplaneInfo.avsc with real data
  H.it "AirplaneInfo.avsc: forward adapter processes real data end-to-end" $ do
    schemaJson <- readAvscFile "src/test/avro/aviationdemo/AirplaneInfo.avsc"
    dataJson <- readJsonFile "src/test/json/aviationdemo/exampleAirplaneInfo.json"
    case SchemaJson.decodeSchema emptyContext schemaJson of
      Left e -> H.expectationFailure $ "schema decode failed: " ++ show e
      Right schema -> do
        case AvroCoder.avroHydraAdapter emptyContext schema AvroCoder.emptyAvroEnvironment of
          Left e -> H.expectationFailure $ "adapter failed: " ++ show e
          Right (adapter, _) -> do
            -- Encode the data JSON through the adapter
            case Util.coderEncode (Util.adapterCoder adapter) emptyContext dataJson of
              Left e -> H.expectationFailure $ "data encode failed: " ++ show e
              Right hydraTerm -> do
                -- Decode back to JSON
                case Util.coderDecode (Util.adapterCoder adapter) emptyContext hydraTerm of
                  Left e -> H.expectationFailure $ "data decode failed: " ++ show e
                  Right jsonResult -> do
                    -- The round-tripped JSON should be structurally equivalent
                    case (dataJson, jsonResult) of
                      (Json.ValueObject orig, Json.ValueObject result) -> do
                        -- Key fields should be preserved
                        M.lookup "recordId" result `H.shouldBe` M.lookup "recordId" orig
                        M.lookup "performanceProfile" result `H.shouldBe` M.lookup "performanceProfile" orig
                      _ -> H.expectationFailure "expected JSON objects"

  -- Full round-trip through Review.avsc with real data
  H.it "Review.avsc: forward adapter processes real data end-to-end" $ do
    schemaJson <- readAvscFile "src/test/avro/moviedemo/Review.avsc"
    dataJson <- readJsonFile "src/test/json/moviedemo/exampleReview.json"
    case SchemaJson.decodeSchema emptyContext schemaJson of
      Left e -> H.expectationFailure $ "schema decode failed: " ++ show e
      Right schema -> do
        case AvroCoder.avroHydraAdapter emptyContext schema AvroCoder.emptyAvroEnvironment of
          Left e -> H.expectationFailure $ "adapter failed: " ++ show e
          Right (adapter, _) -> do
            case Util.coderEncode (Util.adapterCoder adapter) emptyContext dataJson of
              Left e -> H.expectationFailure $ "data encode failed: " ++ show e
              Right hydraTerm -> do
                case Util.coderDecode (Util.adapterCoder adapter) emptyContext hydraTerm of
                  Left e -> H.expectationFailure $ "data decode failed: " ++ show e
                  Right jsonResult -> do
                    case (dataJson, jsonResult) of
                      (Json.ValueObject orig, Json.ValueObject result) -> do
                        M.lookup "timestamp" result `H.shouldBe` M.lookup "timestamp" orig
                      _ -> H.expectationFailure "expected JSON objects"

  -- Multiple named type references with second-occurrence reference
  H.it "diamond dependency: type referenced from two fields, second is a reference" $ do
    let coordName = Core.Name "geo.Coordinate"
    let segmentName = Core.Name "geo.Segment"
    let routeName = Core.Name "geo.Route"
    let coordType = hydraRecordType [("lat", Types.float64), ("lon", Types.float64)]
    let segmentType = hydraRecordType [
          ("start", Core.TypeVariable coordName),
          ("end", Core.TypeVariable coordName)]
    let routeType = hydraRecordType [
          ("name", Types.string),
          ("segments", Types.list (Core.TypeVariable segmentName)),
          ("origin", Core.TypeVariable coordName)]
    let typeMap = M.fromList [(coordName, coordType), (segmentName, segmentType), (routeName, routeType)]
    case Encoder.encodeType emptyContext typeMap routeName of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right adapter -> do
        case Util.adapterTarget adapter of
          Avro.SchemaNamed named -> do
            Avro.namedName named `H.shouldBe` "Route"
            -- The origin field references Coordinate, which was already inlined in Segment
            case Avro.namedType named of
              Avro.NamedTypeRecord (Avro.Record fields) -> do
                length fields `H.shouldBe` 3
                -- origin (3rd field) should be a reference since Coordinate was already emitted
                case Avro.fieldType (fields !! 2) of
                  Avro.SchemaReference ref -> ref `H.shouldBe` "Coordinate"
                  other -> H.expectationFailure $ "expected reference for origin, got: " ++ show other
              _ -> H.expectationFailure "expected record"
          _ -> H.expectationFailure "expected named schema"


-- ============================================================
-- Hydra kernel type tests
-- ============================================================

kernelTypeSpec :: H.Spec
kernelTypeSpec = H.describe "Hydra kernel types as Avro schemas" $ do

  -- FieldType: a record with a Name (wrapped string) and a Type
  -- This exercises wrap + record
  H.it "Hydra FieldType: record with wrapped Name and recursive Type" $ do
    let fieldTypeName = Core.Name "hydra.core.FieldType"
    let fieldTypeType = hydraRecordType [
          ("name", Core.TypeWrap Types.string),  -- Name is a newtype over String
          ("type", Types.string)]  -- simplified: use string instead of recursive Type
    let typeMap = M.singleton fieldTypeName fieldTypeType
    case Encoder.encodeType emptyContext typeMap fieldTypeName of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right adapter -> do
        case Util.adapterTarget adapter of
          Avro.SchemaNamed named -> do
            Avro.namedName named `H.shouldBe` "FieldType"
            Avro.namedNamespace named `H.shouldBe` Just "hydra.core"
            case Avro.namedType named of
              Avro.NamedTypeRecord (Avro.Record fields) -> do
                length fields `H.shouldBe` 2
                -- Both fields should be string (Name wraps to string, type is string)
                Avro.fieldType (head fields) `H.shouldBe` avroPrim Avro.PrimitiveString
                Avro.fieldType (fields !! 1) `H.shouldBe` avroPrim Avro.PrimitiveString
              _ -> H.expectationFailure "expected record"
          _ -> H.expectationFailure "expected named schema"

  -- Projection: a record with two Name fields
  H.it "Hydra Projection: record with two wrapped Name fields" $ do
    let projName = Core.Name "hydra.core.Projection"
    let projType = hydraRecordType [
          ("typeName", Core.TypeWrap Types.string),
          ("field", Core.TypeWrap Types.string)]
    let typeMap = M.singleton projName projType
    case Encoder.encodeType emptyContext typeMap projName of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right adapter -> do
        -- Term-level round-trip
        let term = hydraRecord "hydra.core.Projection" [
              ("typeName", Terms.string "MyRecord"),
              ("field", Terms.string "myField")]
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> do
            case json of
              Json.ValueObject m -> do
                M.lookup "typeName" m `H.shouldBe` Just (Json.ValueString "MyRecord")
                M.lookup "field" m `H.shouldBe` Just (Json.ValueString "myField")
              _ -> H.expectationFailure "expected JSON object"
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode failed: " ++ show e
              Right term' -> term' `H.shouldBe` term

  -- MapType: a record containing two types (simplified as strings)
  H.it "Hydra MapType analog: record with key and value type fields" $ do
    let mapTypeName = Core.Name "hydra.core.MapType"
    let mapTypeType = hydraRecordType [("keys", Types.string), ("values", Types.string)]
    let typeMap = M.singleton mapTypeName mapTypeType
    case Encoder.encodeType emptyContext typeMap mapTypeName of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right adapter -> do
        let term = hydraRecord "hydra.core.MapType" [
              ("keys", Terms.string "string"),
              ("values", Terms.string "int32")]
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> do
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode failed: " ++ show e
              Right term' -> term' `H.shouldBe` term

  -- A mini graph schema: nodes and edges with various field types
  H.it "Graph-like schema with nodes, edges, labels, and properties" $ do
    let labelName = Core.Name "graph.Label"
    let propName = Core.Name "graph.Property"
    let nodeName = Core.Name "graph.Node"
    let edgeName = Core.Name "graph.Edge"
    let graphName = Core.Name "graph.Graph"
    let labelType = hydraUnionType [("vertex", Types.unit), ("edge", Types.unit), ("property", Types.unit)]
    let propType = hydraRecordType [
          ("key", Types.string),
          ("value", Types.string)]
    let nodeType = hydraRecordType [
          ("id", Types.string),
          ("labels", Types.list (Core.TypeVariable labelName)),
          ("properties", Types.map Types.string (Core.TypeVariable propName))]
    let edgeType = hydraRecordType [
          ("source", Types.string),
          ("target", Types.string),
          ("label", Core.TypeVariable labelName),
          ("weight", Types.optional Types.float64)]
    let graphType = hydraRecordType [
          ("nodes", Types.list (Core.TypeVariable nodeName)),
          ("edges", Types.list (Core.TypeVariable edgeName))]
    let typeMap = M.fromList [
          (labelName, labelType), (propName, propType),
          (nodeName, nodeType), (edgeName, edgeType), (graphName, graphType)]
    case Encoder.encodeType emptyContext typeMap graphName of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right adapter -> do
        case Util.adapterTarget adapter of
          Avro.SchemaNamed named -> do
            Avro.namedName named `H.shouldBe` "Graph"
            case Avro.namedType named of
              Avro.NamedTypeRecord (Avro.Record fields) -> do
                length fields `H.shouldBe` 2
                -- nodes field should be array
                case Avro.fieldType (head fields) of
                  Avro.SchemaArray _ -> return ()
                  other -> H.expectationFailure $ "expected array for edges, got: " ++ show other
                -- edges field should be array
                case Avro.fieldType (fields !! 1) of
                  Avro.SchemaArray _ -> return ()
                  other -> H.expectationFailure $ "expected array for nodes, got: " ++ show other
              _ -> H.expectationFailure "expected record"
          _ -> H.expectationFailure "expected named schema"

  -- Term-level: graph with actual data
  H.it "Graph schema: term-level encode and decode with real data" $ do
    let propName = Core.Name "graph.Property"
    let nodeName = Core.Name "graph.Node"
    let propType = hydraRecordType [("key", Types.string), ("value", Types.string)]
    let nodeType = hydraRecordType [
          ("id", Types.string),
          ("properties", Types.map Types.string (Core.TypeVariable propName))]
    let typeMap = M.fromList [(propName, propType), (nodeName, nodeType)]
    case Encoder.encodeType emptyContext typeMap nodeName of
      Left e -> H.expectationFailure $ "encode failed: " ++ show e
      Right adapter -> do
        let prop1 = hydraRecord "graph.Property" [("key", Terms.string "name"), ("value", Terms.string "Alice")]
        let prop2 = hydraRecord "graph.Property" [("key", Terms.string "age"), ("value", Terms.string "30")]
        let term = hydraRecord "graph.Node" [
              ("id", Terms.string "node-1"),
              ("properties", Core.TermMap (M.fromList [
                (Terms.string "name", prop1),
                (Terms.string "age", prop2)]))]
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> do
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode failed: " ++ show e
              Right term' -> term' `H.shouldBe` term


-- ============================================================
-- Edge case tests
-- ============================================================

edgeCaseSpec :: H.Spec
edgeCaseSpec = H.describe "Edge cases" $ do

  H.it "empty list term round-trips" $ do
    case Encoder.hydraAvroAdapter emptyContext M.empty (Types.list Types.int32) of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        let term = Terms.list []
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> do
            json `H.shouldBe` Json.ValueArray []
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode failed: " ++ show e
              Right term' -> term' `H.shouldBe` term

  H.it "empty map term round-trips" $ do
    case Encoder.hydraAvroAdapter emptyContext M.empty (Types.map Types.string Types.string) of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        let term = Core.TermMap M.empty
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> do
            json `H.shouldBe` Json.ValueObject M.empty
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode failed: " ++ show e
              Right term' -> term' `H.shouldBe` term

  H.it "int32 boundary values" $ do
    case Encoder.hydraAvroAdapter emptyContext M.empty Types.int32 of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        let maxTerm = Terms.int32 2147483647
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext maxTerm of
          Left e -> H.expectationFailure $ "encode max failed: " ++ show e
          Right json -> do
            json `H.shouldBe` Json.ValueNumber 2147483647.0
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode max failed: " ++ show e
              Right term' -> term' `H.shouldBe` maxTerm
        let minTerm = Terms.int32 (-2147483648)
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext minTerm of
          Left e -> H.expectationFailure $ "encode min failed: " ++ show e
          Right json -> case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
            Left e -> H.expectationFailure $ "decode min failed: " ++ show e
            Right term' -> term' `H.shouldBe` minTerm

  H.it "unicode strings round-trip" $ do
    case Encoder.hydraAvroAdapter emptyContext M.empty Types.string of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        let unicodeTerm = Terms.string "\19990\30028"
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext unicodeTerm of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
            Left e -> H.expectationFailure $ "decode failed: " ++ show e
            Right term' -> term' `H.shouldBe` unicodeTerm

  H.it "deeply nested type: list of map of optional of int" $ do
    let hydraType = Types.list (Types.map Types.string (Types.optional Types.int32))
    case Encoder.hydraAvroAdapter emptyContext M.empty hydraType of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        case Util.adapterTarget adapter of
          Avro.SchemaArray (Avro.Array items) -> case items of
            Avro.SchemaMap (Avro.Map vals) -> case vals of
              Avro.SchemaUnion (Avro.Union schemas) -> length schemas `H.shouldBe` 2
              other -> H.expectationFailure $ "expected union, got: " ++ show other
            other -> H.expectationFailure $ "expected map, got: " ++ show other
          other -> H.expectationFailure $ "expected array, got: " ++ show other

  H.it "unit type term round-trip" $ do
    case Encoder.hydraAvroAdapter emptyContext M.empty Types.unit of
      Left e -> H.expectationFailure $ "adapter failed: " ++ show e
      Right adapter -> do
        Util.adapterTarget adapter `H.shouldBe` avroPrim Avro.PrimitiveNull
        let term = Core.TermUnit
        case Util.coderEncode (Util.adapterCoder adapter) emptyContext term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right json -> do
            json `H.shouldBe` Json.ValueNull
            case Util.coderDecode (Util.adapterCoder adapter) emptyContext json of
              Left e -> H.expectationFailure $ "decode failed: " ++ show e
              Right term' -> term' `H.shouldBe` term


-- Helper for reading JSON data files (reuses the same parser as readAvscFile)
readJsonFile :: FilePath -> IO Json.Value
readJsonFile = readAvscFile
