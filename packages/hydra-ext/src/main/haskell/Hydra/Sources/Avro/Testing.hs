-- | Test case types for the bidirectional Avro coder

module Hydra.Sources.Avro.Testing where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Json.Model        as JsonModel
import qualified Hydra.Sources.Avro.Schema   as AvroSchema


ns :: Namespace
ns = Namespace "hydra.avro.testing"

define :: String -> Type -> Binding
define = defineType ns

avro :: String -> Type
avro = typeref AvroSchema.ns

json :: String -> Type
json = typeref JsonModel.ns

local :: String -> Type
local = typeref ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [] [Core.ns, AvroSchema.ns, JsonModel.ns] $
    Just "Test case types for the bidirectional Avro coder"
  where
    definitions = [
      typeLevelForwardTestCase,
      typeLevelReverseTestCase,
      typeLevelRoundTripAvroTestCase,
      typeLevelRoundTripHydraTestCase,
      termLevelForwardTestCase,
      termLevelReverseTestCase,
      termLevelRoundTripJsonTestCase,
      termLevelRoundTripTermTestCase,
      unionTestCase,
      nameMappingTestCase,
      lossinessTestCase,
      schemaSerializationTestCase,
      avroTestCase]

-- | Category 1: Type-level forward (Avro Schema -> Hydra Type)
typeLevelForwardTestCase :: Binding
typeLevelForwardTestCase = define "TypeLevelForwardTestCase" $
  doc "A test case which maps an Avro schema to a Hydra type and compares the result" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "schema">:
      doc "The input Avro schema"
      (avro "Schema"),
    "type">:
      doc "The expected Hydra type"
      Core.type_]

-- | Category 2: Type-level reverse (Hydra Type -> Avro Schema)
typeLevelReverseTestCase :: Binding
typeLevelReverseTestCase = define "TypeLevelReverseTestCase" $
  doc "A test case which maps a Hydra type to an Avro schema and compares the result" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "type">:
      doc "The input Hydra type"
      Core.type_,
    "schema">:
      doc "The expected Avro schema"
      (avro "Schema")]

-- | Category 3: Type-level round-trip starting from Avro (Avro -> Hydra -> Avro)
typeLevelRoundTripAvroTestCase :: Binding
typeLevelRoundTripAvroTestCase = define "TypeLevelRoundTripAvroTestCase" $
  doc "A test case which maps an Avro schema to a Hydra type and back, verifying the result" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "schema">:
      doc "The original Avro schema"
      (avro "Schema"),
    "expectedSchema">:
      doc "The expected Avro schema after the round-trip, which may differ from the original due to normalization or information loss"
      (avro "Schema")]

-- | Category 4: Type-level round-trip starting from Hydra (Hydra -> Avro -> Hydra)
typeLevelRoundTripHydraTestCase :: Binding
typeLevelRoundTripHydraTestCase = define "TypeLevelRoundTripHydraTestCase" $
  doc "A test case which maps a Hydra type to an Avro schema and back, verifying the result" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "type">:
      doc "The original Hydra type"
      Core.type_,
    "expectedType">:
      doc "The expected Hydra type after the round-trip, which may differ from the original due to normalization or information loss"
      Core.type_]

-- | Category 5: Term-level forward (JSON -> Hydra Term)
termLevelForwardTestCase :: Binding
termLevelForwardTestCase = define "TermLevelForwardTestCase" $
  doc "A test case which encodes a JSON value as a Hydra term using an Avro schema" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "schema">:
      doc "The Avro schema describing the JSON value"
      (avro "Schema"),
    "json">:
      doc "The input JSON value"
      (json "Value"),
    "term">:
      doc "The expected Hydra term"
      Core.term]

-- | Category 6: Term-level reverse (Hydra Term -> JSON)
termLevelReverseTestCase :: Binding
termLevelReverseTestCase = define "TermLevelReverseTestCase" $
  doc "A test case which decodes a Hydra term to a JSON value using an Avro schema" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "schema">:
      doc "The Avro schema describing the expected JSON value"
      (avro "Schema"),
    "term">:
      doc "The input Hydra term"
      Core.term,
    "json">:
      doc "The expected JSON value"
      (json "Value")]

-- | Category 7: Term-level round-trip starting from JSON (JSON -> Term -> JSON)
termLevelRoundTripJsonTestCase :: Binding
termLevelRoundTripJsonTestCase = define "TermLevelRoundTripJsonTestCase" $
  doc "A test case which encodes a JSON value as a Hydra term and decodes it back, verifying the result" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "schema">:
      doc "The Avro schema describing the JSON value"
      (avro "Schema"),
    "json">:
      doc "The original JSON value"
      (json "Value"),
    "expectedJson">:
      doc "The expected JSON value after the round-trip, which may differ from the original due to normalization"
      (json "Value")]

-- | Category 8: Term-level round-trip starting from a term (Term -> JSON -> Term)
termLevelRoundTripTermTestCase :: Binding
termLevelRoundTripTermTestCase = define "TermLevelRoundTripTermTestCase" $
  doc "A test case which decodes a Hydra term to JSON and encodes it back, verifying the result" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "type">:
      doc "The Hydra type of the term"
      Core.type_,
    "term">:
      doc "The original Hydra term"
      Core.term,
    "expectedTerm">:
      doc "The expected Hydra term after the round-trip, which may differ from the original due to normalization"
      Core.term]

-- | Category 9: Union-specific tests
unionTestCase :: Binding
unionTestCase = define "UnionTestCase" $
  doc "A test case for union type encoding and decoding, covering the various strategies for representing unions in Avro" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "hydraType">:
      doc "The Hydra union type"
      Core.type_,
    "avroSchema">:
      doc "The expected Avro schema (enum for all-unit unions, record with optional fields for general unions)"
      (avro "Schema"),
    "termPairs">:
      doc "Pairs of corresponding Hydra terms and JSON values for this union"
      (T.list (T.pair Core.term (json "Value")))]

-- | Category 10: Name mapping tests
nameMappingTestCase :: Binding
nameMappingTestCase = define "NameMappingTestCase" $
  doc "A test case for bidirectional name mapping between Hydra and Avro" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "hydraName">:
      doc "The Hydra name"
      Core.name,
    "avroName">:
      doc "The expected Avro qualified name (dotted string)"
      T.string,
    "avroNamespace">:
      doc "The expected Avro namespace, if any"
      (T.optional T.string)]

-- | Category 11: Lossiness and annotation tests
lossinessTestCase :: Binding
lossinessTestCase = define "LossinessTestCase" $
  doc "A test case which verifies that lossy conversions stash original information in annotations" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "originalSchema">:
      doc "The original Avro schema containing information that may be lost"
      (avro "Schema"),
    "hydraType">:
      doc "The resulting Hydra type, which should carry annotations for any lost information"
      Core.type_,
    "recoveredSchema">:
      doc "The Avro schema recovered from the annotated Hydra type"
      (avro "Schema"),
    "isLossy">:
      doc "Whether the adapter reports this conversion as lossy"
      T.boolean]

-- | Category 12: Schema serialization (Avro Schema <-> JSON)
schemaSerializationTestCase :: Binding
schemaSerializationTestCase = define "SchemaSerializationTestCase" $
  doc "A test case for Avro schema serialization to and from JSON" $
  T.record [
    "description">:
      doc "A human-readable description of what this test case covers"
      T.string,
    "schema">:
      doc "The Avro schema"
      (avro "Schema"),
    "json">:
      doc "The expected JSON representation of the schema"
      (json "Value")]

-- | The union of all Avro test case types
avroTestCase :: Binding
avroTestCase = define "AvroTestCase" $
  doc "A test case for the bidirectional Avro coder" $
  T.union [
    "typeLevelForward">:
      doc "Type-level forward mapping (Avro Schema -> Hydra Type)"
      (local "TypeLevelForwardTestCase"),
    "typeLevelReverse">:
      doc "Type-level reverse mapping (Hydra Type -> Avro Schema)"
      (local "TypeLevelReverseTestCase"),
    "typeLevelRoundTripAvro">:
      doc "Type-level round-trip starting from Avro (Avro -> Hydra -> Avro)"
      (local "TypeLevelRoundTripAvroTestCase"),
    "typeLevelRoundTripHydra">:
      doc "Type-level round-trip starting from Hydra (Hydra -> Avro -> Hydra)"
      (local "TypeLevelRoundTripHydraTestCase"),
    "termLevelForward">:
      doc "Term-level forward mapping (JSON -> Hydra Term)"
      (local "TermLevelForwardTestCase"),
    "termLevelReverse">:
      doc "Term-level reverse mapping (Hydra Term -> JSON)"
      (local "TermLevelReverseTestCase"),
    "termLevelRoundTripJson">:
      doc "Term-level round-trip starting from JSON (JSON -> Term -> JSON)"
      (local "TermLevelRoundTripJsonTestCase"),
    "termLevelRoundTripTerm">:
      doc "Term-level round-trip starting from a term (Term -> JSON -> Term)"
      (local "TermLevelRoundTripTermTestCase"),
    "union">:
      doc "Union-specific encoding and decoding"
      (local "UnionTestCase"),
    "nameMapping">:
      doc "Name mapping between Hydra and Avro"
      (local "NameMappingTestCase"),
    "lossiness">:
      doc "Lossy conversion with annotation stashing"
      (local "LossinessTestCase"),
    "schemaSerialization">:
      doc "Schema serialization to and from JSON"
      (local "SchemaSerializationTestCase")]
