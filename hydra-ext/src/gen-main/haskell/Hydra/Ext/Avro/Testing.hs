-- Note: this is an automatically generated file. Do not edit.

-- | Test case types for the bidirectional Avro coder

module Hydra.Ext.Avro.Testing where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Schema
import qualified Hydra.Json.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A test case which maps an Avro schema to a Hydra type and compares the result
data TypeLevelForwardTestCase =
  TypeLevelForwardTestCase {
    -- | A human-readable description of what this test case covers
    typeLevelForwardTestCaseDescription :: String,
    -- | The input Avro schema
    typeLevelForwardTestCaseSchema :: Schema.Schema,
    -- | The expected Hydra type
    typeLevelForwardTestCaseType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_TypeLevelForwardTestCase = Core.Name "hydra.ext.avro.testing.TypeLevelForwardTestCase"

_TypeLevelForwardTestCase_description = Core.Name "description"

_TypeLevelForwardTestCase_schema = Core.Name "schema"

_TypeLevelForwardTestCase_type = Core.Name "type"

-- | A test case which maps a Hydra type to an Avro schema and compares the result
data TypeLevelReverseTestCase =
  TypeLevelReverseTestCase {
    -- | A human-readable description of what this test case covers
    typeLevelReverseTestCaseDescription :: String,
    -- | The input Hydra type
    typeLevelReverseTestCaseType :: Core.Type,
    -- | The expected Avro schema
    typeLevelReverseTestCaseSchema :: Schema.Schema}
  deriving (Eq, Ord, Read, Show)

_TypeLevelReverseTestCase = Core.Name "hydra.ext.avro.testing.TypeLevelReverseTestCase"

_TypeLevelReverseTestCase_description = Core.Name "description"

_TypeLevelReverseTestCase_type = Core.Name "type"

_TypeLevelReverseTestCase_schema = Core.Name "schema"

-- | A test case which maps an Avro schema to a Hydra type and back, verifying the result
data TypeLevelRoundTripAvroTestCase =
  TypeLevelRoundTripAvroTestCase {
    -- | A human-readable description of what this test case covers
    typeLevelRoundTripAvroTestCaseDescription :: String,
    -- | The original Avro schema
    typeLevelRoundTripAvroTestCaseSchema :: Schema.Schema,
    -- | The expected Avro schema after the round-trip, which may differ from the original due to normalization or information loss
    typeLevelRoundTripAvroTestCaseExpectedSchema :: Schema.Schema}
  deriving (Eq, Ord, Read, Show)

_TypeLevelRoundTripAvroTestCase = Core.Name "hydra.ext.avro.testing.TypeLevelRoundTripAvroTestCase"

_TypeLevelRoundTripAvroTestCase_description = Core.Name "description"

_TypeLevelRoundTripAvroTestCase_schema = Core.Name "schema"

_TypeLevelRoundTripAvroTestCase_expectedSchema = Core.Name "expectedSchema"

-- | A test case which maps a Hydra type to an Avro schema and back, verifying the result
data TypeLevelRoundTripHydraTestCase =
  TypeLevelRoundTripHydraTestCase {
    -- | A human-readable description of what this test case covers
    typeLevelRoundTripHydraTestCaseDescription :: String,
    -- | The original Hydra type
    typeLevelRoundTripHydraTestCaseType :: Core.Type,
    -- | The expected Hydra type after the round-trip, which may differ from the original due to normalization or information loss
    typeLevelRoundTripHydraTestCaseExpectedType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_TypeLevelRoundTripHydraTestCase = Core.Name "hydra.ext.avro.testing.TypeLevelRoundTripHydraTestCase"

_TypeLevelRoundTripHydraTestCase_description = Core.Name "description"

_TypeLevelRoundTripHydraTestCase_type = Core.Name "type"

_TypeLevelRoundTripHydraTestCase_expectedType = Core.Name "expectedType"

-- | A test case which encodes a JSON value as a Hydra term using an Avro schema
data TermLevelForwardTestCase =
  TermLevelForwardTestCase {
    -- | A human-readable description of what this test case covers
    termLevelForwardTestCaseDescription :: String,
    -- | The Avro schema describing the JSON value
    termLevelForwardTestCaseSchema :: Schema.Schema,
    -- | The input JSON value
    termLevelForwardTestCaseJson :: Model.Value,
    -- | The expected Hydra term
    termLevelForwardTestCaseTerm :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_TermLevelForwardTestCase = Core.Name "hydra.ext.avro.testing.TermLevelForwardTestCase"

_TermLevelForwardTestCase_description = Core.Name "description"

_TermLevelForwardTestCase_schema = Core.Name "schema"

_TermLevelForwardTestCase_json = Core.Name "json"

_TermLevelForwardTestCase_term = Core.Name "term"

-- | A test case which decodes a Hydra term to a JSON value using an Avro schema
data TermLevelReverseTestCase =
  TermLevelReverseTestCase {
    -- | A human-readable description of what this test case covers
    termLevelReverseTestCaseDescription :: String,
    -- | The Avro schema describing the expected JSON value
    termLevelReverseTestCaseSchema :: Schema.Schema,
    -- | The input Hydra term
    termLevelReverseTestCaseTerm :: Core.Term,
    -- | The expected JSON value
    termLevelReverseTestCaseJson :: Model.Value}
  deriving (Eq, Ord, Read, Show)

_TermLevelReverseTestCase = Core.Name "hydra.ext.avro.testing.TermLevelReverseTestCase"

_TermLevelReverseTestCase_description = Core.Name "description"

_TermLevelReverseTestCase_schema = Core.Name "schema"

_TermLevelReverseTestCase_term = Core.Name "term"

_TermLevelReverseTestCase_json = Core.Name "json"

-- | A test case which encodes a JSON value as a Hydra term and decodes it back, verifying the result
data TermLevelRoundTripJsonTestCase =
  TermLevelRoundTripJsonTestCase {
    -- | A human-readable description of what this test case covers
    termLevelRoundTripJsonTestCaseDescription :: String,
    -- | The Avro schema describing the JSON value
    termLevelRoundTripJsonTestCaseSchema :: Schema.Schema,
    -- | The original JSON value
    termLevelRoundTripJsonTestCaseJson :: Model.Value,
    -- | The expected JSON value after the round-trip, which may differ from the original due to normalization
    termLevelRoundTripJsonTestCaseExpectedJson :: Model.Value}
  deriving (Eq, Ord, Read, Show)

_TermLevelRoundTripJsonTestCase = Core.Name "hydra.ext.avro.testing.TermLevelRoundTripJsonTestCase"

_TermLevelRoundTripJsonTestCase_description = Core.Name "description"

_TermLevelRoundTripJsonTestCase_schema = Core.Name "schema"

_TermLevelRoundTripJsonTestCase_json = Core.Name "json"

_TermLevelRoundTripJsonTestCase_expectedJson = Core.Name "expectedJson"

-- | A test case which decodes a Hydra term to JSON and encodes it back, verifying the result
data TermLevelRoundTripTermTestCase =
  TermLevelRoundTripTermTestCase {
    -- | A human-readable description of what this test case covers
    termLevelRoundTripTermTestCaseDescription :: String,
    -- | The Hydra type of the term
    termLevelRoundTripTermTestCaseType :: Core.Type,
    -- | The original Hydra term
    termLevelRoundTripTermTestCaseTerm :: Core.Term,
    -- | The expected Hydra term after the round-trip, which may differ from the original due to normalization
    termLevelRoundTripTermTestCaseExpectedTerm :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_TermLevelRoundTripTermTestCase = Core.Name "hydra.ext.avro.testing.TermLevelRoundTripTermTestCase"

_TermLevelRoundTripTermTestCase_description = Core.Name "description"

_TermLevelRoundTripTermTestCase_type = Core.Name "type"

_TermLevelRoundTripTermTestCase_term = Core.Name "term"

_TermLevelRoundTripTermTestCase_expectedTerm = Core.Name "expectedTerm"

-- | A test case for union type encoding and decoding, covering the various strategies for representing unions in Avro
data UnionTestCase =
  UnionTestCase {
    -- | A human-readable description of what this test case covers
    unionTestCaseDescription :: String,
    -- | The Hydra union type
    unionTestCaseHydraType :: Core.Type,
    -- | The expected Avro schema (enum for all-unit unions, record with optional fields for general unions)
    unionTestCaseAvroSchema :: Schema.Schema,
    -- | Pairs of corresponding Hydra terms and JSON values for this union
    unionTestCaseTermPairs :: [(Core.Term, Model.Value)]}
  deriving (Eq, Ord, Read, Show)

_UnionTestCase = Core.Name "hydra.ext.avro.testing.UnionTestCase"

_UnionTestCase_description = Core.Name "description"

_UnionTestCase_hydraType = Core.Name "hydraType"

_UnionTestCase_avroSchema = Core.Name "avroSchema"

_UnionTestCase_termPairs = Core.Name "termPairs"

-- | A test case for bidirectional name mapping between Hydra and Avro
data NameMappingTestCase =
  NameMappingTestCase {
    -- | A human-readable description of what this test case covers
    nameMappingTestCaseDescription :: String,
    -- | The Hydra name
    nameMappingTestCaseHydraName :: Core.Name,
    -- | The expected Avro qualified name (dotted string)
    nameMappingTestCaseAvroName :: String,
    -- | The expected Avro namespace, if any
    nameMappingTestCaseAvroNamespace :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_NameMappingTestCase = Core.Name "hydra.ext.avro.testing.NameMappingTestCase"

_NameMappingTestCase_description = Core.Name "description"

_NameMappingTestCase_hydraName = Core.Name "hydraName"

_NameMappingTestCase_avroName = Core.Name "avroName"

_NameMappingTestCase_avroNamespace = Core.Name "avroNamespace"

-- | A test case which verifies that lossy conversions stash original information in annotations
data LossinessTestCase =
  LossinessTestCase {
    -- | A human-readable description of what this test case covers
    lossinessTestCaseDescription :: String,
    -- | The original Avro schema containing information that may be lost
    lossinessTestCaseOriginalSchema :: Schema.Schema,
    -- | The resulting Hydra type, which should carry annotations for any lost information
    lossinessTestCaseHydraType :: Core.Type,
    -- | The Avro schema recovered from the annotated Hydra type
    lossinessTestCaseRecoveredSchema :: Schema.Schema,
    -- | Whether the adapter reports this conversion as lossy
    lossinessTestCaseIsLossy :: Bool}
  deriving (Eq, Ord, Read, Show)

_LossinessTestCase = Core.Name "hydra.ext.avro.testing.LossinessTestCase"

_LossinessTestCase_description = Core.Name "description"

_LossinessTestCase_originalSchema = Core.Name "originalSchema"

_LossinessTestCase_hydraType = Core.Name "hydraType"

_LossinessTestCase_recoveredSchema = Core.Name "recoveredSchema"

_LossinessTestCase_isLossy = Core.Name "isLossy"

-- | A test case for Avro schema serialization to and from JSON
data SchemaSerializationTestCase =
  SchemaSerializationTestCase {
    -- | A human-readable description of what this test case covers
    schemaSerializationTestCaseDescription :: String,
    -- | The Avro schema
    schemaSerializationTestCaseSchema :: Schema.Schema,
    -- | The expected JSON representation of the schema
    schemaSerializationTestCaseJson :: Model.Value}
  deriving (Eq, Ord, Read, Show)

_SchemaSerializationTestCase = Core.Name "hydra.ext.avro.testing.SchemaSerializationTestCase"

_SchemaSerializationTestCase_description = Core.Name "description"

_SchemaSerializationTestCase_schema = Core.Name "schema"

_SchemaSerializationTestCase_json = Core.Name "json"

-- | A test case for the bidirectional Avro coder
data AvroTestCase =
  -- | Type-level forward mapping (Avro Schema -> Hydra Type)
  AvroTestCaseTypeLevelForward TypeLevelForwardTestCase |
  -- | Type-level reverse mapping (Hydra Type -> Avro Schema)
  AvroTestCaseTypeLevelReverse TypeLevelReverseTestCase |
  -- | Type-level round-trip starting from Avro (Avro -> Hydra -> Avro)
  AvroTestCaseTypeLevelRoundTripAvro TypeLevelRoundTripAvroTestCase |
  -- | Type-level round-trip starting from Hydra (Hydra -> Avro -> Hydra)
  AvroTestCaseTypeLevelRoundTripHydra TypeLevelRoundTripHydraTestCase |
  -- | Term-level forward mapping (JSON -> Hydra Term)
  AvroTestCaseTermLevelForward TermLevelForwardTestCase |
  -- | Term-level reverse mapping (Hydra Term -> JSON)
  AvroTestCaseTermLevelReverse TermLevelReverseTestCase |
  -- | Term-level round-trip starting from JSON (JSON -> Term -> JSON)
  AvroTestCaseTermLevelRoundTripJson TermLevelRoundTripJsonTestCase |
  -- | Term-level round-trip starting from a term (Term -> JSON -> Term)
  AvroTestCaseTermLevelRoundTripTerm TermLevelRoundTripTermTestCase |
  -- | Union-specific encoding and decoding
  AvroTestCaseUnion UnionTestCase |
  -- | Name mapping between Hydra and Avro
  AvroTestCaseNameMapping NameMappingTestCase |
  -- | Lossy conversion with annotation stashing
  AvroTestCaseLossiness LossinessTestCase |
  -- | Schema serialization to and from JSON
  AvroTestCaseSchemaSerialization SchemaSerializationTestCase
  deriving (Eq, Ord, Read, Show)

_AvroTestCase = Core.Name "hydra.ext.avro.testing.AvroTestCase"

_AvroTestCase_typeLevelForward = Core.Name "typeLevelForward"

_AvroTestCase_typeLevelReverse = Core.Name "typeLevelReverse"

_AvroTestCase_typeLevelRoundTripAvro = Core.Name "typeLevelRoundTripAvro"

_AvroTestCase_typeLevelRoundTripHydra = Core.Name "typeLevelRoundTripHydra"

_AvroTestCase_termLevelForward = Core.Name "termLevelForward"

_AvroTestCase_termLevelReverse = Core.Name "termLevelReverse"

_AvroTestCase_termLevelRoundTripJson = Core.Name "termLevelRoundTripJson"

_AvroTestCase_termLevelRoundTripTerm = Core.Name "termLevelRoundTripTerm"

_AvroTestCase_union = Core.Name "union"

_AvroTestCase_nameMapping = Core.Name "nameMapping"

_AvroTestCase_lossiness = Core.Name "lossiness"

_AvroTestCase_schemaSerialization = Core.Name "schemaSerialization"
