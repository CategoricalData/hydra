# Note: this is an automatically generated file. Do not edit.

r"""Test case types for the bidirectional Avro coder."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.avro.schema
import hydra.core
import hydra.json.model

@dataclass(frozen=True)
class TypeLevelForwardTestCase:
    r"""A test case which maps an Avro schema to a Hydra type and compares the result."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    schema: Annotated[hydra.avro.schema.Schema, "The input Avro schema"]
    type: Annotated[hydra.core.Type, "The expected Hydra type"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.TypeLevelForwardTestCase")
    DESCRIPTION = hydra.core.Name("description")
    SCHEMA = hydra.core.Name("schema")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class TypeLevelReverseTestCase:
    r"""A test case which maps a Hydra type to an Avro schema and compares the result."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    type: Annotated[hydra.core.Type, "The input Hydra type"]
    schema: Annotated[hydra.avro.schema.Schema, "The expected Avro schema"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.TypeLevelReverseTestCase")
    DESCRIPTION = hydra.core.Name("description")
    TYPE = hydra.core.Name("type")
    SCHEMA = hydra.core.Name("schema")

@dataclass(frozen=True)
class TypeLevelRoundTripAvroTestCase:
    r"""A test case which maps an Avro schema to a Hydra type and back, verifying the result."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    schema: Annotated[hydra.avro.schema.Schema, "The original Avro schema"]
    expected_schema: Annotated[hydra.avro.schema.Schema, "The expected Avro schema after the round-trip, which may differ from the original due to normalization or information loss"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.TypeLevelRoundTripAvroTestCase")
    DESCRIPTION = hydra.core.Name("description")
    SCHEMA = hydra.core.Name("schema")
    EXPECTED_SCHEMA = hydra.core.Name("expectedSchema")

@dataclass(frozen=True)
class TypeLevelRoundTripHydraTestCase:
    r"""A test case which maps a Hydra type to an Avro schema and back, verifying the result."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    type: Annotated[hydra.core.Type, "The original Hydra type"]
    expected_type: Annotated[hydra.core.Type, "The expected Hydra type after the round-trip, which may differ from the original due to normalization or information loss"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.TypeLevelRoundTripHydraTestCase")
    DESCRIPTION = hydra.core.Name("description")
    TYPE = hydra.core.Name("type")
    EXPECTED_TYPE = hydra.core.Name("expectedType")

@dataclass(frozen=True)
class TermLevelForwardTestCase:
    r"""A test case which encodes a JSON value as a Hydra term using an Avro schema."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    schema: Annotated[hydra.avro.schema.Schema, "The Avro schema describing the JSON value"]
    json: Annotated[hydra.json.model.Value, "The input JSON value"]
    term: Annotated[hydra.core.Term, "The expected Hydra term"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.TermLevelForwardTestCase")
    DESCRIPTION = hydra.core.Name("description")
    SCHEMA = hydra.core.Name("schema")
    JSON = hydra.core.Name("json")
    TERM = hydra.core.Name("term")

@dataclass(frozen=True)
class TermLevelReverseTestCase:
    r"""A test case which decodes a Hydra term to a JSON value using an Avro schema."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    schema: Annotated[hydra.avro.schema.Schema, "The Avro schema describing the expected JSON value"]
    term: Annotated[hydra.core.Term, "The input Hydra term"]
    json: Annotated[hydra.json.model.Value, "The expected JSON value"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.TermLevelReverseTestCase")
    DESCRIPTION = hydra.core.Name("description")
    SCHEMA = hydra.core.Name("schema")
    TERM = hydra.core.Name("term")
    JSON = hydra.core.Name("json")

@dataclass(frozen=True)
class TermLevelRoundTripJsonTestCase:
    r"""A test case which encodes a JSON value as a Hydra term and decodes it back, verifying the result."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    schema: Annotated[hydra.avro.schema.Schema, "The Avro schema describing the JSON value"]
    json: Annotated[hydra.json.model.Value, "The original JSON value"]
    expected_json: Annotated[hydra.json.model.Value, "The expected JSON value after the round-trip, which may differ from the original due to normalization"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.TermLevelRoundTripJsonTestCase")
    DESCRIPTION = hydra.core.Name("description")
    SCHEMA = hydra.core.Name("schema")
    JSON = hydra.core.Name("json")
    EXPECTED_JSON = hydra.core.Name("expectedJson")

@dataclass(frozen=True)
class TermLevelRoundTripTermTestCase:
    r"""A test case which decodes a Hydra term to JSON and encodes it back, verifying the result."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    type: Annotated[hydra.core.Type, "The Hydra type of the term"]
    term: Annotated[hydra.core.Term, "The original Hydra term"]
    expected_term: Annotated[hydra.core.Term, "The expected Hydra term after the round-trip, which may differ from the original due to normalization"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.TermLevelRoundTripTermTestCase")
    DESCRIPTION = hydra.core.Name("description")
    TYPE = hydra.core.Name("type")
    TERM = hydra.core.Name("term")
    EXPECTED_TERM = hydra.core.Name("expectedTerm")

@dataclass(frozen=True)
class UnionTestCase:
    r"""A test case for union type encoding and decoding, covering the various strategies for representing unions in Avro."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    hydra_type: Annotated[hydra.core.Type, "The Hydra union type"]
    avro_schema: Annotated[hydra.avro.schema.Schema, "The expected Avro schema (enum for all-unit unions, record with optional fields for general unions)"]
    term_pairs: Annotated[frozenlist[tuple[hydra.core.Term, hydra.json.model.Value]], "Pairs of corresponding Hydra terms and JSON values for this union"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.UnionTestCase")
    DESCRIPTION = hydra.core.Name("description")
    HYDRA_TYPE = hydra.core.Name("hydraType")
    AVRO_SCHEMA = hydra.core.Name("avroSchema")
    TERM_PAIRS = hydra.core.Name("termPairs")

@dataclass(frozen=True)
class NameMappingTestCase:
    r"""A test case for bidirectional name mapping between Hydra and Avro."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    hydra_name: Annotated[hydra.core.Name, "The Hydra name"]
    avro_name: Annotated[str, "The expected Avro qualified name (dotted string)"]
    avro_namespace: Annotated[Maybe[str], "The expected Avro namespace, if any"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.NameMappingTestCase")
    DESCRIPTION = hydra.core.Name("description")
    HYDRA_NAME = hydra.core.Name("hydraName")
    AVRO_NAME = hydra.core.Name("avroName")
    AVRO_NAMESPACE = hydra.core.Name("avroNamespace")

@dataclass(frozen=True)
class LossinessTestCase:
    r"""A test case which verifies that lossy conversions stash original information in annotations."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    original_schema: Annotated[hydra.avro.schema.Schema, "The original Avro schema containing information that may be lost"]
    hydra_type: Annotated[hydra.core.Type, "The resulting Hydra type, which should carry annotations for any lost information"]
    recovered_schema: Annotated[hydra.avro.schema.Schema, "The Avro schema recovered from the annotated Hydra type"]
    is_lossy: Annotated[bool, "Whether the adapter reports this conversion as lossy"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.LossinessTestCase")
    DESCRIPTION = hydra.core.Name("description")
    ORIGINAL_SCHEMA = hydra.core.Name("originalSchema")
    HYDRA_TYPE = hydra.core.Name("hydraType")
    RECOVERED_SCHEMA = hydra.core.Name("recoveredSchema")
    IS_LOSSY = hydra.core.Name("isLossy")

@dataclass(frozen=True)
class SchemaSerializationTestCase:
    r"""A test case for Avro schema serialization to and from JSON."""

    description: Annotated[str, "A human-readable description of what this test case covers"]
    schema: Annotated[hydra.avro.schema.Schema, "The Avro schema"]
    json: Annotated[hydra.json.model.Value, "The expected JSON representation of the schema"]

    TYPE_ = hydra.core.Name("hydra.avro.testing.SchemaSerializationTestCase")
    DESCRIPTION = hydra.core.Name("description")
    SCHEMA = hydra.core.Name("schema")
    JSON = hydra.core.Name("json")

class AvroTestCaseTypeLevelForward(Node["TypeLevelForwardTestCase"]):
    r"""Type-level forward mapping (Avro Schema -> Hydra Type)"""

class AvroTestCaseTypeLevelReverse(Node["TypeLevelReverseTestCase"]):
    r"""Type-level reverse mapping (Hydra Type -> Avro Schema)"""

class AvroTestCaseTypeLevelRoundTripAvro(Node["TypeLevelRoundTripAvroTestCase"]):
    r"""Type-level round-trip starting from Avro (Avro -> Hydra -> Avro)"""

class AvroTestCaseTypeLevelRoundTripHydra(Node["TypeLevelRoundTripHydraTestCase"]):
    r"""Type-level round-trip starting from Hydra (Hydra -> Avro -> Hydra)"""

class AvroTestCaseTermLevelForward(Node["TermLevelForwardTestCase"]):
    r"""Term-level forward mapping (JSON -> Hydra Term)"""

class AvroTestCaseTermLevelReverse(Node["TermLevelReverseTestCase"]):
    r"""Term-level reverse mapping (Hydra Term -> JSON)"""

class AvroTestCaseTermLevelRoundTripJson(Node["TermLevelRoundTripJsonTestCase"]):
    r"""Term-level round-trip starting from JSON (JSON -> Term -> JSON)"""

class AvroTestCaseTermLevelRoundTripTerm(Node["TermLevelRoundTripTermTestCase"]):
    r"""Term-level round-trip starting from a term (Term -> JSON -> Term)"""

class AvroTestCaseUnion(Node["UnionTestCase"]):
    r"""Union-specific encoding and decoding"""

class AvroTestCaseNameMapping(Node["NameMappingTestCase"]):
    r"""Name mapping between Hydra and Avro"""

class AvroTestCaseLossiness(Node["LossinessTestCase"]):
    r"""Lossy conversion with annotation stashing"""

class AvroTestCaseSchemaSerialization(Node["SchemaSerializationTestCase"]):
    r"""Schema serialization to and from JSON"""

class _AvroTestCaseMeta(type):
    def __getitem__(cls, item):
        return object

# A test case for the bidirectional Avro coder.
class AvroTestCase(metaclass=_AvroTestCaseMeta):
    r"""AvroTestCaseTypeLevelForward | AvroTestCaseTypeLevelReverse | AvroTestCaseTypeLevelRoundTripAvro | AvroTestCaseTypeLevelRoundTripHydra | AvroTestCaseTermLevelForward | AvroTestCaseTermLevelReverse | AvroTestCaseTermLevelRoundTripJson | AvroTestCaseTermLevelRoundTripTerm | AvroTestCaseUnion | AvroTestCaseNameMapping | AvroTestCaseLossiness | AvroTestCaseSchemaSerialization"""

    TYPE_ = hydra.core.Name("hydra.avro.testing.AvroTestCase")
    TYPE_LEVEL_FORWARD = hydra.core.Name("typeLevelForward")
    TYPE_LEVEL_REVERSE = hydra.core.Name("typeLevelReverse")
    TYPE_LEVEL_ROUND_TRIP_AVRO = hydra.core.Name("typeLevelRoundTripAvro")
    TYPE_LEVEL_ROUND_TRIP_HYDRA = hydra.core.Name("typeLevelRoundTripHydra")
    TERM_LEVEL_FORWARD = hydra.core.Name("termLevelForward")
    TERM_LEVEL_REVERSE = hydra.core.Name("termLevelReverse")
    TERM_LEVEL_ROUND_TRIP_JSON = hydra.core.Name("termLevelRoundTripJson")
    TERM_LEVEL_ROUND_TRIP_TERM = hydra.core.Name("termLevelRoundTripTerm")
    UNION = hydra.core.Name("union")
    NAME_MAPPING = hydra.core.Name("nameMapping")
    LOSSINESS = hydra.core.Name("lossiness")
    SCHEMA_SERIALIZATION = hydra.core.Name("schemaSerialization")
