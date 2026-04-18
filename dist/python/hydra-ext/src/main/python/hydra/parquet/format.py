# Note: this is an automatically generated file. Do not edit.

r"""A model for the Parquet format. Based on the Thrift-based specification at:
  https://github.com/apache/parquet-format/blob/master/src/main/thrift/parquet.thrift."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

class Type(Enum):
    r"""Types supported by Parquet.  These types are intended to be used in combination with the encodings to control the on disk storage format. For example INT16 is not included as a type since a good encoding of INT32 would handle this."""

    BOOLEAN = hydra.core.Name("boolean")

    INT32 = hydra.core.Name("int32")

    INT64 = hydra.core.Name("int64")

    FLOAT = hydra.core.Name("float")

    DOUBLE = hydra.core.Name("double")

    BYTE_ARRAY = hydra.core.Name("byteArray")

    FIXED_LEN_BYTE_ARRAY = hydra.core.Name("fixedLenByteArray")

Type.TYPE_ = hydra.core.Name("hydra.parquet.format.Type")

class FieldRepetitionType(Enum):
    r"""Representation of Schemas."""

    REQUIRED = hydra.core.Name("required")
    r"""This field is required (can not be null) and each record has exactly 1 value."""

    OPTIONAL = hydra.core.Name("optional")
    r"""The field is optional (can be null) and each record has 0 or 1 values."""

    REPEATED = hydra.core.Name("repeated")
    r"""The field is repeated and can contain 0 or more values"""

FieldRepetitionType.TYPE_ = hydra.core.Name("hydra.parquet.format.FieldRepetitionType")

@dataclass(frozen=True)
class Statistics:
    r"""Statistics per row group and per page. All fields are optional."""

    null_count: Maybe[int]
    distinct_count: Maybe[int]
    max_value: Annotated[Maybe[bytes], "Max value for the column, determined by its ColumnOrder. Values are encoded using PLAIN encoding, except that variable-length byte arrays do not include a length prefix."]
    min_value: Annotated[Maybe[bytes], "Max value for the column, determined by its ColumnOrder. Values are encoded using PLAIN encoding, except that variable-length byte arrays do not include a length prefix."]

    TYPE_ = hydra.core.Name("hydra.parquet.format.Statistics")
    NULL_COUNT = hydra.core.Name("nullCount")
    DISTINCT_COUNT = hydra.core.Name("distinctCount")
    MAX_VALUE = hydra.core.Name("maxValue")
    MIN_VALUE = hydra.core.Name("minValue")

@dataclass(frozen=True)
class DecimalType:
    r"""Decimal logical type annotation. To maintain forward-compatibility in v1, implementations using this logical type must also set scale and precision on the annotated SchemaElement. Allowed for physical types: INT32, INT64, FIXED, and BINARY."""

    scale: int
    precision: int

    TYPE_ = hydra.core.Name("hydra.parquet.format.DecimalType")
    SCALE = hydra.core.Name("scale")
    PRECISION = hydra.core.Name("precision")

class TimeUnit(Enum):
    MILLIS = hydra.core.Name("millis")

    MICROS = hydra.core.Name("micros")

    NANOS = hydra.core.Name("nanos")

TimeUnit.TYPE_ = hydra.core.Name("hydra.parquet.format.TimeUnit")

@dataclass(frozen=True)
class TimestampType:
    r"""Timestamp logical type annotation. Allowed for physical types: INT64."""

    is_adjusted_to_utc: bool
    unit: TimeUnit

    TYPE_ = hydra.core.Name("hydra.parquet.format.TimestampType")
    IS_ADJUSTED_TO_UTC = hydra.core.Name("isAdjustedToUtc")
    UNIT = hydra.core.Name("unit")

@dataclass(frozen=True)
class TimeType:
    r"""Time logical type annotation. Allowed for physical types: INT32 (millis), INT64 (micros, nanos)."""

    is_adjusted_to_utc: bool
    unit: TimeUnit

    TYPE_ = hydra.core.Name("hydra.parquet.format.TimeType")
    IS_ADJUSTED_TO_UTC = hydra.core.Name("isAdjustedToUtc")
    UNIT = hydra.core.Name("unit")

@dataclass(frozen=True)
class IntType:
    r"""Integer logical type annotation. bitWidth must be 8, 16, 32, or 64. Allowed for physical types: INT32, INT64."""

    bit_width: int
    is_signed: bool

    TYPE_ = hydra.core.Name("hydra.parquet.format.IntType")
    BIT_WIDTH = hydra.core.Name("bitWidth")
    IS_SIGNED = hydra.core.Name("isSigned")

class LogicalTypeString:
    r"""use ConvertedType UTF8"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LogicalTypeString)
    def __hash__(self):
        return hash("LogicalTypeString")

class LogicalTypeMap:
    r"""use ConvertedType MAP"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LogicalTypeMap)
    def __hash__(self):
        return hash("LogicalTypeMap")

class LogicalTypeList:
    r"""use ConvertedType LIST"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LogicalTypeList)
    def __hash__(self):
        return hash("LogicalTypeList")

class LogicalTypeEnum:
    r"""use ConvertedType ENUM"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LogicalTypeEnum)
    def __hash__(self):
        return hash("LogicalTypeEnum")

class LogicalTypeDecimal(Node["DecimalType"]):
    r"""use ConvertedType DECIMAL + SchemaElement.{scale, precision}"""

class LogicalTypeDate:
    r"""use ConvertedType DATE"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LogicalTypeDate)
    def __hash__(self):
        return hash("LogicalTypeDate")

class LogicalTypeTime(Node["TimeType"]):
    r"""use ConvertedType TIME_MICROS for TIME(isAdjustedToUTC = *, unit = MICROS). use ConvertedType TIME_MILLIS for TIME(isAdjustedToUTC = *, unit = MILLIS)"""

class LogicalTypeTimestamp(Node["TimestampType"]):
    r"""use ConvertedType TIMESTAMP_MICROS for TIMESTAMP(isAdjustedToUTC = *, unit = MICROS). use ConvertedType TIMESTAMP_MILLIS for TIMESTAMP(isAdjustedToUTC = *, unit = MILLIS)"""

class LogicalTypeInteger(Node["IntType"]):
    r"""use ConvertedType INT_* or UINT_*"""

class LogicalTypeUnknown:
    r"""no compatible ConvertedType"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LogicalTypeUnknown)
    def __hash__(self):
        return hash("LogicalTypeUnknown")

class LogicalTypeJson:
    r"""use ConvertedType JSON"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LogicalTypeJson)
    def __hash__(self):
        return hash("LogicalTypeJson")

class LogicalTypeBson:
    r"""use ConvertedType BSON"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LogicalTypeBson)
    def __hash__(self):
        return hash("LogicalTypeBson")

class LogicalTypeUuid:
    r"""no compatible ConvertedType"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LogicalTypeUuid)
    def __hash__(self):
        return hash("LogicalTypeUuid")

class _LogicalTypeMeta(type):
    def __getitem__(cls, item):
        return object

# LogicalType annotations to replace ConvertedType. To maintain compatibility, implementations using LogicalType for a SchemaElement aust also set the corresponding ConvertedType (if any) from the following table.
class LogicalType(metaclass=_LogicalTypeMeta):
    r"""LogicalTypeString | LogicalTypeMap | LogicalTypeList | LogicalTypeEnum | LogicalTypeDecimal | LogicalTypeDate | LogicalTypeTime | LogicalTypeTimestamp | LogicalTypeInteger | LogicalTypeUnknown | LogicalTypeJson | LogicalTypeBson | LogicalTypeUuid"""

    TYPE_ = hydra.core.Name("hydra.parquet.format.LogicalType")
    STRING = hydra.core.Name("string")
    MAP = hydra.core.Name("map")
    LIST = hydra.core.Name("list")
    ENUM = hydra.core.Name("enum")
    DECIMAL = hydra.core.Name("decimal")
    DATE = hydra.core.Name("date")
    TIME = hydra.core.Name("time")
    TIMESTAMP = hydra.core.Name("timestamp")
    INTEGER = hydra.core.Name("integer")
    UNKNOWN = hydra.core.Name("unknown")
    JSON = hydra.core.Name("json")
    BSON = hydra.core.Name("bson")
    UUID = hydra.core.Name("uuid")

@dataclass(frozen=True)
class SchemaElement:
    r"""Represents a element inside a schema definition.
    - if it is a group (inner node) then type is undefined and num_children is defined
    - if it is a primitive type (leaf) then type is defined and num_children is undefined
    the nodes are listed in depth first traversal order."""

    type: Annotated[Maybe[Type], "Data type for this field. Not set if the current element is a non-leaf node"]
    type_length: Annotated[Maybe[int], "If type is FIXED_LEN_BYTE_ARRAY, this is the byte length of the values. Otherwise, if specified, this is the maximum bit length to store any of the values. (e.g. a low cardinality INT col could have this set to 3).  Note that this is in the schema, and therefore fixed for the entire file."]
    repetition_type: Annotated[Maybe[FieldRepetitionType], "repetition of the field. The root of the schema does not have a repetition_type. All other nodes must have one"]
    name: Annotated[str, "Name of the field in the schema"]
    num_children: Annotated[Maybe[int], "Nested fields.  Since thrift does not support nested fields, the nesting is flattened to a single list by a depth-first traversal. The children count is used to construct the nested relationship. This field is not set when the element is a primitive type"]
    field_id: Annotated[Maybe[int], "When the original schema supports field ids, this will save the original field id in the parquet schema"]
    logical_type: Annotated[Maybe[LogicalType], "The logical type of this SchemaElement. LogicalType replaces ConvertedType, but ConvertedType is still required for some logical types to ensure forward-compatibility in format v1."]

    TYPE_ = hydra.core.Name("hydra.parquet.format.SchemaElement")
    TYPE = hydra.core.Name("type")
    TYPE_LENGTH = hydra.core.Name("typeLength")
    REPETITION_TYPE = hydra.core.Name("repetitionType")
    NAME = hydra.core.Name("name")
    NUM_CHILDREN = hydra.core.Name("numChildren")
    FIELD_ID = hydra.core.Name("fieldId")
    LOGICAL_TYPE = hydra.core.Name("logicalType")

class Encoding(Enum):
    r"""Encodings supported by Parquet.  Not all encodings are valid for all types.  These enums are also used to specify the encoding of definition and repetition levels. See the accompanying doc for the details of the more complicated encodings."""

    PLAIN = hydra.core.Name("plain")
    r"""Default encoding.
    BOOLEAN - 1 bit per value. 0 is false; 1 is true.
    INT32 - 4 bytes per value.  Stored as little-endian.
    INT64 - 8 bytes per value.  Stored as little-endian.
    FLOAT - 4 bytes per value.  IEEE. Stored as little-endian.
    DOUBLE - 8 bytes per value.  IEEE. Stored as little-endian.
    BYTE_ARRAY - 4 byte length stored as little endian, followed by bytes.
    FIXED_LEN_BYTE_ARRAY - Just the bytes."""

    RLE = hydra.core.Name("rle")
    r"""Group packed run length encoding. Usable for definition/repetition levels encoding and Booleans (on one bit: 0 is false; 1 is true.)"""

    BIT_PACKED = hydra.core.Name("bitPacked")
    r"""Bit packed encoding.  This can only be used if the data has a known max width.  Usable for definition/repetition levels encoding."""

    DELTA_BINARY_PACKED = hydra.core.Name("deltaBinaryPacked")
    r"""Delta encoding for integers. This can be used for int columns and works best on sorted data"""

    DELTA_LENGTH_BYTE_ARRAY = hydra.core.Name("deltaLengthByteArray")
    r"""Encoding for byte arrays to separate the length values and the data. The lengths are encoded using DELTA_BINARY_PACKED"""

    DELTA_BYTE_ARRAY = hydra.core.Name("deltaByteArray")
    r"""Incremental-encoded byte array. Prefix lengths are encoded using DELTA_BINARY_PACKED. Suffixes are stored as delta length byte arrays."""

    RLE_DICTIONARY = hydra.core.Name("rleDictionary")
    r"""Dictionary encoding: the ids are encoded using the RLE encoding"""

    BYTE_STREAM_SPLIT = hydra.core.Name("byteStreamSplit")
    r"""Encoding for floating-point data. K byte-streams are created where K is the size in bytes of the data type. The individual bytes of an FP value are scattered to the corresponding stream and the streams are concatenated. This itself does not reduce the size of the data but can lead to better compression afterwards."""

Encoding.TYPE_ = hydra.core.Name("hydra.parquet.format.Encoding")

class CompressionCodec(Enum):
    r"""Supported compression algorithms. Codecs added in format version X.Y can be read by readers based on X.Y and later. Codec support may vary between readers based on the format version and libraries available at runtime. See Compression.md for a detailed specification of these algorithms."""

    UNCOMPRESSED = hydra.core.Name("uncompressed")

    SNAPPY = hydra.core.Name("snappy")

    GZIP = hydra.core.Name("gzip")

    LZO = hydra.core.Name("lzo")

    BROTLI = hydra.core.Name("brotli")
    r"""Added in 2.4"""

    ZSTD = hydra.core.Name("zstd")
    r"""Added in 2.4"""

    LZ4_RAW = hydra.core.Name("lz4Raw")
    r"""Added in 2.9"""

CompressionCodec.TYPE_ = hydra.core.Name("hydra.parquet.format.CompressionCodec")

class PageType(Enum):
    DATA_PAGE = hydra.core.Name("dataPage")

    INDEX_PAGE = hydra.core.Name("indexPage")

    DICTIONARY_PAGE = hydra.core.Name("dictionaryPage")

    DATA_PAGE_V2 = hydra.core.Name("dataPageV2")

PageType.TYPE_ = hydra.core.Name("hydra.parquet.format.PageType")

class BoundaryOrder(Enum):
    r"""Enum to annotate whether lists of min/max elements inside ColumnIndex are ordered and if so, in which direction."""

    UNORDERED = hydra.core.Name("unordered")

    ASCENDING = hydra.core.Name("ascending")

    DESCENDING = hydra.core.Name("descending")

BoundaryOrder.TYPE_ = hydra.core.Name("hydra.parquet.format.BoundaryOrder")

@dataclass(frozen=True)
class DataPageHeader:
    r"""Data page header."""

    num_values: Annotated[int, "Number of values, including NULLs, in this data page."]
    encoding: Annotated[Encoding, "Encoding used for this data page"]
    definition_level_encoding: Annotated[Encoding, "Encoding used for definition levels"]
    repetition_level_encoding: Annotated[Encoding, "Encoding used for repetition levels"]
    statistics: Annotated[Maybe[Statistics], "Optional statistics for the data in this page"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.DataPageHeader")
    NUM_VALUES = hydra.core.Name("numValues")
    ENCODING = hydra.core.Name("encoding")
    DEFINITION_LEVEL_ENCODING = hydra.core.Name("definitionLevelEncoding")
    REPETITION_LEVEL_ENCODING = hydra.core.Name("repetitionLevelEncoding")
    STATISTICS = hydra.core.Name("statistics")

@dataclass(frozen=True)
class IndexPageHeader:
    TYPE_ = hydra.core.Name("hydra.parquet.format.IndexPageHeader")

@dataclass(frozen=True)
class DictionaryPageHeader:
    r"""The dictionary page must be placed at the first position of the column chunk if it is partly or completely dictionary encoded. At most one dictionary page can be placed in a column chunk."""

    num_values: Annotated[int, "Number of values in the dictionary"]
    encoding: Annotated[Encoding, "Encoding using this dictionary page"]
    is_sorted: Annotated[Maybe[bool], "If true, the entries in the dictionary are sorted in ascending order"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.DictionaryPageHeader")
    NUM_VALUES = hydra.core.Name("numValues")
    ENCODING = hydra.core.Name("encoding")
    IS_SORTED = hydra.core.Name("isSorted")

@dataclass(frozen=True)
class DataPageHeaderV2:
    r"""New page format allowing reading levels without decompressing the data Repetition and definition levels are uncompressed The remaining section containing the data is compressed if is_compressed is true."""

    num_values: Annotated[int, "Number of values, including NULLs, in this data page."]
    num_nulls: Annotated[int, "Number of NULL values, in this data page. Number of non-null = num_values - num_nulls which is also the number of values in the data section"]
    num_rows: Annotated[int, "Number of rows in this data page. which means pages change on record boundaries (r = 0)"]
    encoding: Annotated[Encoding, "Encoding used for data in this page"]
    definition_levels_byte_length: Annotated[int, "length of the definition levels"]
    repetition_levels_byte_length: Annotated[int, "length of the repetition levels"]
    is_compressed: Annotated[Maybe[bool], "whether the values are compressed. Which means the section of the page between definition_levels_byte_length + repetition_levels_byte_length + 1 and compressed_page_size (included) is compressed with the compression_codec. If missing it is considered compressed"]
    statistics: Annotated[Maybe[Statistics], "optional statistics for the data in this page"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.DataPageHeaderV2")
    NUM_VALUES = hydra.core.Name("numValues")
    NUM_NULLS = hydra.core.Name("numNulls")
    NUM_ROWS = hydra.core.Name("numRows")
    ENCODING = hydra.core.Name("encoding")
    DEFINITION_LEVELS_BYTE_LENGTH = hydra.core.Name("definitionLevelsByteLength")
    REPETITION_LEVELS_BYTE_LENGTH = hydra.core.Name("repetitionLevelsByteLength")
    IS_COMPRESSED = hydra.core.Name("isCompressed")
    STATISTICS = hydra.core.Name("statistics")

class BloomFilterAlgorithm(Enum):
    r"""The algorithm used in Bloom filter."""

    BLOCK = hydra.core.Name("block")
    r"""Block-based Bloom filter."""

BloomFilterAlgorithm.TYPE_ = hydra.core.Name("hydra.parquet.format.BloomFilterAlgorithm")

class BloomFilterHash(Enum):
    r"""The hash function used in Bloom filter. This function takes the hash of a column value using plain encoding."""

    XXHASH = hydra.core.Name("xxhash")
    r"""xxHash Strategy."""

BloomFilterHash.TYPE_ = hydra.core.Name("hydra.parquet.format.BloomFilterHash")

class BloomFilterCompression(Enum):
    r"""The compression used in the Bloom filter."""

    UNCOMPRESSED = hydra.core.Name("uncompressed")

BloomFilterCompression.TYPE_ = hydra.core.Name("hydra.parquet.format.BloomFilterCompression")

@dataclass(frozen=True)
class BloomFilterHeader:
    r"""Bloom filter header is stored at beginning of Bloom filter data of each column and followed by its bitset."""

    num_bytes: Annotated[int, "The size of bitset in bytes"]
    algorithm: Annotated[BloomFilterAlgorithm, "The algorithm for setting bits."]
    hash: Annotated[BloomFilterHash, "The hash function used for Bloom filter."]
    compression: Annotated[BloomFilterCompression, "The compression used in the Bloom filter"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.BloomFilterHeader")
    NUM_BYTES = hydra.core.Name("numBytes")
    ALGORITHM = hydra.core.Name("algorithm")
    HASH = hydra.core.Name("hash")
    COMPRESSION = hydra.core.Name("compression")

@dataclass(frozen=True)
class PageHeader:
    type: Annotated[PageType, "the type of the page: indicates which of the *_header fields is set"]
    uncompressed_page_size: Annotated[int, "Uncompressed page size in bytes (not including this header)"]
    compressed_page_size: Annotated[int, "Compressed (and potentially encrypted) page size in bytes, not including this header"]
    crc: Annotated[Maybe[int], "The 32bit CRC for the page, to be be calculated as follows:\n- Using the standard CRC32 algorithm\n- On the data only, i.e. this header should not be included. 'Data'\n  hereby refers to the concatenation of the repetition levels, the\n  definition levels and the column value, in this exact order.\n- On the encoded versions of the repetition levels, definition levels and\n  column values\n- On the compressed versions of the repetition levels, definition levels\n  and column values where possible;\n  - For v1 data pages, the repetition levels, definition levels and column\n    values are always compressed together. If a compression scheme is\n    specified, the CRC shall be calculated on the compressed version of\n    this concatenation. If no compression scheme is specified, the CRC\n    shall be calculated on the uncompressed version of this concatenation.\n  - For v2 data pages, the repetition levels and definition levels are\n    handled separately from the data and are never compressed (only\n    encoded). If a compression scheme is specified, the CRC shall be\n    calculated on the concatenation of the uncompressed repetition levels,\n    uncompressed definition levels and the compressed column values.\n    If no compression scheme is specified, the CRC shall be calculated on\n    the uncompressed concatenation.\n- In encrypted columns, CRC is calculated after page encryption; the\n  encryption itself is performed after page compression (if compressed)\nIf enabled, this allows for disabling checksumming in HDFS if only a few pages need to be read. "]
    data_page_header: Maybe[DataPageHeader]
    index_page_header: Maybe[IndexPageHeader]
    dictionary_page_header: Maybe[DictionaryPageHeader]
    data_page_header_v2: Maybe[DataPageHeaderV2]

    TYPE_ = hydra.core.Name("hydra.parquet.format.PageHeader")
    TYPE = hydra.core.Name("type")
    UNCOMPRESSED_PAGE_SIZE = hydra.core.Name("uncompressedPageSize")
    COMPRESSED_PAGE_SIZE = hydra.core.Name("compressedPageSize")
    CRC = hydra.core.Name("crc")
    DATA_PAGE_HEADER = hydra.core.Name("dataPageHeader")
    INDEX_PAGE_HEADER = hydra.core.Name("indexPageHeader")
    DICTIONARY_PAGE_HEADER = hydra.core.Name("dictionaryPageHeader")
    DATA_PAGE_HEADER_V2 = hydra.core.Name("dataPageHeaderV2")

@dataclass(frozen=True)
class KeyValue:
    r"""Wrapper struct to store key values."""

    key: str
    value: Maybe[str]

    TYPE_ = hydra.core.Name("hydra.parquet.format.KeyValue")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class SortingColumn:
    r"""Wrapper struct to specify sort order."""

    column_idx: Annotated[int, "The column index (in this row group)"]
    descending: Annotated[bool, "If true, indicates this column is sorted in descending order."]
    nulls_first: Annotated[bool, "If true, nulls will come before non-null values, otherwise, nulls go at the end."]

    TYPE_ = hydra.core.Name("hydra.parquet.format.SortingColumn")
    COLUMN_IDX = hydra.core.Name("columnIdx")
    DESCENDING = hydra.core.Name("descending")
    NULLS_FIRST = hydra.core.Name("nullsFirst")

@dataclass(frozen=True)
class PageEncodingStats:
    r"""statistics of a given page type and encoding."""

    page_type: Annotated[PageType, "the page type (data/dic/...)"]
    encoding: Annotated[Encoding, "encoding of the page"]
    count: Annotated[int, "number of pages of this type with this encoding"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.PageEncodingStats")
    PAGE_TYPE = hydra.core.Name("pageType")
    ENCODING = hydra.core.Name("encoding")
    COUNT = hydra.core.Name("count")

@dataclass(frozen=True)
class ColumnMetaData:
    r"""Description for column metadata."""

    type: Annotated[Type, "Type of this column"]
    encodings: Annotated[frozenlist[Encoding], "Set of all encodings used for this column. The purpose is to validate whether we can decode those pages."]
    path_in_schema: Annotated[frozenlist[str], "Path in schema"]
    codec: Annotated[CompressionCodec, "Compression codec"]
    num_values: Annotated[int, "Number of values in this column"]
    total_uncompressed_size: Annotated[int, "total byte size of all uncompressed pages in this column chunk (including the headers)"]
    total_compressed_size: Annotated[int, "total byte size of all compressed, and potentially encrypted, pages in this column chunk (including the headers)"]
    key_value_metadata: Annotated[Maybe[frozenlist[KeyValue]], "Optional key/value metadata"]
    data_page_offset: Annotated[int, "Byte offset from beginning of file to first data page"]
    index_page_offset: Annotated[Maybe[int], "Byte offset from beginning of file to root index page"]
    dictionary_page_offset: Annotated[Maybe[int], "Byte offset from the beginning of file to first (only) dictionary page"]
    statistics: Annotated[Maybe[Statistics], "optional statistics for this column chunk"]
    encoding_stats: Annotated[Maybe[frozenlist[PageEncodingStats]], "Set of all encodings used for pages in this column chunk. This information can be used to determine if all data pages are dictionary encoded for example"]
    bloom_filter_offset: Annotated[Maybe[int], "Byte offset from beginning of file to Bloom filter data."]

    TYPE_ = hydra.core.Name("hydra.parquet.format.ColumnMetaData")
    TYPE = hydra.core.Name("type")
    ENCODINGS = hydra.core.Name("encodings")
    PATH_IN_SCHEMA = hydra.core.Name("pathInSchema")
    CODEC = hydra.core.Name("codec")
    NUM_VALUES = hydra.core.Name("numValues")
    TOTAL_UNCOMPRESSED_SIZE = hydra.core.Name("totalUncompressedSize")
    TOTAL_COMPRESSED_SIZE = hydra.core.Name("totalCompressedSize")
    KEY_VALUE_METADATA = hydra.core.Name("keyValueMetadata")
    DATA_PAGE_OFFSET = hydra.core.Name("dataPageOffset")
    INDEX_PAGE_OFFSET = hydra.core.Name("indexPageOffset")
    DICTIONARY_PAGE_OFFSET = hydra.core.Name("dictionaryPageOffset")
    STATISTICS = hydra.core.Name("statistics")
    ENCODING_STATS = hydra.core.Name("encodingStats")
    BLOOM_FILTER_OFFSET = hydra.core.Name("bloomFilterOffset")

@dataclass(frozen=True)
class EncryptionWithFooterKey:
    TYPE_ = hydra.core.Name("hydra.parquet.format.EncryptionWithFooterKey")

@dataclass(frozen=True)
class EncryptionWithColumnKey:
    path_in_schema: Annotated[frozenlist[str], "Column path in schema"]
    key_metadata: Annotated[Maybe[bytes], "Retrieval metadata of column encryption key"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.EncryptionWithColumnKey")
    PATH_IN_SCHEMA = hydra.core.Name("pathInSchema")
    KEY_METADATA = hydra.core.Name("keyMetadata")

class ColumnCryptoMetaDataEncryptionWithFooterKey(Node["EncryptionWithFooterKey"]):
    ...

class ColumnCryptoMetaDataEncryptionWithColumnKey(Node["EncryptionWithColumnKey"]):
    ...

class _ColumnCryptoMetaDataMeta(type):
    def __getitem__(cls, item):
        return object

class ColumnCryptoMetaData(metaclass=_ColumnCryptoMetaDataMeta):
    r"""ColumnCryptoMetaDataEncryptionWithFooterKey | ColumnCryptoMetaDataEncryptionWithColumnKey"""

    TYPE_ = hydra.core.Name("hydra.parquet.format.ColumnCryptoMetaData")
    ENCRYPTION_WITH_FOOTER_KEY = hydra.core.Name("encryptionWithFooterKey")
    ENCRYPTION_WITH_COLUMN_KEY = hydra.core.Name("encryptionWithColumnKey")

@dataclass(frozen=True)
class ColumnChunk:
    file_path: Annotated[Maybe[str], "File where column data is stored.  If not set, assumed to be same file as metadata.  This path is relative to the current file."]
    file_offset: Annotated[int, "Byte offset in file_path to the ColumnMetaData"]
    meta_data: Annotated[Maybe[ColumnMetaData], "Column metadata for this chunk. This is the same content as what is at file_path/file_offset.  Having it here has it replicated in the file metadata."]
    offset_index_offset: Annotated[Maybe[int], "File offset of ColumnChunk's OffsetIndex"]
    offset_index_length: Annotated[Maybe[int], "Size of ColumnChunk's OffsetIndex, in bytes"]
    column_index_offset: Annotated[Maybe[int], "File offset of ColumnChunk's ColumnIndex"]
    column_index_length: Annotated[Maybe[int], "Size of ColumnChunk's ColumnIndex, in bytes"]
    crypto_metadata: Annotated[Maybe[ColumnCryptoMetaData], "Crypto metadata of encrypted columns"]
    encrypted_column_metadata: Annotated[Maybe[bytes], "Encrypted column metadata for this chunk"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.ColumnChunk")
    FILE_PATH = hydra.core.Name("filePath")
    FILE_OFFSET = hydra.core.Name("fileOffset")
    META_DATA = hydra.core.Name("metaData")
    OFFSET_INDEX_OFFSET = hydra.core.Name("offsetIndexOffset")
    OFFSET_INDEX_LENGTH = hydra.core.Name("offsetIndexLength")
    COLUMN_INDEX_OFFSET = hydra.core.Name("columnIndexOffset")
    COLUMN_INDEX_LENGTH = hydra.core.Name("columnIndexLength")
    CRYPTO_METADATA = hydra.core.Name("cryptoMetadata")
    ENCRYPTED_COLUMN_METADATA = hydra.core.Name("encryptedColumnMetadata")

@dataclass(frozen=True)
class RowGroup:
    columns: Annotated[frozenlist[ColumnChunk], "Metadata for each column chunk in this row group. This list must have the same order as the SchemaElement list in FileMetaData."]
    total_byte_size: Annotated[int, "Total byte size of all the uncompressed column data in this row group"]
    num_rows: Annotated[int, "Number of rows in this row group"]
    sorting_columns: Annotated[Maybe[frozenlist[SortingColumn]], "If set, specifies a sort ordering of the rows in this RowGroup. The sorting columns can be a subset of all the columns."]
    file_offset: Annotated[Maybe[int], "Byte offset from beginning of file to first page (data or dictionary) in this row group"]
    total_compressed_size: Annotated[Maybe[int], "Total byte size of all compressed (and potentially encrypted) column data in this row group"]
    ordinal: Annotated[Maybe[int], "Row group ordinal in the file"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.RowGroup")
    COLUMNS = hydra.core.Name("columns")
    TOTAL_BYTE_SIZE = hydra.core.Name("totalByteSize")
    NUM_ROWS = hydra.core.Name("numRows")
    SORTING_COLUMNS = hydra.core.Name("sortingColumns")
    FILE_OFFSET = hydra.core.Name("fileOffset")
    TOTAL_COMPRESSED_SIZE = hydra.core.Name("totalCompressedSize")
    ORDINAL = hydra.core.Name("ordinal")

class ColumnOrder(Enum):
    r"""Union to specify the order used for the min_value and max_value fields for a column. This union takes the role of an enhanced enum that allows rich elements (which will be needed for a collation-based ordering in the future). Possible values are:
    * TypeDefinedOrder - the column uses the order defined by its logical or physical type (if there is no logical type).
    If the reader does not support the value of this union, min and max stats for this column should be ignored."""

    TYPE_ORDER = hydra.core.Name("typeOrder")
    r"""The sort orders for logical types are:
      UTF8 - unsigned byte-wise comparison
      INT8 - signed comparison
      INT16 - signed comparison
      INT32 - signed comparison
      INT64 - signed comparison
      UINT8 - unsigned comparison
      UINT16 - unsigned comparison
      UINT32 - unsigned comparison
      UINT64 - unsigned comparison
      DECIMAL - signed comparison of the represented value
      DATE - signed comparison
      TIME_MILLIS - signed comparison
      TIME_MICROS - signed comparison
      TIMESTAMP_MILLIS - signed comparison
      TIMESTAMP_MICROS - signed comparison
      INTERVAL - unsigned comparison
      JSON - unsigned byte-wise comparison
      BSON - unsigned byte-wise comparison
      ENUM - unsigned byte-wise comparison
      LIST - undefined
      MAP - undefined
    In the absence of logical types, the sort order is determined by the physical type:
      BOOLEAN - false, true
      INT32 - signed comparison
      INT64 - signed comparison
      INT96 (only used for legacy timestamps) - undefined
      FLOAT - signed comparison of the represented value (*)
      DOUBLE - signed comparison of the represented value (*)
      BYTE_ARRAY - unsigned byte-wise comparison
      FIXED_LEN_BYTE_ARRAY - unsigned byte-wise comparison
    (*) Because the sorting order is not specified properly for floating
        point values (relations vs. total ordering) the following
        compatibility rules should be applied when reading statistics:
        - If the min is a NaN, it should be ignored.
        - If the max is a NaN, it should be ignored.
        - If the min is +0, the row group may contain -0 values as well.
        - If the max is -0, the row group may contain +0 values as well.
        - When looking for NaN values, min and max should be ignored."""

ColumnOrder.TYPE_ = hydra.core.Name("hydra.parquet.format.ColumnOrder")

@dataclass(frozen=True)
class PageLocation:
    offset: Annotated[int, "Offset of the page in the file"]
    compressed_page_size: Annotated[int, "Size of the page, including header. Sum of compressed_page_size and header length"]
    first_row_index: Annotated[int, "Index within the RowGroup of the first row of the page; this means pages change on record boundaries (r = 0)."]

    TYPE_ = hydra.core.Name("hydra.parquet.format.PageLocation")
    OFFSET = hydra.core.Name("offset")
    COMPRESSED_PAGE_SIZE = hydra.core.Name("compressedPageSize")
    FIRST_ROW_INDEX = hydra.core.Name("firstRowIndex")

@dataclass(frozen=True)
class OffsetIndex:
    page_locations: Annotated[frozenlist[PageLocation], "PageLocations, ordered by increasing PageLocation.offset. It is required that page_locations[i].first_row_index < page_locations[i+1].first_row_index."]

    TYPE_ = hydra.core.Name("hydra.parquet.format.OffsetIndex")
    PAGE_LOCATIONS = hydra.core.Name("pageLocations")

@dataclass(frozen=True)
class ColumnIndex:
    r"""Description for ColumnIndex. Each <array-field>[i] refers to the page at OffsetIndex.page_locations[i]."""

    null_pages: Annotated[frozenlist[bool], "A list of Boolean values to determine the validity of the corresponding min and max values. If true, a page contains only null values, and writers have to set the corresponding entries in min_values and max_values to byte[0], so that all lists have the same length. If false, the corresponding entries in min_values and max_values must be valid."]
    min_values: Annotated[frozenlist[bytes], "minValues and maxValues are lists containing lower and upper bounds for the values of each page determined by the ColumnOrder of the column. These may be the actual minimum and maximum values found on a page, but can also be (more compact) values that do not exist on a page. For example, instead of storing \"Blart Versenwald III\", a writer may set min_values[i]=\"B\", max_values[i]=\"C\". Such more compact values must still be valid values within the column's logical type. Readers must make sure that list entries are populated before using them by inspecting null_pages."]
    max_values: frozenlist[bytes]
    boundary_order: Annotated[BoundaryOrder, "Stores whether both min_values and max_values are orderd and if so, in which direction. This allows readers to perform binary searches in both lists. Readers cannot assume that max_values[i] <= min_values[i+1], even if the lists are ordered."]
    null_counts: Annotated[Maybe[frozenlist[int]], "A list containing the number of null values for each page"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.ColumnIndex")
    NULL_PAGES = hydra.core.Name("nullPages")
    MIN_VALUES = hydra.core.Name("minValues")
    MAX_VALUES = hydra.core.Name("maxValues")
    BOUNDARY_ORDER = hydra.core.Name("boundaryOrder")
    NULL_COUNTS = hydra.core.Name("nullCounts")

@dataclass(frozen=True)
class AesGcmV1:
    aad_prefix: Annotated[Maybe[bytes], "AAD prefix"]
    aad_file_unique: Annotated[Maybe[bytes], "Unique file identifier part of AAD suffix"]
    supply_aad_prefix: Annotated[Maybe[bool], "In files encrypted with AAD prefix without storing it, readers must supply the prefix"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.AesGcmV1")
    AAD_PREFIX = hydra.core.Name("aadPrefix")
    AAD_FILE_UNIQUE = hydra.core.Name("aadFileUnique")
    SUPPLY_AAD_PREFIX = hydra.core.Name("supplyAadPrefix")

@dataclass(frozen=True)
class AesGcmCtrV1:
    aad_prefix: Annotated[Maybe[bytes], "AAD prefix"]
    aad_file_unique: Annotated[Maybe[bytes], "Unique file identifier part of AAD suffix"]
    supply_aad_prefix: Annotated[Maybe[bool], "In files encrypted with AAD prefix without storing it, readers must supply the prefix"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.AesGcmCtrV1")
    AAD_PREFIX = hydra.core.Name("aadPrefix")
    AAD_FILE_UNIQUE = hydra.core.Name("aadFileUnique")
    SUPPLY_AAD_PREFIX = hydra.core.Name("supplyAadPrefix")

class EncryptionAlgorithmAesGcmV1(Node["AesGcmV1"]):
    ...

class EncryptionAlgorithmAesGcmCtrV1(Node["AesGcmCtrV1"]):
    ...

class _EncryptionAlgorithmMeta(type):
    def __getitem__(cls, item):
        return object

class EncryptionAlgorithm(metaclass=_EncryptionAlgorithmMeta):
    r"""EncryptionAlgorithmAesGcmV1 | EncryptionAlgorithmAesGcmCtrV1"""

    TYPE_ = hydra.core.Name("hydra.parquet.format.EncryptionAlgorithm")
    AES_GCM_V1 = hydra.core.Name("aesGcmV1")
    AES_GCM_CTR_V1 = hydra.core.Name("aesGcmCtrV1")

@dataclass(frozen=True)
class FileMetaData:
    r"""Description for file metadata."""

    version: Annotated[int, "Version of this file"]
    schema: Annotated[frozenlist[SchemaElement], "Parquet schema for this file.  This schema contains metadata for all the columns. The schema is represented as a tree with a single root.  The nodes of the tree are flattened to a list by doing a depth-first traversal. The column metadata contains the path in the schema for that column which can be used to map columns to nodes in the schema. The first element is the root"]
    num_rows: Annotated[int, "Number of rows in this file"]
    row_groups: Annotated[frozenlist[RowGroup], "Row groups in this file"]
    key_value_metadata: Annotated[Maybe[frozenlist[KeyValue]], "Optional key/value metadata"]
    created_by: Annotated[Maybe[str], "String for application that wrote this file.  This should be in the format <Application> version <App Version> (build <App Build Hash>). e.g. impala version 1.0 (build 6cf94d29b2b7115df4de2c06e2ab4326d721eb55)"]
    column_orders: Annotated[Maybe[frozenlist[ColumnOrder]], "Sort order used for the min_value and max_value fields in the Statistics objects and the min_values and max_values fields in the ColumnIndex objects of each column in this file. Sort orders are listed in the order matching the columns in the schema. The indexes are not necessary the same though, because only leaf nodes of the schema are represented in the list of sort orders.\nWithout column_orders, the meaning of the min_value and max_value fields in the Statistics object and the ColumnIndex object is undefined. To ensure well-defined behaviour, if these fields are written to a Parquet file, column_orders must be written as well.\nThe obsolete min and max fields in the Statistics object are always sorted by signed comparison regardless of column_orders."]
    encryption_algorithm: Annotated[Maybe[EncryptionAlgorithm], "Encryption algorithm. This field is set only in encrypted files with plaintext footer. Files with encrypted footer store algorithm id in FileCryptoMetaData structure."]
    footer_signing_key_metadata: Annotated[Maybe[bytes], "Retrieval metadata of key used for signing the footer. Used only in encrypted files with plaintext footer."]

    TYPE_ = hydra.core.Name("hydra.parquet.format.FileMetaData")
    VERSION = hydra.core.Name("version")
    SCHEMA = hydra.core.Name("schema")
    NUM_ROWS = hydra.core.Name("numRows")
    ROW_GROUPS = hydra.core.Name("rowGroups")
    KEY_VALUE_METADATA = hydra.core.Name("keyValueMetadata")
    CREATED_BY = hydra.core.Name("createdBy")
    COLUMN_ORDERS = hydra.core.Name("columnOrders")
    ENCRYPTION_ALGORITHM = hydra.core.Name("encryptionAlgorithm")
    FOOTER_SIGNING_KEY_METADATA = hydra.core.Name("footerSigningKeyMetadata")

@dataclass(frozen=True)
class FileCryptoMetaData:
    r"""Crypto metadata for files with encrypted footer."""

    encryption_algorithm: Annotated[EncryptionAlgorithm, "Encryption algorithm. This field is only used for files with encrypted footer. Files with plaintext footer store algorithm id inside footer (FileMetaData structure)."]
    key_metadata: Annotated[Maybe[bytes], "Retrieval metadata of key used for encryption of footer, and (possibly) columns"]

    TYPE_ = hydra.core.Name("hydra.parquet.format.FileCryptoMetaData")
    ENCRYPTION_ALGORITHM = hydra.core.Name("encryptionAlgorithm")
    KEY_METADATA = hydra.core.Name("keyMetadata")
