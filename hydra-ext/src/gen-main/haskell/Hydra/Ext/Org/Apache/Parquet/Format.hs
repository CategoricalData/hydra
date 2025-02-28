-- | A model for the Parquet format. Based on the Thrift-based specification at:
-- |   https://github.com/apache/parquet-format/blob/master/src/main/thrift/parquet.thrift

module Hydra.Ext.Org.Apache.Parquet.Format where

import qualified Hydra.Core as Core
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Types supported by Parquet.  These types are intended to be used in combination with the encodings to control the on disk storage format. For example INT16 is not included as a type since a good encoding of INT32 would handle this.
data Type = 
  TypeBoolean  |
  TypeInt32  |
  TypeInt64  |
  TypeFloat  |
  TypeDouble  |
  TypeByteArray  |
  TypeFixedLenByteArray 
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra.ext.org.apache.parquet.format.Type")

_Type_boolean = (Core.Name "boolean")

_Type_int32 = (Core.Name "int32")

_Type_int64 = (Core.Name "int64")

_Type_float = (Core.Name "float")

_Type_double = (Core.Name "double")

_Type_byteArray = (Core.Name "byteArray")

_Type_fixedLenByteArray = (Core.Name "fixedLenByteArray")

-- | Representation of Schemas
data FieldRepetitionType = 
  -- | This field is required (can not be null) and each record has exactly 1 value.
  FieldRepetitionTypeRequired  |
  -- | The field is optional (can be null) and each record has 0 or 1 values.
  FieldRepetitionTypeOptional  |
  -- | The field is repeated and can contain 0 or more values
  FieldRepetitionTypeRepeated 
  deriving (Eq, Ord, Read, Show)

_FieldRepetitionType = (Core.Name "hydra.ext.org.apache.parquet.format.FieldRepetitionType")

_FieldRepetitionType_required = (Core.Name "required")

_FieldRepetitionType_optional = (Core.Name "optional")

_FieldRepetitionType_repeated = (Core.Name "repeated")

-- | Statistics per row group and per page. All fields are optional.
data Statistics = 
  Statistics {
    statisticsNullCount :: (Maybe Integer),
    statisticsDistinctCount :: (Maybe Integer),
    -- | Max value for the column, determined by its ColumnOrder. Values are encoded using PLAIN encoding, except that variable-length byte arrays do not include a length prefix.
    statisticsMaxValue :: (Maybe String),
    -- | Max value for the column, determined by its ColumnOrder. Values are encoded using PLAIN encoding, except that variable-length byte arrays do not include a length prefix.
    statisticsMinValue :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Statistics = (Core.Name "hydra.ext.org.apache.parquet.format.Statistics")

_Statistics_nullCount = (Core.Name "nullCount")

_Statistics_distinctCount = (Core.Name "distinctCount")

_Statistics_maxValue = (Core.Name "maxValue")

_Statistics_minValue = (Core.Name "minValue")

-- | Decimal logical type annotation. To maintain forward-compatibility in v1, implementations using this logical type must also set scale and precision on the annotated SchemaElement. Allowed for physical types: INT32, INT64, FIXED, and BINARY
data DecimalType = 
  DecimalType {
    decimalTypeScale :: Int,
    decimalTypePrecision :: Int}
  deriving (Eq, Ord, Read, Show)

_DecimalType = (Core.Name "hydra.ext.org.apache.parquet.format.DecimalType")

_DecimalType_scale = (Core.Name "scale")

_DecimalType_precision = (Core.Name "precision")

data TimeUnit = 
  TimeUnitMillis  |
  TimeUnitMicros  |
  TimeUnitNanos 
  deriving (Eq, Ord, Read, Show)

_TimeUnit = (Core.Name "hydra.ext.org.apache.parquet.format.TimeUnit")

_TimeUnit_millis = (Core.Name "millis")

_TimeUnit_micros = (Core.Name "micros")

_TimeUnit_nanos = (Core.Name "nanos")

-- | Timestamp logical type annotation. Allowed for physical types: INT64
data TimestampType = 
  TimestampType {
    timestampTypeIsAdjustedToUtc :: Bool,
    timestampTypeUnit :: TimeUnit}
  deriving (Eq, Ord, Read, Show)

_TimestampType = (Core.Name "hydra.ext.org.apache.parquet.format.TimestampType")

_TimestampType_isAdjustedToUtc = (Core.Name "isAdjustedToUtc")

_TimestampType_unit = (Core.Name "unit")

-- | Time logical type annotation. Allowed for physical types: INT32 (millis), INT64 (micros, nanos)
data TimeType = 
  TimeType {
    timeTypeIsAdjustedToUtc :: Bool,
    timeTypeUnit :: TimeUnit}
  deriving (Eq, Ord, Read, Show)

_TimeType = (Core.Name "hydra.ext.org.apache.parquet.format.TimeType")

_TimeType_isAdjustedToUtc = (Core.Name "isAdjustedToUtc")

_TimeType_unit = (Core.Name "unit")

-- | Integer logical type annotation. bitWidth must be 8, 16, 32, or 64. Allowed for physical types: INT32, INT64
data IntType = 
  IntType {
    intTypeBitWidth :: I.Int16,
    intTypeIsSigned :: Bool}
  deriving (Eq, Ord, Read, Show)

_IntType = (Core.Name "hydra.ext.org.apache.parquet.format.IntType")

_IntType_bitWidth = (Core.Name "bitWidth")

_IntType_isSigned = (Core.Name "isSigned")

-- | LogicalType annotations to replace ConvertedType. To maintain compatibility, implementations using LogicalType for a SchemaElement aust also set the corresponding ConvertedType (if any) from the following table.
data LogicalType = 
  -- | use ConvertedType UTF8
  LogicalTypeString  |
  -- | use ConvertedType MAP
  LogicalTypeMap  |
  -- | use ConvertedType LIST
  LogicalTypeList  |
  -- | use ConvertedType ENUM
  LogicalTypeEnum  |
  -- | use ConvertedType DECIMAL + SchemaElement.{scale, precision}
  LogicalTypeDecimal DecimalType |
  -- | use ConvertedType DATE
  LogicalTypeDate  |
  -- | use ConvertedType TIME_MICROS for TIME(isAdjustedToUTC = *, unit = MICROS). use ConvertedType TIME_MILLIS for TIME(isAdjustedToUTC = *, unit = MILLIS)
  LogicalTypeTime TimeType |
  -- | use ConvertedType TIMESTAMP_MICROS for TIMESTAMP(isAdjustedToUTC = *, unit = MICROS). use ConvertedType TIMESTAMP_MILLIS for TIMESTAMP(isAdjustedToUTC = *, unit = MILLIS)
  LogicalTypeTimestamp TimestampType |
  -- | use ConvertedType INT_* or UINT_*
  LogicalTypeInteger IntType |
  -- | no compatible ConvertedType
  LogicalTypeUnknown  |
  -- | use ConvertedType JSON
  LogicalTypeJson  |
  -- | use ConvertedType BSON
  LogicalTypeBson  |
  -- | no compatible ConvertedType
  LogicalTypeUuid 
  deriving (Eq, Ord, Read, Show)

_LogicalType = (Core.Name "hydra.ext.org.apache.parquet.format.LogicalType")

_LogicalType_string = (Core.Name "string")

_LogicalType_map = (Core.Name "map")

_LogicalType_list = (Core.Name "list")

_LogicalType_enum = (Core.Name "enum")

_LogicalType_decimal = (Core.Name "decimal")

_LogicalType_date = (Core.Name "date")

_LogicalType_time = (Core.Name "time")

_LogicalType_timestamp = (Core.Name "timestamp")

_LogicalType_integer = (Core.Name "integer")

_LogicalType_unknown = (Core.Name "unknown")

_LogicalType_json = (Core.Name "json")

_LogicalType_bson = (Core.Name "bson")

_LogicalType_uuid = (Core.Name "uuid")

-- | Represents a element inside a schema definition.
-- | - if it is a group (inner node) then type is undefined and num_children is defined
-- | - if it is a primitive type (leaf) then type is defined and num_children is undefined
-- | the nodes are listed in depth first traversal order.
data SchemaElement = 
  SchemaElement {
    -- | Data type for this field. Not set if the current element is a non-leaf node
    schemaElementType :: (Maybe Type),
    -- | If type is FIXED_LEN_BYTE_ARRAY, this is the byte length of the values. Otherwise, if specified, this is the maximum bit length to store any of the values. (e.g. a low cardinality INT col could have this set to 3).  Note that this is in the schema, and therefore fixed for the entire file.
    schemaElementTypeLength :: (Maybe Int),
    -- | repetition of the field. The root of the schema does not have a repetition_type. All other nodes must have one
    schemaElementRepetitionType :: (Maybe FieldRepetitionType),
    -- | Name of the field in the schema
    schemaElementName :: String,
    -- | Nested fields.  Since thrift does not support nested fields, the nesting is flattened to a single list by a depth-first traversal. The children count is used to construct the nested relationship. This field is not set when the element is a primitive type
    schemaElementNumChildren :: (Maybe Int),
    -- | When the original schema supports field ids, this will save the original field id in the parquet schema
    schemaElementFieldId :: (Maybe Int),
    -- | The logical type of this SchemaElement. LogicalType replaces ConvertedType, but ConvertedType is still required for some logical types to ensure forward-compatibility in format v1.
    schemaElementLogicalType :: (Maybe LogicalType)}
  deriving (Eq, Ord, Read, Show)

_SchemaElement = (Core.Name "hydra.ext.org.apache.parquet.format.SchemaElement")

_SchemaElement_type = (Core.Name "type")

_SchemaElement_typeLength = (Core.Name "typeLength")

_SchemaElement_repetitionType = (Core.Name "repetitionType")

_SchemaElement_name = (Core.Name "name")

_SchemaElement_numChildren = (Core.Name "numChildren")

_SchemaElement_fieldId = (Core.Name "fieldId")

_SchemaElement_logicalType = (Core.Name "logicalType")

-- | Encodings supported by Parquet.  Not all encodings are valid for all types.  These enums are also used to specify the encoding of definition and repetition levels. See the accompanying doc for the details of the more complicated encodings.
data Encoding = 
  -- | Default encoding.
  -- | BOOLEAN - 1 bit per value. 0 is false; 1 is true.
  -- | INT32 - 4 bytes per value.  Stored as little-endian.
  -- | INT64 - 8 bytes per value.  Stored as little-endian.
  -- | FLOAT - 4 bytes per value.  IEEE. Stored as little-endian.
  -- | DOUBLE - 8 bytes per value.  IEEE. Stored as little-endian.
  -- | BYTE_ARRAY - 4 byte length stored as little endian, followed by bytes.
  -- | FIXED_LEN_BYTE_ARRAY - Just the bytes.
  EncodingPlain  |
  -- | Group packed run length encoding. Usable for definition/repetition levels encoding and Booleans (on one bit: 0 is false; 1 is true.)
  EncodingRle  |
  -- | Bit packed encoding.  This can only be used if the data has a known max width.  Usable for definition/repetition levels encoding.
  EncodingBitPacked  |
  -- | Delta encoding for integers. This can be used for int columns and works best on sorted data
  EncodingDeltaBinaryPacked  |
  -- | Encoding for byte arrays to separate the length values and the data. The lengths are encoded using DELTA_BINARY_PACKED
  EncodingDeltaLengthByteArray  |
  -- | Incremental-encoded byte array. Prefix lengths are encoded using DELTA_BINARY_PACKED. Suffixes are stored as delta length byte arrays.
  EncodingDeltaByteArray  |
  -- | Dictionary encoding: the ids are encoded using the RLE encoding
  EncodingRleDictionary  |
  -- | Encoding for floating-point data. K byte-streams are created where K is the size in bytes of the data type. The individual bytes of an FP value are scattered to the corresponding stream and the streams are concatenated. This itself does not reduce the size of the data but can lead to better compression afterwards.
  EncodingByteStreamSplit 
  deriving (Eq, Ord, Read, Show)

_Encoding = (Core.Name "hydra.ext.org.apache.parquet.format.Encoding")

_Encoding_plain = (Core.Name "plain")

_Encoding_rle = (Core.Name "rle")

_Encoding_bitPacked = (Core.Name "bitPacked")

_Encoding_deltaBinaryPacked = (Core.Name "deltaBinaryPacked")

_Encoding_deltaLengthByteArray = (Core.Name "deltaLengthByteArray")

_Encoding_deltaByteArray = (Core.Name "deltaByteArray")

_Encoding_rleDictionary = (Core.Name "rleDictionary")

_Encoding_byteStreamSplit = (Core.Name "byteStreamSplit")

-- | Supported compression algorithms. Codecs added in format version X.Y can be read by readers based on X.Y and later. Codec support may vary between readers based on the format version and libraries available at runtime. See Compression.md for a detailed specification of these algorithms.
data CompressionCodec = 
  CompressionCodecUncompressed  |
  CompressionCodecSnappy  |
  CompressionCodecGzip  |
  CompressionCodecLzo  |
  -- | Added in 2.4
  CompressionCodecBrotli  |
  -- | Added in 2.4
  CompressionCodecZstd  |
  -- | Added in 2.9
  CompressionCodecLz4Raw 
  deriving (Eq, Ord, Read, Show)

_CompressionCodec = (Core.Name "hydra.ext.org.apache.parquet.format.CompressionCodec")

_CompressionCodec_uncompressed = (Core.Name "uncompressed")

_CompressionCodec_snappy = (Core.Name "snappy")

_CompressionCodec_gzip = (Core.Name "gzip")

_CompressionCodec_lzo = (Core.Name "lzo")

_CompressionCodec_brotli = (Core.Name "brotli")

_CompressionCodec_zstd = (Core.Name "zstd")

_CompressionCodec_lz4Raw = (Core.Name "lz4Raw")

data PageType = 
  PageTypeDataPage  |
  PageTypeIndexPage  |
  PageTypeDictionaryPage  |
  PageTypeDataPageV2 
  deriving (Eq, Ord, Read, Show)

_PageType = (Core.Name "hydra.ext.org.apache.parquet.format.PageType")

_PageType_dataPage = (Core.Name "dataPage")

_PageType_indexPage = (Core.Name "indexPage")

_PageType_dictionaryPage = (Core.Name "dictionaryPage")

_PageType_dataPageV2 = (Core.Name "dataPageV2")

-- | Enum to annotate whether lists of min/max elements inside ColumnIndex are ordered and if so, in which direction.
data BoundaryOrder = 
  BoundaryOrderUnordered  |
  BoundaryOrderAscending  |
  BoundaryOrderDescending 
  deriving (Eq, Ord, Read, Show)

_BoundaryOrder = (Core.Name "hydra.ext.org.apache.parquet.format.BoundaryOrder")

_BoundaryOrder_unordered = (Core.Name "unordered")

_BoundaryOrder_ascending = (Core.Name "ascending")

_BoundaryOrder_descending = (Core.Name "descending")

-- | Data page header
data DataPageHeader = 
  DataPageHeader {
    -- | Number of values, including NULLs, in this data page.
    dataPageHeaderNumValues :: Int,
    -- | Encoding used for this data page
    dataPageHeaderEncoding :: Encoding,
    -- | Encoding used for definition levels
    dataPageHeaderDefinitionLevelEncoding :: Encoding,
    -- | Encoding used for repetition levels
    dataPageHeaderRepetitionLevelEncoding :: Encoding,
    -- | Optional statistics for the data in this page
    dataPageHeaderStatistics :: (Maybe Statistics)}
  deriving (Eq, Ord, Read, Show)

_DataPageHeader = (Core.Name "hydra.ext.org.apache.parquet.format.DataPageHeader")

_DataPageHeader_numValues = (Core.Name "numValues")

_DataPageHeader_encoding = (Core.Name "encoding")

_DataPageHeader_definitionLevelEncoding = (Core.Name "definitionLevelEncoding")

_DataPageHeader_repetitionLevelEncoding = (Core.Name "repetitionLevelEncoding")

_DataPageHeader_statistics = (Core.Name "statistics")

data IndexPageHeader = 
  IndexPageHeader {}
  deriving (Eq, Ord, Read, Show)

_IndexPageHeader = (Core.Name "hydra.ext.org.apache.parquet.format.IndexPageHeader")

-- | The dictionary page must be placed at the first position of the column chunk if it is partly or completely dictionary encoded. At most one dictionary page can be placed in a column chunk.
data DictionaryPageHeader = 
  DictionaryPageHeader {
    -- | Number of values in the dictionary
    dictionaryPageHeaderNumValues :: Int,
    -- | Encoding using this dictionary page
    dictionaryPageHeaderEncoding :: Encoding,
    -- | If true, the entries in the dictionary are sorted in ascending order
    dictionaryPageHeaderIsSorted :: (Maybe Bool)}
  deriving (Eq, Ord, Read, Show)

_DictionaryPageHeader = (Core.Name "hydra.ext.org.apache.parquet.format.DictionaryPageHeader")

_DictionaryPageHeader_numValues = (Core.Name "numValues")

_DictionaryPageHeader_encoding = (Core.Name "encoding")

_DictionaryPageHeader_isSorted = (Core.Name "isSorted")

-- | New page format allowing reading levels without decompressing the data Repetition and definition levels are uncompressed The remaining section containing the data is compressed if is_compressed is true
data DataPageHeaderV2 = 
  DataPageHeaderV2 {
    -- | Number of values, including NULLs, in this data page.
    dataPageHeaderV2NumValues :: Int,
    -- | Number of NULL values, in this data page. Number of non-null = num_values - num_nulls which is also the number of values in the data section
    dataPageHeaderV2NumNulls :: Int,
    -- | Number of rows in this data page. which means pages change on record boundaries (r = 0)
    dataPageHeaderV2NumRows :: Int,
    -- | Encoding used for data in this page
    dataPageHeaderV2Encoding :: Encoding,
    -- | length of the definition levels
    dataPageHeaderV2DefinitionLevelsByteLength :: Int,
    -- | length of the repetition levels
    dataPageHeaderV2RepetitionLevelsByteLength :: Int,
    -- | whether the values are compressed. Which means the section of the page between definition_levels_byte_length + repetition_levels_byte_length + 1 and compressed_page_size (included) is compressed with the compression_codec. If missing it is considered compressed
    dataPageHeaderV2IsCompressed :: (Maybe Bool),
    -- | optional statistics for the data in this page
    dataPageHeaderV2Statistics :: (Maybe Statistics)}
  deriving (Eq, Ord, Read, Show)

_DataPageHeaderV2 = (Core.Name "hydra.ext.org.apache.parquet.format.DataPageHeaderV2")

_DataPageHeaderV2_numValues = (Core.Name "numValues")

_DataPageHeaderV2_numNulls = (Core.Name "numNulls")

_DataPageHeaderV2_numRows = (Core.Name "numRows")

_DataPageHeaderV2_encoding = (Core.Name "encoding")

_DataPageHeaderV2_definitionLevelsByteLength = (Core.Name "definitionLevelsByteLength")

_DataPageHeaderV2_repetitionLevelsByteLength = (Core.Name "repetitionLevelsByteLength")

_DataPageHeaderV2_isCompressed = (Core.Name "isCompressed")

_DataPageHeaderV2_statistics = (Core.Name "statistics")

-- | The algorithm used in Bloom filter.
data BloomFilterAlgorithm = 
  -- | Block-based Bloom filter.
  BloomFilterAlgorithmBlock 
  deriving (Eq, Ord, Read, Show)

_BloomFilterAlgorithm = (Core.Name "hydra.ext.org.apache.parquet.format.BloomFilterAlgorithm")

_BloomFilterAlgorithm_block = (Core.Name "block")

-- | The hash function used in Bloom filter. This function takes the hash of a column value using plain encoding.
data BloomFilterHash = 
  -- | xxHash Strategy.
  BloomFilterHashXxhash 
  deriving (Eq, Ord, Read, Show)

_BloomFilterHash = (Core.Name "hydra.ext.org.apache.parquet.format.BloomFilterHash")

_BloomFilterHash_xxhash = (Core.Name "xxhash")

-- | The compression used in the Bloom filter.
data BloomFilterCompression = 
  BloomFilterCompressionUncompressed 
  deriving (Eq, Ord, Read, Show)

_BloomFilterCompression = (Core.Name "hydra.ext.org.apache.parquet.format.BloomFilterCompression")

_BloomFilterCompression_uncompressed = (Core.Name "uncompressed")

-- | Bloom filter header is stored at beginning of Bloom filter data of each column and followed by its bitset.
data BloomFilterHeader = 
  BloomFilterHeader {
    -- | The size of bitset in bytes
    bloomFilterHeaderNumBytes :: Int,
    -- | The algorithm for setting bits.
    bloomFilterHeaderAlgorithm :: BloomFilterAlgorithm,
    -- | The hash function used for Bloom filter.
    bloomFilterHeaderHash :: BloomFilterHash,
    -- | The compression used in the Bloom filter
    bloomFilterHeaderCompression :: BloomFilterCompression}
  deriving (Eq, Ord, Read, Show)

_BloomFilterHeader = (Core.Name "hydra.ext.org.apache.parquet.format.BloomFilterHeader")

_BloomFilterHeader_numBytes = (Core.Name "numBytes")

_BloomFilterHeader_algorithm = (Core.Name "algorithm")

_BloomFilterHeader_hash = (Core.Name "hash")

_BloomFilterHeader_compression = (Core.Name "compression")

data PageHeader = 
  PageHeader {
    -- | the type of the page: indicates which of the *_header fields is set
    pageHeaderType :: PageType,
    -- | Uncompressed page size in bytes (not including this header)
    pageHeaderUncompressedPageSize :: Int,
    -- | Compressed (and potentially encrypted) page size in bytes, not including this header
    pageHeaderCompressedPageSize :: Int,
    -- | The 32bit CRC for the page, to be be calculated as follows:
    -- | - Using the standard CRC32 algorithm
    -- | - On the data only, i.e. this header should not be included. 'Data'
    -- |   hereby refers to the concatenation of the repetition levels, the
    -- |   definition levels and the column value, in this exact order.
    -- | - On the encoded versions of the repetition levels, definition levels and
    -- |   column values
    -- | - On the compressed versions of the repetition levels, definition levels
    -- |   and column values where possible;
    -- |   - For v1 data pages, the repetition levels, definition levels and column
    -- |     values are always compressed together. If a compression scheme is
    -- |     specified, the CRC shall be calculated on the compressed version of
    -- |     this concatenation. If no compression scheme is specified, the CRC
    -- |     shall be calculated on the uncompressed version of this concatenation.
    -- |   - For v2 data pages, the repetition levels and definition levels are
    -- |     handled separately from the data and are never compressed (only
    -- |     encoded). If a compression scheme is specified, the CRC shall be
    -- |     calculated on the concatenation of the uncompressed repetition levels,
    -- |     uncompressed definition levels and the compressed column values.
    -- |     If no compression scheme is specified, the CRC shall be calculated on
    -- |     the uncompressed concatenation.
    -- | - In encrypted columns, CRC is calculated after page encryption; the
    -- |   encryption itself is performed after page compression (if compressed)
    -- | If enabled, this allows for disabling checksumming in HDFS if only a few pages need to be read. 
    pageHeaderCrc :: (Maybe Int),
    pageHeaderDataPageHeader :: (Maybe DataPageHeader),
    pageHeaderIndexPageHeader :: (Maybe IndexPageHeader),
    pageHeaderDictionaryPageHeader :: (Maybe DictionaryPageHeader),
    pageHeaderDataPageHeaderV2 :: (Maybe DataPageHeaderV2)}
  deriving (Eq, Ord, Read, Show)

_PageHeader = (Core.Name "hydra.ext.org.apache.parquet.format.PageHeader")

_PageHeader_type = (Core.Name "type")

_PageHeader_uncompressedPageSize = (Core.Name "uncompressedPageSize")

_PageHeader_compressedPageSize = (Core.Name "compressedPageSize")

_PageHeader_crc = (Core.Name "crc")

_PageHeader_dataPageHeader = (Core.Name "dataPageHeader")

_PageHeader_indexPageHeader = (Core.Name "indexPageHeader")

_PageHeader_dictionaryPageHeader = (Core.Name "dictionaryPageHeader")

_PageHeader_dataPageHeaderV2 = (Core.Name "dataPageHeaderV2")

-- | Wrapper struct to store key values
data KeyValue = 
  KeyValue {
    keyValueKey :: String,
    keyValueValue :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_KeyValue = (Core.Name "hydra.ext.org.apache.parquet.format.KeyValue")

_KeyValue_key = (Core.Name "key")

_KeyValue_value = (Core.Name "value")

-- | Wrapper struct to specify sort order
data SortingColumn = 
  SortingColumn {
    -- | The column index (in this row group)
    sortingColumnColumnIdx :: Int,
    -- | If true, indicates this column is sorted in descending order.
    sortingColumnDescending :: Bool,
    -- | If true, nulls will come before non-null values, otherwise, nulls go at the end.
    sortingColumnNullsFirst :: Bool}
  deriving (Eq, Ord, Read, Show)

_SortingColumn = (Core.Name "hydra.ext.org.apache.parquet.format.SortingColumn")

_SortingColumn_columnIdx = (Core.Name "columnIdx")

_SortingColumn_descending = (Core.Name "descending")

_SortingColumn_nullsFirst = (Core.Name "nullsFirst")

-- | statistics of a given page type and encoding
data PageEncodingStats = 
  PageEncodingStats {
    -- | the page type (data/dic/...)
    pageEncodingStatsPageType :: PageType,
    -- | encoding of the page
    pageEncodingStatsEncoding :: Encoding,
    -- | number of pages of this type with this encoding
    pageEncodingStatsCount :: Int}
  deriving (Eq, Ord, Read, Show)

_PageEncodingStats = (Core.Name "hydra.ext.org.apache.parquet.format.PageEncodingStats")

_PageEncodingStats_pageType = (Core.Name "pageType")

_PageEncodingStats_encoding = (Core.Name "encoding")

_PageEncodingStats_count = (Core.Name "count")

-- | Description for column metadata
data ColumnMetaData = 
  ColumnMetaData {
    -- | Type of this column
    columnMetaDataType :: Type,
    -- | Set of all encodings used for this column. The purpose is to validate whether we can decode those pages.
    columnMetaDataEncodings :: [Encoding],
    -- | Path in schema
    columnMetaDataPathInSchema :: [String],
    -- | Compression codec
    columnMetaDataCodec :: CompressionCodec,
    -- | Number of values in this column
    columnMetaDataNumValues :: I.Int64,
    -- | total byte size of all uncompressed pages in this column chunk (including the headers)
    columnMetaDataTotalUncompressedSize :: I.Int64,
    -- | total byte size of all compressed, and potentially encrypted, pages in this column chunk (including the headers)
    columnMetaDataTotalCompressedSize :: I.Int64,
    -- | Optional key/value metadata
    columnMetaDataKeyValueMetadata :: (Maybe [KeyValue]),
    -- | Byte offset from beginning of file to first data page
    columnMetaDataDataPageOffset :: I.Int64,
    -- | Byte offset from beginning of file to root index page
    columnMetaDataIndexPageOffset :: (Maybe I.Int64),
    -- | Byte offset from the beginning of file to first (only) dictionary page
    columnMetaDataDictionaryPageOffset :: (Maybe I.Int64),
    -- | optional statistics for this column chunk
    columnMetaDataStatistics :: (Maybe Statistics),
    -- | Set of all encodings used for pages in this column chunk. This information can be used to determine if all data pages are dictionary encoded for example
    columnMetaDataEncodingStats :: (Maybe [PageEncodingStats]),
    -- | Byte offset from beginning of file to Bloom filter data.
    columnMetaDataBloomFilterOffset :: (Maybe I.Int64)}
  deriving (Eq, Ord, Read, Show)

_ColumnMetaData = (Core.Name "hydra.ext.org.apache.parquet.format.ColumnMetaData")

_ColumnMetaData_type = (Core.Name "type")

_ColumnMetaData_encodings = (Core.Name "encodings")

_ColumnMetaData_pathInSchema = (Core.Name "pathInSchema")

_ColumnMetaData_codec = (Core.Name "codec")

_ColumnMetaData_numValues = (Core.Name "numValues")

_ColumnMetaData_totalUncompressedSize = (Core.Name "totalUncompressedSize")

_ColumnMetaData_totalCompressedSize = (Core.Name "totalCompressedSize")

_ColumnMetaData_keyValueMetadata = (Core.Name "keyValueMetadata")

_ColumnMetaData_dataPageOffset = (Core.Name "dataPageOffset")

_ColumnMetaData_indexPageOffset = (Core.Name "indexPageOffset")

_ColumnMetaData_dictionaryPageOffset = (Core.Name "dictionaryPageOffset")

_ColumnMetaData_statistics = (Core.Name "statistics")

_ColumnMetaData_encodingStats = (Core.Name "encodingStats")

_ColumnMetaData_bloomFilterOffset = (Core.Name "bloomFilterOffset")

data EncryptionWithFooterKey = 
  EncryptionWithFooterKey {}
  deriving (Eq, Ord, Read, Show)

_EncryptionWithFooterKey = (Core.Name "hydra.ext.org.apache.parquet.format.EncryptionWithFooterKey")

data EncryptionWithColumnKey = 
  EncryptionWithColumnKey {
    -- | Column path in schema
    encryptionWithColumnKeyPathInSchema :: [String],
    -- | Retrieval metadata of column encryption key
    encryptionWithColumnKeyKeyMetadata :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_EncryptionWithColumnKey = (Core.Name "hydra.ext.org.apache.parquet.format.EncryptionWithColumnKey")

_EncryptionWithColumnKey_pathInSchema = (Core.Name "pathInSchema")

_EncryptionWithColumnKey_keyMetadata = (Core.Name "keyMetadata")

data ColumnCryptoMetaData = 
  ColumnCryptoMetaDataEncryptionWithFooterKey EncryptionWithFooterKey |
  ColumnCryptoMetaDataEncryptionWithColumnKey EncryptionWithColumnKey
  deriving (Eq, Ord, Read, Show)

_ColumnCryptoMetaData = (Core.Name "hydra.ext.org.apache.parquet.format.ColumnCryptoMetaData")

_ColumnCryptoMetaData_encryptionWithFooterKey = (Core.Name "encryptionWithFooterKey")

_ColumnCryptoMetaData_encryptionWithColumnKey = (Core.Name "encryptionWithColumnKey")

data ColumnChunk = 
  ColumnChunk {
    -- | File where column data is stored.  If not set, assumed to be same file as metadata.  This path is relative to the current file.
    columnChunkFilePath :: (Maybe String),
    -- | Byte offset in file_path to the ColumnMetaData
    columnChunkFileOffset :: I.Int64,
    -- | Column metadata for this chunk. This is the same content as what is at file_path/file_offset.  Having it here has it replicated in the file metadata.
    columnChunkMetaData :: (Maybe ColumnMetaData),
    -- | File offset of ColumnChunk's OffsetIndex
    columnChunkOffsetIndexOffset :: (Maybe I.Int64),
    -- | Size of ColumnChunk's OffsetIndex, in bytes
    columnChunkOffsetIndexLength :: (Maybe Int),
    -- | File offset of ColumnChunk's ColumnIndex
    columnChunkColumnIndexOffset :: (Maybe I.Int64),
    -- | Size of ColumnChunk's ColumnIndex, in bytes
    columnChunkColumnIndexLength :: (Maybe Int),
    -- | Crypto metadata of encrypted columns
    columnChunkCryptoMetadata :: (Maybe ColumnCryptoMetaData),
    -- | Encrypted column metadata for this chunk
    columnChunkEncryptedColumnMetadata :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_ColumnChunk = (Core.Name "hydra.ext.org.apache.parquet.format.ColumnChunk")

_ColumnChunk_filePath = (Core.Name "filePath")

_ColumnChunk_fileOffset = (Core.Name "fileOffset")

_ColumnChunk_metaData = (Core.Name "metaData")

_ColumnChunk_offsetIndexOffset = (Core.Name "offsetIndexOffset")

_ColumnChunk_offsetIndexLength = (Core.Name "offsetIndexLength")

_ColumnChunk_columnIndexOffset = (Core.Name "columnIndexOffset")

_ColumnChunk_columnIndexLength = (Core.Name "columnIndexLength")

_ColumnChunk_cryptoMetadata = (Core.Name "cryptoMetadata")

_ColumnChunk_encryptedColumnMetadata = (Core.Name "encryptedColumnMetadata")

data RowGroup = 
  RowGroup {
    -- | Metadata for each column chunk in this row group. This list must have the same order as the SchemaElement list in FileMetaData.
    rowGroupColumns :: [ColumnChunk],
    -- | Total byte size of all the uncompressed column data in this row group
    rowGroupTotalByteSize :: I.Int64,
    -- | Number of rows in this row group
    rowGroupNumRows :: I.Int64,
    -- | If set, specifies a sort ordering of the rows in this RowGroup. The sorting columns can be a subset of all the columns.
    rowGroupSortingColumns :: (Maybe [SortingColumn]),
    -- | Byte offset from beginning of file to first page (data or dictionary) in this row group
    rowGroupFileOffset :: (Maybe I.Int64),
    -- | Total byte size of all compressed (and potentially encrypted) column data in this row group
    rowGroupTotalCompressedSize :: (Maybe I.Int64),
    -- | Row group ordinal in the file
    rowGroupOrdinal :: (Maybe I.Int16)}
  deriving (Eq, Ord, Read, Show)

_RowGroup = (Core.Name "hydra.ext.org.apache.parquet.format.RowGroup")

_RowGroup_columns = (Core.Name "columns")

_RowGroup_totalByteSize = (Core.Name "totalByteSize")

_RowGroup_numRows = (Core.Name "numRows")

_RowGroup_sortingColumns = (Core.Name "sortingColumns")

_RowGroup_fileOffset = (Core.Name "fileOffset")

_RowGroup_totalCompressedSize = (Core.Name "totalCompressedSize")

_RowGroup_ordinal = (Core.Name "ordinal")

-- | Union to specify the order used for the min_value and max_value fields for a column. This union takes the role of an enhanced enum that allows rich elements (which will be needed for a collation-based ordering in the future). Possible values are:
-- | * TypeDefinedOrder - the column uses the order defined by its logical or physical type (if there is no logical type).
-- | If the reader does not support the value of this union, min and max stats for this column should be ignored. 
data ColumnOrder = 
  -- | The sort orders for logical types are:
  -- |   UTF8 - unsigned byte-wise comparison
  -- |   INT8 - signed comparison
  -- |   INT16 - signed comparison
  -- |   INT32 - signed comparison
  -- |   INT64 - signed comparison
  -- |   UINT8 - unsigned comparison
  -- |   UINT16 - unsigned comparison
  -- |   UINT32 - unsigned comparison
  -- |   UINT64 - unsigned comparison
  -- |   DECIMAL - signed comparison of the represented value
  -- |   DATE - signed comparison
  -- |   TIME_MILLIS - signed comparison
  -- |   TIME_MICROS - signed comparison
  -- |   TIMESTAMP_MILLIS - signed comparison
  -- |   TIMESTAMP_MICROS - signed comparison
  -- |   INTERVAL - unsigned comparison
  -- |   JSON - unsigned byte-wise comparison
  -- |   BSON - unsigned byte-wise comparison
  -- |   ENUM - unsigned byte-wise comparison
  -- |   LIST - undefined
  -- |   MAP - undefined
  -- | In the absence of logical types, the sort order is determined by the physical type:
  -- |   BOOLEAN - false, true
  -- |   INT32 - signed comparison
  -- |   INT64 - signed comparison
  -- |   INT96 (only used for legacy timestamps) - undefined
  -- |   FLOAT - signed comparison of the represented value (*)
  -- |   DOUBLE - signed comparison of the represented value (*)
  -- |   BYTE_ARRAY - unsigned byte-wise comparison
  -- |   FIXED_LEN_BYTE_ARRAY - unsigned byte-wise comparison
  -- | (*) Because the sorting order is not specified properly for floating
  -- |     point values (relations vs. total ordering) the following
  -- |     compatibility rules should be applied when reading statistics:
  -- |     - If the min is a NaN, it should be ignored.
  -- |     - If the max is a NaN, it should be ignored.
  -- |     - If the min is +0, the row group may contain -0 values as well.
  -- |     - If the max is -0, the row group may contain +0 values as well.
  -- |     - When looking for NaN values, min and max should be ignored.
  ColumnOrderTypeOrder 
  deriving (Eq, Ord, Read, Show)

_ColumnOrder = (Core.Name "hydra.ext.org.apache.parquet.format.ColumnOrder")

_ColumnOrder_typeOrder = (Core.Name "typeOrder")

data PageLocation = 
  PageLocation {
    -- | Offset of the page in the file
    pageLocationOffset :: I.Int64,
    -- | Size of the page, including header. Sum of compressed_page_size and header length
    pageLocationCompressedPageSize :: Int,
    -- | Index within the RowGroup of the first row of the page; this means pages change on record boundaries (r = 0).
    pageLocationFirstRowIndex :: I.Int64}
  deriving (Eq, Ord, Read, Show)

_PageLocation = (Core.Name "hydra.ext.org.apache.parquet.format.PageLocation")

_PageLocation_offset = (Core.Name "offset")

_PageLocation_compressedPageSize = (Core.Name "compressedPageSize")

_PageLocation_firstRowIndex = (Core.Name "firstRowIndex")

data OffsetIndex = 
  OffsetIndex {
    -- | PageLocations, ordered by increasing PageLocation.offset. It is required that page_locations[i].first_row_index < page_locations[i+1].first_row_index.
    offsetIndexPageLocations :: [PageLocation]}
  deriving (Eq, Ord, Read, Show)

_OffsetIndex = (Core.Name "hydra.ext.org.apache.parquet.format.OffsetIndex")

_OffsetIndex_pageLocations = (Core.Name "pageLocations")

-- | Description for ColumnIndex. Each <array-field>[i] refers to the page at OffsetIndex.page_locations[i]
data ColumnIndex = 
  ColumnIndex {
    -- | A list of Boolean values to determine the validity of the corresponding min and max values. If true, a page contains only null values, and writers have to set the corresponding entries in min_values and max_values to byte[0], so that all lists have the same length. If false, the corresponding entries in min_values and max_values must be valid.
    columnIndexNullPages :: [Bool],
    -- | minValues and maxValues are lists containing lower and upper bounds for the values of each page determined by the ColumnOrder of the column. These may be the actual minimum and maximum values found on a page, but can also be (more compact) values that do not exist on a page. For example, instead of storing "Blart Versenwald III", a writer may set min_values[i]="B", max_values[i]="C". Such more compact values must still be valid values within the column's logical type. Readers must make sure that list entries are populated before using them by inspecting null_pages.
    columnIndexMinValues :: [String],
    columnIndexMaxValues :: [String],
    -- | Stores whether both min_values and max_values are orderd and if so, in which direction. This allows readers to perform binary searches in both lists. Readers cannot assume that max_values[i] <= min_values[i+1], even if the lists are ordered.
    columnIndexBoundaryOrder :: BoundaryOrder,
    -- | A list containing the number of null values for each page
    columnIndexNullCounts :: (Maybe [I.Int64])}
  deriving (Eq, Ord, Read, Show)

_ColumnIndex = (Core.Name "hydra.ext.org.apache.parquet.format.ColumnIndex")

_ColumnIndex_nullPages = (Core.Name "nullPages")

_ColumnIndex_minValues = (Core.Name "minValues")

_ColumnIndex_maxValues = (Core.Name "maxValues")

_ColumnIndex_boundaryOrder = (Core.Name "boundaryOrder")

_ColumnIndex_nullCounts = (Core.Name "nullCounts")

data AesGcmV1 = 
  AesGcmV1 {
    -- | AAD prefix
    aesGcmV1AadPrefix :: (Maybe String),
    -- | Unique file identifier part of AAD suffix
    aesGcmV1AadFileUnique :: (Maybe String),
    -- | In files encrypted with AAD prefix without storing it, readers must supply the prefix
    aesGcmV1SupplyAadPrefix :: (Maybe Bool)}
  deriving (Eq, Ord, Read, Show)

_AesGcmV1 = (Core.Name "hydra.ext.org.apache.parquet.format.AesGcmV1")

_AesGcmV1_aadPrefix = (Core.Name "aadPrefix")

_AesGcmV1_aadFileUnique = (Core.Name "aadFileUnique")

_AesGcmV1_supplyAadPrefix = (Core.Name "supplyAadPrefix")

data AesGcmCtrV1 = 
  AesGcmCtrV1 {
    -- | AAD prefix
    aesGcmCtrV1AadPrefix :: (Maybe String),
    -- | Unique file identifier part of AAD suffix
    aesGcmCtrV1AadFileUnique :: (Maybe String),
    -- | In files encrypted with AAD prefix without storing it, readers must supply the prefix
    aesGcmCtrV1SupplyAadPrefix :: (Maybe Bool)}
  deriving (Eq, Ord, Read, Show)

_AesGcmCtrV1 = (Core.Name "hydra.ext.org.apache.parquet.format.AesGcmCtrV1")

_AesGcmCtrV1_aadPrefix = (Core.Name "aadPrefix")

_AesGcmCtrV1_aadFileUnique = (Core.Name "aadFileUnique")

_AesGcmCtrV1_supplyAadPrefix = (Core.Name "supplyAadPrefix")

data EncryptionAlgorithm = 
  EncryptionAlgorithmAesGcmV1 AesGcmV1 |
  EncryptionAlgorithmAesGcmCtrV1 AesGcmCtrV1
  deriving (Eq, Ord, Read, Show)

_EncryptionAlgorithm = (Core.Name "hydra.ext.org.apache.parquet.format.EncryptionAlgorithm")

_EncryptionAlgorithm_aesGcmV1 = (Core.Name "aesGcmV1")

_EncryptionAlgorithm_aesGcmCtrV1 = (Core.Name "aesGcmCtrV1")

-- | Description for file metadata
data FileMetaData = 
  FileMetaData {
    -- | Version of this file
    fileMetaDataVersion :: Int,
    -- | Parquet schema for this file.  This schema contains metadata for all the columns. The schema is represented as a tree with a single root.  The nodes of the tree are flattened to a list by doing a depth-first traversal. The column metadata contains the path in the schema for that column which can be used to map columns to nodes in the schema. The first element is the root
    fileMetaDataSchema :: [SchemaElement],
    -- | Number of rows in this file
    fileMetaDataNumRows :: I.Int64,
    -- | Row groups in this file
    fileMetaDataRowGroups :: [RowGroup],
    -- | Optional key/value metadata
    fileMetaDataKeyValueMetadata :: (Maybe [KeyValue]),
    -- | String for application that wrote this file.  This should be in the format <Application> version <App Version> (build <App Build Hash>). e.g. impala version 1.0 (build 6cf94d29b2b7115df4de2c06e2ab4326d721eb55)
    fileMetaDataCreatedBy :: (Maybe String),
    -- | Sort order used for the min_value and max_value fields in the Statistics objects and the min_values and max_values fields in the ColumnIndex objects of each column in this file. Sort orders are listed in the order matching the columns in the schema. The indexes are not necessary the same though, because only leaf nodes of the schema are represented in the list of sort orders.
    -- | Without column_orders, the meaning of the min_value and max_value fields in the Statistics object and the ColumnIndex object is undefined. To ensure well-defined behaviour, if these fields are written to a Parquet file, column_orders must be written as well.
    -- | The obsolete min and max fields in the Statistics object are always sorted by signed comparison regardless of column_orders.
    fileMetaDataColumnOrders :: (Maybe [ColumnOrder]),
    -- | Encryption algorithm. This field is set only in encrypted files with plaintext footer. Files with encrypted footer store algorithm id in FileCryptoMetaData structure.
    fileMetaDataEncryptionAlgorithm :: (Maybe EncryptionAlgorithm),
    -- | Retrieval metadata of key used for signing the footer. Used only in encrypted files with plaintext footer.
    fileMetaDataFooterSigningKeyMetadata :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_FileMetaData = (Core.Name "hydra.ext.org.apache.parquet.format.FileMetaData")

_FileMetaData_version = (Core.Name "version")

_FileMetaData_schema = (Core.Name "schema")

_FileMetaData_numRows = (Core.Name "numRows")

_FileMetaData_rowGroups = (Core.Name "rowGroups")

_FileMetaData_keyValueMetadata = (Core.Name "keyValueMetadata")

_FileMetaData_createdBy = (Core.Name "createdBy")

_FileMetaData_columnOrders = (Core.Name "columnOrders")

_FileMetaData_encryptionAlgorithm = (Core.Name "encryptionAlgorithm")

_FileMetaData_footerSigningKeyMetadata = (Core.Name "footerSigningKeyMetadata")

-- | Crypto metadata for files with encrypted footer
data FileCryptoMetaData = 
  FileCryptoMetaData {
    -- | Encryption algorithm. This field is only used for files with encrypted footer. Files with plaintext footer store algorithm id inside footer (FileMetaData structure).
    fileCryptoMetaDataEncryptionAlgorithm :: EncryptionAlgorithm,
    -- | Retrieval metadata of key used for encryption of footer, and (possibly) columns
    fileCryptoMetaDataKeyMetadata :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_FileCryptoMetaData = (Core.Name "hydra.ext.org.apache.parquet.format.FileCryptoMetaData")

_FileCryptoMetaData_encryptionAlgorithm = (Core.Name "encryptionAlgorithm")

_FileCryptoMetaData_keyMetadata = (Core.Name "keyMetadata")