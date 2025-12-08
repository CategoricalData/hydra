module Hydra.Ext.Sources.Parquet.Format where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.org.apache.parquet.format"

define :: String -> Type -> Binding
define = defineType ns

parquet :: String -> Type
parquet = typeref ns

-- Note: deprecated and trivial/empty type definitions are excluded from this model
module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just ("A model for the Parquet format. Based on the Thrift-based specification at:\n" ++
      "  https://github.com/apache/parquet-format/blob/master/src/main/thrift/parquet.thrift")
  where
    elements = [
      type_,
      fieldRepetitionType,
      statistics,
      decimalType,
      timeUnit,
      timestampType,
      timeType,
      intType,
      logicalType,
      schemaElement,
      encoding,
      compressionCodec,
      pageType,
      boundaryOrder,
      dataPageHeader,
      indexPageHeader,
      dictionaryPageHeader,
      dataPageHeaderV2,
      bloomFilterAlgorithm,
      bloomFilterHash,
      bloomFilterCompression,
      bloomFilterHeader,
      pageHeader,
      keyValue,
      sortingColumn,
      pageEncodingStats,
      columnMetaData,
      encryptionWithFooterKey,
      encryptionWithColumnKey,
      columnCryptoMetaData,
      columnChunk,
      rowGroup,
      columnOrder,
      pageLocation,
      offsetIndex,
      columnIndex,
      aesGcmV1,
      aesGcmCtrV1,
      encryptionAlgorithm,
      fileMetaData,
      fileCryptoMetaData]

type_ :: Binding
type_ = define "Type" $
  doc ("Types supported by Parquet.  These types are intended to be used in combination " ++
       "with the encodings to control the on disk storage format. " ++
       "For example INT16 is not included as a type since a good encoding of INT32 " ++
       "would handle this.") $
  T.enum [
    "boolean",
    "int32",
    "int64",
    "float",
    "double",
    "byteArray",
    "fixedLenByteArray"]

fieldRepetitionType :: Binding
fieldRepetitionType = define "FieldRepetitionType" $
  doc "Representation of Schemas" $
  T.union [
    "required">: doc "This field is required (can not be null) and each record has exactly 1 value." T.unit,
    "optional">: doc "The field is optional (can be null) and each record has 0 or 1 values." T.unit,
    "repeated">: doc "The field is repeated and can contain 0 or more values" T.unit]

statistics :: Binding
statistics = define "Statistics" $
  doc "Statistics per row group and per page. All fields are optional." $
  T.record [
    "nullCount">: T.maybe T.uint64,
    "distinctCount">: T.maybe T.uint64,
    "maxValue">:
      doc ("Max value for the column, determined by its ColumnOrder. " ++
           "Values are encoded using PLAIN encoding, except that variable-length byte " ++
           "arrays do not include a length prefix.") $
      T.maybe T.binary,
    "minValue">:
      doc ("Max value for the column, determined by its ColumnOrder. " ++
           "Values are encoded using PLAIN encoding, except that variable-length byte " ++
           "arrays do not include a length prefix.") $
      T.maybe T.binary]

decimalType :: Binding
decimalType = define "DecimalType" $
  doc ("Decimal logical type annotation. " ++
       "To maintain forward-compatibility in v1, implementations using this logical " ++
       "type must also set scale and precision on the annotated SchemaElement. " ++
       "Allowed for physical types: INT32, INT64, FIXED, and BINARY") $
  T.record [
    "scale">: T.int32,
    "precision">: T.int32]

timeUnit :: Binding
timeUnit = define "TimeUnit" $
  T.enum [
    "millis",
    "micros",
    "nanos"]

timestampType :: Binding
timestampType = define "TimestampType" $
  doc ("Timestamp logical type annotation. " ++
       "Allowed for physical types: INT64") $
  T.record [
    "isAdjustedToUtc">: T.boolean,
    "unit">: parquet "TimeUnit"]

timeType :: Binding
timeType = define "TimeType" $
  doc ("Time logical type annotation. " ++
       "Allowed for physical types: INT32 (millis), INT64 (micros, nanos)") $
  T.record [
    "isAdjustedToUtc">: T.boolean,
    "unit">: parquet "TimeUnit"]

intType :: Binding
intType = define "IntType" $
  doc ("Integer logical type annotation. " ++
      "bitWidth must be 8, 16, 32, or 64. " ++
      "Allowed for physical types: INT32, INT64") $
  T.record [
    "bitWidth">: T.uint8,
    "isSigned">: T.boolean]

logicalType :: Binding
logicalType = define "LogicalType" $
  doc ("LogicalType annotations to replace ConvertedType. " ++
       "To maintain compatibility, implementations using LogicalType for a " ++
       "SchemaElement aust also set the corresponding ConvertedType (if any) " ++
       "from the following table.") $
  T.union [
    "string">: doc "use ConvertedType UTF8" T.unit,
    "map">: doc "use ConvertedType MAP" T.unit,
    "list">: doc "use ConvertedType LIST" T.unit,
    "enum">: doc "use ConvertedType ENUM" T.unit,
    "decimal">:
      doc "use ConvertedType DECIMAL + SchemaElement.{scale, precision}" $
      parquet "DecimalType",
    "date">: doc "use ConvertedType DATE" T.unit,
    "time">:
      doc ("use ConvertedType TIME_MICROS for TIME(isAdjustedToUTC = *, unit = MICROS). " ++
           "use ConvertedType TIME_MILLIS for TIME(isAdjustedToUTC = *, unit = MILLIS)") $
      parquet "TimeType",
    "timestamp">:
      doc ("use ConvertedType TIMESTAMP_MICROS for TIMESTAMP(isAdjustedToUTC = *, unit = MICROS). " ++
           "use ConvertedType TIMESTAMP_MILLIS for TIMESTAMP(isAdjustedToUTC = *, unit = MILLIS)") $
      parquet "TimestampType",
    "integer">:
      doc "use ConvertedType INT_* or UINT_*" $
      parquet "IntType",
    "unknown">:
      doc "no compatible ConvertedType" T.unit,
    "json">: doc "use ConvertedType JSON" T.unit,
    "bson">: doc "use ConvertedType BSON" T.unit,
    "uuid">: doc "no compatible ConvertedType" T.unit]

schemaElement :: Binding
schemaElement = define "SchemaElement" $
  doc ("Represents a element inside a schema definition.\n" ++
       "- if it is a group (inner node) then type is undefined and num_children is defined\n" ++
       "- if it is a primitive type (leaf) then type is defined and num_children is undefined\n" ++
       "the nodes are listed in depth first traversal order.") $
  T.record [
    "type">:
      doc "Data type for this field. Not set if the current element is a non-leaf node" $
      T.maybe $ parquet "Type",
    "typeLength">:
      doc ("If type is FIXED_LEN_BYTE_ARRAY, this is the byte length of the values. " ++
           "Otherwise, if specified, this is the maximum bit length to store any of the values. " ++
           "(e.g. a low cardinality INT col could have this set to 3).  Note that this is " ++
           "in the schema, and therefore fixed for the entire file.") $
      T.maybe T.int32,
    "repetitionType">:
      doc ("repetition of the field. The root of the schema does not have a repetition_type. " ++
           "All other nodes must have one") $
      T.maybe $ parquet "FieldRepetitionType",
    "name">:
      doc "Name of the field in the schema"
      T.string,
    "numChildren">:
      doc ("Nested fields.  Since thrift does not support nested fields, " ++
           "the nesting is flattened to a single list by a depth-first traversal. " ++
           "The children count is used to construct the nested relationship. " ++
           "This field is not set when the element is a primitive type") $
      T.maybe T.int32,
    "fieldId">:
      doc ("When the original schema supports field ids, this will save the " ++
           "original field id in the parquet schema") $
      T.maybe T.int32,
    "logicalType">:
      doc ("The logical type of this SchemaElement. " ++
           "LogicalType replaces ConvertedType, but ConvertedType is still required " ++
           "for some logical types to ensure forward-compatibility in format v1.") $
      T.maybe $ parquet "LogicalType"]

encoding :: Binding
encoding = define "Encoding" $
  doc ("Encodings supported by Parquet.  Not all encodings are valid for all types.  These " ++
       "enums are also used to specify the encoding of definition and repetition levels. " ++
       "See the accompanying doc for the details of the more complicated encodings.") $
  T.union [
    "plain">:
      doc ("Default encoding.\n" ++
           "BOOLEAN - 1 bit per value. 0 is false; 1 is true.\n" ++
           "INT32 - 4 bytes per value.  Stored as little-endian.\n" ++
           "INT64 - 8 bytes per value.  Stored as little-endian.\n" ++
           "FLOAT - 4 bytes per value.  IEEE. Stored as little-endian.\n" ++
           "DOUBLE - 8 bytes per value.  IEEE. Stored as little-endian.\n" ++
           "BYTE_ARRAY - 4 byte length stored as little endian, followed by bytes.\n" ++
           "FIXED_LEN_BYTE_ARRAY - Just the bytes.") $
      T.unit,
    "rle">:
      doc ("Group packed run length encoding. Usable for definition/repetition levels " ++
           "encoding and Booleans (on one bit: 0 is false; 1 is true.)") T.unit,
    "bitPacked">:
      doc ("Bit packed encoding.  This can only be used if the data has a known max " ++
           "width.  Usable for definition/repetition levels encoding.") T.unit,
    "deltaBinaryPacked">:
      doc ("Delta encoding for integers. This can be used for int columns and works best " ++
           "on sorted data") T.unit,
    "deltaLengthByteArray">:
      doc ("Encoding for byte arrays to separate the length values and the data. The lengths " ++
           "are encoded using DELTA_BINARY_PACKED") T.unit,
    "deltaByteArray">:
      doc ("Incremental-encoded byte array. Prefix lengths are encoded using DELTA_BINARY_PACKED. " ++
           "Suffixes are stored as delta length byte arrays.") T.unit,
    "rleDictionary">:
      doc ("Dictionary encoding: the ids are encoded using the RLE encoding") T.unit,
    "byteStreamSplit">:
      doc ("Encoding for floating-point data. " ++
           "K byte-streams are created where K is the size in bytes of the data type. " ++
           "The individual bytes of an FP value are scattered to the corresponding stream and " ++
           "the streams are concatenated. " ++
           "This itself does not reduce the size of the data but can lead to better compression " ++
           "afterwards.") T.unit]

compressionCodec :: Binding
compressionCodec = define "CompressionCodec" $
  doc ("Supported compression algorithms. " ++
       "Codecs added in format version X.Y can be read by readers based on X.Y and later. " ++
       "Codec support may vary between readers based on the format version and " ++
       "libraries available at runtime. " ++
       "See Compression.md for a detailed specification of these algorithms.") $
  T.union [
    "uncompressed">: T.unit,
    "snappy">: T.unit,
    "gzip">: T.unit,
    "lzo">: T.unit,
    "brotli">:
      doc "Added in 2.4" T.unit,
    "zstd">:
      doc "Added in 2.4" T.unit,
    "lz4Raw">:
      doc "Added in 2.9" T.unit]

pageType :: Binding
pageType = define "PageType" $
  T.enum [
    "dataPage",
    "indexPage",
    "dictionaryPage",
    "dataPageV2"]

boundaryOrder :: Binding
boundaryOrder = define "BoundaryOrder" $
  doc ("Enum to annotate whether lists of min/max elements inside ColumnIndex " ++
       "are ordered and if so, in which direction.") $
  T.enum [
    "unordered",
    "ascending",
    "descending"]

dataPageHeader :: Binding
dataPageHeader = define "DataPageHeader" $
  doc "Data page header" $
  T.record [
    "numValues">:
      doc "Number of values, including NULLs, in this data page."
      T.int32,
    "encoding">:
      doc "Encoding used for this data page" $
      parquet "Encoding",
    "definitionLevelEncoding">:
      doc "Encoding used for definition levels" $
      parquet "Encoding",
    "repetitionLevelEncoding">:
      doc "Encoding used for repetition levels" $
      parquet "Encoding",
    "statistics">:
      doc "Optional statistics for the data in this page" $
      T.maybe $ parquet "Statistics"]

indexPageHeader :: Binding
indexPageHeader = define "IndexPageHeader" $ T.record []

dictionaryPageHeader :: Binding
dictionaryPageHeader = define "DictionaryPageHeader" $
  doc ("The dictionary page must be placed at the first position of the column chunk " ++
       "if it is partly or completely dictionary encoded. At most one dictionary page " ++
       "can be placed in a column chunk.") $
  T.record [
    "numValues">:
      doc "Number of values in the dictionary" $
      T.int32,
    "encoding">:
      doc "Encoding using this dictionary page" $
      parquet "Encoding",
    "isSorted">:
      doc "If true, the entries in the dictionary are sorted in ascending order" $
      T.maybe T.boolean]

dataPageHeaderV2 :: Binding
dataPageHeaderV2 = define "DataPageHeaderV2" $
  doc ("New page format allowing reading levels without decompressing the data " ++
       "Repetition and definition levels are uncompressed " ++
       "The remaining section containing the data is compressed if is_compressed is true") $
  T.record [
    "numValues">:
      doc "Number of values, including NULLs, in this data page." $
      T.int32,
    "numNulls">:
      doc ("Number of NULL values, in this data page. " ++
           "Number of non-null = num_values - num_nulls which is also the number of values in the data section") $
      T.int32,
    "numRows">:
      doc "Number of rows in this data page. which means pages change on record boundaries (r = 0)" $
      T.int32,
    "encoding">:
      doc "Encoding used for data in this page" $
      parquet "Encoding",
    "definitionLevelsByteLength">:
      doc "length of the definition levels" $
      T.int32,
    "repetitionLevelsByteLength">:
      doc "length of the repetition levels" $
      T.int32,
    "isCompressed">:
      doc ("whether the values are compressed. " ++
           "Which means the section of the page between " ++
           "definition_levels_byte_length + repetition_levels_byte_length + 1 and compressed_page_size (included) " ++
           "is compressed with the compression_codec. " ++
           "If missing it is considered compressed") $
      T.maybe T.boolean,
    "statistics">:
      doc "optional statistics for the data in this page" $
      T.maybe $ parquet "Statistics"]

bloomFilterAlgorithm :: Binding
bloomFilterAlgorithm = define "BloomFilterAlgorithm" $
  doc "The algorithm used in Bloom filter." $
  T.union [
    "block">:
      doc "Block-based Bloom filter." T.unit]

bloomFilterHash :: Binding
bloomFilterHash = define "BloomFilterHash" $
  doc ("The hash function used in Bloom filter. This function takes the hash of a column value " ++
       "using plain encoding.") $
  T.union [
    "xxhash">:
      doc "xxHash Strategy." T.unit]

bloomFilterCompression :: Binding
bloomFilterCompression = define "BloomFilterCompression" $
  doc "The compression used in the Bloom filter." $
  T.enum [
    "uncompressed"]

bloomFilterHeader :: Binding
bloomFilterHeader = define "BloomFilterHeader" $
  doc ("Bloom filter header is stored at beginning of Bloom filter data of each column " ++
       "and followed by its bitset.") $
  T.record [
    "numBytes">:
      doc "The size of bitset in bytes" $
      T.int32,
    "algorithm">:
      doc "The algorithm for setting bits." $
      parquet "BloomFilterAlgorithm",
    "hash">:
      doc "The hash function used for Bloom filter." $
      parquet "BloomFilterHash",
    "compression">:
      doc "The compression used in the Bloom filter" $
      parquet "BloomFilterCompression"]

pageHeader :: Binding
pageHeader = define "PageHeader" $
  T.record [
    "type">:
      doc "the type of the page: indicates which of the *_header fields is set" $
      parquet "PageType",
    "uncompressedPageSize">:
      doc "Uncompressed page size in bytes (not including this header)" $
     T.int32,
    "compressedPageSize">:
      doc "Compressed (and potentially encrypted) page size in bytes, not including this header" $
      T.int32,
    "crc">:
      doc ("The 32bit CRC for the page, to be be calculated as follows:\n" ++
           "- Using the standard CRC32 algorithm\n" ++
           "- On the data only, i.e. this header should not be included. 'Data'\n" ++
           "  hereby refers to the concatenation of the repetition levels, the\n" ++
           "  definition levels and the column value, in this exact order.\n" ++
           "- On the encoded versions of the repetition levels, definition levels and\n" ++
           "  column values\n" ++
           "- On the compressed versions of the repetition levels, definition levels\n" ++
           "  and column values where possible;\n" ++
           "  - For v1 data pages, the repetition levels, definition levels and column\n" ++
           "    values are always compressed together. If a compression scheme is\n" ++
           "    specified, the CRC shall be calculated on the compressed version of\n" ++
           "    this concatenation. If no compression scheme is specified, the CRC\n" ++
           "    shall be calculated on the uncompressed version of this concatenation.\n" ++
           "  - For v2 data pages, the repetition levels and definition levels are\n" ++
           "    handled separately from the data and are never compressed (only\n" ++
           "    encoded). If a compression scheme is specified, the CRC shall be\n" ++
           "    calculated on the concatenation of the uncompressed repetition levels,\n" ++
           "    uncompressed definition levels and the compressed column values.\n" ++
           "    If no compression scheme is specified, the CRC shall be calculated on\n" ++
           "    the uncompressed concatenation.\n" ++
           "- In encrypted columns, CRC is calculated after page encryption; the\n" ++
           "  encryption itself is performed after page compression (if compressed)\n" ++
           "If enabled, this allows for disabling checksumming in HDFS if only a few " ++
           "pages need to be read. ") $
      T.maybe T.int32,
    "dataPageHeader">:
      T.maybe $ parquet "DataPageHeader",
    "indexPageHeader">:
      T.maybe $ parquet "IndexPageHeader",
    "dictionaryPageHeader">:
      T.maybe $ parquet "DictionaryPageHeader",
    "dataPageHeaderV2">:
      T.maybe $ parquet "DataPageHeaderV2"]

keyValue :: Binding
keyValue = define "KeyValue" $
  doc "Wrapper struct to store key values" $
  T.record [
    "key">: T.string,
    "value">: T.maybe T.string]

sortingColumn :: Binding
sortingColumn = define "SortingColumn" $
  doc "Wrapper struct to specify sort order" $
  T.record [
    "columnIdx">:
      doc "The column index (in this row group)"
      T.int32,
    "descending">:
      doc "If true, indicates this column is sorted in descending order."
      T.boolean,
    "nullsFirst">:
      doc ("If true, nulls will come before non-null values, otherwise, " ++
           "nulls go at the end.")
      T.boolean]

pageEncodingStats :: Binding
pageEncodingStats = define "PageEncodingStats" $
  doc "statistics of a given page type and encoding" $
  T.record [
    "pageType">:
      doc "the page type (data/dic/...)" $
      parquet "PageType",
    "encoding">:
      doc "encoding of the page" $
      parquet "Encoding",
    "count">:
      doc "number of pages of this type with this encoding"
      T.int32]

columnMetaData :: Binding
columnMetaData = define "ColumnMetaData" $
  doc "Description for column metadata" $
  T.record [
    "type">:
      doc "Type of this column" $
      parquet "Type",
    "encodings">:
      doc ("Set of all encodings used for this column. The purpose is to validate " ++
           "whether we can decode those pages.") $
      T.list $ parquet "Encoding",
    "pathInSchema">:
      doc "Path in schema" $
      T.list T.string,
    "codec">:
      doc "Compression codec" $
      parquet "CompressionCodec",
    "numValues">:
      doc "Number of values in this column"
      T.int64,
    "totalUncompressedSize">:
      doc "total byte size of all uncompressed pages in this column chunk (including the headers)"
      T.int64,
    "totalCompressedSize">:
      doc ("total byte size of all compressed, and potentially encrypted, pages " ++
           "in this column chunk (including the headers)")
      T.int64,
    "keyValueMetadata">:
      doc "Optional key/value metadata" $
      T.maybe $ T.list $ parquet "KeyValue",
    "dataPageOffset">:
      doc "Byte offset from beginning of file to first data page"
      T.int64,
    "indexPageOffset">:
      doc "Byte offset from beginning of file to root index page" $
      T.maybe T.int64,
    "dictionaryPageOffset">:
      doc "Byte offset from the beginning of file to first (only) dictionary page" $
      T.maybe T.int64,
    "statistics">:
      doc "optional statistics for this column chunk" $
      T.maybe $ parquet "Statistics",
    "encodingStats">:
      doc ("Set of all encodings used for pages in this column chunk. " ++
           "This information can be used to determine if all data pages are " ++
           "dictionary encoded for example") $
      T.maybe $ T.list $ parquet "PageEncodingStats",
    "bloomFilterOffset">:
      doc "Byte offset from beginning of file to Bloom filter data." $
      T.maybe T.int64]

encryptionWithFooterKey :: Binding
encryptionWithFooterKey = define "EncryptionWithFooterKey" $ T.record []

encryptionWithColumnKey :: Binding
encryptionWithColumnKey = define "EncryptionWithColumnKey" $
  T.record [
    "pathInSchema">:
      doc "Column path in schema" $
      T.list T.string,
    "keyMetadata">:
      doc "Retrieval metadata of column encryption key" $
      T.maybe T.binary]

columnCryptoMetaData :: Binding
columnCryptoMetaData = define "ColumnCryptoMetaData" $
  T.union [
    "encryptionWithFooterKey">: parquet "EncryptionWithFooterKey",
    "encryptionWithColumnKey">: parquet "EncryptionWithColumnKey"]

columnChunk :: Binding
columnChunk = define "ColumnChunk" $
  T.record [
    "filePath">:
      doc ("File where column data is stored.  If not set, assumed to be same file as " ++
           "metadata.  This path is relative to the current file.") $
      T.maybe T.string,
    "fileOffset">:
      doc "Byte offset in file_path to the ColumnMetaData"
      T.int64,
    "metaData">:
      doc ("Column metadata for this chunk. This is the same content as what is at " ++
           "file_path/file_offset.  Having it here has it replicated in the file " ++
           "metadata.") $
      T.maybe $ parquet "ColumnMetaData",
    "offsetIndexOffset">:
      doc "File offset of ColumnChunk's OffsetIndex" $
      T.maybe T.int64,
    "offsetIndexLength">:
      doc "Size of ColumnChunk's OffsetIndex, in bytes" $
      T.maybe T.int32,
    "columnIndexOffset">:
      doc "File offset of ColumnChunk's ColumnIndex" $
      T.maybe T.int64,
    "columnIndexLength">:
      doc "Size of ColumnChunk's ColumnIndex, in bytes" $
      T.maybe T.int32,
    "cryptoMetadata">:
      doc "Crypto metadata of encrypted columns" $
      T.maybe $ parquet "ColumnCryptoMetaData",
    "encryptedColumnMetadata">:
      doc "Encrypted column metadata for this chunk" $
      T.maybe T.binary]

rowGroup :: Binding
rowGroup = define "RowGroup" $
  T.record [
    "columns">:
      doc ("Metadata for each column chunk in this row group. " ++
           "This list must have the same order as the SchemaElement list in FileMetaData.") $
      T.list $ parquet "ColumnChunk",
    "totalByteSize">:
      doc "Total byte size of all the uncompressed column data in this row group"
      T.int64,
    "numRows">:
      doc "Number of rows in this row group"
      T.int64,
    "sortingColumns">:
      doc ("If set, specifies a sort ordering of the rows in this RowGroup. " ++
           "The sorting columns can be a subset of all the columns.") $
      T.maybe $ T.list $ parquet "SortingColumn",
    "fileOffset">:
      doc ("Byte offset from beginning of file to first page (data or dictionary) " ++
           "in this row group") $
      T.maybe T.int64,
    "totalCompressedSize">:
      doc ("Total byte size of all compressed (and potentially encrypted) column data " ++
           "in this row group") $
      T.maybe T.int64,
    "ordinal">:
      doc "Row group ordinal in the file" $
      T.maybe T.int16]

columnOrder :: Binding
columnOrder = define "ColumnOrder" $
  doc ("Union to specify the order used for the min_value and max_value fields for a " ++
       "column. This union takes the role of an enhanced enum that allows rich " ++
       "elements (which will be needed for a collation-based ordering in the future). " ++
       "Possible values are:\n" ++
       "* TypeDefinedOrder - the column uses the order defined by its logical or " ++
       "physical type (if there is no logical type).\n" ++
       "If the reader does not support the value of this union, min and max stats " ++
       "for this column should be ignored. ") $
  T.union [
    "typeOrder">:
      doc ("The sort orders for logical types are:\n" ++
           "  UTF8 - unsigned byte-wise comparison\n" ++
           "  INT8 - signed comparison\n" ++
           "  INT16 - signed comparison\n" ++
           "  INT32 - signed comparison\n" ++
           "  INT64 - signed comparison\n" ++
           "  UINT8 - unsigned comparison\n" ++
           "  UINT16 - unsigned comparison\n" ++
           "  UINT32 - unsigned comparison\n" ++
           "  UINT64 - unsigned comparison\n" ++
           "  DECIMAL - signed comparison of the represented value\n" ++
           "  DATE - signed comparison\n" ++
           "  TIME_MILLIS - signed comparison\n" ++
           "  TIME_MICROS - signed comparison\n" ++
           "  TIMESTAMP_MILLIS - signed comparison\n" ++
           "  TIMESTAMP_MICROS - signed comparison\n" ++
           "  INTERVAL - unsigned comparison\n" ++
           "  JSON - unsigned byte-wise comparison\n" ++
           "  BSON - unsigned byte-wise comparison\n" ++
           "  ENUM - unsigned byte-wise comparison\n" ++
           "  LIST - undefined\n" ++
           "  MAP - undefined\n" ++
           "In the absence of logical types, the sort order is determined by the physical type:\n" ++
           "  BOOLEAN - false, true\n" ++
           "  INT32 - signed comparison\n" ++
           "  INT64 - signed comparison\n" ++
           "  INT96 (only used for legacy timestamps) - undefined\n" ++
           "  FLOAT - signed comparison of the represented value (*)\n" ++
           "  DOUBLE - signed comparison of the represented value (*)\n" ++
           "  BYTE_ARRAY - unsigned byte-wise comparison\n" ++
           "  FIXED_LEN_BYTE_ARRAY - unsigned byte-wise comparison\n" ++
           "(*) Because the sorting order is not specified properly for floating\n" ++
           "    point values (relations vs. total ordering) the following\n" ++
           "    compatibility rules should be applied when reading statistics:\n" ++
           "    - If the min is a NaN, it should be ignored.\n" ++
           "    - If the max is a NaN, it should be ignored.\n" ++
           "    - If the min is +0, the row group may contain -0 values as well.\n" ++
           "    - If the max is -0, the row group may contain +0 values as well.\n" ++
           "    - When looking for NaN values, min and max should be ignored.") T.unit]

pageLocation :: Binding
pageLocation = define "PageLocation" $
  T.record [
    "offset">:
      doc "Offset of the page in the file"
      T.int64,
    "compressedPageSize">:
      doc ("Size of the page, including header. Sum of compressed_page_size and header " ++
           "length")
      T.int32,
    "firstRowIndex">:
      doc ("Index within the RowGroup of the first row of the page; this means pages " ++
           "change on record boundaries (r = 0).")
      T.int64]

offsetIndex :: Binding
offsetIndex = define "OffsetIndex" $
  T.record [
    "pageLocations">:
      doc ("PageLocations, ordered by increasing PageLocation.offset. It is required " ++
           "that page_locations[i].first_row_index < page_locations[i+1].first_row_index.") $
      T.list $ parquet "PageLocation"]

columnIndex :: Binding
columnIndex = define "ColumnIndex" $
  doc ("Description for ColumnIndex. " ++
       "Each <array-field>[i] refers to the page at OffsetIndex.page_locations[i]") $
  T.record [
    "nullPages">:
      doc ("A list of Boolean values to determine the validity of the corresponding " ++
           "min and max values. If true, a page contains only null values, and writers " ++
           "have to set the corresponding entries in min_values and max_values to " ++
           "byte[0], so that all lists have the same length. If false, the " ++
           "corresponding entries in min_values and max_values must be valid.") $
      T.list T.boolean,
    "minValues">:
      doc ("minValues and maxValues are lists containing lower and upper bounds for the values of each page " ++
           "determined by the ColumnOrder of the column. These may be the actual " ++
           "minimum and maximum values found on a page, but can also be (more compact) " ++
           "values that do not exist on a page. For example, instead of storing \"Blart " ++
           "Versenwald III\", a writer may set min_values[i]=\"B\", max_values[i]=\"C\". " ++
           "Such more compact values must still be valid values within the column's " ++
           "logical type. Readers must make sure that list entries are populated before " ++
           "using them by inspecting null_pages.") $
      T.list T.binary,
    "maxValues">: T.list T.binary,
    "boundaryOrder">:
      doc ("Stores whether both min_values and max_values are orderd and if so, in " ++
           "which direction. This allows readers to perform binary searches in both " ++
           "lists. Readers cannot assume that max_values[i] <= min_values[i+1], even " ++
           "if the lists are ordered.") $
      parquet "BoundaryOrder",
    "nullCounts">:
      doc "A list containing the number of null values for each page" $
      T.maybe $ T.list T.int64]

aesGcmV1 :: Binding
aesGcmV1 = define "AesGcmV1" $
  T.record [
    "aadPrefix">:
      doc "AAD prefix" $
      T.maybe T.binary,
    "aadFileUnique">:
      doc "Unique file identifier part of AAD suffix" $
      T.maybe T.binary,
    "supplyAadPrefix">:
      doc ("In files encrypted with AAD prefix without storing it, " ++
           "readers must supply the prefix") $
      T.maybe T.boolean]

aesGcmCtrV1 :: Binding
aesGcmCtrV1 = define "AesGcmCtrV1" $
  T.record [
    "aadPrefix">:
      doc "AAD prefix" $
      T.maybe T.binary,
    "aadFileUnique">:
      doc "Unique file identifier part of AAD suffix" $
      T.maybe T.binary,
    "supplyAadPrefix">:
      doc ("In files encrypted with AAD prefix without storing it, " ++
           "readers must supply the prefix") $
      T.maybe T.boolean]

encryptionAlgorithm :: Binding
encryptionAlgorithm = define "EncryptionAlgorithm" $
  T.union [
    "aesGcmV1">: parquet "AesGcmV1",
    "aesGcmCtrV1">: parquet "AesGcmCtrV1"]

fileMetaData :: Binding
fileMetaData = define "FileMetaData" $
  doc "Description for file metadata" $
  T.record [
    "version">:
      doc "Version of this file"
      T.int32,
    "schema">:
      doc ("Parquet schema for this file.  This schema contains metadata for all the columns. " ++
           "The schema is represented as a tree with a single root.  The nodes of the tree " ++
           "are flattened to a list by doing a depth-first traversal. " ++
           "The column metadata contains the path in the schema for that column which can be " ++
           "used to map columns to nodes in the schema. " ++
           "The first element is the root") $
      T.list $ parquet "SchemaElement",
    "numRows">:
      doc "Number of rows in this file"
      T.int64,
    "rowGroups">:
      doc "Row groups in this file" $
      T.list $ parquet "RowGroup",
    "keyValueMetadata">:
      doc "Optional key/value metadata" $
      T.maybe $ T.list $ parquet "KeyValue",
    "createdBy">:
      doc ("String for application that wrote this file.  This should be in the format " ++
           "<Application> version <App Version> (build <App Build Hash>). " ++
           "e.g. impala version 1.0 (build 6cf94d29b2b7115df4de2c06e2ab4326d721eb55)") $
      T.maybe T.string,
    "columnOrders">:
      doc ("Sort order used for the min_value and max_value fields in the Statistics " ++
           "objects and the min_values and max_values fields in the ColumnIndex " ++
           "objects of each column in this file. Sort orders are listed in the order " ++
           "matching the columns in the schema. The indexes are not necessary the same " ++
           "though, because only leaf nodes of the schema are represented in the list " ++
           "of sort orders.\n" ++
           "Without column_orders, the meaning of the min_value and max_value fields " ++
           "in the Statistics object and the ColumnIndex object is undefined. To ensure " ++
           "well-defined behaviour, if these fields are written to a Parquet file, " ++
           "column_orders must be written as well.\n" ++
           "The obsolete min and max fields in the Statistics object are always sorted " ++
           "by signed comparison regardless of column_orders.") $
      T.maybe $ T.list $ parquet "ColumnOrder",
    "encryptionAlgorithm">:
      doc ("Encryption algorithm. This field is set only in encrypted files " ++
           "with plaintext footer. Files with encrypted footer store algorithm id " ++
           "in FileCryptoMetaData structure.") $
      T.maybe $ parquet "EncryptionAlgorithm",
    "footerSigningKeyMetadata">:
      doc ("Retrieval metadata of key used for signing the footer. " ++
           "Used only in encrypted files with plaintext footer.") $
      T.maybe T.binary]

fileCryptoMetaData :: Binding
fileCryptoMetaData = define "FileCryptoMetaData" $
  doc "Crypto metadata for files with encrypted footer" $
  T.record [
    "encryptionAlgorithm">:
      doc ("Encryption algorithm. This field is only used for files " ++
           "with encrypted footer. Files with plaintext footer store algorithm id " ++
           "inside footer (FileMetaData structure).") $
      parquet "EncryptionAlgorithm",
    "keyMetadata">:
      doc ("Retrieval metadata of key used for encryption of footer, " ++
           "and (possibly) columns") $
      T.maybe T.binary]
