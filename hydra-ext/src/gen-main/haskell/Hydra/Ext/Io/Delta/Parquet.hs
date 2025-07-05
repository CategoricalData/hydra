-- | A partial Delta Parquet model, based on DataType and its subclasses as specified in the 3.0.0 Java API: https://docs.delta.io/3.0.0/api/java/kernel/io/delta/kernel/types/DataType.html

module Hydra.Ext.Io.Delta.Parquet where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Represent array data type.
data ArrayType = 
  ArrayType {
    arrayTypeElementType :: DataType,
    arrayTypeContainsNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra.ext.io.delta.parquet.ArrayType")

_ArrayType_elementType = (Core.Name "elementType")

_ArrayType_containsNull = (Core.Name "containsNull")

-- | Base class for all primitive types DataType.
data BasePrimitiveType = 
  -- | The data type representing byte[] values.
  BasePrimitiveTypeBinary  |
  -- | Data type representing boolean type values.
  BasePrimitiveTypeBoolean  |
  -- | The data type representing byte type values.
  BasePrimitiveTypeByte  |
  -- | A date type, supporting "0001-01-01" through "9999-12-31". Internally, this is represented as the number of days from 1970-01-01.
  BasePrimitiveTypeDate  |
  -- | The data type representing double type values.
  BasePrimitiveTypeDouble  |
  -- | The data type representing float type values.
  BasePrimitiveTypeFloat  |
  -- | The data type representing integer type values.
  BasePrimitiveTypeInteger  |
  -- | The data type representing long type values.
  BasePrimitiveTypeLong  |
  -- | The data type representing short type values.
  BasePrimitiveTypeShort  |
  -- | The data type representing string type values.
  BasePrimitiveTypeString  |
  -- | A timestamp type, supporting [0001-01-01T00:00:00.000000Z, 9999-12-31T23:59:59.999999Z] where the left/right-bound is a date and time of the proleptic Gregorian calendar in UTC+00:00. Internally, this is represented as the number of microseconds since the Unix epoch, 1970-01-01 00:00:00 UTC.
  BasePrimitiveTypeTimestamp 
  deriving (Eq, Ord, Read, Show)

_BasePrimitiveType = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType")

_BasePrimitiveType_binary = (Core.Name "binary")

_BasePrimitiveType_boolean = (Core.Name "boolean")

_BasePrimitiveType_byte = (Core.Name "byte")

_BasePrimitiveType_date = (Core.Name "date")

_BasePrimitiveType_double = (Core.Name "double")

_BasePrimitiveType_float = (Core.Name "float")

_BasePrimitiveType_integer = (Core.Name "integer")

_BasePrimitiveType_long = (Core.Name "long")

_BasePrimitiveType_short = (Core.Name "short")

_BasePrimitiveType_string = (Core.Name "string")

_BasePrimitiveType_timestamp = (Core.Name "timestamp")

data DataType = 
  -- | Represent array data type.
  DataTypeArray ArrayType |
  -- | Base class for all primitive types DataType.
  DataTypeBase BasePrimitiveType |
  -- | A decimal data type.
  DataTypeDecimal DecimalType |
  -- | Data type representing a map type.
  DataTypeMap MapType |
  -- | Struct type which contains one or more columns.
  DataTypeStruct StructType
  deriving (Eq, Ord, Read, Show)

_DataType = (Core.Name "hydra.ext.io.delta.parquet.DataType")

_DataType_array = (Core.Name "array")

_DataType_base = (Core.Name "base")

_DataType_decimal = (Core.Name "decimal")

_DataType_map = (Core.Name "map")

_DataType_struct = (Core.Name "struct")

-- | A decimal data type with fixed precision (the maximum number of digits) and scale (the number of digits on right side of dot). The precision can be up to 38, scale can also be up to 38 (less or equal to precision).
data DecimalType = 
  DecimalType {
    decimalTypePrecision :: Int,
    decimalTypeScale :: Int}
  deriving (Eq, Ord, Read, Show)

_DecimalType = (Core.Name "hydra.ext.io.delta.parquet.DecimalType")

_DecimalType_precision = (Core.Name "precision")

_DecimalType_scale = (Core.Name "scale")

-- | Data type representing a map type.
data MapType = 
  MapType {
    mapTypeKeyType :: DataType,
    mapTypeValueType :: DataType,
    mapTypeValueContainsNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_MapType = (Core.Name "hydra.ext.io.delta.parquet.MapType")

_MapType_keyType = (Core.Name "keyType")

_MapType_valueType = (Core.Name "valueType")

_MapType_valueContainsNull = (Core.Name "valueContainsNull")

-- | Represents a subfield of StructType with additional properties and metadata.
data StructField = 
  StructField {
    structFieldName :: String,
    structFieldDataType :: DataType,
    structFieldNullable :: Bool}
  deriving (Eq, Ord, Read, Show)

_StructField = (Core.Name "hydra.ext.io.delta.parquet.StructField")

_StructField_name = (Core.Name "name")

_StructField_dataType = (Core.Name "dataType")

_StructField_nullable = (Core.Name "nullable")

-- | Struct type which contains one or more columns.
data StructType = 
  StructType {
    structTypeFields :: [StructField]}
  deriving (Eq, Ord, Read, Show)

_StructType = (Core.Name "hydra.ext.io.delta.parquet.StructType")

_StructType_fields = (Core.Name "fields")
