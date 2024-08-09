-- | A partial Delta Parquet model

module Hydra.Langs.Parquet.Delta where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

data ArrayType = 
  ArrayType {
    arrayTypeElementType :: DataType,
    arrayTypeContainsNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra/langs/parquet/delta.ArrayType")

_ArrayType_elementType = (Core.Name "elementType")

_ArrayType_containsNull = (Core.Name "containsNull")

data DataType = 
  DataTypeArray ArrayType |
  DataTypeBinary  |
  DataTypeBoolean  |
  DataTypeByte  |
  DataTypeDate  |
  DataTypeDecimal DecimalType |
  DataTypeDouble  |
  DataTypeFloat  |
  DataTypeInteger  |
  DataTypeLong  |
  DataTypeMap MapType |
  DataTypeNull  |
  DataTypeShort  |
  DataTypeString  |
  DataTypeStruct StructType
  deriving (Eq, Ord, Read, Show)

_DataType = (Core.Name "hydra/langs/parquet/delta.DataType")

_DataType_array = (Core.Name "array")

_DataType_binary = (Core.Name "binary")

_DataType_boolean = (Core.Name "boolean")

_DataType_byte = (Core.Name "byte")

_DataType_date = (Core.Name "date")

_DataType_decimal = (Core.Name "decimal")

_DataType_double = (Core.Name "double")

_DataType_float = (Core.Name "float")

_DataType_integer = (Core.Name "integer")

_DataType_long = (Core.Name "long")

_DataType_map = (Core.Name "map")

_DataType_null = (Core.Name "null")

_DataType_short = (Core.Name "short")

_DataType_string = (Core.Name "string")

_DataType_struct = (Core.Name "struct")

data DecimalType = 
  DecimalType {
    decimalTypePrecision :: Int,
    decimalTypeScale :: Int}
  deriving (Eq, Ord, Read, Show)

_DecimalType = (Core.Name "hydra/langs/parquet/delta.DecimalType")

_DecimalType_precision = (Core.Name "precision")

_DecimalType_scale = (Core.Name "scale")

data MapType = 
  MapType {
    mapTypeKeyType :: DataType,
    mapTypeValueType :: DataType,
    mapTypeValueContainsNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_MapType = (Core.Name "hydra/langs/parquet/delta.MapType")

_MapType_keyType = (Core.Name "keyType")

_MapType_valueType = (Core.Name "valueType")

_MapType_valueContainsNull = (Core.Name "valueContainsNull")

data StructField = 
  StructField {
    structFieldName :: String,
    structFieldDataType :: DataType,
    structFieldNullable :: Bool}
  deriving (Eq, Ord, Read, Show)

_StructField = (Core.Name "hydra/langs/parquet/delta.StructField")

_StructField_name = (Core.Name "name")

_StructField_dataType = (Core.Name "dataType")

_StructField_nullable = (Core.Name "nullable")

data StructType = 
  StructType {
    structTypeFields :: [StructField]}
  deriving (Eq, Ord, Read, Show)

_StructType = (Core.Name "hydra/langs/parquet/delta.StructType")

_StructType_fields = (Core.Name "fields")