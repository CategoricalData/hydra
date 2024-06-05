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

_ArrayType_elementType = (Core.FieldName "elementType")

_ArrayType_containsNull = (Core.FieldName "containsNull")

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

_DataType_array = (Core.FieldName "array")

_DataType_binary = (Core.FieldName "binary")

_DataType_boolean = (Core.FieldName "boolean")

_DataType_byte = (Core.FieldName "byte")

_DataType_date = (Core.FieldName "date")

_DataType_decimal = (Core.FieldName "decimal")

_DataType_double = (Core.FieldName "double")

_DataType_float = (Core.FieldName "float")

_DataType_integer = (Core.FieldName "integer")

_DataType_long = (Core.FieldName "long")

_DataType_map = (Core.FieldName "map")

_DataType_null = (Core.FieldName "null")

_DataType_short = (Core.FieldName "short")

_DataType_string = (Core.FieldName "string")

_DataType_struct = (Core.FieldName "struct")

data DecimalType = 
  DecimalType {
    decimalTypePrecision :: Int,
    decimalTypeScale :: Int}
  deriving (Eq, Ord, Read, Show)

_DecimalType = (Core.Name "hydra/langs/parquet/delta.DecimalType")

_DecimalType_precision = (Core.FieldName "precision")

_DecimalType_scale = (Core.FieldName "scale")

data MapType = 
  MapType {
    mapTypeKeyType :: DataType,
    mapTypeValueType :: DataType,
    mapTypeValueContainsNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_MapType = (Core.Name "hydra/langs/parquet/delta.MapType")

_MapType_keyType = (Core.FieldName "keyType")

_MapType_valueType = (Core.FieldName "valueType")

_MapType_valueContainsNull = (Core.FieldName "valueContainsNull")

data StructField = 
  StructField {
    structFieldName :: String,
    structFieldDataType :: DataType,
    structFieldNullable :: Bool}
  deriving (Eq, Ord, Read, Show)

_StructField = (Core.Name "hydra/langs/parquet/delta.StructField")

_StructField_name = (Core.FieldName "name")

_StructField_dataType = (Core.FieldName "dataType")

_StructField_nullable = (Core.FieldName "nullable")

data StructType = 
  StructType {
    structTypeFields :: [StructField]}
  deriving (Eq, Ord, Read, Show)

_StructType = (Core.Name "hydra/langs/parquet/delta.StructType")

_StructType_fields = (Core.FieldName "fields")