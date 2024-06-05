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
  DataTypeMap MapType |
  DataTypeScalar ScalarType |
  DataTypeStruct StructType
  deriving (Eq, Ord, Read, Show)

_DataType = (Core.Name "hydra/langs/parquet/delta.DataType")

_DataType_array = (Core.FieldName "array")

_DataType_map = (Core.FieldName "map")

_DataType_scalar = (Core.FieldName "scalar")

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

data NumberType = 
  NumberTypeByte  |
  NumberTypeDecimal DecimalType |
  NumberTypeDouble  |
  NumberTypeFloat  |
  NumberTypeInteger  |
  NumberTypeLong  |
  NumberTypeShort 
  deriving (Eq, Ord, Read, Show)

_NumberType = (Core.Name "hydra/langs/parquet/delta.NumberType")

_NumberType_byte = (Core.FieldName "byte")

_NumberType_decimal = (Core.FieldName "decimal")

_NumberType_double = (Core.FieldName "double")

_NumberType_float = (Core.FieldName "float")

_NumberType_integer = (Core.FieldName "integer")

_NumberType_long = (Core.FieldName "long")

_NumberType_short = (Core.FieldName "short")

data ScalarType = 
  ScalarTypeBinary  |
  ScalarTypeBoolean  |
  ScalarTypeDate  |
  ScalarTypeNull  |
  ScalarTypeNumber NumberType |
  ScalarTypeString 
  deriving (Eq, Ord, Read, Show)

_ScalarType = (Core.Name "hydra/langs/parquet/delta.ScalarType")

_ScalarType_binary = (Core.FieldName "binary")

_ScalarType_boolean = (Core.FieldName "boolean")

_ScalarType_date = (Core.FieldName "date")

_ScalarType_null = (Core.FieldName "null")

_ScalarType_number = (Core.FieldName "number")

_ScalarType_string = (Core.FieldName "string")

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