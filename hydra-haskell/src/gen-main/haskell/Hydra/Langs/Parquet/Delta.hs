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

_ArrayType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/parquet/delta.ArrayType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "elementType"),
      Core.fieldTypeType = _DataType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "containsNull"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}]}))

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

_DataType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/parquet/delta.DataType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _ArrayType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "binary"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "byte"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "date"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "decimal"),
      Core.fieldTypeType = _DecimalType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "double"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "float"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "integer"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "long"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "map"),
      Core.fieldTypeType = _MapType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "null"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "short"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "struct"),
      Core.fieldTypeType = _StructType_type_}]}))

data DecimalType = 
  DecimalType {
    decimalTypePrecision :: Int,
    decimalTypeScale :: Int}
  deriving (Eq, Ord, Read, Show)

_DecimalType = (Core.Name "hydra/langs/parquet/delta.DecimalType")

_DecimalType_precision = (Core.Name "precision")

_DecimalType_scale = (Core.Name "scale")

_DecimalType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/parquet/delta.DecimalType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "precision"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scale"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}]}))

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

_MapType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/parquet/delta.MapType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "keyType"),
      Core.fieldTypeType = _DataType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "valueType"),
      Core.fieldTypeType = _DataType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "valueContainsNull"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}]}))

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

_StructField_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/parquet/delta.StructField"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataType"),
      Core.fieldTypeType = _DataType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nullable"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}]}))

data StructType = 
  StructType {
    structTypeFields :: [StructField]}
  deriving (Eq, Ord, Read, Show)

_StructType = (Core.Name "hydra/langs/parquet/delta.StructType")

_StructType_fields = (Core.Name "fields")

_StructType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/parquet/delta.StructType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fields"),
      Core.fieldTypeType = (Core.TypeList _StructField_type_)}]}))