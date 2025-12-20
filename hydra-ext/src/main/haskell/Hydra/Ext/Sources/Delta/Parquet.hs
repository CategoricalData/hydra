module Hydra.Ext.Sources.Delta.Parquet where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.io.delta.parquet"

define :: String -> Type -> Binding
define = defineType ns

delta :: String -> Type
delta = typeref ns

enumVal :: String -> String -> FieldType
enumVal name desc = name>: doc desc T.unit

module_ :: Module
module_ = Module ns elements [Core.ns] [Core.ns] $
    Just ("A partial Delta Parquet model, based on DataType and its subclasses as specified in the 3.0.0 Java API:"
      ++ " https://docs.delta.io/3.0.0/api/java/kernel/io/delta/kernel/types/DataType.html")
  where
    elements = [
      arrayType,
      basePrimitiveType,
      dataType,
      decimalType,
      mapType,
      structField,
      structType]

arrayType :: Binding
arrayType = define "ArrayType" $
  doc "Represent array data type." $
  T.record [
    "elementType">: delta "DataType",
    "containsNull">: T.boolean]

basePrimitiveType :: Binding
basePrimitiveType = define "BasePrimitiveType" $
  doc "Base class for all primitive types DataType." $
  T.union [
    enumVal "binary" "The data type representing byte[] values.",
    enumVal "boolean" "Data type representing boolean type values.",
    enumVal "byte" "The data type representing byte type values.",
    enumVal "date" (
      "A date type, supporting \"0001-01-01\" through \"9999-12-31\"."
      ++ " Internally, this is represented as the number of days from 1970-01-01."),
    enumVal "double" "The data type representing double type values.",
    enumVal "float" "The data type representing float type values.",
    enumVal "integer" "The data type representing integer type values.",
    enumVal "long" "The data type representing long type values.",
    enumVal "short" "The data type representing short type values.",
    enumVal "string" "The data type representing string type values.",
    enumVal "timestamp" (
      "A timestamp type, supporting [0001-01-01T00:00:00.000000Z, 9999-12-31T23:59:59.999999Z]"
      ++ " where the left/right-bound is a date and time of the proleptic Gregorian calendar in UTC+00:00."
      ++ " Internally, this is represented as the number of microseconds since the Unix epoch,"
      ++ " 1970-01-01 00:00:00 UTC.")]

dataType :: Binding
dataType = define "DataType" $
  T.union [
    "array">:
      doc "Represent array data type." $
      delta "ArrayType",
    "base">:
      doc "Base class for all primitive types DataType." $
      delta "BasePrimitiveType",
    "decimal">:
      doc "A decimal data type." $
      delta "DecimalType",
    "map">:
      doc "Data type representing a map type." $
      delta "MapType",
    "struct">:
      doc "Struct type which contains one or more columns." $
      delta "StructType"]

decimalType :: Binding
decimalType = define "DecimalType" $
  doc ("A decimal data type with fixed precision (the maximum number of digits)"
    ++ " and scale (the number of digits on right side of dot)."
    ++ " The precision can be up to 38, scale can also be up to 38 (less or equal to precision).") $
  T.record [
    "precision">: T.int32,
    "scale">: T.int32]

mapType :: Binding
mapType = define "MapType" $
  doc "Data type representing a map type." $
  T.record [
    "keyType">: delta "DataType",
    "valueType">: delta "DataType",
    "valueContainsNull">: T.boolean]

structField :: Binding
structField = define "StructField" $
  doc "Represents a subfield of StructType with additional properties and metadata." $
  T.record [
    "name">: T.string,
    "dataType">: delta "DataType",
    "nullable">: T.boolean]

structType :: Binding
structType = define "StructType" $
  doc "Struct type which contains one or more columns." $
  T.record [
    "fields">: T.list $ delta "StructField"]
