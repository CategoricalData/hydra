-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.io.delta.parquet

module Hydra.Dsl.Ext.Io.Delta.Parquet where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Io.Delta.Parquet as Parquet
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

arrayType :: (Phantoms.TTerm Parquet.DataType -> Phantoms.TTerm Bool -> Phantoms.TTerm Parquet.ArrayType)
arrayType elementType containsNull = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.ArrayType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "elementType"),
      Core.fieldTerm = (Phantoms.unTTerm elementType)},
    Core.Field {
      Core.fieldName = (Core.Name "containsNull"),
      Core.fieldTerm = (Phantoms.unTTerm containsNull)}]})))

arrayTypeElementType :: (Phantoms.TTerm Parquet.ArrayType -> Phantoms.TTerm Parquet.DataType)
arrayTypeElementType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.ArrayType"),
    Core.projectionField = (Core.Name "elementType")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

arrayTypeContainsNull :: (Phantoms.TTerm Parquet.ArrayType -> Phantoms.TTerm Bool)
arrayTypeContainsNull x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.ArrayType"),
    Core.projectionField = (Core.Name "containsNull")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

arrayTypeWithElementType :: (Phantoms.TTerm Parquet.ArrayType -> Phantoms.TTerm Parquet.DataType -> Phantoms.TTerm Parquet.ArrayType)
arrayTypeWithElementType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.ArrayType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "elementType"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "containsNull"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.ArrayType"),
          Core.projectionField = (Core.Name "containsNull")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

arrayTypeWithContainsNull :: (Phantoms.TTerm Parquet.ArrayType -> Phantoms.TTerm Bool -> Phantoms.TTerm Parquet.ArrayType)
arrayTypeWithContainsNull original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.ArrayType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "elementType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.ArrayType"),
          Core.projectionField = (Core.Name "elementType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "containsNull"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

basePrimitiveTypeBinary :: (Phantoms.TTerm Parquet.BasePrimitiveType)
basePrimitiveTypeBinary = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "binary"),
    Core.fieldTerm = Core.TermUnit}})))

basePrimitiveTypeBoolean :: (Phantoms.TTerm Parquet.BasePrimitiveType)
basePrimitiveTypeBoolean = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "boolean"),
    Core.fieldTerm = Core.TermUnit}})))

basePrimitiveTypeByte :: (Phantoms.TTerm Parquet.BasePrimitiveType)
basePrimitiveTypeByte = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "byte"),
    Core.fieldTerm = Core.TermUnit}})))

basePrimitiveTypeDate :: (Phantoms.TTerm Parquet.BasePrimitiveType)
basePrimitiveTypeDate = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "date"),
    Core.fieldTerm = Core.TermUnit}})))

basePrimitiveTypeDouble :: (Phantoms.TTerm Parquet.BasePrimitiveType)
basePrimitiveTypeDouble = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "double"),
    Core.fieldTerm = Core.TermUnit}})))

basePrimitiveTypeFloat :: (Phantoms.TTerm Parquet.BasePrimitiveType)
basePrimitiveTypeFloat = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "float"),
    Core.fieldTerm = Core.TermUnit}})))

basePrimitiveTypeInteger :: (Phantoms.TTerm Parquet.BasePrimitiveType)
basePrimitiveTypeInteger = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "integer"),
    Core.fieldTerm = Core.TermUnit}})))

basePrimitiveTypeLong :: (Phantoms.TTerm Parquet.BasePrimitiveType)
basePrimitiveTypeLong = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "long"),
    Core.fieldTerm = Core.TermUnit}})))

basePrimitiveTypeShort :: (Phantoms.TTerm Parquet.BasePrimitiveType)
basePrimitiveTypeShort = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "short"),
    Core.fieldTerm = Core.TermUnit}})))

basePrimitiveTypeString :: (Phantoms.TTerm Parquet.BasePrimitiveType)
basePrimitiveTypeString = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "string"),
    Core.fieldTerm = Core.TermUnit}})))

basePrimitiveTypeTimestamp :: (Phantoms.TTerm Parquet.BasePrimitiveType)
basePrimitiveTypeTimestamp = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.BasePrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "timestamp"),
    Core.fieldTerm = Core.TermUnit}})))

dataTypeArray :: (Phantoms.TTerm Parquet.ArrayType -> Phantoms.TTerm Parquet.DataType)
dataTypeArray x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.DataType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "array"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataTypeBase :: (Phantoms.TTerm Parquet.BasePrimitiveType -> Phantoms.TTerm Parquet.DataType)
dataTypeBase x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.DataType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "base"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataTypeDecimal :: (Phantoms.TTerm Parquet.DecimalType -> Phantoms.TTerm Parquet.DataType)
dataTypeDecimal x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.DataType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "decimal"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataTypeMap :: (Phantoms.TTerm Parquet.MapType -> Phantoms.TTerm Parquet.DataType)
dataTypeMap x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.DataType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "map"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataTypeStruct :: (Phantoms.TTerm Parquet.StructType -> Phantoms.TTerm Parquet.DataType)
dataTypeStruct x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.DataType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "struct"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

decimalType :: (Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm Parquet.DecimalType)
decimalType precision scale = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.DecimalType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "precision"),
      Core.fieldTerm = (Phantoms.unTTerm precision)},
    Core.Field {
      Core.fieldName = (Core.Name "scale"),
      Core.fieldTerm = (Phantoms.unTTerm scale)}]})))

decimalTypePrecision :: (Phantoms.TTerm Parquet.DecimalType -> Phantoms.TTerm Int)
decimalTypePrecision x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.DecimalType"),
    Core.projectionField = (Core.Name "precision")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decimalTypeScale :: (Phantoms.TTerm Parquet.DecimalType -> Phantoms.TTerm Int)
decimalTypeScale x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.DecimalType"),
    Core.projectionField = (Core.Name "scale")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decimalTypeWithPrecision :: (Phantoms.TTerm Parquet.DecimalType -> Phantoms.TTerm Int -> Phantoms.TTerm Parquet.DecimalType)
decimalTypeWithPrecision original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.DecimalType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "precision"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "scale"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.DecimalType"),
          Core.projectionField = (Core.Name "scale")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decimalTypeWithScale :: (Phantoms.TTerm Parquet.DecimalType -> Phantoms.TTerm Int -> Phantoms.TTerm Parquet.DecimalType)
decimalTypeWithScale original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.DecimalType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "precision"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.DecimalType"),
          Core.projectionField = (Core.Name "precision")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "scale"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

mapType :: (Phantoms.TTerm Parquet.DataType -> Phantoms.TTerm Parquet.DataType -> Phantoms.TTerm Bool -> Phantoms.TTerm Parquet.MapType)
mapType keyType valueType valueContainsNull = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "keyType"),
      Core.fieldTerm = (Phantoms.unTTerm keyType)},
    Core.Field {
      Core.fieldName = (Core.Name "valueType"),
      Core.fieldTerm = (Phantoms.unTTerm valueType)},
    Core.Field {
      Core.fieldName = (Core.Name "valueContainsNull"),
      Core.fieldTerm = (Phantoms.unTTerm valueContainsNull)}]})))

mapTypeKeyType :: (Phantoms.TTerm Parquet.MapType -> Phantoms.TTerm Parquet.DataType)
mapTypeKeyType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
    Core.projectionField = (Core.Name "keyType")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

mapTypeValueType :: (Phantoms.TTerm Parquet.MapType -> Phantoms.TTerm Parquet.DataType)
mapTypeValueType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
    Core.projectionField = (Core.Name "valueType")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

mapTypeValueContainsNull :: (Phantoms.TTerm Parquet.MapType -> Phantoms.TTerm Bool)
mapTypeValueContainsNull x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
    Core.projectionField = (Core.Name "valueContainsNull")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

mapTypeWithKeyType :: (Phantoms.TTerm Parquet.MapType -> Phantoms.TTerm Parquet.DataType -> Phantoms.TTerm Parquet.MapType)
mapTypeWithKeyType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "keyType"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "valueType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
          Core.projectionField = (Core.Name "valueType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "valueContainsNull"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
          Core.projectionField = (Core.Name "valueContainsNull")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

mapTypeWithValueType :: (Phantoms.TTerm Parquet.MapType -> Phantoms.TTerm Parquet.DataType -> Phantoms.TTerm Parquet.MapType)
mapTypeWithValueType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "keyType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
          Core.projectionField = (Core.Name "keyType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "valueType"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "valueContainsNull"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
          Core.projectionField = (Core.Name "valueContainsNull")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

mapTypeWithValueContainsNull :: (Phantoms.TTerm Parquet.MapType -> Phantoms.TTerm Bool -> Phantoms.TTerm Parquet.MapType)
mapTypeWithValueContainsNull original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "keyType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
          Core.projectionField = (Core.Name "keyType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "valueType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.MapType"),
          Core.projectionField = (Core.Name "valueType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "valueContainsNull"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

structField :: (Phantoms.TTerm String -> Phantoms.TTerm Parquet.DataType -> Phantoms.TTerm Bool -> Phantoms.TTerm Parquet.StructField)
structField name dataType nullable = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "dataType"),
      Core.fieldTerm = (Phantoms.unTTerm dataType)},
    Core.Field {
      Core.fieldName = (Core.Name "nullable"),
      Core.fieldTerm = (Phantoms.unTTerm nullable)}]})))

structFieldName :: (Phantoms.TTerm Parquet.StructField -> Phantoms.TTerm String)
structFieldName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

structFieldDataType :: (Phantoms.TTerm Parquet.StructField -> Phantoms.TTerm Parquet.DataType)
structFieldDataType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
    Core.projectionField = (Core.Name "dataType")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

structFieldNullable :: (Phantoms.TTerm Parquet.StructField -> Phantoms.TTerm Bool)
structFieldNullable x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
    Core.projectionField = (Core.Name "nullable")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

structFieldWithName :: (Phantoms.TTerm Parquet.StructField -> Phantoms.TTerm String -> Phantoms.TTerm Parquet.StructField)
structFieldWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "dataType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
          Core.projectionField = (Core.Name "dataType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "nullable"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
          Core.projectionField = (Core.Name "nullable")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

structFieldWithDataType :: (Phantoms.TTerm Parquet.StructField -> Phantoms.TTerm Parquet.DataType -> Phantoms.TTerm Parquet.StructField)
structFieldWithDataType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "dataType"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "nullable"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
          Core.projectionField = (Core.Name "nullable")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

structFieldWithNullable :: (Phantoms.TTerm Parquet.StructField -> Phantoms.TTerm Bool -> Phantoms.TTerm Parquet.StructField)
structFieldWithNullable original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "dataType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructField"),
          Core.projectionField = (Core.Name "dataType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "nullable"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

structType :: (Phantoms.TTerm [Parquet.StructField] -> Phantoms.TTerm Parquet.StructType)
structType fields = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = (Phantoms.unTTerm fields)}]})))

structTypeFields :: (Phantoms.TTerm Parquet.StructType -> Phantoms.TTerm [Parquet.StructField])
structTypeFields x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructType"),
    Core.projectionField = (Core.Name "fields")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

structTypeWithFields :: (Phantoms.TTerm Parquet.StructType -> Phantoms.TTerm [Parquet.StructField] -> Phantoms.TTerm Parquet.StructType)
structTypeWithFields original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.io.delta.parquet.StructType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))
