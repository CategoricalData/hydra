-- | Implementation of LambdaGraph's epsilon encoding, which maps types to terms and back

module Hydra.TypeEncoding where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Optionals as Optionals
import Data.Int
import Data.List
import Data.Map
import Data.Set

epsilonEncodeAnnotatedType :: (Core.Annotated (Core.Type a) a -> Core.Term a)
epsilonEncodeAnnotatedType at = (Core.TermAnnotated (Core.Annotated {
  Core.annotatedSubject = (epsilonEncodeType (Core.annotatedSubject at)),
  Core.annotatedAnnotation = (Core.annotatedAnnotation at)}))

epsilonEncodeApplicationType :: (Core.ApplicationType a -> Core.Term a)
epsilonEncodeApplicationType at = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.ApplicationType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "function"),
      Core.fieldTerm = (epsilonEncodeType (Core.applicationTypeFunction at))},
    Core.Field {
      Core.fieldName = (Core.FieldName "argument"),
      Core.fieldTerm = (epsilonEncodeType (Core.applicationTypeArgument at))}]}))

epsilonEncodeFieldName :: (Core.FieldName -> Core.Term a)
epsilonEncodeFieldName fn = (Core.TermLiteral (Core.LiteralString (Core.unFieldName fn)))

epsilonEncodeFieldType :: (Core.FieldType a -> Core.Term a)
epsilonEncodeFieldType ft = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.FieldType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "name"),
      Core.fieldTerm = (epsilonEncodeFieldName (Core.fieldTypeName ft))},
    Core.Field {
      Core.fieldName = (Core.FieldName "type"),
      Core.fieldTerm = (epsilonEncodeType (Core.fieldTypeType ft))}]}))

epsilonEncodeFloatType :: (Core.FloatType -> Core.Term a)
epsilonEncodeFloatType x = case x of
  Core.FloatTypeBigfloat -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "bigfloat"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.FloatTypeFloat32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float32"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.FloatTypeFloat64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float64"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))

epsilonEncodeFunctionType :: (Core.FunctionType a -> Core.Term a)
epsilonEncodeFunctionType ft = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.FunctionType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "domain"),
      Core.fieldTerm = (epsilonEncodeType (Core.functionTypeDomain ft))},
    Core.Field {
      Core.fieldName = (Core.FieldName "codomain"),
      Core.fieldTerm = (epsilonEncodeType (Core.functionTypeCodomain ft))}]}))

epsilonEncodeIntegerType :: (Core.IntegerType -> Core.Term a)
epsilonEncodeIntegerType x = case x of
  Core.IntegerTypeBigint -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "bigint"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt8 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int8"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt16 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int16"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int32"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int64"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint8 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint8"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint16 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint16"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint32"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint64"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))

epsilonEncodeLambdaType :: (Core.LambdaType a -> Core.Term a)
epsilonEncodeLambdaType lt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.LambdaType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "parameter"),
      Core.fieldTerm = (epsilonEncodeName (Core.lambdaTypeParameter lt))},
    Core.Field {
      Core.fieldName = (Core.FieldName "body"),
      Core.fieldTerm = (epsilonEncodeType (Core.lambdaTypeBody lt))}]}))

epsilonEncodeLiteralType :: (Core.LiteralType -> Core.Term a)
epsilonEncodeLiteralType x = case x of
  Core.LiteralTypeBinary -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "binary"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.LiteralTypeBoolean -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "boolean"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))
  Core.LiteralTypeFloat v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float"),
      Core.fieldTerm = (epsilonEncodeFloatType v)}}))
  Core.LiteralTypeInteger v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "integer"),
      Core.fieldTerm = (epsilonEncodeIntegerType v)}}))
  Core.LiteralTypeString -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "string"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.UnitType"),
        Core.recordFields = []}))}}))

epsilonEncodeMapType :: (Core.MapType a -> Core.Term a)
epsilonEncodeMapType mt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "keys"),
      Core.fieldTerm = (epsilonEncodeType (Core.mapTypeKeys mt))},
    Core.Field {
      Core.fieldName = (Core.FieldName "values"),
      Core.fieldTerm = (epsilonEncodeType (Core.mapTypeValues mt))}]}))

epsilonEncodeName :: (Core.Name -> Core.Term a)
epsilonEncodeName name = (Core.TermLiteral (Core.LiteralString (Core.unName name)))

epsilonEncodeNominalType :: (Core.Nominal (Core.Type a) -> Core.Term a)
epsilonEncodeNominalType nt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Nominal"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (epsilonEncodeName (Core.nominalTypeName nt))},
    Core.Field {
      Core.fieldName = (Core.FieldName "object"),
      Core.fieldTerm = (epsilonEncodeType (Core.nominalObject nt))}]}))

epsilonEncodeRowType :: (Core.RowType a -> Core.Term a)
epsilonEncodeRowType rt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.RowType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (epsilonEncodeName (Core.rowTypeTypeName rt))},
    Core.Field {
      Core.fieldName = (Core.FieldName "extends"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map epsilonEncodeName (Core.rowTypeExtends rt)))},
    Core.Field {
      Core.fieldName = (Core.FieldName "fields"),
      Core.fieldTerm = (Core.TermList (Lists.map epsilonEncodeFieldType (Core.rowTypeFields rt)))}]}))

epsilonEncodeType :: (Core.Type a -> Core.Term a)
epsilonEncodeType x = case x of
  Core.TypeAnnotated v -> (Core.TermAnnotated (Core.Annotated {
    Core.annotatedSubject = (epsilonEncodeType (Core.annotatedSubject v)),
    Core.annotatedAnnotation = (Core.annotatedAnnotation v)}))
  Core.TypeApplication v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "application"),
      Core.fieldTerm = (epsilonEncodeApplicationType v)}}))
  Core.TypeFunction v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "function"),
      Core.fieldTerm = (epsilonEncodeFunctionType v)}}))
  Core.TypeLambda v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "lambda"),
      Core.fieldTerm = (epsilonEncodeLambdaType v)}}))
  Core.TypeList v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "list"),
      Core.fieldTerm = (epsilonEncodeType v)}}))
  Core.TypeLiteral v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "literal"),
      Core.fieldTerm = (epsilonEncodeLiteralType v)}}))
  Core.TypeMap v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "map"),
      Core.fieldTerm = (epsilonEncodeMapType v)}}))
  Core.TypeOptional v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "optional"),
      Core.fieldTerm = (epsilonEncodeType v)}}))
  Core.TypeProduct v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "product"),
      Core.fieldTerm = (Core.TermList (Lists.map epsilonEncodeType v))}}))
  Core.TypeRecord v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "record"),
      Core.fieldTerm = (epsilonEncodeRowType v)}}))
  Core.TypeSet v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "set"),
      Core.fieldTerm = (epsilonEncodeType v)}}))
  Core.TypeStream v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "stream"),
      Core.fieldTerm = (epsilonEncodeType v)}}))
  Core.TypeSum v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "sum"),
      Core.fieldTerm = (Core.TermList (Lists.map epsilonEncodeType v))}}))
  Core.TypeUnion v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "union"),
      Core.fieldTerm = (epsilonEncodeRowType v)}}))
  Core.TypeVariable v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "variable"),
      Core.fieldTerm = (epsilonEncodeName v)}}))
  Core.TypeWrap v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "wrap"),
      Core.fieldTerm = (epsilonEncodeNominalType v)}}))