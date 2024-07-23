-- | Mapping of hydra/core constructs in a host language like Haskell or Java  to their native Hydra counterparts as terms.  This includes an implementation of LambdaGraph's epsilon encoding (types to terms).

module Hydra.CoreEncoding where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Optionals as Optionals
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

coreEncodeAnnotatedTerm :: (Core.Annotated Core.Term -> Core.Term)
coreEncodeAnnotatedTerm a = (Core.TermAnnotated (Core.Annotated {
  Core.annotatedSubject = (coreEncodeTerm (Core.annotatedSubject a)),
  Core.annotatedAnnotation = (Core.annotatedAnnotation a)}))

coreEncodeAnnotatedType :: (Core.Annotated Core.Type -> Core.Term)
coreEncodeAnnotatedType at = (Core.TermAnnotated (Core.Annotated {
  Core.annotatedSubject = (coreEncodeType (Core.annotatedSubject at)),
  Core.annotatedAnnotation = (Core.annotatedAnnotation at)}))

coreEncodeApplication :: (Core.Application -> Core.Term)
coreEncodeApplication app = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Application"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "function"),
      Core.fieldTerm = (coreEncodeTerm (Core.applicationFunction app))},
    Core.Field {
      Core.fieldName = (Core.FieldName "argument"),
      Core.fieldTerm = (coreEncodeTerm (Core.applicationArgument app))}]}))

coreEncodeApplicationType :: (Core.ApplicationType -> Core.Term)
coreEncodeApplicationType at = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.ApplicationType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "function"),
      Core.fieldTerm = (coreEncodeType (Core.applicationTypeFunction at))},
    Core.Field {
      Core.fieldName = (Core.FieldName "argument"),
      Core.fieldTerm = (coreEncodeType (Core.applicationTypeArgument at))}]}))

coreEncodeCaseStatement :: (Core.CaseStatement -> Core.Term)
coreEncodeCaseStatement cs = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.CaseStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.caseStatementTypeName cs))},
    Core.Field {
      Core.fieldName = (Core.FieldName "default"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map coreEncodeTerm (Core.caseStatementDefault cs)))},
    Core.Field {
      Core.fieldName = (Core.FieldName "cases"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeField (Core.caseStatementCases cs)))}]}))

coreEncodeElimination :: (Core.Elimination -> Core.Term)
coreEncodeElimination x = case x of
  Core.EliminationList v0 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "list"),
      Core.fieldTerm = (coreEncodeTerm v0)}}))
  Core.EliminationOptional v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "optional"),
      Core.fieldTerm = (coreEncodeOptionalCases v1)}}))
  Core.EliminationProduct v2 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "product"),
      Core.fieldTerm = (coreEncodeTupleProjection v2)}}))
  Core.EliminationRecord v3 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "record"),
      Core.fieldTerm = (coreEncodeProjection v3)}}))
  Core.EliminationUnion v4 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "union"),
      Core.fieldTerm = (coreEncodeCaseStatement v4)}}))
  Core.EliminationWrap v5 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "wrap"),
      Core.fieldTerm = (coreEncodeName v5)}}))

coreEncodeField :: (Core.Field -> Core.Term)
coreEncodeField f = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Field"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "name"),
      Core.fieldTerm = (Core.TermWrap (Core.Nominal {
        Core.nominalTypeName = (Core.Name "hydra/core.FieldName"),
        Core.nominalObject = (Core.TermLiteral (Core.LiteralString (Core.unFieldName (Core.fieldName f))))}))},
    Core.Field {
      Core.fieldName = (Core.FieldName "term"),
      Core.fieldTerm = (coreEncodeTerm (Core.fieldTerm f))}]}))

coreEncodeFieldName :: (Core.FieldName -> Core.Term)
coreEncodeFieldName fn = (Core.TermWrap (Core.Nominal {
  Core.nominalTypeName = (Core.Name "hydra/core.FieldName"),
  Core.nominalObject = (Core.TermLiteral (Core.LiteralString (Core.unFieldName fn)))}))

coreEncodeFieldType :: (Core.FieldType -> Core.Term)
coreEncodeFieldType ft = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.FieldType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "name"),
      Core.fieldTerm = (coreEncodeFieldName (Core.fieldTypeName ft))},
    Core.Field {
      Core.fieldName = (Core.FieldName "type"),
      Core.fieldTerm = (coreEncodeType (Core.fieldTypeType ft))}]}))

coreEncodeFloatType :: (Core.FloatType -> Core.Term)
coreEncodeFloatType x = case x of
  Core.FloatTypeBigfloat -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "bigfloat"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.FloatTypeFloat32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float32"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.FloatTypeFloat64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float64"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))

coreEncodeFloatValue :: (Core.FloatValue -> Core.Term)
coreEncodeFloatValue x = case x of
  Core.FloatValueBigfloat v9 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "bigfloat"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat v9)))}}))
  Core.FloatValueFloat32 v10 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float32"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 v10)))}}))
  Core.FloatValueFloat64 v11 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float64"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 v11)))}}))

coreEncodeFunction :: (Core.Function -> Core.Term)
coreEncodeFunction x = case x of
  Core.FunctionElimination v12 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "elimination"),
      Core.fieldTerm = (coreEncodeElimination v12)}}))
  Core.FunctionLambda v13 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "lambda"),
      Core.fieldTerm = (coreEncodeLambda v13)}}))
  Core.FunctionPrimitive v14 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "primitive"),
      Core.fieldTerm = (coreEncodeName v14)}}))

coreEncodeFunctionType :: (Core.FunctionType -> Core.Term)
coreEncodeFunctionType ft = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.FunctionType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "domain"),
      Core.fieldTerm = (coreEncodeType (Core.functionTypeDomain ft))},
    Core.Field {
      Core.fieldName = (Core.FieldName "codomain"),
      Core.fieldTerm = (coreEncodeType (Core.functionTypeCodomain ft))}]}))

coreEncodeInjection :: (Core.Injection -> Core.Term)
coreEncodeInjection i = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Injection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.injectionTypeName i))},
    Core.Field {
      Core.fieldName = (Core.FieldName "field"),
      Core.fieldTerm = (coreEncodeField (Core.injectionField i))}]}))

coreEncodeIntegerType :: (Core.IntegerType -> Core.Term)
coreEncodeIntegerType x = case x of
  Core.IntegerTypeBigint -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "bigint"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt8 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int8"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt16 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int16"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int32"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int64"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint8 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint8"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint16 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint16"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint32"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint64"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))

coreEncodeIntegerValue :: (Core.IntegerValue -> Core.Term)
coreEncodeIntegerValue x = case x of
  Core.IntegerValueBigint v24 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "bigint"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint v24)))}}))
  Core.IntegerValueInt8 v25 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int8"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 v25)))}}))
  Core.IntegerValueInt16 v26 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int16"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 v26)))}}))
  Core.IntegerValueInt32 v27 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int32"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v27)))}}))
  Core.IntegerValueInt64 v28 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int64"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 v28)))}}))
  Core.IntegerValueUint8 v29 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint8"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 v29)))}}))
  Core.IntegerValueUint16 v30 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint16"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 v30)))}}))
  Core.IntegerValueUint32 v31 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint32"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 v31)))}}))
  Core.IntegerValueUint64 v32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint64"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 v32)))}}))

coreEncodeLambda :: (Core.Lambda -> Core.Term)
coreEncodeLambda l = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "parameter"),
      Core.fieldTerm = (coreEncodeName (Core.lambdaParameter l))},
    Core.Field {
      Core.fieldName = (Core.FieldName "body"),
      Core.fieldTerm = (coreEncodeTerm (Core.lambdaBody l))}]}))

coreEncodeLambdaType :: (Core.LambdaType -> Core.Term)
coreEncodeLambdaType lt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.LambdaType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "parameter"),
      Core.fieldTerm = (coreEncodeName (Core.lambdaTypeParameter lt))},
    Core.Field {
      Core.fieldName = (Core.FieldName "body"),
      Core.fieldTerm = (coreEncodeType (Core.lambdaTypeBody lt))}]}))

coreEncodeLiteral :: (Core.Literal -> Core.Term)
coreEncodeLiteral x = case x of
  Core.LiteralBinary v33 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "binary"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralBinary v33))}}))
  Core.LiteralBoolean v34 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "boolean"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean v34))}}))
  Core.LiteralFloat v35 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float"),
      Core.fieldTerm = (coreEncodeFloatValue v35)}}))
  Core.LiteralInteger v36 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "integer"),
      Core.fieldTerm = (coreEncodeIntegerValue v36)}}))
  Core.LiteralString v37 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "string"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v37))}}))

coreEncodeLiteralType :: (Core.LiteralType -> Core.Term)
coreEncodeLiteralType x = case x of
  Core.LiteralTypeBinary -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "binary"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.LiteralTypeBoolean -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "boolean"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))
  Core.LiteralTypeFloat v40 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float"),
      Core.fieldTerm = (coreEncodeFloatType v40)}}))
  Core.LiteralTypeInteger v41 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "integer"),
      Core.fieldTerm = (coreEncodeIntegerType v41)}}))
  Core.LiteralTypeString -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "string"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra/core.Unit"),
        Core.recordFields = []}))}}))

coreEncodeMapType :: (Core.MapType -> Core.Term)
coreEncodeMapType mt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "keys"),
      Core.fieldTerm = (coreEncodeType (Core.mapTypeKeys mt))},
    Core.Field {
      Core.fieldName = (Core.FieldName "values"),
      Core.fieldTerm = (coreEncodeType (Core.mapTypeValues mt))}]}))

coreEncodeName :: (Core.Name -> Core.Term)
coreEncodeName fn = (Core.TermWrap (Core.Nominal {
  Core.nominalTypeName = (Core.Name "hydra/core.Name"),
  Core.nominalObject = (Core.TermLiteral (Core.LiteralString (Core.unName fn)))}))

coreEncodeNominalTerm :: (Core.Nominal Core.Term -> Core.Term)
coreEncodeNominalTerm n = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Nominal"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.nominalTypeName n))},
    Core.Field {
      Core.fieldName = (Core.FieldName "object"),
      Core.fieldTerm = (coreEncodeTerm (Core.nominalObject n))}]}))

coreEncodeNominalType :: (Core.Nominal Core.Type -> Core.Term)
coreEncodeNominalType nt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Nominal"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.nominalTypeName nt))},
    Core.Field {
      Core.fieldName = (Core.FieldName "object"),
      Core.fieldTerm = (coreEncodeType (Core.nominalObject nt))}]}))

coreEncodeOptionalCases :: (Core.OptionalCases -> Core.Term)
coreEncodeOptionalCases oc = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.OptionalCases"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "nothing"),
      Core.fieldTerm = (coreEncodeTerm (Core.optionalCasesNothing oc))},
    Core.Field {
      Core.fieldName = (Core.FieldName "just"),
      Core.fieldTerm = (coreEncodeTerm (Core.optionalCasesJust oc))}]}))

coreEncodeProjection :: (Core.Projection -> Core.Term)
coreEncodeProjection p = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Projection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.projectionTypeName p))},
    Core.Field {
      Core.fieldName = (Core.FieldName "field"),
      Core.fieldTerm = (coreEncodeFieldName (Core.projectionField p))}]}))

coreEncodeRecord :: (Core.Record -> Core.Term)
coreEncodeRecord r = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Record"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.recordTypeName r))},
    Core.Field {
      Core.fieldName = (Core.FieldName "fields"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeField (Core.recordFields r)))}]}))

coreEncodeRowType :: (Core.RowType -> Core.Term)
coreEncodeRowType rt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.RowType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.rowTypeTypeName rt))},
    Core.Field {
      Core.fieldName = (Core.FieldName "extends"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map coreEncodeName (Core.rowTypeExtends rt)))},
    Core.Field {
      Core.fieldName = (Core.FieldName "fields"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeFieldType (Core.rowTypeFields rt)))}]}))

coreEncodeSum :: (Core.Sum -> Core.Term)
coreEncodeSum s = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Sum"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "index"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.sumIndex s))))},
    Core.Field {
      Core.fieldName = (Core.FieldName "size"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.sumSize s))))},
    Core.Field {
      Core.fieldName = (Core.FieldName "term"),
      Core.fieldTerm = (coreEncodeTerm (Core.sumTerm s))}]}))

coreEncodeTerm :: (Core.Term -> Core.Term)
coreEncodeTerm x = case x of
  Core.TermAnnotated v43 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "annotated"),
      Core.fieldTerm = (coreEncodeAnnotatedTerm v43)}}))
  Core.TermApplication v44 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "application"),
      Core.fieldTerm = (coreEncodeApplication v44)}}))
  Core.TermFunction v45 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "function"),
      Core.fieldTerm = (coreEncodeFunction v45)}}))
  Core.TermLiteral v46 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "literal"),
      Core.fieldTerm = (coreEncodeLiteral v46)}}))
  Core.TermList v47 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "list"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeTerm v47))}}))
  Core.TermOptional v48 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "optional"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map coreEncodeTerm v48))}}))
  Core.TermProduct v49 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "product"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeTerm v49))}}))
  Core.TermRecord v50 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "record"),
      Core.fieldTerm = (coreEncodeRecord v50)}}))
  Core.TermSum v51 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "sum"),
      Core.fieldTerm = (coreEncodeSum v51)}}))
  Core.TermUnion v52 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "union"),
      Core.fieldTerm = (coreEncodeInjection v52)}}))
  Core.TermVariable v53 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "variable"),
      Core.fieldTerm = (coreEncodeName v53)}}))
  Core.TermWrap v54 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "wrap"),
      Core.fieldTerm = (coreEncodeNominalTerm v54)}}))
  _ -> (Core.TermLiteral (Core.LiteralString "not implemented"))

coreEncodeTupleProjection :: (Core.TupleProjection -> Core.Term)
coreEncodeTupleProjection tp = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.TupleProjection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "arity"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.tupleProjectionArity tp))))},
    Core.Field {
      Core.fieldName = (Core.FieldName "index"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.tupleProjectionIndex tp))))}]}))

coreEncodeType :: (Core.Type -> Core.Term)
coreEncodeType x = case x of
  Core.TypeAnnotated v55 -> (Core.TermAnnotated (Core.Annotated {
    Core.annotatedSubject = (coreEncodeType (Core.annotatedSubject v55)),
    Core.annotatedAnnotation = (Core.annotatedAnnotation v55)}))
  Core.TypeApplication v56 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "application"),
      Core.fieldTerm = (coreEncodeApplicationType v56)}}))
  Core.TypeFunction v57 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "function"),
      Core.fieldTerm = (coreEncodeFunctionType v57)}}))
  Core.TypeLambda v58 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "lambda"),
      Core.fieldTerm = (coreEncodeLambdaType v58)}}))
  Core.TypeList v59 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "list"),
      Core.fieldTerm = (coreEncodeType v59)}}))
  Core.TypeLiteral v60 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "literal"),
      Core.fieldTerm = (coreEncodeLiteralType v60)}}))
  Core.TypeMap v61 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "map"),
      Core.fieldTerm = (coreEncodeMapType v61)}}))
  Core.TypeOptional v62 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "optional"),
      Core.fieldTerm = (coreEncodeType v62)}}))
  Core.TypeProduct v63 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "product"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeType v63))}}))
  Core.TypeRecord v64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "record"),
      Core.fieldTerm = (coreEncodeRowType v64)}}))
  Core.TypeSet v65 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "set"),
      Core.fieldTerm = (coreEncodeType v65)}}))
  Core.TypeStream v66 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "stream"),
      Core.fieldTerm = (coreEncodeType v66)}}))
  Core.TypeSum v67 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "sum"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeType v67))}}))
  Core.TypeUnion v68 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "union"),
      Core.fieldTerm = (coreEncodeRowType v68)}}))
  Core.TypeVariable v69 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "variable"),
      Core.fieldTerm = (coreEncodeName v69)}}))
  Core.TypeWrap v70 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "wrap"),
      Core.fieldTerm = (coreEncodeNominalType v70)}}))