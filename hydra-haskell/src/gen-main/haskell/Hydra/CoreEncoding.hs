-- | Mapping of hydra.core constructs in a host language like Haskell or Java  to their native Hydra counterparts as terms.  This includes an implementation of LambdaGraph's epsilon encoding (types to terms).

module Hydra.CoreEncoding where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Strip as Strip
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

coreEncodeAnnotatedTerm :: (Core.AnnotatedTerm -> Core.Term)
coreEncodeAnnotatedTerm a = (Core.TermAnnotated (Core.AnnotatedTerm {
  Core.annotatedTermSubject = (coreEncodeTerm (Core.annotatedTermSubject a)),
  Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation a)}))

coreEncodeAnnotatedType :: (Core.AnnotatedType -> Core.Term)
coreEncodeAnnotatedType at = (Core.TermAnnotated (Core.AnnotatedTerm {
  Core.annotatedTermSubject = (coreEncodeType (Core.annotatedTypeSubject at)),
  Core.annotatedTermAnnotation = (Core.annotatedTypeAnnotation at)}))

coreEncodeApplication :: (Core.Application -> Core.Term)
coreEncodeApplication app = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Application"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (coreEncodeTerm (Core.applicationFunction app))},
    Core.Field {
      Core.fieldName = (Core.Name "argument"),
      Core.fieldTerm = (coreEncodeTerm (Core.applicationArgument app))}]}))

coreEncodeApplicationType :: (Core.ApplicationType -> Core.Term)
coreEncodeApplicationType at = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.ApplicationType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (coreEncodeType (Core.applicationTypeFunction at))},
    Core.Field {
      Core.fieldName = (Core.Name "argument"),
      Core.fieldTerm = (coreEncodeType (Core.applicationTypeArgument at))}]}))

coreEncodeCaseStatement :: (Core.CaseStatement -> Core.Term)
coreEncodeCaseStatement cs = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.caseStatementTypeName cs))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map coreEncodeTerm (Core.caseStatementDefault cs)))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeField (Core.caseStatementCases cs)))}]}))

coreEncodeElimination :: (Core.Elimination -> Core.Term)
coreEncodeElimination x = case x of
  Core.EliminationList v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "list"),
      Core.fieldTerm = (coreEncodeTerm v1)}}))
  Core.EliminationOptional v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "optional"),
      Core.fieldTerm = (coreEncodeOptionalCases v1)}}))
  Core.EliminationProduct v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "product"),
      Core.fieldTerm = (coreEncodeTupleProjection v1)}}))
  Core.EliminationRecord v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "record"),
      Core.fieldTerm = (coreEncodeProjection v1)}}))
  Core.EliminationUnion v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = (coreEncodeCaseStatement v1)}}))
  Core.EliminationWrap v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "wrap"),
      Core.fieldTerm = (coreEncodeName v1)}}))

coreEncodeField :: (Core.Field -> Core.Term)
coreEncodeField f = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Field"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
        Core.wrappedTermObject = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldName f))))}))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (coreEncodeTerm (Core.fieldTerm f))}]}))

coreEncodeFieldType :: (Core.FieldType -> Core.Term)
coreEncodeFieldType ft = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.FieldType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (coreEncodeName (Core.fieldTypeName ft))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (coreEncodeType (Core.fieldTypeType ft))}]}))

coreEncodeFloatType :: (Core.FloatType -> Core.Term)
coreEncodeFloatType x = case x of
  Core.FloatTypeBigfloat -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "bigfloat"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.FloatTypeFloat32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "float32"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.FloatTypeFloat64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "float64"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))

coreEncodeFloatValue :: (Core.FloatValue -> Core.Term)
coreEncodeFloatValue x = case x of
  Core.FloatValueBigfloat v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "bigfloat"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat v1)))}}))
  Core.FloatValueFloat32 v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "float32"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 v1)))}}))
  Core.FloatValueFloat64 v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "float64"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 v1)))}}))

coreEncodeFunction :: (Core.Function -> Core.Term)
coreEncodeFunction x = case x of
  Core.FunctionElimination v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "elimination"),
      Core.fieldTerm = (coreEncodeElimination v1)}}))
  Core.FunctionLambda v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "lambda"),
      Core.fieldTerm = (coreEncodeLambda v1)}}))
  Core.FunctionPrimitive v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "primitive"),
      Core.fieldTerm = (coreEncodeName v1)}}))

coreEncodeFunctionType :: (Core.FunctionType -> Core.Term)
coreEncodeFunctionType ft = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.FunctionType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "domain"),
      Core.fieldTerm = (coreEncodeType (Core.functionTypeDomain ft))},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = (coreEncodeType (Core.functionTypeCodomain ft))}]}))

coreEncodeInjection :: (Core.Injection -> Core.Term)
coreEncodeInjection i = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Injection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.injectionTypeName i))},
    Core.Field {
      Core.fieldName = (Core.Name "field"),
      Core.fieldTerm = (coreEncodeField (Core.injectionField i))}]}))

coreEncodeIntegerType :: (Core.IntegerType -> Core.Term)
coreEncodeIntegerType x = case x of
  Core.IntegerTypeBigint -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "bigint"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt8 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int8"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt16 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int16"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int32"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeInt64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int64"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint8 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint8"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint16 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint16"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint32"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.IntegerTypeUint64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint64"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))

coreEncodeIntegerValue :: (Core.IntegerValue -> Core.Term)
coreEncodeIntegerValue x = case x of
  Core.IntegerValueBigint v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "bigint"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint v1)))}}))
  Core.IntegerValueInt8 v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int8"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 v1)))}}))
  Core.IntegerValueInt16 v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int16"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 v1)))}}))
  Core.IntegerValueInt32 v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int32"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v1)))}}))
  Core.IntegerValueInt64 v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int64"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 v1)))}}))
  Core.IntegerValueUint8 v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint8"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 v1)))}}))
  Core.IntegerValueUint16 v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint16"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 v1)))}}))
  Core.IntegerValueUint32 v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint32"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 v1)))}}))
  Core.IntegerValueUint64 v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint64"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 v1)))}}))

coreEncodeLambda :: (Core.Lambda -> Core.Term)
coreEncodeLambda l = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameter"),
      Core.fieldTerm = (coreEncodeName (Core.lambdaParameter l))},
    Core.Field {
      Core.fieldName = (Core.Name "domain"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map coreEncodeType (Core.lambdaDomain l)))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (coreEncodeTerm (Core.lambdaBody l))}]}))

coreEncodeLambdaType :: (Core.LambdaType -> Core.Term)
coreEncodeLambdaType lt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.LambdaType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameter"),
      Core.fieldTerm = (coreEncodeName (Core.lambdaTypeParameter lt))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (coreEncodeType (Core.lambdaTypeBody lt))}]}))

coreEncodeLet :: (Core.Let -> Core.Term)
coreEncodeLet l = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Let"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeLetBinding (Core.letBindings l)))},
    Core.Field {
      Core.fieldName = (Core.Name "environment"),
      Core.fieldTerm = (coreEncodeTerm (Core.letEnvironment l))}]}))

coreEncodeLetBinding :: (Core.LetBinding -> Core.Term)
coreEncodeLetBinding b = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.LetBinding"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (coreEncodeName (Core.letBindingName b))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (coreEncodeTerm (Core.letBindingTerm b))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map coreEncodeTypeScheme (Core.letBindingType b)))}]}))

coreEncodeLiteral :: (Core.Literal -> Core.Term)
coreEncodeLiteral x = case x of
  Core.LiteralBinary v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "binary"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralBinary v1))}}))
  Core.LiteralBoolean v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "boolean"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean v1))}}))
  Core.LiteralFloat v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "float"),
      Core.fieldTerm = (coreEncodeFloatValue v1)}}))
  Core.LiteralInteger v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "integer"),
      Core.fieldTerm = (coreEncodeIntegerValue v1)}}))
  Core.LiteralString v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "string"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v1))}}))

coreEncodeLiteralType :: (Core.LiteralType -> Core.Term)
coreEncodeLiteralType x = case x of
  Core.LiteralTypeBinary -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "binary"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.LiteralTypeBoolean -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "boolean"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))
  Core.LiteralTypeFloat v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "float"),
      Core.fieldTerm = (coreEncodeFloatType v1)}}))
  Core.LiteralTypeInteger v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "integer"),
      Core.fieldTerm = (coreEncodeIntegerType v1)}}))
  Core.LiteralTypeString -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "string"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))

coreEncodeMapType :: (Core.MapType -> Core.Term)
coreEncodeMapType mt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "keys"),
      Core.fieldTerm = (coreEncodeType (Core.mapTypeKeys mt))},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (coreEncodeType (Core.mapTypeValues mt))}]}))

coreEncodeName :: (Core.Name -> Core.Term)
coreEncodeName fn = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
  Core.wrappedTermObject = (Core.TermLiteral (Core.LiteralString (Core.unName fn)))}))

coreEncodeOptionalCases :: (Core.OptionalCases -> Core.Term)
coreEncodeOptionalCases oc = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.OptionalCases"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "nothing"),
      Core.fieldTerm = (coreEncodeTerm (Core.optionalCasesNothing oc))},
    Core.Field {
      Core.fieldName = (Core.Name "just"),
      Core.fieldTerm = (coreEncodeTerm (Core.optionalCasesJust oc))}]}))

coreEncodeProjection :: (Core.Projection -> Core.Term)
coreEncodeProjection p = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Projection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.projectionTypeName p))},
    Core.Field {
      Core.fieldName = (Core.Name "field"),
      Core.fieldTerm = (coreEncodeName (Core.projectionField p))}]}))

coreEncodeRecord :: (Core.Record -> Core.Term)
coreEncodeRecord r = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Record"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.recordTypeName r))},
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeField (Core.recordFields r)))}]}))

coreEncodeRowType :: (Core.RowType -> Core.Term)
coreEncodeRowType rt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.RowType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.rowTypeTypeName rt))},
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeFieldType (Core.rowTypeFields rt)))}]}))

coreEncodeSum :: (Core.Sum -> Core.Term)
coreEncodeSum s = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Sum"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "index"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.sumIndex s))))},
    Core.Field {
      Core.fieldName = (Core.Name "size"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.sumSize s))))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (coreEncodeTerm (Core.sumTerm s))}]}))

coreEncodeTerm :: (Core.Term -> Core.Term)
coreEncodeTerm x = case x of
  Core.TermAnnotated v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "annotated"),
      Core.fieldTerm = (coreEncodeAnnotatedTerm v1)}}))
  Core.TermApplication v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "application"),
      Core.fieldTerm = (coreEncodeApplication v1)}}))
  Core.TermFunction v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (coreEncodeFunction v1)}}))
  Core.TermLet v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "let"),
      Core.fieldTerm = (coreEncodeLet v1)}}))
  Core.TermLiteral v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "literal"),
      Core.fieldTerm = (coreEncodeLiteral v1)}}))
  Core.TermList v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "list"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeTerm v1))}}))
  Core.TermOptional v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "optional"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map coreEncodeTerm v1))}}))
  Core.TermProduct v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "product"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeTerm v1))}}))
  Core.TermRecord v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "record"),
      Core.fieldTerm = (coreEncodeRecord v1)}}))
  Core.TermSet v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "set"),
      Core.fieldTerm = (Core.TermSet (Sets.map coreEncodeTerm v1))}}))
  Core.TermSum v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "sum"),
      Core.fieldTerm = (coreEncodeSum v1)}}))
  Core.TermTypeAbstraction v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeAbstraction"),
      Core.fieldTerm = (coreEncodeTypeAbstraction v1)}}))
  Core.TermTypeApplication v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeApplication"),
      Core.fieldTerm = (coreEncodeTypedTerm v1)}}))
  Core.TermTyped v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typed"),
      Core.fieldTerm = (coreEncodeTypedTerm v1)}}))
  Core.TermUnion v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = (coreEncodeInjection v1)}}))
  Core.TermVariable v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "variable"),
      Core.fieldTerm = (coreEncodeName v1)}}))
  Core.TermWrap v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "wrap"),
      Core.fieldTerm = (coreEncodeWrappedTerm v1)}}))
  _ -> (Core.TermLiteral (Core.LiteralString "not implemented"))

coreEncodeTupleProjection :: (Core.TupleProjection -> Core.Term)
coreEncodeTupleProjection tp = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TupleProjection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "arity"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.tupleProjectionArity tp))))},
    Core.Field {
      Core.fieldName = (Core.Name "index"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.tupleProjectionIndex tp))))}]}))

coreEncodeType :: (Core.Type -> Core.Term)
coreEncodeType x = case x of
  Core.TypeAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
    Core.annotatedTermSubject = (coreEncodeType (Core.annotatedTypeSubject v1)),
    Core.annotatedTermAnnotation = (Core.annotatedTypeAnnotation v1)}))
  Core.TypeApplication v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "application"),
      Core.fieldTerm = (coreEncodeApplicationType v1)}}))
  Core.TypeFunction v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (coreEncodeFunctionType v1)}}))
  Core.TypeLambda v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "lambda"),
      Core.fieldTerm = (coreEncodeLambdaType v1)}}))
  Core.TypeList v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "list"),
      Core.fieldTerm = (coreEncodeType v1)}}))
  Core.TypeLiteral v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "literal"),
      Core.fieldTerm = (coreEncodeLiteralType v1)}}))
  Core.TypeMap v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "map"),
      Core.fieldTerm = (coreEncodeMapType v1)}}))
  Core.TypeOptional v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "optional"),
      Core.fieldTerm = (coreEncodeType v1)}}))
  Core.TypeProduct v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "product"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeType v1))}}))
  Core.TypeRecord v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "record"),
      Core.fieldTerm = (coreEncodeRowType v1)}}))
  Core.TypeSet v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "set"),
      Core.fieldTerm = (coreEncodeType v1)}}))
  Core.TypeSum v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "sum"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeType v1))}}))
  Core.TypeUnion v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = (coreEncodeRowType v1)}}))
  Core.TypeVariable v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "variable"),
      Core.fieldTerm = (coreEncodeName v1)}}))
  Core.TypeWrap v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "wrap"),
      Core.fieldTerm = (coreEncodeWrappedType v1)}}))

coreEncodeTypeAbstraction :: (Core.TypeAbstraction -> Core.Term)
coreEncodeTypeAbstraction l = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypeAbstraction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameter"),
      Core.fieldTerm = (coreEncodeName (Core.typeAbstractionParameter l))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (coreEncodeTerm (Core.typeAbstractionBody l))}]}))

coreEncodeTypeScheme :: (Core.TypeScheme -> Core.Term)
coreEncodeTypeScheme ts = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "variables"),
      Core.fieldTerm = (Core.TermList (Lists.map coreEncodeName (Core.typeSchemeVariables ts)))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (coreEncodeType (Core.typeSchemeType ts))}]}))

coreEncodeTypedTerm :: (Core.TypedTerm -> Core.Term)
coreEncodeTypedTerm tt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypedTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (coreEncodeTerm (Core.typedTermTerm tt))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (coreEncodeType (Core.typedTermType tt))}]}))

coreEncodeWrappedTerm :: (Core.WrappedTerm -> Core.Term)
coreEncodeWrappedTerm n = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.wrappedTermTypeName n))},
    Core.Field {
      Core.fieldName = (Core.Name "object"),
      Core.fieldTerm = (coreEncodeTerm (Core.wrappedTermObject n))}]}))

coreEncodeWrappedType :: (Core.WrappedType -> Core.Term)
coreEncodeWrappedType nt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.WrappedType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (coreEncodeName (Core.wrappedTypeTypeName nt))},
    Core.Field {
      Core.fieldName = (Core.Name "object"),
      Core.fieldTerm = (coreEncodeType (Core.wrappedTypeObject nt))}]}))

isEncodedType :: (Core.Term -> Bool)
isEncodedType t = ((\x -> case x of
  Core.TermApplication v1 -> (isEncodedType (Core.applicationFunction v1))
  Core.TermUnion v1 -> (Equality.equalString "hydra.core.Type" (Core.unName (Core.injectionTypeName v1)))
  _ -> False) (Strip.stripTerm t))

isType :: (Core.Type -> Bool)
isType t = ((\x -> case x of
  Core.TypeApplication v1 -> (isType (Core.applicationTypeFunction v1))
  Core.TypeLambda v1 -> (isType (Core.lambdaTypeBody v1))
  Core.TypeUnion v1 -> (Equality.equalString "hydra.core.Type" (Core.unName (Core.rowTypeTypeName v1)))
  _ -> False) (Strip.stripType t))

isUnitTerm :: (Core.Term -> Bool)
isUnitTerm t = (Equality.equalTerm (Strip.fullyStripTerm t) (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Unit"),
  Core.recordFields = []})))

isUnitType :: (Core.Type -> Bool)
isUnitType t = (Equality.equalType (Strip.stripType t) (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra.core.Unit"),
  Core.rowTypeFields = []})))