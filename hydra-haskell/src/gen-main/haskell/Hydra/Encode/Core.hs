-- | Mapping of hydra.core constructs in a host language like Haskell or Java  to their native Hydra counterparts as terms.  This includes an implementation of LambdaGraph's epsilon encoding (types to terms).

module Hydra.Encode.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Strip as Strip
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotatedTerm :: (Core.AnnotatedTerm -> Core.Term)
annotatedTerm a = (Core.TermAnnotated (Core.AnnotatedTerm {
  Core.annotatedTermSubject = (term (Core.annotatedTermSubject a)),
  Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation a)}))

annotatedType :: (Core.AnnotatedType -> Core.Term)
annotatedType at = (Core.TermAnnotated (Core.AnnotatedTerm {
  Core.annotatedTermSubject = (type_ (Core.annotatedTypeSubject at)),
  Core.annotatedTermAnnotation = (Core.annotatedTypeAnnotation at)}))

application :: (Core.Application -> Core.Term)
application app = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Application"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (term (Core.applicationFunction app))},
    Core.Field {
      Core.fieldName = (Core.Name "argument"),
      Core.fieldTerm = (term (Core.applicationArgument app))}]}))

applicationType :: (Core.ApplicationType -> Core.Term)
applicationType at = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.ApplicationType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (type_ (Core.applicationTypeFunction at))},
    Core.Field {
      Core.fieldName = (Core.Name "argument"),
      Core.fieldTerm = (type_ (Core.applicationTypeArgument at))}]}))

caseStatement :: (Core.CaseStatement -> Core.Term)
caseStatement cs = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.caseStatementTypeName cs))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map term (Core.caseStatementDefault cs)))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Core.TermList (Lists.map field (Core.caseStatementCases cs)))}]}))

elimination :: (Core.Elimination -> Core.Term)
elimination x = case x of
  Core.EliminationProduct v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "product"),
      Core.fieldTerm = (tupleProjection v1)}}))
  Core.EliminationRecord v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "record"),
      Core.fieldTerm = (projection v1)}}))
  Core.EliminationUnion v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = (caseStatement v1)}}))
  Core.EliminationWrap v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "wrap"),
      Core.fieldTerm = (name v1)}}))

field :: (Core.Field -> Core.Term)
field f = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Field"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
        Core.wrappedTermObject = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldName f))))}))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (term (Core.fieldTerm f))}]}))

fieldType :: (Core.FieldType -> Core.Term)
fieldType ft = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.FieldType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (name (Core.fieldTypeName ft))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (type_ (Core.fieldTypeType ft))}]}))

floatType :: (Core.FloatType -> Core.Term)
floatType x = case x of
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

floatValue :: (Core.FloatValue -> Core.Term)
floatValue x = case x of
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

function :: (Core.Function -> Core.Term)
function x = case x of
  Core.FunctionElimination v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "elimination"),
      Core.fieldTerm = (elimination v1)}}))
  Core.FunctionLambda v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "lambda"),
      Core.fieldTerm = (lambda v1)}}))
  Core.FunctionPrimitive v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "primitive"),
      Core.fieldTerm = (name v1)}}))

functionType :: (Core.FunctionType -> Core.Term)
functionType ft = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.FunctionType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "domain"),
      Core.fieldTerm = (type_ (Core.functionTypeDomain ft))},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = (type_ (Core.functionTypeCodomain ft))}]}))

injection :: (Core.Injection -> Core.Term)
injection i = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Injection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.injectionTypeName i))},
    Core.Field {
      Core.fieldName = (Core.Name "field"),
      Core.fieldTerm = (field (Core.injectionField i))}]}))

integerType :: (Core.IntegerType -> Core.Term)
integerType x = case x of
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

integerValue :: (Core.IntegerValue -> Core.Term)
integerValue x = case x of
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

lambda :: (Core.Lambda -> Core.Term)
lambda l = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameter"),
      Core.fieldTerm = (name (Core.lambdaParameter l))},
    Core.Field {
      Core.fieldName = (Core.Name "domain"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map type_ (Core.lambdaDomain l)))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.lambdaBody l))}]}))

forallType :: (Core.ForallType -> Core.Term)
forallType lt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.ForallType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameter"),
      Core.fieldTerm = (name (Core.forallTypeParameter lt))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (type_ (Core.forallTypeBody lt))}]}))

let_ :: (Core.Let -> Core.Term)
let_ l = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Let"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = (Core.TermList (Lists.map letBinding (Core.letBindings l)))},
    Core.Field {
      Core.fieldName = (Core.Name "environment"),
      Core.fieldTerm = (term (Core.letEnvironment l))}]}))

letBinding :: (Core.LetBinding -> Core.Term)
letBinding b = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.LetBinding"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (name (Core.letBindingName b))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (term (Core.letBindingTerm b))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map typeScheme (Core.letBindingType b)))}]}))

literal :: (Core.Literal -> Core.Term)
literal x = case x of
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
      Core.fieldTerm = (floatValue v1)}}))
  Core.LiteralInteger v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "integer"),
      Core.fieldTerm = (integerValue v1)}}))
  Core.LiteralString v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "string"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v1))}}))

literalType :: (Core.LiteralType -> Core.Term)
literalType x = case x of
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
      Core.fieldTerm = (floatType v1)}}))
  Core.LiteralTypeInteger v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "integer"),
      Core.fieldTerm = (integerType v1)}}))
  Core.LiteralTypeString -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "string"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Unit"),
        Core.recordFields = []}))}}))

mapType :: (Core.MapType -> Core.Term)
mapType mt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "keys"),
      Core.fieldTerm = (type_ (Core.mapTypeKeys mt))},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (type_ (Core.mapTypeValues mt))}]}))

name :: (Core.Name -> Core.Term)
name fn = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
  Core.wrappedTermObject = (Core.TermLiteral (Core.LiteralString (Core.unName fn)))}))

projection :: (Core.Projection -> Core.Term)
projection p = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Projection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.projectionTypeName p))},
    Core.Field {
      Core.fieldName = (Core.Name "field"),
      Core.fieldTerm = (name (Core.projectionField p))}]}))

record :: (Core.Record -> Core.Term)
record r = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Record"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.recordTypeName r))},
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = (Core.TermList (Lists.map field (Core.recordFields r)))}]}))

rowType :: (Core.RowType -> Core.Term)
rowType rt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.RowType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.rowTypeTypeName rt))},
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = (Core.TermList (Lists.map fieldType (Core.rowTypeFields rt)))}]}))

sum_ :: (Core.Sum -> Core.Term)
sum_ s = (Core.TermRecord (Core.Record {
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
      Core.fieldTerm = (term (Core.sumTerm s))}]}))

term :: (Core.Term -> Core.Term)
term x = case x of
  Core.TermAnnotated v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "annotated"),
      Core.fieldTerm = (annotatedTerm v1)}}))
  Core.TermApplication v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "application"),
      Core.fieldTerm = (application v1)}}))
  Core.TermFunction v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (function v1)}}))
  Core.TermLet v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "let"),
      Core.fieldTerm = (let_ v1)}}))
  Core.TermLiteral v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "literal"),
      Core.fieldTerm = (literal v1)}}))
  Core.TermList v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "list"),
      Core.fieldTerm = (Core.TermList (Lists.map term v1))}}))
  Core.TermMap v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "map"),
      Core.fieldTerm = (Core.TermMap (Maps.bimap term term v1))}}))
  Core.TermOptional v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "optional"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map term v1))}}))
  Core.TermProduct v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "product"),
      Core.fieldTerm = (Core.TermList (Lists.map term v1))}}))
  Core.TermRecord v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "record"),
      Core.fieldTerm = (record v1)}}))
  Core.TermSet v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "set"),
      Core.fieldTerm = (Core.TermSet (Sets.map term v1))}}))
  Core.TermSum v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "sum"),
      Core.fieldTerm = (sum_ v1)}}))
  Core.TermTypeAbstraction v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeAbstraction"),
      Core.fieldTerm = (typeAbstraction v1)}}))
  Core.TermTypeApplication v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeApplication"),
      Core.fieldTerm = (typedTerm v1)}}))
  Core.TermTyped v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typed"),
      Core.fieldTerm = (typedTerm v1)}}))
  Core.TermUnion v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = (injection v1)}}))
  Core.TermVariable v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "variable"),
      Core.fieldTerm = (name v1)}}))
  Core.TermWrap v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "wrap"),
      Core.fieldTerm = (wrappedTerm v1)}}))

tupleProjection :: (Core.TupleProjection -> Core.Term)
tupleProjection tp = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TupleProjection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "arity"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.tupleProjectionArity tp))))},
    Core.Field {
      Core.fieldName = (Core.Name "index"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.tupleProjectionIndex tp))))},
    Core.Field {
      Core.fieldName = (Core.Name "domain"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map encodeTypes (Core.tupleProjectionDomain tp)))}]})) 
  where 
    encodeTypes = (\types -> Core.TermList (Lists.map type_ types))

type_ :: (Core.Type -> Core.Term)
type_ x = case x of
  Core.TypeAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
    Core.annotatedTermSubject = (type_ (Core.annotatedTypeSubject v1)),
    Core.annotatedTermAnnotation = (Core.annotatedTypeAnnotation v1)}))
  Core.TypeApplication v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "application"),
      Core.fieldTerm = (applicationType v1)}}))
  Core.TypeFunction v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (functionType v1)}}))
  Core.TypeForall v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "forall"),
      Core.fieldTerm = (forallType v1)}}))
  Core.TypeList v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "list"),
      Core.fieldTerm = (type_ v1)}}))
  Core.TypeLiteral v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "literal"),
      Core.fieldTerm = (literalType v1)}}))
  Core.TypeMap v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "map"),
      Core.fieldTerm = (mapType v1)}}))
  Core.TypeOptional v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "optional"),
      Core.fieldTerm = (type_ v1)}}))
  Core.TypeProduct v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "product"),
      Core.fieldTerm = (Core.TermList (Lists.map type_ v1))}}))
  Core.TypeRecord v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "record"),
      Core.fieldTerm = (rowType v1)}}))
  Core.TypeSet v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "set"),
      Core.fieldTerm = (type_ v1)}}))
  Core.TypeSum v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "sum"),
      Core.fieldTerm = (Core.TermList (Lists.map type_ v1))}}))
  Core.TypeUnion v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = (rowType v1)}}))
  Core.TypeVariable v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "variable"),
      Core.fieldTerm = (name v1)}}))
  Core.TypeWrap v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "wrap"),
      Core.fieldTerm = (wrappedType v1)}}))

typeAbstraction :: (Core.TypeAbstraction -> Core.Term)
typeAbstraction l = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypeAbstraction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameter"),
      Core.fieldTerm = (name (Core.typeAbstractionParameter l))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.typeAbstractionBody l))}]}))

typeScheme :: (Core.TypeScheme -> Core.Term)
typeScheme ts = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "variables"),
      Core.fieldTerm = (Core.TermList (Lists.map name (Core.typeSchemeVariables ts)))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (type_ (Core.typeSchemeType ts))}]}))

typedTerm :: (Core.TypedTerm -> Core.Term)
typedTerm tt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypedTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (term (Core.typedTermTerm tt))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (type_ (Core.typedTermType tt))}]}))

wrappedTerm :: (Core.WrappedTerm -> Core.Term)
wrappedTerm n = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.wrappedTermTypeName n))},
    Core.Field {
      Core.fieldName = (Core.Name "object"),
      Core.fieldTerm = (term (Core.wrappedTermObject n))}]}))

wrappedType :: (Core.WrappedType -> Core.Term)
wrappedType nt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.WrappedType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.wrappedTypeTypeName nt))},
    Core.Field {
      Core.fieldName = (Core.Name "object"),
      Core.fieldTerm = (type_ (Core.wrappedTypeObject nt))}]}))

isEncodedType :: (Core.Term -> Bool)
isEncodedType t = ((\x -> case x of
  Core.TermApplication v1 -> (isEncodedType (Core.applicationFunction v1))
  Core.TermUnion v1 -> (Equality.equalString "hydra.core.Type" (Core.unName (Core.injectionTypeName v1)))
  _ -> False) (Strip.stripTerm t))

isType :: (Core.Type -> Bool)
isType t = ((\x -> case x of
  Core.TypeApplication v1 -> (isType (Core.applicationTypeFunction v1))
  Core.TypeForall v1 -> (isType (Core.forallTypeBody v1))
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
