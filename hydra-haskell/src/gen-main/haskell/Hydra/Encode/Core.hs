-- Note: this is an automatically generated file. Do not edit.

-- | Mapping of hydra.core constructs in a host language like Haskell or Java  to their native Hydra counterparts as terms.  This includes an implementation of LambdaGraph's epsilon encoding (types to terms).

module Hydra.Encode.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Encode an annotated term as a term
annotatedTerm :: (Core.AnnotatedTerm -> Core.Term)
annotatedTerm a = (Core.TermAnnotated (Core.AnnotatedTerm {
  Core.annotatedTermBody = (term (Core.annotatedTermBody a)),
  Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation a)}))

-- | Encode an annotated type as a term
annotatedType :: (Core.AnnotatedType -> Core.Term)
annotatedType at = (Core.TermAnnotated (Core.AnnotatedTerm {
  Core.annotatedTermBody = (type_ (Core.annotatedTypeBody at)),
  Core.annotatedTermAnnotation = (Core.annotatedTypeAnnotation at)}))

-- | Encode an application as a term
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

-- | Encode an application type as a term
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

-- | Encode a case statement as a term
caseStatement :: (Core.CaseStatement -> Core.Term)
caseStatement cs = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.caseStatementTypeName cs))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermMaybe (Maybes.map term (Core.caseStatementDefault cs)))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Core.TermList (Lists.map field (Core.caseStatementCases cs)))}]}))

-- | Encode an elimination as a term
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

-- | Encode a field as a term
field :: (Core.Field -> Core.Term)
field f = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Field"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldName f))))}))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (term (Core.fieldTerm f))}]}))

-- | Encode a field type as a term
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

-- | Encode a floating-point type as a term
floatType :: (Core.FloatType -> Core.Term)
floatType x = case x of
  Core.FloatTypeBigfloat -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "bigfloat"),
      Core.fieldTerm = Core.TermUnit}}))
  Core.FloatTypeFloat32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "float32"),
      Core.fieldTerm = Core.TermUnit}}))
  Core.FloatTypeFloat64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "float64"),
      Core.fieldTerm = Core.TermUnit}}))

-- | Encode a floating-point value as a term
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

-- | Encode an either type as a term
eitherType :: (Core.EitherType -> Core.Term)
eitherType et = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.EitherType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (type_ (Core.eitherTypeLeft et))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (type_ (Core.eitherTypeRight et))}]}))

-- | Encode a pair type as a term
pairType :: (Core.PairType -> Core.Term)
pairType pt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.PairType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "first"),
      Core.fieldTerm = (type_ (Core.pairTypeFirst pt))},
    Core.Field {
      Core.fieldName = (Core.Name "second"),
      Core.fieldTerm = (type_ (Core.pairTypeSecond pt))}]}))

-- | Encode a function as a term
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

-- | Encode a function type as a term
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

-- | Encode an injection as a term
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

-- | Encode an integer type as a term
integerType :: (Core.IntegerType -> Core.Term)
integerType x = case x of
  Core.IntegerTypeBigint -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "bigint"),
      Core.fieldTerm = Core.TermUnit}}))
  Core.IntegerTypeInt8 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int8"),
      Core.fieldTerm = Core.TermUnit}}))
  Core.IntegerTypeInt16 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int16"),
      Core.fieldTerm = Core.TermUnit}}))
  Core.IntegerTypeInt32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int32"),
      Core.fieldTerm = Core.TermUnit}}))
  Core.IntegerTypeInt64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "int64"),
      Core.fieldTerm = Core.TermUnit}}))
  Core.IntegerTypeUint8 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint8"),
      Core.fieldTerm = Core.TermUnit}}))
  Core.IntegerTypeUint16 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint16"),
      Core.fieldTerm = Core.TermUnit}}))
  Core.IntegerTypeUint32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint32"),
      Core.fieldTerm = Core.TermUnit}}))
  Core.IntegerTypeUint64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "uint64"),
      Core.fieldTerm = Core.TermUnit}}))

-- | Encode an integer value as a term
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

-- | Encode a lambda as a term
lambda :: (Core.Lambda -> Core.Term)
lambda l = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameter"),
      Core.fieldTerm = (name (Core.lambdaParameter l))},
    Core.Field {
      Core.fieldName = (Core.Name "domain"),
      Core.fieldTerm = (Core.TermMaybe (Maybes.map type_ (Core.lambdaDomain l)))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.lambdaBody l))}]}))

-- | Encode a forall type as a term
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

-- | Encode a let expression as a term
let_ :: (Core.Let -> Core.Term)
let_ l = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Let"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = (Core.TermList (Lists.map binding (Core.letBindings l)))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.letBody l))}]}))

-- | Encode a let binding as a term
binding :: (Core.Binding -> Core.Term)
binding b = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Binding"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (name (Core.bindingName b))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (term (Core.bindingTerm b))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermMaybe (Maybes.map typeScheme (Core.bindingType b)))}]}))

-- | Encode a literal as a term
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

-- | Encode a literal type as a term
literalType :: (Core.LiteralType -> Core.Term)
literalType x = case x of
  Core.LiteralTypeBinary -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "binary"),
      Core.fieldTerm = Core.TermUnit}}))
  Core.LiteralTypeBoolean -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "boolean"),
      Core.fieldTerm = Core.TermUnit}}))
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
      Core.fieldTerm = Core.TermUnit}}))

-- | Encode a map type as a term
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

-- | Encode a name as a term
name :: (Core.Name -> Core.Term)
name fn = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName fn)))}))

-- | Encode a projection as a term
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

-- | Encode a record as a term
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

-- | Encode a row type as a term
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

-- | Encode a term as a term (identity encoding)
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
  Core.TermEither v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "either"),
      Core.fieldTerm = (Core.TermEither (Eithers.either (\l -> Left (term l)) (\r -> Right (term r)) v1))}}))
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
  Core.TermList v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "list"),
      Core.fieldTerm = (Core.TermList (Lists.map term v1))}}))
  Core.TermLiteral v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "literal"),
      Core.fieldTerm = (literal v1)}}))
  Core.TermMap v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "map"),
      Core.fieldTerm = (Core.TermMap (Maps.bimap term term v1))}}))
  Core.TermMaybe v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "maybe"),
      Core.fieldTerm = (Core.TermMaybe (Maybes.map term v1))}}))
  Core.TermPair v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "pair"),
      Core.fieldTerm = (Core.TermPair (term (Pairs.first v1), (term (Pairs.second v1))))}}))
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
  Core.TermTypeApplication v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeApplication"),
      Core.fieldTerm = (typeApplicationTerm v1)}}))
  Core.TermTypeLambda v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeLambda"),
      Core.fieldTerm = (typeLambda v1)}}))
  Core.TermUnion v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = (injection v1)}}))
  Core.TermUnit -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "unit"),
      Core.fieldTerm = Core.TermUnit}}))
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

-- | Encode a tuple projection as a term
tupleProjection :: (Core.TupleProjection -> Core.Term)
tupleProjection tp =  
  let encodeTypes = (\types -> Core.TermList (Lists.map type_ types))
  in (Core.TermRecord (Core.Record {
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
        Core.fieldTerm = (Core.TermMaybe (Maybes.map encodeTypes (Core.tupleProjectionDomain tp)))}]}))

-- | Encode a type as a term (epsilon encoding)
type_ :: (Core.Type -> Core.Term)
type_ x = case x of
  Core.TypeAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
    Core.annotatedTermBody = (type_ (Core.annotatedTypeBody v1)),
    Core.annotatedTermAnnotation = (Core.annotatedTypeAnnotation v1)}))
  Core.TypeApplication v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "application"),
      Core.fieldTerm = (applicationType v1)}}))
  Core.TypeEither v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "either"),
      Core.fieldTerm = (eitherType v1)}}))
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
  Core.TypeMaybe v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "maybe"),
      Core.fieldTerm = (type_ v1)}}))
  Core.TypePair v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "pair"),
      Core.fieldTerm = (pairType v1)}}))
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
  Core.TypeUnion v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = (rowType v1)}}))
  Core.TypeUnit -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "unit"),
      Core.fieldTerm = Core.TermUnit}}))
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

-- | Encode a type lambda as a term
typeLambda :: (Core.TypeLambda -> Core.Term)
typeLambda l = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypeLambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameter"),
      Core.fieldTerm = (name (Core.typeLambdaParameter l))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.typeLambdaBody l))}]}))

-- | Encode a type scheme as a term
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

-- | Encode a type application term as a term
typeApplicationTerm :: (Core.TypeApplicationTerm -> Core.Term)
typeApplicationTerm tt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.typeApplicationTermBody tt))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (type_ (Core.typeApplicationTermType tt))}]}))

-- | Encode a wrapped term as a term
wrappedTerm :: (Core.WrappedTerm -> Core.Term)
wrappedTerm n = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.wrappedTermTypeName n))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.wrappedTermBody n))}]}))

-- | Encode a wrapped type as a term
wrappedType :: (Core.WrappedType -> Core.Term)
wrappedType nt = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.WrappedType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.wrappedTypeTypeName nt))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (type_ (Core.wrappedTypeBody nt))}]}))

-- | Determines whether a given term is an encoded type
isEncodedType :: (Core.Term -> Bool)
isEncodedType t = ((\x -> case x of
  Core.TermApplication v1 -> (isEncodedType (Core.applicationFunction v1))
  Core.TermUnion v1 -> (Equality.equal "hydra.core.Type" (Core.unName (Core.injectionTypeName v1)))
  _ -> False) (Rewriting.deannotateTerm t))

-- | Check whether a type is a type (always true for non-encoded types)
isType :: (Core.Type -> Bool)
isType t = ((\x -> case x of
  Core.TypeApplication v1 -> (isType (Core.applicationTypeFunction v1))
  Core.TypeForall v1 -> (isType (Core.forallTypeBody v1))
  Core.TypeUnion v1 -> (Equality.equal "hydra.core.Type" (Core.unName (Core.rowTypeTypeName v1)))
  Core.TypeVariable v1 -> (Equality.equal v1 (Core.Name "hydra.core.Type"))
  _ -> False) (Rewriting.deannotateType t))

-- | Check whether a term is the unit term
isUnitTerm :: (Core.Term -> Bool)
isUnitTerm x = case x of
  Core.TermUnit -> True
  _ -> False

-- | Check whether a type is the unit type
isUnitType :: (Core.Type -> Bool)
isUnitType x = case x of
  Core.TypeUnit -> True
  _ -> False
