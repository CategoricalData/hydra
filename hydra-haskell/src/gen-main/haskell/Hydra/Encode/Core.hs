-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.core

module Hydra.Encode.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotatedTerm :: (Core.AnnotatedTerm -> Core.Term)
annotatedTerm x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.annotatedTermBody x))},
    Core.Field {
      Core.fieldName = (Core.Name "annotation"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap name term m)) (Core.annotatedTermAnnotation x))}]}))

annotatedType :: (Core.AnnotatedType -> Core.Term)
annotatedType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.AnnotatedType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (type_ (Core.annotatedTypeBody x))},
    Core.Field {
      Core.fieldName = (Core.Name "annotation"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap name term m)) (Core.annotatedTypeAnnotation x))}]}))

application :: (Core.Application -> Core.Term)
application x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Application"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (term (Core.applicationFunction x))},
    Core.Field {
      Core.fieldName = (Core.Name "argument"),
      Core.fieldTerm = (term (Core.applicationArgument x))}]}))

applicationType :: (Core.ApplicationType -> Core.Term)
applicationType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.ApplicationType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (type_ (Core.applicationTypeFunction x))},
    Core.Field {
      Core.fieldName = (Core.Name "argument"),
      Core.fieldTerm = (type_ (Core.applicationTypeArgument x))}]}))

binding :: (Core.Binding -> Core.Term)
binding x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Binding"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (name (Core.bindingName x))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (term (Core.bindingTerm x))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map typeScheme opt)) (Core.bindingType x))}]}))

caseStatement :: (Core.CaseStatement -> Core.Term)
caseStatement x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.caseStatementTypeName x))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map term opt)) (Core.caseStatementDefault x))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map field xs)) (Core.caseStatementCases x))}]}))

eitherType :: (Core.EitherType -> Core.Term)
eitherType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.EitherType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (type_ (Core.eitherTypeLeft x))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (type_ (Core.eitherTypeRight x))}]}))

pairType :: (Core.PairType -> Core.Term)
pairType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.PairType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "first"),
      Core.fieldTerm = (type_ (Core.pairTypeFirst x))},
    Core.Field {
      Core.fieldName = (Core.Name "second"),
      Core.fieldTerm = (type_ (Core.pairTypeSecond x))}]}))

elimination :: (Core.Elimination -> Core.Term)
elimination x = case x of
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
field x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Field"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (name (Core.fieldName x))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (term (Core.fieldTerm x))}]}))

fieldType :: (Core.FieldType -> Core.Term)
fieldType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.FieldType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (name (Core.fieldTypeName x))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (type_ (Core.fieldTypeType x))}]}))

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

forallType :: (Core.ForallType -> Core.Term)
forallType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.ForallType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameter"),
      Core.fieldTerm = (name (Core.forallTypeParameter x))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (type_ (Core.forallTypeBody x))}]}))

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
functionType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.FunctionType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "domain"),
      Core.fieldTerm = (type_ (Core.functionTypeDomain x))},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = (type_ (Core.functionTypeCodomain x))}]}))

injection :: (Core.Injection -> Core.Term)
injection x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Injection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.injectionTypeName x))},
    Core.Field {
      Core.fieldName = (Core.Name "field"),
      Core.fieldTerm = (field (Core.injectionField x))}]}))

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
lambda x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameter"),
      Core.fieldTerm = (name (Core.lambdaParameter x))},
    Core.Field {
      Core.fieldName = (Core.Name "domain"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map type_ opt)) (Core.lambdaDomain x))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.lambdaBody x))}]}))

let_ :: (Core.Let -> Core.Term)
let_ x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Let"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map binding xs)) (Core.letBindings x))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.letBody x))}]}))

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

mapType :: (Core.MapType -> Core.Term)
mapType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "keys"),
      Core.fieldTerm = (type_ (Core.mapTypeKeys x))},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (type_ (Core.mapTypeValues x))}]}))

name :: (Core.Name -> Core.Term)
name x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Core.unName x))}))

projection :: (Core.Projection -> Core.Term)
projection x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Projection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.projectionTypeName x))},
    Core.Field {
      Core.fieldName = (Core.Name "field"),
      Core.fieldTerm = (name (Core.projectionField x))}]}))

record :: (Core.Record -> Core.Term)
record x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.Record"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.recordTypeName x))},
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map field xs)) (Core.recordFields x))}]}))

rowType :: (Core.RowType -> Core.Term)
rowType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.RowType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.rowTypeTypeName x))},
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map fieldType xs)) (Core.rowTypeFields x))}]}))

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
      Core.fieldTerm = (Core.TermEither (Eithers.bimap term term v1))}}))
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
      Core.fieldTerm = (Core.TermPair (Pairs.bimap term term v1))}}))
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

type_ :: (Core.Type -> Core.Term)
type_ x = case x of
  Core.TypeAnnotated v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "annotated"),
      Core.fieldTerm = (annotatedType v1)}}))
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
  Core.TypeForall v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "forall"),
      Core.fieldTerm = (forallType v1)}}))
  Core.TypeFunction v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Type"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (functionType v1)}}))
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

typeApplicationTerm :: (Core.TypeApplicationTerm -> Core.Term)
typeApplicationTerm x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.typeApplicationTermBody x))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (type_ (Core.typeApplicationTermType x))}]}))

typeLambda :: (Core.TypeLambda -> Core.Term)
typeLambda x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypeLambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameter"),
      Core.fieldTerm = (name (Core.typeLambdaParameter x))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.typeLambdaBody x))}]}))

typeScheme :: (Core.TypeScheme -> Core.Term)
typeScheme x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "variables"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map name xs)) (Core.typeSchemeVariables x))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (type_ (Core.typeSchemeType x))},
    Core.Field {
      Core.fieldName = (Core.Name "constraints"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map (\m -> Core.TermMap (Maps.bimap name typeVariableMetadata m)) opt)) (Core.typeSchemeConstraints x))}]}))

typeVariableMetadata :: (Core.TypeVariableMetadata -> Core.Term)
typeVariableMetadata x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.TypeVariableMetadata"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "classes"),
      Core.fieldTerm = ((\s -> Core.TermSet (Sets.map name s)) (Core.typeVariableMetadataClasses x))}]}))

wrappedTerm :: (Core.WrappedTerm -> Core.Term)
wrappedTerm x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.wrappedTermTypeName x))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (term (Core.wrappedTermBody x))}]}))

wrappedType :: (Core.WrappedType -> Core.Term)
wrappedType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.core.WrappedType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (name (Core.wrappedTypeTypeName x))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (type_ (Core.wrappedTypeBody x))}]}))
