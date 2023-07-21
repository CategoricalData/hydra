-- | Implementation of LambdaGraph's sigma encoding, which represents terms as terms

module Hydra.TermEncoding where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Optionals as Optionals
import Data.Int
import Data.List
import Data.Map
import Data.Set

sigmaEncodeAnnotatedTerm :: (Core.Annotated (Core.Term a) a -> Core.Term a)
sigmaEncodeAnnotatedTerm a = (Core.TermAnnotated (Core.Annotated {
  Core.annotatedSubject = (sigmaEncodeTerm (Core.annotatedSubject a)),
  Core.annotatedAnnotation = (Core.annotatedAnnotation a)}))

sigmaEncodeApplication :: (Core.Application a -> Core.Term a)
sigmaEncodeApplication app = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Application"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "function"),
      Core.fieldTerm = (sigmaEncodeTerm (Core.applicationFunction app))},
    Core.Field {
      Core.fieldName = (Core.FieldName "argument"),
      Core.fieldTerm = (sigmaEncodeTerm (Core.applicationArgument app))}]}))

sigmaEncodeCaseStatement :: (Core.CaseStatement a -> Core.Term a)
sigmaEncodeCaseStatement cs = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.CaseStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (sigmaEncodeName (Core.caseStatementTypeName cs))},
    Core.Field {
      Core.fieldName = (Core.FieldName "default"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map sigmaEncodeTerm (Core.caseStatementDefault cs)))},
    Core.Field {
      Core.fieldName = (Core.FieldName "cases"),
      Core.fieldTerm = (Core.TermList (Lists.map sigmaEncodeField (Core.caseStatementCases cs)))}]}))

sigmaEncodeElimination :: (Core.Elimination a -> Core.Term a)
sigmaEncodeElimination x = case x of
  Core.EliminationList v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "list"),
      Core.fieldTerm = (sigmaEncodeTerm v)}}))
  Core.EliminationOptional v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "optional"),
      Core.fieldTerm = (sigmaEncodeOptionalCases v)}}))
  Core.EliminationRecord v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "record"),
      Core.fieldTerm = (sigmaEncodeProjection v)}}))
  Core.EliminationUnion v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "union"),
      Core.fieldTerm = (sigmaEncodeCaseStatement v)}}))
  Core.EliminationWrap v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Elimination"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "wrap"),
      Core.fieldTerm = (sigmaEncodeName v)}}))

sigmaEncodeField :: (Core.Field a -> Core.Term a)
sigmaEncodeField f = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Field"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "name"),
      Core.fieldTerm = (Core.TermWrap (Core.Nominal {
        Core.nominalTypeName = (Core.Name "hydra/core.FieldName"),
        Core.nominalObject = (Core.TermLiteral (Core.LiteralString (Core.unFieldName (Core.fieldName f))))}))},
    Core.Field {
      Core.fieldName = (Core.FieldName "term"),
      Core.fieldTerm = (sigmaEncodeTerm (Core.fieldTerm f))}]}))

sigmaEncodeFieldName :: (Core.FieldName -> Core.Term a)
sigmaEncodeFieldName fn = (Core.TermWrap (Core.Nominal {
  Core.nominalTypeName = (Core.Name "hydra/core.FieldName"),
  Core.nominalObject = (Core.TermLiteral (Core.LiteralString (Core.unFieldName fn)))}))

sigmaEncodeFloatValue :: (Core.FloatValue -> Core.Term a)
sigmaEncodeFloatValue x = case x of
  Core.FloatValueBigfloat v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "bigfloat"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat v)))}}))
  Core.FloatValueFloat32 v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float32"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 v)))}}))
  Core.FloatValueFloat64 v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.FloatValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float64"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 v)))}}))

sigmaEncodeFunction :: (Core.Function a -> Core.Term a)
sigmaEncodeFunction x = case x of
  Core.FunctionElimination v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "elimination"),
      Core.fieldTerm = (sigmaEncodeElimination v)}}))
  Core.FunctionLambda v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "lambda"),
      Core.fieldTerm = (sigmaEncodeLambda v)}}))
  Core.FunctionPrimitive v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Function"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "primitive"),
      Core.fieldTerm = (sigmaEncodeName v)}}))

sigmaEncodeInjection :: (Core.Injection a -> Core.Term a)
sigmaEncodeInjection i = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Injection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (sigmaEncodeName (Core.injectionTypeName i))},
    Core.Field {
      Core.fieldName = (Core.FieldName "field"),
      Core.fieldTerm = (sigmaEncodeField (Core.injectionField i))}]}))

sigmaEncodeIntegerValue :: (Core.IntegerValue -> Core.Term a)
sigmaEncodeIntegerValue x = case x of
  Core.IntegerValueBigint v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "bigint"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint v)))}}))
  Core.IntegerValueInt8 v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int8"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 v)))}}))
  Core.IntegerValueInt16 v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int16"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 v)))}}))
  Core.IntegerValueInt32 v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int32"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v)))}}))
  Core.IntegerValueInt64 v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "int64"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 v)))}}))
  Core.IntegerValueUint8 v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint8"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 v)))}}))
  Core.IntegerValueUint16 v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint16"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 v)))}}))
  Core.IntegerValueUint32 v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint32"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 v)))}}))
  Core.IntegerValueUint64 v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.IntegerValue"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "uint64"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 v)))}}))

sigmaEncodeLambda :: (Core.Lambda a -> Core.Term a)
sigmaEncodeLambda l = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "parameter"),
      Core.fieldTerm = (sigmaEncodeName (Core.lambdaParameter l))},
    Core.Field {
      Core.fieldName = (Core.FieldName "body"),
      Core.fieldTerm = (sigmaEncodeTerm (Core.lambdaBody l))}]}))

sigmaEncodeLiteral :: (Core.Literal -> Core.Term a)
sigmaEncodeLiteral x = case x of
  Core.LiteralBinary v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "binary"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralBinary v))}}))
  Core.LiteralBoolean v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "boolean"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean v))}}))
  Core.LiteralFloat v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "float"),
      Core.fieldTerm = (sigmaEncodeFloatValue v)}}))
  Core.LiteralInteger v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "integer"),
      Core.fieldTerm = (sigmaEncodeIntegerValue v)}}))
  Core.LiteralString v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Literal"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "string"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v))}}))

sigmaEncodeName :: (Core.Name -> Core.Term a)
sigmaEncodeName fn = (Core.TermWrap (Core.Nominal {
  Core.nominalTypeName = (Core.Name "hydra/core.Name"),
  Core.nominalObject = (Core.TermLiteral (Core.LiteralString (Core.unName fn)))}))

sigmaEncodeNominalTerm :: (Core.Nominal (Core.Term a) -> Core.Term a)
sigmaEncodeNominalTerm n = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Nominal"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (sigmaEncodeName (Core.nominalTypeName n))},
    Core.Field {
      Core.fieldName = (Core.FieldName "object"),
      Core.fieldTerm = (sigmaEncodeTerm (Core.nominalObject n))}]}))

sigmaEncodeOptionalCases :: (Core.OptionalCases a -> Core.Term a)
sigmaEncodeOptionalCases oc = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.OptionalCases"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "nothing"),
      Core.fieldTerm = (sigmaEncodeTerm (Core.optionalCasesNothing oc))},
    Core.Field {
      Core.fieldName = (Core.FieldName "just"),
      Core.fieldTerm = (sigmaEncodeTerm (Core.optionalCasesJust oc))}]}))

sigmaEncodeProjection :: (Core.Projection -> Core.Term a)
sigmaEncodeProjection p = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Projection"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (sigmaEncodeName (Core.projectionTypeName p))},
    Core.Field {
      Core.fieldName = (Core.FieldName "field"),
      Core.fieldTerm = (sigmaEncodeFieldName (Core.projectionField p))}]}))

sigmaEncodeRecord :: (Core.Record a -> Core.Term a)
sigmaEncodeRecord r = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra/core.Record"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.FieldName "typeName"),
      Core.fieldTerm = (sigmaEncodeName (Core.recordTypeName r))},
    Core.Field {
      Core.fieldName = (Core.FieldName "fields"),
      Core.fieldTerm = (Core.TermList (Lists.map sigmaEncodeField (Core.recordFields r)))}]}))

sigmaEncodeSum :: (Core.Sum a -> Core.Term a)
sigmaEncodeSum s = (Core.TermRecord (Core.Record {
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
      Core.fieldTerm = (sigmaEncodeTerm (Core.sumTerm s))}]}))

sigmaEncodeTerm :: (Core.Term a -> Core.Term a)
sigmaEncodeTerm x = case x of
  Core.TermAnnotated v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "annotated"),
      Core.fieldTerm = (sigmaEncodeAnnotatedTerm v)}}))
  Core.TermApplication v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "application"),
      Core.fieldTerm = (sigmaEncodeApplication v)}}))
  Core.TermFunction v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "function"),
      Core.fieldTerm = (sigmaEncodeFunction v)}}))
  Core.TermLiteral v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "literal"),
      Core.fieldTerm = (sigmaEncodeLiteral v)}}))
  Core.TermList v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "list"),
      Core.fieldTerm = (Core.TermList (Lists.map sigmaEncodeTerm v))}}))
  Core.TermOptional v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "optional"),
      Core.fieldTerm = (Core.TermOptional (Optionals.map sigmaEncodeTerm v))}}))
  Core.TermProduct v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "product"),
      Core.fieldTerm = (Core.TermList (Lists.map sigmaEncodeTerm v))}}))
  Core.TermRecord v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "record"),
      Core.fieldTerm = (sigmaEncodeRecord v)}}))
  Core.TermSum v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "sum"),
      Core.fieldTerm = (sigmaEncodeSum v)}}))
  Core.TermUnion v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "union"),
      Core.fieldTerm = (sigmaEncodeInjection v)}}))
  Core.TermVariable v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "variable"),
      Core.fieldTerm = (sigmaEncodeName v)}}))
  Core.TermWrap v -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra/core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.FieldName "wrap"),
      Core.fieldTerm = (sigmaEncodeNominalTerm v)}}))
  _ -> (Core.TermLiteral (Core.LiteralString "not implemented"))