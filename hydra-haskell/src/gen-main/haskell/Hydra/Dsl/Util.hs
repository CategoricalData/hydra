-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.util

module Hydra.Dsl.Util where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

adapter :: Phantoms.TTerm Bool -> Phantoms.TTerm t1 -> Phantoms.TTerm t2 -> Phantoms.TTerm (Util.Coder v1 v2) -> Phantoms.TTerm (Util.Adapter t1 t2 v1 v2)
adapter isLossy source target coder =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Phantoms.unTTerm isLossy)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Phantoms.unTTerm coder)}]}))

adapterCoder :: Phantoms.TTerm (Util.Adapter t1 t2 v1 v2) -> Phantoms.TTerm (Util.Coder v1 v2)
adapterCoder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
        Core.projectionField = (Core.Name "coder")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adapterIsLossy :: Phantoms.TTerm (Util.Adapter t1 t2 v1 v2) -> Phantoms.TTerm Bool
adapterIsLossy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
        Core.projectionField = (Core.Name "isLossy")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adapterSource :: Phantoms.TTerm (Util.Adapter t1 t2 v1 v2) -> Phantoms.TTerm t1
adapterSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
        Core.projectionField = (Core.Name "source")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adapterTarget :: Phantoms.TTerm (Util.Adapter t1 t2 v1 v2) -> Phantoms.TTerm t2
adapterTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
        Core.projectionField = (Core.Name "target")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adapterWithCoder :: Phantoms.TTerm (Util.Adapter t1 t2 v1 v2) -> Phantoms.TTerm (Util.Coder v1 v2) -> Phantoms.TTerm (Util.Adapter t1 t2 v1 v2)
adapterWithCoder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "isLossy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

adapterWithIsLossy :: Phantoms.TTerm (Util.Adapter t1 t2 v1 v2) -> Phantoms.TTerm Bool -> Phantoms.TTerm (Util.Adapter t1 t2 v1 v2)
adapterWithIsLossy original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "coder")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

adapterWithSource :: Phantoms.TTerm (Util.Adapter t1 t2 v1 v2) -> Phantoms.TTerm t1 -> Phantoms.TTerm (Util.Adapter t1 t2 v1 v2)
adapterWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "isLossy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "coder")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

adapterWithTarget :: Phantoms.TTerm (Util.Adapter t1 t2 v1 v2) -> Phantoms.TTerm t2 -> Phantoms.TTerm (Util.Adapter t1 t2 v1 v2)
adapterWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "isLossy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Adapter"),
              Core.projectionField = (Core.Name "coder")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bicoder :: Phantoms.TTerm (t1 -> Util.Adapter t1 t2 v1 v2) -> Phantoms.TTerm (t2 -> Util.Adapter t2 t1 v2 v1) -> Phantoms.TTerm (Util.Bicoder t1 t2 v1 v2)
bicoder encode decode =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.Bicoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Phantoms.unTTerm encode)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Phantoms.unTTerm decode)}]}))

bicoderDecode :: Phantoms.TTerm (Util.Bicoder t1 t2 v1 v2) -> Phantoms.TTerm (t2 -> Util.Adapter t2 t1 v2 v1)
bicoderDecode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.Bicoder"),
        Core.projectionField = (Core.Name "decode")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bicoderEncode :: Phantoms.TTerm (Util.Bicoder t1 t2 v1 v2) -> Phantoms.TTerm (t1 -> Util.Adapter t1 t2 v1 v2)
bicoderEncode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.Bicoder"),
        Core.projectionField = (Core.Name "encode")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bicoderWithDecode :: Phantoms.TTerm (Util.Bicoder t1 t2 v1 v2) -> Phantoms.TTerm (t2 -> Util.Adapter t2 t1 v2 v1) -> Phantoms.TTerm (Util.Bicoder t1 t2 v1 v2)
bicoderWithDecode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.Bicoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Bicoder"),
              Core.projectionField = (Core.Name "encode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

bicoderWithEncode :: Phantoms.TTerm (Util.Bicoder t1 t2 v1 v2) -> Phantoms.TTerm (t1 -> Util.Adapter t1 t2 v1 v2) -> Phantoms.TTerm (Util.Bicoder t1 t2 v1 v2)
bicoderWithEncode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.Bicoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Bicoder"),
              Core.projectionField = (Core.Name "decode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseConventionCamel :: Phantoms.TTerm Util.CaseConvention
caseConventionCamel =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "camel"),
        Core.fieldTerm = Core.TermUnit}}))

caseConventionLowerSnake :: Phantoms.TTerm Util.CaseConvention
caseConventionLowerSnake =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lowerSnake"),
        Core.fieldTerm = Core.TermUnit}}))

caseConventionPascal :: Phantoms.TTerm Util.CaseConvention
caseConventionPascal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pascal"),
        Core.fieldTerm = Core.TermUnit}}))

caseConventionUpperSnake :: Phantoms.TTerm Util.CaseConvention
caseConventionUpperSnake =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "upperSnake"),
        Core.fieldTerm = Core.TermUnit}}))

coder :: Phantoms.TTerm (Context.Context -> v1 -> Either (Context.InContext Errors.Error) v2) -> Phantoms.TTerm (Context.Context -> v2 -> Either (Context.InContext Errors.Error) v1) -> Phantoms.TTerm (Util.Coder v1 v2)
coder encode decode =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.Coder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Phantoms.unTTerm encode)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Phantoms.unTTerm decode)}]}))

coderDecode :: Phantoms.TTerm (Util.Coder v1 v2) -> Phantoms.TTerm (Context.Context -> v2 -> Either (Context.InContext Errors.Error) v1)
coderDecode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.Coder"),
        Core.projectionField = (Core.Name "decode")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

coderEncode :: Phantoms.TTerm (Util.Coder v1 v2) -> Phantoms.TTerm (Context.Context -> v1 -> Either (Context.InContext Errors.Error) v2)
coderEncode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.Coder"),
        Core.projectionField = (Core.Name "encode")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

coderWithDecode :: Phantoms.TTerm (Util.Coder v1 v2) -> Phantoms.TTerm (Context.Context -> v2 -> Either (Context.InContext Errors.Error) v1) -> Phantoms.TTerm (Util.Coder v1 v2)
coderWithDecode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.Coder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Coder"),
              Core.projectionField = (Core.Name "encode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

coderWithEncode :: Phantoms.TTerm (Util.Coder v1 v2) -> Phantoms.TTerm (Context.Context -> v1 -> Either (Context.InContext Errors.Error) v2) -> Phantoms.TTerm (Util.Coder v1 v2)
coderWithEncode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.Coder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.Coder"),
              Core.projectionField = (Core.Name "decode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

comparisonEqualTo :: Phantoms.TTerm Util.Comparison
comparisonEqualTo =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equalTo"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonGreaterThan :: Phantoms.TTerm Util.Comparison
comparisonGreaterThan =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonLessThan :: Phantoms.TTerm Util.Comparison
comparisonLessThan =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))

precisionArbitrary :: Phantoms.TTerm Util.Precision
precisionArbitrary =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arbitrary"),
        Core.fieldTerm = Core.TermUnit}}))

precisionBits :: Phantoms.TTerm Int -> Phantoms.TTerm Util.Precision
precisionBits x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bits"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
