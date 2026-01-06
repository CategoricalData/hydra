-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.util

module Hydra.Encode.Util where

import qualified Hydra.Core as Core
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

caseConvention :: (Util.CaseConvention -> Core.Term)
caseConvention x = case x of
  Util.CaseConventionCamel -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "camel"),
      Core.fieldTerm = Core.TermUnit}}))
  Util.CaseConventionPascal -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "pascal"),
      Core.fieldTerm = Core.TermUnit}}))
  Util.CaseConventionLowerSnake -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "lowerSnake"),
      Core.fieldTerm = Core.TermUnit}}))
  Util.CaseConventionUpperSnake -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "upperSnake"),
      Core.fieldTerm = Core.TermUnit}}))

comparison :: (Util.Comparison -> Core.Term)
comparison x = case x of
  Util.ComparisonLessThan -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "lessThan"),
      Core.fieldTerm = Core.TermUnit}}))
  Util.ComparisonEqualTo -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "equalTo"),
      Core.fieldTerm = Core.TermUnit}}))
  Util.ComparisonGreaterThan -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "greaterThan"),
      Core.fieldTerm = Core.TermUnit}}))

decodingError :: (Util.DecodingError -> Core.Term)
decodingError x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Util.unDecodingError x))}))

precision :: (Util.Precision -> Core.Term)
precision x = case x of
  Util.PrecisionArbitrary -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "arbitrary"),
      Core.fieldTerm = Core.TermUnit}}))
  Util.PrecisionBits v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "bits"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v1)))}}))
