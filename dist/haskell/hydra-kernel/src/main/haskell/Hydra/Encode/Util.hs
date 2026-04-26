-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.util

module Hydra.Encode.Util where
import qualified Hydra.Core as Core
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
caseConvention :: Util.CaseConvention -> Core.Term
caseConvention x =
    case x of
      Util.CaseConventionCamel -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "camel"),
          Core.fieldTerm = Core.TermUnit}})
      Util.CaseConventionPascal -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "pascal"),
          Core.fieldTerm = Core.TermUnit}})
      Util.CaseConventionLowerSnake -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "lowerSnake"),
          Core.fieldTerm = Core.TermUnit}})
      Util.CaseConventionUpperSnake -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "upperSnake"),
          Core.fieldTerm = Core.TermUnit}})
comparison :: Util.Comparison -> Core.Term
comparison x =
    case x of
      Util.ComparisonLessThan -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "lessThan"),
          Core.fieldTerm = Core.TermUnit}})
      Util.ComparisonEqualTo -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "equalTo"),
          Core.fieldTerm = Core.TermUnit}})
      Util.ComparisonGreaterThan -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "greaterThan"),
          Core.fieldTerm = Core.TermUnit}})
precision :: Util.Precision -> Core.Term
precision x =
    case x of
      Util.PrecisionArbitrary -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "arbitrary"),
          Core.fieldTerm = Core.TermUnit}})
      Util.PrecisionBits v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "bits"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v0)))}})
