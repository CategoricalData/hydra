-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.util

module Hydra.Dsl.Util where
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the camel variant of hydra.util.CaseConvention
caseConventionCamel :: Phantoms.TTerm Util.CaseConvention
caseConventionCamel =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "camel"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lowerSnake variant of hydra.util.CaseConvention
caseConventionLowerSnake :: Phantoms.TTerm Util.CaseConvention
caseConventionLowerSnake =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lowerSnake"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pascal variant of hydra.util.CaseConvention
caseConventionPascal :: Phantoms.TTerm Util.CaseConvention
caseConventionPascal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pascal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the upperSnake variant of hydra.util.CaseConvention
caseConventionUpperSnake :: Phantoms.TTerm Util.CaseConvention
caseConventionUpperSnake =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "upperSnake"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the equalTo variant of hydra.util.Comparison
comparisonEqualTo :: Phantoms.TTerm Util.Comparison
comparisonEqualTo =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equalTo"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterThan variant of hydra.util.Comparison
comparisonGreaterThan :: Phantoms.TTerm Util.Comparison
comparisonGreaterThan =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessThan variant of hydra.util.Comparison
comparisonLessThan :: Phantoms.TTerm Util.Comparison
comparisonLessThan =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the arbitrary variant of hydra.util.Precision
precisionArbitrary :: Phantoms.TTerm Util.Precision
precisionArbitrary =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arbitrary"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bits variant of hydra.util.Precision
precisionBits :: Phantoms.TTerm Int -> Phantoms.TTerm Util.Precision
precisionBits x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bits"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
