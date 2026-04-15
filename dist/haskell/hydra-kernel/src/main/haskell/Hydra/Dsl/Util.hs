-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.util

module Hydra.Dsl.Util where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

caseConventionCamel :: Phantoms.TTerm Util.CaseConvention
caseConventionCamel =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "camel"),
        Core.fieldTerm = Core.TermUnit}}))

caseConventionLowerSnake :: Phantoms.TTerm Util.CaseConvention
caseConventionLowerSnake =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lowerSnake"),
        Core.fieldTerm = Core.TermUnit}}))

caseConventionPascal :: Phantoms.TTerm Util.CaseConvention
caseConventionPascal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pascal"),
        Core.fieldTerm = Core.TermUnit}}))

caseConventionUpperSnake :: Phantoms.TTerm Util.CaseConvention
caseConventionUpperSnake =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "upperSnake"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonEqualTo :: Phantoms.TTerm Util.Comparison
comparisonEqualTo =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equalTo"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonGreaterThan :: Phantoms.TTerm Util.Comparison
comparisonGreaterThan =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonLessThan :: Phantoms.TTerm Util.Comparison
comparisonLessThan =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))

precisionArbitrary :: Phantoms.TTerm Util.Precision
precisionArbitrary =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arbitrary"),
        Core.fieldTerm = Core.TermUnit}}))

precisionBits :: Phantoms.TTerm Int -> Phantoms.TTerm Util.Precision
precisionBits x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bits"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
