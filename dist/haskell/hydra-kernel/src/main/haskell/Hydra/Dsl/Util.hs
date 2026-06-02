-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.util

module Hydra.Dsl.Util where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typed as Typed
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | DSL injection for the camel variant of hydra.util.CaseConvention
caseConventionCamel :: Typed.TypedTerm Util.CaseConvention
caseConventionCamel =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "camel"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lowerSnake variant of hydra.util.CaseConvention
caseConventionLowerSnake :: Typed.TypedTerm Util.CaseConvention
caseConventionLowerSnake =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lowerSnake"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pascal variant of hydra.util.CaseConvention
caseConventionPascal :: Typed.TypedTerm Util.CaseConvention
caseConventionPascal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pascal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the upperSnake variant of hydra.util.CaseConvention
caseConventionUpperSnake :: Typed.TypedTerm Util.CaseConvention
caseConventionUpperSnake =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "upperSnake"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the equalTo variant of hydra.util.Comparison
comparisonEqualTo :: Typed.TypedTerm Util.Comparison
comparisonEqualTo =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equalTo"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterThan variant of hydra.util.Comparison
comparisonGreaterThan :: Typed.TypedTerm Util.Comparison
comparisonGreaterThan =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessThan variant of hydra.util.Comparison
comparisonLessThan :: Typed.TypedTerm Util.Comparison
comparisonLessThan =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.util.FileExtension wrapper
fileExtension :: Typed.TypedTerm String -> Typed.TypedTerm Util.FileExtension
fileExtension x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.util.FileExtension"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.util.ModuleNames
moduleNames :: Typed.TypedTerm (Packaging.ModuleName, n) -> Typed.TypedTerm (M.Map Packaging.ModuleName n) -> Typed.TypedTerm (Util.ModuleNames n)
moduleNames focus mapping =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.ModuleNames"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "focus"),
          Core.fieldTerm = (Typed.unTypedTerm focus)},
        Core.Field {
          Core.fieldName = (Core.Name "mapping"),
          Core.fieldTerm = (Typed.unTypedTerm mapping)}]}))
-- | DSL accessor for the focus field of hydra.util.ModuleNames
moduleNamesFocus :: Typed.TypedTerm (Util.ModuleNames n) -> Typed.TypedTerm (Packaging.ModuleName, n)
moduleNamesFocus x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.ModuleNames"),
        Core.projectionFieldName = (Core.Name "focus")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mapping field of hydra.util.ModuleNames
moduleNamesMapping :: Typed.TypedTerm (Util.ModuleNames n) -> Typed.TypedTerm (M.Map Packaging.ModuleName n)
moduleNamesMapping x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.ModuleNames"),
        Core.projectionFieldName = (Core.Name "mapping")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the focus field of hydra.util.ModuleNames
moduleNamesWithFocus :: Typed.TypedTerm (Util.ModuleNames n) -> Typed.TypedTerm (Packaging.ModuleName, n) -> Typed.TypedTerm (Util.ModuleNames n)
moduleNamesWithFocus original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.ModuleNames"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "focus"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "mapping"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.ModuleNames"),
              Core.projectionFieldName = (Core.Name "mapping")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the mapping field of hydra.util.ModuleNames
moduleNamesWithMapping :: Typed.TypedTerm (Util.ModuleNames n) -> Typed.TypedTerm (M.Map Packaging.ModuleName n) -> Typed.TypedTerm (Util.ModuleNames n)
moduleNamesWithMapping original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.ModuleNames"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "focus"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.ModuleNames"),
              Core.projectionFieldName = (Core.Name "focus")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mapping"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the arbitrary variant of hydra.util.Precision
precisionArbitrary :: Typed.TypedTerm Util.Precision
precisionArbitrary =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arbitrary"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bits variant of hydra.util.Precision
precisionBits :: Typed.TypedTerm Int -> Typed.TypedTerm Util.Precision
precisionBits x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bits"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.util.QualifiedName
qualifiedName :: Typed.TypedTerm (Maybe Packaging.ModuleName) -> Typed.TypedTerm String -> Typed.TypedTerm Util.QualifiedName
qualifiedName moduleName local =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Typed.unTypedTerm local)}]}))
-- | DSL accessor for the local field of hydra.util.QualifiedName
qualifiedNameLocal :: Typed.TypedTerm Util.QualifiedName -> Typed.TypedTerm String
qualifiedNameLocal x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.QualifiedName"),
        Core.projectionFieldName = (Core.Name "local")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the moduleName field of hydra.util.QualifiedName
qualifiedNameModuleName :: Typed.TypedTerm Util.QualifiedName -> Typed.TypedTerm (Maybe Packaging.ModuleName)
qualifiedNameModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.util.QualifiedName"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the local field of hydra.util.QualifiedName
qualifiedNameWithLocal :: Typed.TypedTerm Util.QualifiedName -> Typed.TypedTerm String -> Typed.TypedTerm Util.QualifiedName
qualifiedNameWithLocal original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.QualifiedName"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the moduleName field of hydra.util.QualifiedName
qualifiedNameWithModuleName :: Typed.TypedTerm Util.QualifiedName -> Typed.TypedTerm (Maybe Packaging.ModuleName) -> Typed.TypedTerm Util.QualifiedName
qualifiedNameWithModuleName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.util.QualifiedName"),
              Core.projectionFieldName = (Core.Name "local")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the body of hydra.util.FileExtension
unFileExtension :: Typed.TypedTerm Util.FileExtension -> Typed.TypedTerm String
unFileExtension x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.util.FileExtension")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
