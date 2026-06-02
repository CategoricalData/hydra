-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.error.pg

module Hydra.Dsl.Error.Pg where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Pg as Pg
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Typed as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

invalidEdgeErrorId :: Phantoms.TypedTerm Pg.InvalidValueError -> Phantoms.TypedTerm Pg.InvalidEdgeError
invalidEdgeErrorId x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "id"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidEdgeErrorInVertexLabel :: Phantoms.TypedTerm Pg.WrongVertexLabelError -> Phantoms.TypedTerm Pg.InvalidEdgeError
invalidEdgeErrorInVertexLabel x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inVertexLabel"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidEdgeErrorInVertexNotFound :: Phantoms.TypedTerm Pg.InvalidEdgeError
invalidEdgeErrorInVertexNotFound =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inVertexNotFound"),
        Core.fieldTerm = Core.TermUnit}}))

invalidEdgeErrorLabel :: Phantoms.TypedTerm Pg.NoSuchEdgeLabelError -> Phantoms.TypedTerm Pg.InvalidEdgeError
invalidEdgeErrorLabel x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "label"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidEdgeErrorOutVertexLabel :: Phantoms.TypedTerm Pg.WrongVertexLabelError -> Phantoms.TypedTerm Pg.InvalidEdgeError
invalidEdgeErrorOutVertexLabel x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outVertexLabel"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidEdgeErrorOutVertexNotFound :: Phantoms.TypedTerm Pg.InvalidEdgeError
invalidEdgeErrorOutVertexNotFound =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outVertexNotFound"),
        Core.fieldTerm = Core.TermUnit}}))

invalidEdgeErrorProperty :: Phantoms.TypedTerm Pg.InvalidElementPropertyError -> Phantoms.TypedTerm Pg.InvalidEdgeError
invalidEdgeErrorProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidElementPropertyError :: Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm Pg.InvalidPropertyError -> Phantoms.TypedTerm Pg.InvalidElementPropertyError
invalidElementPropertyError key error =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTypedTerm error)}]}))

invalidElementPropertyErrorError :: Phantoms.TypedTerm Pg.InvalidElementPropertyError -> Phantoms.TypedTerm Pg.InvalidPropertyError
invalidElementPropertyErrorError x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
        Core.projectionFieldName = (Core.Name "error")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

invalidElementPropertyErrorKey :: Phantoms.TypedTerm Pg.InvalidElementPropertyError -> Phantoms.TypedTerm Model.PropertyKey
invalidElementPropertyErrorKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

invalidElementPropertyErrorWithError :: Phantoms.TypedTerm Pg.InvalidElementPropertyError -> Phantoms.TypedTerm Pg.InvalidPropertyError -> Phantoms.TypedTerm Pg.InvalidElementPropertyError
invalidElementPropertyErrorWithError original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

invalidElementPropertyErrorWithKey :: Phantoms.TypedTerm Pg.InvalidElementPropertyError -> Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm Pg.InvalidElementPropertyError
invalidElementPropertyErrorWithKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
              Core.projectionFieldName = (Core.Name "error")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

invalidGraphEdgeError :: Phantoms.TypedTerm v -> Phantoms.TypedTerm Pg.InvalidEdgeError -> Phantoms.TypedTerm (Pg.InvalidGraphEdgeError v)
invalidGraphEdgeError id error =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTypedTerm error)}]}))

invalidGraphEdgeErrorError :: Phantoms.TypedTerm (Pg.InvalidGraphEdgeError v) -> Phantoms.TypedTerm Pg.InvalidEdgeError
invalidGraphEdgeErrorError x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
        Core.projectionFieldName = (Core.Name "error")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

invalidGraphEdgeErrorId :: Phantoms.TypedTerm (Pg.InvalidGraphEdgeError v) -> Phantoms.TypedTerm v
invalidGraphEdgeErrorId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

invalidGraphEdgeErrorWithError :: Phantoms.TypedTerm (Pg.InvalidGraphEdgeError v) -> Phantoms.TypedTerm Pg.InvalidEdgeError -> Phantoms.TypedTerm (Pg.InvalidGraphEdgeError v)
invalidGraphEdgeErrorWithError original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

invalidGraphEdgeErrorWithId :: Phantoms.TypedTerm (Pg.InvalidGraphEdgeError v) -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Pg.InvalidGraphEdgeError v)
invalidGraphEdgeErrorWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
              Core.projectionFieldName = (Core.Name "error")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

invalidGraphErrorEdge :: Phantoms.TypedTerm (Pg.InvalidGraphEdgeError v) -> Phantoms.TypedTerm (Pg.InvalidGraphError v)
invalidGraphErrorEdge x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidGraphErrorVertex :: Phantoms.TypedTerm (Pg.InvalidGraphVertexError v) -> Phantoms.TypedTerm (Pg.InvalidGraphError v)
invalidGraphErrorVertex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidGraphVertexError :: Phantoms.TypedTerm v -> Phantoms.TypedTerm Pg.InvalidVertexError -> Phantoms.TypedTerm (Pg.InvalidGraphVertexError v)
invalidGraphVertexError id error =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTypedTerm error)}]}))

invalidGraphVertexErrorError :: Phantoms.TypedTerm (Pg.InvalidGraphVertexError v) -> Phantoms.TypedTerm Pg.InvalidVertexError
invalidGraphVertexErrorError x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
        Core.projectionFieldName = (Core.Name "error")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

invalidGraphVertexErrorId :: Phantoms.TypedTerm (Pg.InvalidGraphVertexError v) -> Phantoms.TypedTerm v
invalidGraphVertexErrorId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

invalidGraphVertexErrorWithError :: Phantoms.TypedTerm (Pg.InvalidGraphVertexError v) -> Phantoms.TypedTerm Pg.InvalidVertexError -> Phantoms.TypedTerm (Pg.InvalidGraphVertexError v)
invalidGraphVertexErrorWithError original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

invalidGraphVertexErrorWithId :: Phantoms.TypedTerm (Pg.InvalidGraphVertexError v) -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Pg.InvalidGraphVertexError v)
invalidGraphVertexErrorWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
              Core.projectionFieldName = (Core.Name "error")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

invalidPropertyErrorInvalidValue :: Phantoms.TypedTerm Pg.InvalidValueError -> Phantoms.TypedTerm Pg.InvalidPropertyError
invalidPropertyErrorInvalidValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidPropertyError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidValue"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidPropertyErrorMissingRequired :: Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm Pg.InvalidPropertyError
invalidPropertyErrorMissingRequired x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidPropertyError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "missingRequired"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidPropertyErrorUnexpectedKey :: Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm Pg.InvalidPropertyError
invalidPropertyErrorUnexpectedKey x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidPropertyError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedKey"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidValueError :: Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Pg.InvalidValueError
invalidValueError expectedType value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Phantoms.unTypedTerm expectedType)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

invalidValueErrorExpectedType :: Phantoms.TypedTerm Pg.InvalidValueError -> Phantoms.TypedTerm String
invalidValueErrorExpectedType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
        Core.projectionFieldName = (Core.Name "expectedType")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

invalidValueErrorValue :: Phantoms.TypedTerm Pg.InvalidValueError -> Phantoms.TypedTerm String
invalidValueErrorValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

invalidValueErrorWithExpectedType :: Phantoms.TypedTerm Pg.InvalidValueError -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Pg.InvalidValueError
invalidValueErrorWithExpectedType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

invalidValueErrorWithValue :: Phantoms.TypedTerm Pg.InvalidValueError -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Pg.InvalidValueError
invalidValueErrorWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
              Core.projectionFieldName = (Core.Name "expectedType")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

invalidVertexErrorId :: Phantoms.TypedTerm Pg.InvalidValueError -> Phantoms.TypedTerm Pg.InvalidVertexError
invalidVertexErrorId x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidVertexError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "id"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidVertexErrorLabel :: Phantoms.TypedTerm Pg.NoSuchVertexLabelError -> Phantoms.TypedTerm Pg.InvalidVertexError
invalidVertexErrorLabel x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidVertexError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "label"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

invalidVertexErrorProperty :: Phantoms.TypedTerm Pg.InvalidElementPropertyError -> Phantoms.TypedTerm Pg.InvalidVertexError
invalidVertexErrorProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidVertexError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

noSuchEdgeLabelError :: Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm Pg.NoSuchEdgeLabelError
noSuchEdgeLabelError label =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.NoSuchEdgeLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm label)}]}))

noSuchEdgeLabelErrorLabel :: Phantoms.TypedTerm Pg.NoSuchEdgeLabelError -> Phantoms.TypedTerm Model.EdgeLabel
noSuchEdgeLabelErrorLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.NoSuchEdgeLabelError"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

noSuchEdgeLabelErrorWithLabel :: Phantoms.TypedTerm Pg.NoSuchEdgeLabelError -> Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm Pg.NoSuchEdgeLabelError
noSuchEdgeLabelErrorWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.NoSuchEdgeLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

noSuchVertexLabelError :: Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm Pg.NoSuchVertexLabelError
noSuchVertexLabelError label =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.NoSuchVertexLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm label)}]}))

noSuchVertexLabelErrorLabel :: Phantoms.TypedTerm Pg.NoSuchVertexLabelError -> Phantoms.TypedTerm Model.VertexLabel
noSuchVertexLabelErrorLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.NoSuchVertexLabelError"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

noSuchVertexLabelErrorWithLabel :: Phantoms.TypedTerm Pg.NoSuchVertexLabelError -> Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm Pg.NoSuchVertexLabelError
noSuchVertexLabelErrorWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.NoSuchVertexLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

wrongVertexLabelError :: Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm Pg.WrongVertexLabelError
wrongVertexLabelError expected actual =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTypedTerm expected)},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Phantoms.unTypedTerm actual)}]}))

wrongVertexLabelErrorActual :: Phantoms.TypedTerm Pg.WrongVertexLabelError -> Phantoms.TypedTerm Model.VertexLabel
wrongVertexLabelErrorActual x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
        Core.projectionFieldName = (Core.Name "actual")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

wrongVertexLabelErrorExpected :: Phantoms.TypedTerm Pg.WrongVertexLabelError -> Phantoms.TypedTerm Model.VertexLabel
wrongVertexLabelErrorExpected x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
        Core.projectionFieldName = (Core.Name "expected")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

wrongVertexLabelErrorWithActual :: Phantoms.TypedTerm Pg.WrongVertexLabelError -> Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm Pg.WrongVertexLabelError
wrongVertexLabelErrorWithActual original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
              Core.projectionFieldName = (Core.Name "expected")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

wrongVertexLabelErrorWithExpected :: Phantoms.TypedTerm Pg.WrongVertexLabelError -> Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm Pg.WrongVertexLabelError
wrongVertexLabelErrorWithExpected original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
              Core.projectionFieldName = (Core.Name "actual")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
