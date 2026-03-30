-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.error.pg

module Hydra.Dsl.Error.Pg where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Pg as Pg
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

invalidEdgeErrorId :: Phantoms.TTerm Pg.InvalidValueError -> Phantoms.TTerm Pg.InvalidEdgeError
invalidEdgeErrorId x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "id"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidEdgeErrorInVertexLabel :: Phantoms.TTerm Pg.WrongVertexLabelError -> Phantoms.TTerm Pg.InvalidEdgeError
invalidEdgeErrorInVertexLabel x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inVertexLabel"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidEdgeErrorInVertexNotFound :: Phantoms.TTerm Pg.InvalidEdgeError
invalidEdgeErrorInVertexNotFound =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inVertexNotFound"),
        Core.fieldTerm = Core.TermUnit}}))

invalidEdgeErrorLabel :: Phantoms.TTerm Pg.NoSuchEdgeLabelError -> Phantoms.TTerm Pg.InvalidEdgeError
invalidEdgeErrorLabel x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "label"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidEdgeErrorOutVertexLabel :: Phantoms.TTerm Pg.WrongVertexLabelError -> Phantoms.TTerm Pg.InvalidEdgeError
invalidEdgeErrorOutVertexLabel x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outVertexLabel"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidEdgeErrorOutVertexNotFound :: Phantoms.TTerm Pg.InvalidEdgeError
invalidEdgeErrorOutVertexNotFound =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outVertexNotFound"),
        Core.fieldTerm = Core.TermUnit}}))

invalidEdgeErrorProperty :: Phantoms.TTerm Pg.InvalidElementPropertyError -> Phantoms.TTerm Pg.InvalidEdgeError
invalidEdgeErrorProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidEdgeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidElementPropertyError :: Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm Pg.InvalidPropertyError -> Phantoms.TTerm Pg.InvalidElementPropertyError
invalidElementPropertyError key error =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTTerm error)}]}))

invalidElementPropertyErrorError :: Phantoms.TTerm Pg.InvalidElementPropertyError -> Phantoms.TTerm Pg.InvalidPropertyError
invalidElementPropertyErrorError x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
        Core.projectionField = (Core.Name "error")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidElementPropertyErrorKey :: Phantoms.TTerm Pg.InvalidElementPropertyError -> Phantoms.TTerm Model.PropertyKey
invalidElementPropertyErrorKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
        Core.projectionField = (Core.Name "key")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidElementPropertyErrorWithError :: Phantoms.TTerm Pg.InvalidElementPropertyError -> Phantoms.TTerm Pg.InvalidPropertyError -> Phantoms.TTerm Pg.InvalidElementPropertyError
invalidElementPropertyErrorWithError original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

invalidElementPropertyErrorWithKey :: Phantoms.TTerm Pg.InvalidElementPropertyError -> Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm Pg.InvalidElementPropertyError
invalidElementPropertyErrorWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidElementPropertyError"),
              Core.projectionField = (Core.Name "error")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

invalidGraphEdgeError :: Phantoms.TTerm v -> Phantoms.TTerm Pg.InvalidEdgeError -> Phantoms.TTerm (Pg.InvalidGraphEdgeError v)
invalidGraphEdgeError id error =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTTerm error)}]}))

invalidGraphEdgeErrorError :: Phantoms.TTerm (Pg.InvalidGraphEdgeError v) -> Phantoms.TTerm Pg.InvalidEdgeError
invalidGraphEdgeErrorError x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
        Core.projectionField = (Core.Name "error")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidGraphEdgeErrorId :: Phantoms.TTerm (Pg.InvalidGraphEdgeError v) -> Phantoms.TTerm v
invalidGraphEdgeErrorId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidGraphEdgeErrorWithError :: Phantoms.TTerm (Pg.InvalidGraphEdgeError v) -> Phantoms.TTerm Pg.InvalidEdgeError -> Phantoms.TTerm (Pg.InvalidGraphEdgeError v)
invalidGraphEdgeErrorWithError original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

invalidGraphEdgeErrorWithId :: Phantoms.TTerm (Pg.InvalidGraphEdgeError v) -> Phantoms.TTerm v -> Phantoms.TTerm (Pg.InvalidGraphEdgeError v)
invalidGraphEdgeErrorWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphEdgeError"),
              Core.projectionField = (Core.Name "error")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

invalidGraphErrorEdge :: Phantoms.TTerm (Pg.InvalidGraphEdgeError v) -> Phantoms.TTerm (Pg.InvalidGraphError v)
invalidGraphErrorEdge x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidGraphErrorVertex :: Phantoms.TTerm (Pg.InvalidGraphVertexError v) -> Phantoms.TTerm (Pg.InvalidGraphError v)
invalidGraphErrorVertex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidGraphVertexError :: Phantoms.TTerm v -> Phantoms.TTerm Pg.InvalidVertexError -> Phantoms.TTerm (Pg.InvalidGraphVertexError v)
invalidGraphVertexError id error =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTTerm error)}]}))

invalidGraphVertexErrorError :: Phantoms.TTerm (Pg.InvalidGraphVertexError v) -> Phantoms.TTerm Pg.InvalidVertexError
invalidGraphVertexErrorError x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
        Core.projectionField = (Core.Name "error")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidGraphVertexErrorId :: Phantoms.TTerm (Pg.InvalidGraphVertexError v) -> Phantoms.TTerm v
invalidGraphVertexErrorId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidGraphVertexErrorWithError :: Phantoms.TTerm (Pg.InvalidGraphVertexError v) -> Phantoms.TTerm Pg.InvalidVertexError -> Phantoms.TTerm (Pg.InvalidGraphVertexError v)
invalidGraphVertexErrorWithError original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

invalidGraphVertexErrorWithId :: Phantoms.TTerm (Pg.InvalidGraphVertexError v) -> Phantoms.TTerm v -> Phantoms.TTerm (Pg.InvalidGraphVertexError v)
invalidGraphVertexErrorWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "error"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidGraphVertexError"),
              Core.projectionField = (Core.Name "error")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

invalidPropertyErrorInvalidValue :: Phantoms.TTerm Pg.InvalidValueError -> Phantoms.TTerm Pg.InvalidPropertyError
invalidPropertyErrorInvalidValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidPropertyError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidPropertyErrorMissingRequired :: Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm Pg.InvalidPropertyError
invalidPropertyErrorMissingRequired x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidPropertyError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "missingRequired"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidPropertyErrorUnexpectedKey :: Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm Pg.InvalidPropertyError
invalidPropertyErrorUnexpectedKey x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidPropertyError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedKey"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidValueError :: Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Pg.InvalidValueError
invalidValueError expectedType value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Phantoms.unTTerm expectedType)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

invalidValueErrorExpectedType :: Phantoms.TTerm Pg.InvalidValueError -> Phantoms.TTerm String
invalidValueErrorExpectedType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
        Core.projectionField = (Core.Name "expectedType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidValueErrorValue :: Phantoms.TTerm Pg.InvalidValueError -> Phantoms.TTerm String
invalidValueErrorValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidValueErrorWithExpectedType :: Phantoms.TTerm Pg.InvalidValueError -> Phantoms.TTerm String -> Phantoms.TTerm Pg.InvalidValueError
invalidValueErrorWithExpectedType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

invalidValueErrorWithValue :: Phantoms.TTerm Pg.InvalidValueError -> Phantoms.TTerm String -> Phantoms.TTerm Pg.InvalidValueError
invalidValueErrorWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.InvalidValueError"),
              Core.projectionField = (Core.Name "expectedType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

invalidVertexErrorId :: Phantoms.TTerm Pg.InvalidValueError -> Phantoms.TTerm Pg.InvalidVertexError
invalidVertexErrorId x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidVertexError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "id"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidVertexErrorLabel :: Phantoms.TTerm Pg.NoSuchVertexLabelError -> Phantoms.TTerm Pg.InvalidVertexError
invalidVertexErrorLabel x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidVertexError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "label"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidVertexErrorProperty :: Phantoms.TTerm Pg.InvalidElementPropertyError -> Phantoms.TTerm Pg.InvalidVertexError
invalidVertexErrorProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.pg.InvalidVertexError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

noSuchEdgeLabelError :: Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm Pg.NoSuchEdgeLabelError
noSuchEdgeLabelError label =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.NoSuchEdgeLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)}]}))

noSuchEdgeLabelErrorLabel :: Phantoms.TTerm Pg.NoSuchEdgeLabelError -> Phantoms.TTerm Model.EdgeLabel
noSuchEdgeLabelErrorLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.NoSuchEdgeLabelError"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

noSuchEdgeLabelErrorWithLabel :: Phantoms.TTerm Pg.NoSuchEdgeLabelError -> Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm Pg.NoSuchEdgeLabelError
noSuchEdgeLabelErrorWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.NoSuchEdgeLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

noSuchVertexLabelError :: Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm Pg.NoSuchVertexLabelError
noSuchVertexLabelError label =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.NoSuchVertexLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)}]}))

noSuchVertexLabelErrorLabel :: Phantoms.TTerm Pg.NoSuchVertexLabelError -> Phantoms.TTerm Model.VertexLabel
noSuchVertexLabelErrorLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.NoSuchVertexLabelError"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

noSuchVertexLabelErrorWithLabel :: Phantoms.TTerm Pg.NoSuchVertexLabelError -> Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm Pg.NoSuchVertexLabelError
noSuchVertexLabelErrorWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.NoSuchVertexLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

wrongVertexLabelError :: Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm Pg.WrongVertexLabelError
wrongVertexLabelError expected actual =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm expected)},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Phantoms.unTTerm actual)}]}))

wrongVertexLabelErrorActual :: Phantoms.TTerm Pg.WrongVertexLabelError -> Phantoms.TTerm Model.VertexLabel
wrongVertexLabelErrorActual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
        Core.projectionField = (Core.Name "actual")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

wrongVertexLabelErrorExpected :: Phantoms.TTerm Pg.WrongVertexLabelError -> Phantoms.TTerm Model.VertexLabel
wrongVertexLabelErrorExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
        Core.projectionField = (Core.Name "expected")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

wrongVertexLabelErrorWithActual :: Phantoms.TTerm Pg.WrongVertexLabelError -> Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm Pg.WrongVertexLabelError
wrongVertexLabelErrorWithActual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

wrongVertexLabelErrorWithExpected :: Phantoms.TTerm Pg.WrongVertexLabelError -> Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm Pg.WrongVertexLabelError
wrongVertexLabelErrorWithExpected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.pg.WrongVertexLabelError"),
              Core.projectionField = (Core.Name "actual")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
