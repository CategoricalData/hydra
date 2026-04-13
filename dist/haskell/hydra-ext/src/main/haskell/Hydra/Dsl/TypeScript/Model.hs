-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.typeScript.model

module Hydra.Dsl.TypeScript.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.TypeScript.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

functionType :: Phantoms.TTerm [Model.Parameter] -> Phantoms.TTerm Model.Type -> Phantoms.TTerm Model.FunctionType
functionType parameters range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.model.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)}]}))

functionTypeParameters :: Phantoms.TTerm Model.FunctionType -> Phantoms.TTerm [Model.Parameter]
functionTypeParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.model.FunctionType"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeRange :: Phantoms.TTerm Model.FunctionType -> Phantoms.TTerm Model.Type
functionTypeRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.model.FunctionType"),
        Core.projectionField = (Core.Name "range")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeWithParameters :: Phantoms.TTerm Model.FunctionType -> Phantoms.TTerm [Model.Parameter] -> Phantoms.TTerm Model.FunctionType
functionTypeWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.model.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.model.FunctionType"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionTypeWithRange :: Phantoms.TTerm Model.FunctionType -> Phantoms.TTerm Model.Type -> Phantoms.TTerm Model.FunctionType
functionTypeWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.model.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.model.FunctionType"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

parameter :: Phantoms.TTerm String -> Phantoms.TTerm Model.Type -> Phantoms.TTerm Model.Parameter
parameter name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.model.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

parameterName :: Phantoms.TTerm Model.Parameter -> Phantoms.TTerm String
parameterName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.model.Parameter"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parameterType :: Phantoms.TTerm Model.Parameter -> Phantoms.TTerm Model.Type
parameterType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.model.Parameter"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parameterWithName :: Phantoms.TTerm Model.Parameter -> Phantoms.TTerm String -> Phantoms.TTerm Model.Parameter
parameterWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.model.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.model.Parameter"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

parameterWithType :: Phantoms.TTerm Model.Parameter -> Phantoms.TTerm Model.Type -> Phantoms.TTerm Model.Parameter
parameterWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.model.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.model.Parameter"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

primitiveTypeBigint :: Phantoms.TTerm Model.PrimitiveType
primitiveTypeBigint =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigint"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveTypeBoolean :: Phantoms.TTerm Model.PrimitiveType
primitiveTypeBoolean =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveTypeNull :: Phantoms.TTerm Model.PrimitiveType
primitiveTypeNull =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveTypeNumber :: Phantoms.TTerm Model.PrimitiveType
primitiveTypeNumber =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveTypeObject :: Phantoms.TTerm Model.PrimitiveType
primitiveTypeObject =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveTypeString :: Phantoms.TTerm Model.PrimitiveType
primitiveTypeString =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveTypeSymbol :: Phantoms.TTerm Model.PrimitiveType
primitiveTypeSymbol =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symbol"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveTypeUndefined :: Phantoms.TTerm Model.PrimitiveType
primitiveTypeUndefined =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefined"),
        Core.fieldTerm = Core.TermUnit}}))

typeArray :: Phantoms.TTerm Model.Type -> Phantoms.TTerm Model.Type
typeArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeFunction :: Phantoms.TTerm Model.FunctionType -> Phantoms.TTerm Model.Type
typeFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeNever :: Phantoms.TTerm Model.Type
typeNever =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "never"),
        Core.fieldTerm = Core.TermUnit}}))

typeObjectLiteral :: Phantoms.TTerm Model.Type
typeObjectLiteral =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectLiteral"),
        Core.fieldTerm = Core.TermUnit}}))

typePrimitive :: Phantoms.TTerm Model.PrimitiveType -> Phantoms.TTerm Model.Type
typePrimitive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeTuple :: Phantoms.TTerm [Model.Type] -> Phantoms.TTerm Model.Type
typeTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeUnknown :: Phantoms.TTerm Model.Type
typeUnknown =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unknown"),
        Core.fieldTerm = Core.TermUnit}}))

typeVoid :: Phantoms.TTerm Model.Type
typeVoid =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.model.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
