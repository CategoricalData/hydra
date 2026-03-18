-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.protobuf.proto3

module Hydra.Dsl.Ext.Protobuf.Proto3 where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Protobuf.Proto3 as Proto3
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

definitionEnum :: Phantoms.TTerm Proto3.EnumDefinition -> Phantoms.TTerm Proto3.Definition
definitionEnum x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

definitionMessage :: Phantoms.TTerm Proto3.MessageDefinition -> Phantoms.TTerm Proto3.Definition
definitionMessage x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "message"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumDefinition :: Phantoms.TTerm Proto3.TypeName -> Phantoms.TTerm [Proto3.EnumValue] -> Phantoms.TTerm [Proto3.Option] -> Phantoms.TTerm Proto3.EnumDefinition
enumDefinition name values options =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm values)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm options)}]}))

enumDefinitionName :: Phantoms.TTerm Proto3.EnumDefinition -> Phantoms.TTerm Proto3.TypeName
enumDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefinitionValues :: Phantoms.TTerm Proto3.EnumDefinition -> Phantoms.TTerm [Proto3.EnumValue]
enumDefinitionValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
        Core.projectionField = (Core.Name "values")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefinitionOptions :: Phantoms.TTerm Proto3.EnumDefinition -> Phantoms.TTerm [Proto3.Option]
enumDefinitionOptions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
        Core.projectionField = (Core.Name "options")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefinitionWithName :: Phantoms.TTerm Proto3.EnumDefinition -> Phantoms.TTerm Proto3.TypeName -> Phantoms.TTerm Proto3.EnumDefinition
enumDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
              Core.projectionField = (Core.Name "values")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefinitionWithValues :: Phantoms.TTerm Proto3.EnumDefinition -> Phantoms.TTerm [Proto3.EnumValue] -> Phantoms.TTerm Proto3.EnumDefinition
enumDefinitionWithValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefinitionWithOptions :: Phantoms.TTerm Proto3.EnumDefinition -> Phantoms.TTerm [Proto3.Option] -> Phantoms.TTerm Proto3.EnumDefinition
enumDefinitionWithOptions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumDefinition"),
              Core.projectionField = (Core.Name "values")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumValue :: Phantoms.TTerm Proto3.EnumValueName -> Phantoms.TTerm Int -> Phantoms.TTerm [Proto3.Option] -> Phantoms.TTerm Proto3.EnumValue
enumValue name number options =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Phantoms.unTTerm number)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm options)}]}))

enumValueName :: Phantoms.TTerm Proto3.EnumValue -> Phantoms.TTerm Proto3.EnumValueName
enumValueName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumValueNumber :: Phantoms.TTerm Proto3.EnumValue -> Phantoms.TTerm Int
enumValueNumber x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
        Core.projectionField = (Core.Name "number")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumValueOptions :: Phantoms.TTerm Proto3.EnumValue -> Phantoms.TTerm [Proto3.Option]
enumValueOptions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
        Core.projectionField = (Core.Name "options")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumValueWithName :: Phantoms.TTerm Proto3.EnumValue -> Phantoms.TTerm Proto3.EnumValueName -> Phantoms.TTerm Proto3.EnumValue
enumValueWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
              Core.projectionField = (Core.Name "number")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumValueWithNumber :: Phantoms.TTerm Proto3.EnumValue -> Phantoms.TTerm Int -> Phantoms.TTerm Proto3.EnumValue
enumValueWithNumber original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumValueWithOptions :: Phantoms.TTerm Proto3.EnumValue -> Phantoms.TTerm [Proto3.Option] -> Phantoms.TTerm Proto3.EnumValue
enumValueWithOptions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValue"),
              Core.projectionField = (Core.Name "number")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumValueName_ :: Phantoms.TTerm String -> Phantoms.TTerm Proto3.EnumValueName
enumValueName_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.protobuf.proto3.EnumValueName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unEnumValueName :: Phantoms.TTerm Proto3.EnumValueName -> Phantoms.TTerm String
unEnumValueName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.protobuf.proto3.EnumValueName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

field :: Phantoms.TTerm Proto3.FieldName -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Proto3.FieldType -> Phantoms.TTerm Int -> Phantoms.TTerm [Proto3.Option] -> Phantoms.TTerm Proto3.Field
field name jsonName type_ number options =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "jsonName"),
          Core.fieldTerm = (Phantoms.unTTerm jsonName)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Phantoms.unTTerm number)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm options)}]}))

fieldName :: Phantoms.TTerm Proto3.Field -> Phantoms.TTerm Proto3.FieldName
fieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldJsonName :: Phantoms.TTerm Proto3.Field -> Phantoms.TTerm (Maybe String)
fieldJsonName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
        Core.projectionField = (Core.Name "jsonName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldType :: Phantoms.TTerm Proto3.Field -> Phantoms.TTerm Proto3.FieldType
fieldType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldNumber :: Phantoms.TTerm Proto3.Field -> Phantoms.TTerm Int
fieldNumber x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
        Core.projectionField = (Core.Name "number")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldOptions :: Phantoms.TTerm Proto3.Field -> Phantoms.TTerm [Proto3.Option]
fieldOptions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
        Core.projectionField = (Core.Name "options")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldWithName :: Phantoms.TTerm Proto3.Field -> Phantoms.TTerm Proto3.FieldName -> Phantoms.TTerm Proto3.Field
fieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "jsonName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "jsonName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "number")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithJsonName :: Phantoms.TTerm Proto3.Field -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Proto3.Field
fieldWithJsonName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "jsonName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "number")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithType :: Phantoms.TTerm Proto3.Field -> Phantoms.TTerm Proto3.FieldType -> Phantoms.TTerm Proto3.Field
fieldWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "jsonName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "jsonName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "number")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithNumber :: Phantoms.TTerm Proto3.Field -> Phantoms.TTerm Int -> Phantoms.TTerm Proto3.Field
fieldWithNumber original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "jsonName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "jsonName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithOptions :: Phantoms.TTerm Proto3.Field -> Phantoms.TTerm [Proto3.Option] -> Phantoms.TTerm Proto3.Field
fieldWithOptions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "jsonName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "jsonName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Field"),
              Core.projectionField = (Core.Name "number")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldName_ :: Phantoms.TTerm String -> Phantoms.TTerm Proto3.FieldName
fieldName_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.protobuf.proto3.FieldName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unFieldName :: Phantoms.TTerm Proto3.FieldName -> Phantoms.TTerm String
unFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.protobuf.proto3.FieldName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldTypeMap :: Phantoms.TTerm Proto3.MapType -> Phantoms.TTerm Proto3.FieldType
fieldTypeMap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.FieldType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fieldTypeOneof :: Phantoms.TTerm [Proto3.Field] -> Phantoms.TTerm Proto3.FieldType
fieldTypeOneof x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.FieldType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "oneof"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fieldTypeRepeated :: Phantoms.TTerm Proto3.SimpleType -> Phantoms.TTerm Proto3.FieldType
fieldTypeRepeated x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.FieldType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fieldTypeSimple :: Phantoms.TTerm Proto3.SimpleType -> Phantoms.TTerm Proto3.FieldType
fieldTypeSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.FieldType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fileReference :: Phantoms.TTerm String -> Phantoms.TTerm Proto3.FileReference
fileReference x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.protobuf.proto3.FileReference"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unFileReference :: Phantoms.TTerm Proto3.FileReference -> Phantoms.TTerm String
unFileReference x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.protobuf.proto3.FileReference")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapType :: Phantoms.TTerm Proto3.SimpleType -> Phantoms.TTerm Proto3.SimpleType -> Phantoms.TTerm Proto3.MapType
mapType keys values =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm keys)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm values)}]}))

mapTypeKeys :: Phantoms.TTerm Proto3.MapType -> Phantoms.TTerm Proto3.SimpleType
mapTypeKeys x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MapType"),
        Core.projectionField = (Core.Name "keys")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapTypeValues :: Phantoms.TTerm Proto3.MapType -> Phantoms.TTerm Proto3.SimpleType
mapTypeValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MapType"),
        Core.projectionField = (Core.Name "values")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapTypeWithKeys :: Phantoms.TTerm Proto3.MapType -> Phantoms.TTerm Proto3.SimpleType -> Phantoms.TTerm Proto3.MapType
mapTypeWithKeys original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MapType"),
              Core.projectionField = (Core.Name "values")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

mapTypeWithValues :: Phantoms.TTerm Proto3.MapType -> Phantoms.TTerm Proto3.SimpleType -> Phantoms.TTerm Proto3.MapType
mapTypeWithValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MapType"),
              Core.projectionField = (Core.Name "keys")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

messageDefinition :: Phantoms.TTerm Proto3.TypeName -> Phantoms.TTerm [Proto3.Field] -> Phantoms.TTerm [Proto3.Option] -> Phantoms.TTerm Proto3.MessageDefinition
messageDefinition name fields options =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm options)}]}))

messageDefinitionName :: Phantoms.TTerm Proto3.MessageDefinition -> Phantoms.TTerm Proto3.TypeName
messageDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

messageDefinitionFields :: Phantoms.TTerm Proto3.MessageDefinition -> Phantoms.TTerm [Proto3.Field]
messageDefinitionFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

messageDefinitionOptions :: Phantoms.TTerm Proto3.MessageDefinition -> Phantoms.TTerm [Proto3.Option]
messageDefinitionOptions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
        Core.projectionField = (Core.Name "options")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

messageDefinitionWithName :: Phantoms.TTerm Proto3.MessageDefinition -> Phantoms.TTerm Proto3.TypeName -> Phantoms.TTerm Proto3.MessageDefinition
messageDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

messageDefinitionWithFields :: Phantoms.TTerm Proto3.MessageDefinition -> Phantoms.TTerm [Proto3.Field] -> Phantoms.TTerm Proto3.MessageDefinition
messageDefinitionWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

messageDefinitionWithOptions :: Phantoms.TTerm Proto3.MessageDefinition -> Phantoms.TTerm [Proto3.Option] -> Phantoms.TTerm Proto3.MessageDefinition
messageDefinitionWithOptions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.MessageDefinition"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

option :: Phantoms.TTerm String -> Phantoms.TTerm Proto3.Value -> Phantoms.TTerm Proto3.Option
option name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

optionName :: Phantoms.TTerm Proto3.Option -> Phantoms.TTerm String
optionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Option"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionValue :: Phantoms.TTerm Proto3.Option -> Phantoms.TTerm Proto3.Value
optionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Option"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionWithName :: Phantoms.TTerm Proto3.Option -> Phantoms.TTerm String -> Phantoms.TTerm Proto3.Option
optionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Option"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

optionWithValue :: Phantoms.TTerm Proto3.Option -> Phantoms.TTerm Proto3.Value -> Phantoms.TTerm Proto3.Option
optionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Option"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

packageName :: Phantoms.TTerm String -> Phantoms.TTerm Proto3.PackageName
packageName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.protobuf.proto3.PackageName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPackageName :: Phantoms.TTerm Proto3.PackageName -> Phantoms.TTerm String
unPackageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.protobuf.proto3.PackageName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

protoFile :: Phantoms.TTerm Proto3.PackageName -> Phantoms.TTerm [Proto3.FileReference] -> Phantoms.TTerm [Proto3.Definition] -> Phantoms.TTerm [Proto3.Option] -> Phantoms.TTerm Proto3.ProtoFile
protoFile package imports types options =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm options)}]}))

protoFilePackage :: Phantoms.TTerm Proto3.ProtoFile -> Phantoms.TTerm Proto3.PackageName
protoFilePackage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
        Core.projectionField = (Core.Name "package")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

protoFileImports :: Phantoms.TTerm Proto3.ProtoFile -> Phantoms.TTerm [Proto3.FileReference]
protoFileImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
        Core.projectionField = (Core.Name "imports")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

protoFileTypes :: Phantoms.TTerm Proto3.ProtoFile -> Phantoms.TTerm [Proto3.Definition]
protoFileTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
        Core.projectionField = (Core.Name "types")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

protoFileOptions :: Phantoms.TTerm Proto3.ProtoFile -> Phantoms.TTerm [Proto3.Option]
protoFileOptions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
        Core.projectionField = (Core.Name "options")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

protoFileWithPackage :: Phantoms.TTerm Proto3.ProtoFile -> Phantoms.TTerm Proto3.PackageName -> Phantoms.TTerm Proto3.ProtoFile
protoFileWithPackage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

protoFileWithImports :: Phantoms.TTerm Proto3.ProtoFile -> Phantoms.TTerm [Proto3.FileReference] -> Phantoms.TTerm Proto3.ProtoFile
protoFileWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "package")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

protoFileWithTypes :: Phantoms.TTerm Proto3.ProtoFile -> Phantoms.TTerm [Proto3.Definition] -> Phantoms.TTerm Proto3.ProtoFile
protoFileWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "package")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

protoFileWithOptions :: Phantoms.TTerm Proto3.ProtoFile -> Phantoms.TTerm [Proto3.Option] -> Phantoms.TTerm Proto3.ProtoFile
protoFileWithOptions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "package")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ProtoFile"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

scalarTypeBool :: Phantoms.TTerm Proto3.ScalarType
scalarTypeBool =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bool"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeBytes :: Phantoms.TTerm Proto3.ScalarType
scalarTypeBytes =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bytes"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeDouble :: Phantoms.TTerm Proto3.ScalarType
scalarTypeDouble =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeFixed32 :: Phantoms.TTerm Proto3.ScalarType
scalarTypeFixed32 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fixed32"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeFixed64 :: Phantoms.TTerm Proto3.ScalarType
scalarTypeFixed64 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fixed64"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeFloat :: Phantoms.TTerm Proto3.ScalarType
scalarTypeFloat =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeInt32 :: Phantoms.TTerm Proto3.ScalarType
scalarTypeInt32 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int32"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeInt64 :: Phantoms.TTerm Proto3.ScalarType
scalarTypeInt64 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int64"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeSfixed32 :: Phantoms.TTerm Proto3.ScalarType
scalarTypeSfixed32 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sfixed32"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeSfixed64 :: Phantoms.TTerm Proto3.ScalarType
scalarTypeSfixed64 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sfixed64"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeSint32 :: Phantoms.TTerm Proto3.ScalarType
scalarTypeSint32 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sint32"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeSint64 :: Phantoms.TTerm Proto3.ScalarType
scalarTypeSint64 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sint64"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeString :: Phantoms.TTerm Proto3.ScalarType
scalarTypeString =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeUint32 :: Phantoms.TTerm Proto3.ScalarType
scalarTypeUint32 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint32"),
        Core.fieldTerm = Core.TermUnit}}))

scalarTypeUint64 :: Phantoms.TTerm Proto3.ScalarType
scalarTypeUint64 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.ScalarType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint64"),
        Core.fieldTerm = Core.TermUnit}}))

simpleTypeReference :: Phantoms.TTerm Proto3.TypeName -> Phantoms.TTerm Proto3.SimpleType
simpleTypeReference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.SimpleType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

simpleTypeScalar :: Phantoms.TTerm Proto3.ScalarType -> Phantoms.TTerm Proto3.SimpleType
simpleTypeScalar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.SimpleType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scalar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeName :: Phantoms.TTerm String -> Phantoms.TTerm Proto3.TypeName
typeName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.protobuf.proto3.TypeName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTypeName :: Phantoms.TTerm Proto3.TypeName -> Phantoms.TTerm String
unTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.protobuf.proto3.TypeName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeReference :: Phantoms.TTerm String -> Phantoms.TTerm Proto3.TypeReference
typeReference x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.protobuf.proto3.TypeReference"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTypeReference :: Phantoms.TTerm Proto3.TypeReference -> Phantoms.TTerm String
unTypeReference x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.protobuf.proto3.TypeReference")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valueBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Proto3.Value
valueBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueString :: Phantoms.TTerm String -> Phantoms.TTerm Proto3.Value
valueString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.proto3.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
