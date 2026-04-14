-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.avro.schema

module Hydra.Dsl.Avro.Schema where

import qualified Hydra.Avro.Schema as Schema
import qualified Hydra.Core as Core
import qualified Hydra.Json.Model as Model
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

array :: Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.Array
array items =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Array"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm items)}]}))

arrayItems :: Phantoms.TTerm Schema.Array -> Phantoms.TTerm Schema.Schema
arrayItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Array"),
        Core.projectionField = (Core.Name "items")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayWithItems :: Phantoms.TTerm Schema.Array -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.Array
arrayWithItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Array"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enum :: Phantoms.TTerm [String] -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Schema.Enum
enum symbols default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Enum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbols"),
          Core.fieldTerm = (Phantoms.unTTerm symbols)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))

enumDefault :: Phantoms.TTerm Schema.Enum -> Phantoms.TTerm (Maybe String)
enumDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Enum"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumSymbols :: Phantoms.TTerm Schema.Enum -> Phantoms.TTerm [String]
enumSymbols x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Enum"),
        Core.projectionField = (Core.Name "symbols")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumWithDefault :: Phantoms.TTerm Schema.Enum -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Schema.Enum
enumWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Enum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbols"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Enum"),
              Core.projectionField = (Core.Name "symbols")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumWithSymbols :: Phantoms.TTerm Schema.Enum -> Phantoms.TTerm [String] -> Phantoms.TTerm Schema.Enum
enumWithSymbols original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Enum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbols"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Enum"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

field :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm (Maybe Model.Value) -> Phantoms.TTerm (Maybe Schema.Order) -> Phantoms.TTerm (Maybe [String]) -> Phantoms.TTerm (M.Map String Model.Value) -> Phantoms.TTerm Schema.Field
field name doc type_ default_ order aliases annotations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTTerm order)},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Phantoms.unTTerm aliases)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)}]}))

fieldAliases :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm (Maybe [String])
fieldAliases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
        Core.projectionField = (Core.Name "aliases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldAnnotations :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm (M.Map String Model.Value)
fieldAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDefault :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm (Maybe Model.Value)
fieldDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDoc :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm (Maybe String)
fieldDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldName :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm String
fieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldOrder :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm (Maybe Schema.Order)
fieldOrder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
        Core.projectionField = (Core.Name "order")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldType :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm Schema.Schema
fieldType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldWithAliases :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm (Maybe [String]) -> Phantoms.TTerm Schema.Field
fieldWithAliases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithAnnotations :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm (M.Map String Model.Value) -> Phantoms.TTerm Schema.Field
fieldWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldWithDefault :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm (Maybe Model.Value) -> Phantoms.TTerm Schema.Field
fieldWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithDoc :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Schema.Field
fieldWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithName :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm String -> Phantoms.TTerm Schema.Field
fieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithOrder :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm (Maybe Schema.Order) -> Phantoms.TTerm Schema.Field
fieldWithOrder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithType :: Phantoms.TTerm Schema.Field -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.Field
fieldWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Field"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixed :: Phantoms.TTerm Int -> Phantoms.TTerm Schema.Fixed
fixed size =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Fixed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "size"),
          Core.fieldTerm = (Phantoms.unTTerm size)}]}))

fixedSize :: Phantoms.TTerm Schema.Fixed -> Phantoms.TTerm Int
fixedSize x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Fixed"),
        Core.projectionField = (Core.Name "size")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixedWithSize :: Phantoms.TTerm Schema.Fixed -> Phantoms.TTerm Int -> Phantoms.TTerm Schema.Fixed
fixedWithSize original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Fixed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "size"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

map :: Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.Map
map values =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Map"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm values)}]}))

mapValues :: Phantoms.TTerm Schema.Map -> Phantoms.TTerm Schema.Schema
mapValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Map"),
        Core.projectionField = (Core.Name "values")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapWithValues :: Phantoms.TTerm Schema.Map -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.Map
mapWithValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Map"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

named :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe [String]) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Schema.NamedType -> Phantoms.TTerm (M.Map String Model.Value) -> Phantoms.TTerm Schema.Named
named name namespace aliases doc type_ annotations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Named"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Phantoms.unTTerm aliases)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)}]}))

namedAliases :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm (Maybe [String])
namedAliases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
        Core.projectionField = (Core.Name "aliases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namedAnnotations :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm (M.Map String Model.Value)
namedAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namedDoc :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm (Maybe String)
namedDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namedName :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm String
namedName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namedNamespace :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm (Maybe String)
namedNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namedType :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm Schema.NamedType
namedType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namedTypeEnum :: Phantoms.TTerm Schema.Enum -> Phantoms.TTerm Schema.NamedType
namedTypeEnum x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.NamedType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

namedTypeFixed :: Phantoms.TTerm Schema.Fixed -> Phantoms.TTerm Schema.NamedType
namedTypeFixed x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.NamedType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fixed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

namedTypeRecord :: Phantoms.TTerm Schema.Record -> Phantoms.TTerm Schema.NamedType
namedTypeRecord x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.NamedType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

namedWithAliases :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm (Maybe [String]) -> Phantoms.TTerm Schema.Named
namedWithAliases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Named"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

namedWithAnnotations :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm (M.Map String Model.Value) -> Phantoms.TTerm Schema.Named
namedWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Named"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

namedWithDoc :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Schema.Named
namedWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Named"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

namedWithName :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm String -> Phantoms.TTerm Schema.Named
namedWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Named"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

namedWithNamespace :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Schema.Named
namedWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Named"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

namedWithType :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm Schema.NamedType -> Phantoms.TTerm Schema.Named
namedWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Named"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.schema.Named"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

orderAscending :: Phantoms.TTerm Schema.Order
orderAscending =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Order"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ascending"),
        Core.fieldTerm = Core.TermUnit}}))

orderDescending :: Phantoms.TTerm Schema.Order
orderDescending =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Order"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "descending"),
        Core.fieldTerm = Core.TermUnit}}))

orderIgnore :: Phantoms.TTerm Schema.Order
orderIgnore =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Order"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ignore"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveBoolean :: Phantoms.TTerm Schema.Primitive
primitiveBoolean =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Primitive"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveBytes :: Phantoms.TTerm Schema.Primitive
primitiveBytes =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Primitive"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bytes"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveDouble :: Phantoms.TTerm Schema.Primitive
primitiveDouble =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Primitive"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveFloat :: Phantoms.TTerm Schema.Primitive
primitiveFloat =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Primitive"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveInt :: Phantoms.TTerm Schema.Primitive
primitiveInt =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Primitive"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveLong :: Phantoms.TTerm Schema.Primitive
primitiveLong =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Primitive"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveNull :: Phantoms.TTerm Schema.Primitive
primitiveNull =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Primitive"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

primitiveString :: Phantoms.TTerm Schema.Primitive
primitiveString =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Primitive"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))

record :: Phantoms.TTerm [Schema.Field] -> Phantoms.TTerm Schema.Record
record fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Record"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

recordFields :: Phantoms.TTerm Schema.Record -> Phantoms.TTerm [Schema.Field]
recordFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.schema.Record"),
        Core.projectionField = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordWithFields :: Phantoms.TTerm Schema.Record -> Phantoms.TTerm [Schema.Field] -> Phantoms.TTerm Schema.Record
recordWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.schema.Record"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

schemaArray :: Phantoms.TTerm Schema.Array -> Phantoms.TTerm Schema.Schema
schemaArray x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Schema"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaMap :: Phantoms.TTerm Schema.Map -> Phantoms.TTerm Schema.Schema
schemaMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Schema"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaNamed :: Phantoms.TTerm Schema.Named -> Phantoms.TTerm Schema.Schema
schemaNamed x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Schema"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaPrimitive :: Phantoms.TTerm Schema.Primitive -> Phantoms.TTerm Schema.Schema
schemaPrimitive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Schema"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaReference :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Schema
schemaReference x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Schema"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaUnion :: Phantoms.TTerm Schema.Union -> Phantoms.TTerm Schema.Schema
schemaUnion x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.schema.Schema"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unUnion :: Phantoms.TTerm Schema.Union -> Phantoms.TTerm [Schema.Schema]
unUnion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.avro.schema.Union")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

union :: Phantoms.TTerm [Schema.Schema] -> Phantoms.TTerm Schema.Union
union x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.avro.schema.Union"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
