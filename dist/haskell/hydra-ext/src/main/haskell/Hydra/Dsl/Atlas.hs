-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.atlas

module Hydra.Dsl.Atlas where

import qualified Hydra.Atlas as Atlas
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Xml.Schema as Schema
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.Map as M
import qualified Data.Set as S

atlasAttributeDef :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe Atlas.AtlasAttributeDef_Cardinality) -> Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Int -> Phantoms.TTerm (Maybe Atlas.AtlasAttributeDef_IndexType) -> Phantoms.TTerm [Atlas.AtlasConstraintDef] -> Phantoms.TTerm (M.Map String String) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDef name typeName isOptional cardinality valuesMinCount valuesMaxCount isUnique isIndexable includeInNotification defaultValue description searchWeight indexType constraints options displayName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Phantoms.unTTerm isOptional)},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm cardinality)},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Phantoms.unTTerm valuesMinCount)},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Phantoms.unTTerm valuesMaxCount)},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Phantoms.unTTerm isUnique)},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Phantoms.unTTerm isIndexable)},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Phantoms.unTTerm includeInNotification)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm defaultValue)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Phantoms.unTTerm searchWeight)},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Phantoms.unTTerm indexType)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm constraints)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm options)},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Phantoms.unTTerm displayName)}]}))

atlasAttributeDefCardinality :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe Atlas.AtlasAttributeDef_Cardinality)
atlasAttributeDefCardinality x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "cardinality")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefConstraints :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm [Atlas.AtlasConstraintDef]
atlasAttributeDefConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "constraints")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefDefaultValue :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe String)
atlasAttributeDefDefaultValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "defaultValue")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefDescription :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe String)
atlasAttributeDefDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "description")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefDisplayName :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe String)
atlasAttributeDefDisplayName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "displayName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefIncludeInNotification :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Bool
atlasAttributeDefIncludeInNotification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "includeInNotification")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefIndexType :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe Atlas.AtlasAttributeDef_IndexType)
atlasAttributeDefIndexType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "indexType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefIsIndexable :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Bool
atlasAttributeDefIsIndexable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "isIndexable")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefIsOptional :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Bool
atlasAttributeDefIsOptional x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "isOptional")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefIsUnique :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Bool
atlasAttributeDefIsUnique x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "isUnique")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefName :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe String)
atlasAttributeDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefOptions :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (M.Map String String)
atlasAttributeDefOptions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "options")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefSearchWeight :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Int
atlasAttributeDefSearchWeight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "searchWeight")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefTypeName :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe String)
atlasAttributeDefTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "typeName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefValuesMaxCount :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Int
atlasAttributeDefValuesMaxCount x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "valuesMaxCount")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefValuesMinCount :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Int
atlasAttributeDefValuesMinCount x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
        Core.projectionField = (Core.Name "valuesMinCount")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasAttributeDefWithCardinality :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe Atlas.AtlasAttributeDef_Cardinality) -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithCardinality original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithConstraints :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm [Atlas.AtlasConstraintDef] -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithDefaultValue :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithDefaultValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithDescription :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithDisplayName :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithDisplayName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

atlasAttributeDefWithIncludeInNotification :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithIncludeInNotification original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithIndexType :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe Atlas.AtlasAttributeDef_IndexType) -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithIndexType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithIsIndexable :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithIsIndexable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithIsOptional :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithIsOptional original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithIsUnique :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithIsUnique original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithName :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithOptions :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (M.Map String String) -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithOptions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithSearchWeight :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Int -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithSearchWeight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithTypeName :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithValuesMaxCount :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Int -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithValuesMaxCount original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMinCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDefWithValuesMinCount :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Int -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasAttributeDefWithValuesMinCount original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isOptional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isOptional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "cardinality")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMinCount"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "valuesMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "valuesMaxCount")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isUnique")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isIndexable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "isIndexable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includeInNotification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "includeInNotification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "searchWeight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "searchWeight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "indexType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "displayName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef"),
              Core.projectionField = (Core.Name "displayName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasAttributeDef_CardinalityList :: Phantoms.TTerm Atlas.AtlasAttributeDef_Cardinality
atlasAttributeDef_CardinalityList =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef_Cardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = Core.TermUnit}}))

atlasAttributeDef_CardinalitySet :: Phantoms.TTerm Atlas.AtlasAttributeDef_Cardinality
atlasAttributeDef_CardinalitySet =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef_Cardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))

atlasAttributeDef_CardinalitySingle :: Phantoms.TTerm Atlas.AtlasAttributeDef_Cardinality
atlasAttributeDef_CardinalitySingle =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef_Cardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = Core.TermUnit}}))

atlasAttributeDef_IndexTypeDefault :: Phantoms.TTerm Atlas.AtlasAttributeDef_IndexType
atlasAttributeDef_IndexTypeDefault =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef_IndexType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))

atlasAttributeDef_IndexTypeString :: Phantoms.TTerm Atlas.AtlasAttributeDef_IndexType
atlasAttributeDef_IndexTypeString =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.AtlasAttributeDef_IndexType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))

atlasBaseTypeDef :: Phantoms.TTerm (Maybe Atlas.TypeCategory) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe Schema.DateTime) -> Phantoms.TTerm (Maybe Schema.DateTime) -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (M.Map String String) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDef category guid createdBy updatedBy createTime updateTime version name description typeVersion serviceType options =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Phantoms.unTTerm category)},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Phantoms.unTTerm guid)},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Phantoms.unTTerm createdBy)},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Phantoms.unTTerm updatedBy)},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Phantoms.unTTerm createTime)},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Phantoms.unTTerm updateTime)},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Phantoms.unTTerm version)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Phantoms.unTTerm typeVersion)},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Phantoms.unTTerm serviceType)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm options)}]}))

atlasBaseTypeDefCategory :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe Atlas.TypeCategory)
atlasBaseTypeDefCategory x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "category")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefCreateTime :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe Schema.DateTime)
atlasBaseTypeDefCreateTime x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "createTime")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefCreatedBy :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String)
atlasBaseTypeDefCreatedBy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "createdBy")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefDescription :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String)
atlasBaseTypeDefDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "description")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefGuid :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String)
atlasBaseTypeDefGuid x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "guid")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefName :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String)
atlasBaseTypeDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefOptions :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (M.Map String String)
atlasBaseTypeDefOptions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "options")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefServiceType :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String)
atlasBaseTypeDefServiceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "serviceType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefTypeVersion :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String)
atlasBaseTypeDefTypeVersion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "typeVersion")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefUpdateTime :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe Schema.DateTime)
atlasBaseTypeDefUpdateTime x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "updateTime")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefUpdatedBy :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String)
atlasBaseTypeDefUpdatedBy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "updatedBy")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefVersion :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe I.Int64)
atlasBaseTypeDefVersion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
        Core.projectionField = (Core.Name "version")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasBaseTypeDefWithCategory :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe Atlas.TypeCategory) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithCategory original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "guid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createdBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updatedBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updateTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "typeVersion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "serviceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasBaseTypeDefWithCreateTime :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe Schema.DateTime) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithCreateTime original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "category")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "guid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createdBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updatedBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updateTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "typeVersion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "serviceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasBaseTypeDefWithCreatedBy :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithCreatedBy original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "category")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "guid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updatedBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updateTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "typeVersion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "serviceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasBaseTypeDefWithDescription :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "category")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "guid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createdBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updatedBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updateTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "typeVersion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "serviceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasBaseTypeDefWithGuid :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithGuid original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "category")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createdBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updatedBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updateTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "typeVersion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "serviceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasBaseTypeDefWithName :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "category")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "guid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createdBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updatedBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updateTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "typeVersion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "serviceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasBaseTypeDefWithOptions :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (M.Map String String) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithOptions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "category")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "guid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createdBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updatedBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updateTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "typeVersion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "serviceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

atlasBaseTypeDefWithServiceType :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithServiceType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "category")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "guid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createdBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updatedBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updateTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "typeVersion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasBaseTypeDefWithTypeVersion :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithTypeVersion original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "category")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "guid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createdBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updatedBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updateTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "serviceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasBaseTypeDefWithUpdateTime :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe Schema.DateTime) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithUpdateTime original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "category")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "guid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createdBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updatedBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "typeVersion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "serviceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasBaseTypeDefWithUpdatedBy :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithUpdatedBy original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "category")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "guid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createdBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updateTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "typeVersion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "serviceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasBaseTypeDefWithVersion :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasBaseTypeDefWithVersion original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "category"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "category")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "guid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createdBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updatedBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updatedBy")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "createTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updateTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "updateTime")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "typeVersion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "serviceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "serviceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "options"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasBaseTypeDef"),
              Core.projectionField = (Core.Name "options")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasConstraintDef :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (M.Map String String) -> Phantoms.TTerm Atlas.AtlasConstraintDef
atlasConstraintDef type_ params =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasConstraintDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)}]}))

atlasConstraintDefParams :: Phantoms.TTerm Atlas.AtlasConstraintDef -> Phantoms.TTerm (M.Map String String)
atlasConstraintDefParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasConstraintDef"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasConstraintDefType :: Phantoms.TTerm Atlas.AtlasConstraintDef -> Phantoms.TTerm (Maybe String)
atlasConstraintDefType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasConstraintDef"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasConstraintDefWithParams :: Phantoms.TTerm Atlas.AtlasConstraintDef -> Phantoms.TTerm (M.Map String String) -> Phantoms.TTerm Atlas.AtlasConstraintDef
atlasConstraintDefWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasConstraintDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasConstraintDef"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

atlasConstraintDefWithType :: Phantoms.TTerm Atlas.AtlasConstraintDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasConstraintDef
atlasConstraintDefWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasConstraintDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasConstraintDef"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasEntityDef :: Phantoms.TTerm Atlas.AtlasStructDef -> Phantoms.TTerm (S.Set String) -> Phantoms.TTerm (S.Set String) -> Phantoms.TTerm [Atlas.AtlasRelationshipAttributeDef] -> Phantoms.TTerm (M.Map String [Atlas.AtlasAttributeDef]) -> Phantoms.TTerm Atlas.AtlasEntityDef
atlasEntityDef asAtlasStruct superTypes subTypes relationshipAttributeDefs businessAttributeDefs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasStruct"),
          Core.fieldTerm = (Phantoms.unTTerm asAtlasStruct)},
        Core.Field {
          Core.fieldName = (Core.Name "superTypes"),
          Core.fieldTerm = (Phantoms.unTTerm superTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "subTypes"),
          Core.fieldTerm = (Phantoms.unTTerm subTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "relationshipAttributeDefs"),
          Core.fieldTerm = (Phantoms.unTTerm relationshipAttributeDefs)},
        Core.Field {
          Core.fieldName = (Core.Name "businessAttributeDefs"),
          Core.fieldTerm = (Phantoms.unTTerm businessAttributeDefs)}]}))

atlasEntityDefAsAtlasStruct :: Phantoms.TTerm Atlas.AtlasEntityDef -> Phantoms.TTerm Atlas.AtlasStructDef
atlasEntityDefAsAtlasStruct x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
        Core.projectionField = (Core.Name "asAtlasStruct")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasEntityDefBusinessAttributeDefs :: Phantoms.TTerm Atlas.AtlasEntityDef -> Phantoms.TTerm (M.Map String [Atlas.AtlasAttributeDef])
atlasEntityDefBusinessAttributeDefs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
        Core.projectionField = (Core.Name "businessAttributeDefs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasEntityDefRelationshipAttributeDefs :: Phantoms.TTerm Atlas.AtlasEntityDef -> Phantoms.TTerm [Atlas.AtlasRelationshipAttributeDef]
atlasEntityDefRelationshipAttributeDefs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
        Core.projectionField = (Core.Name "relationshipAttributeDefs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasEntityDefSubTypes :: Phantoms.TTerm Atlas.AtlasEntityDef -> Phantoms.TTerm (S.Set String)
atlasEntityDefSubTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
        Core.projectionField = (Core.Name "subTypes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasEntityDefSuperTypes :: Phantoms.TTerm Atlas.AtlasEntityDef -> Phantoms.TTerm (S.Set String)
atlasEntityDefSuperTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
        Core.projectionField = (Core.Name "superTypes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasEntityDefWithAsAtlasStruct :: Phantoms.TTerm Atlas.AtlasEntityDef -> Phantoms.TTerm Atlas.AtlasStructDef -> Phantoms.TTerm Atlas.AtlasEntityDef
atlasEntityDefWithAsAtlasStruct original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasStruct"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "superTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "subTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "relationshipAttributeDefs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "relationshipAttributeDefs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "businessAttributeDefs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "businessAttributeDefs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasEntityDefWithBusinessAttributeDefs :: Phantoms.TTerm Atlas.AtlasEntityDef -> Phantoms.TTerm (M.Map String [Atlas.AtlasAttributeDef]) -> Phantoms.TTerm Atlas.AtlasEntityDef
atlasEntityDefWithBusinessAttributeDefs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasStruct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "asAtlasStruct")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "superTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "subTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "relationshipAttributeDefs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "relationshipAttributeDefs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "businessAttributeDefs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

atlasEntityDefWithRelationshipAttributeDefs :: Phantoms.TTerm Atlas.AtlasEntityDef -> Phantoms.TTerm [Atlas.AtlasRelationshipAttributeDef] -> Phantoms.TTerm Atlas.AtlasEntityDef
atlasEntityDefWithRelationshipAttributeDefs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasStruct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "asAtlasStruct")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "superTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "subTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "relationshipAttributeDefs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "businessAttributeDefs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "businessAttributeDefs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasEntityDefWithSubTypes :: Phantoms.TTerm Atlas.AtlasEntityDef -> Phantoms.TTerm (S.Set String) -> Phantoms.TTerm Atlas.AtlasEntityDef
atlasEntityDefWithSubTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasStruct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "asAtlasStruct")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "superTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "relationshipAttributeDefs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "relationshipAttributeDefs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "businessAttributeDefs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "businessAttributeDefs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasEntityDefWithSuperTypes :: Phantoms.TTerm Atlas.AtlasEntityDef -> Phantoms.TTerm (S.Set String) -> Phantoms.TTerm Atlas.AtlasEntityDef
atlasEntityDefWithSuperTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasStruct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "asAtlasStruct")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "subTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "relationshipAttributeDefs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "relationshipAttributeDefs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "businessAttributeDefs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasEntityDef"),
              Core.projectionField = (Core.Name "businessAttributeDefs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasRelationshipAttributeDef :: Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Bool -> Phantoms.TTerm Atlas.AtlasRelationshipAttributeDef
atlasRelationshipAttributeDef asAtlasAttribute relationshipTypeName isLegacyAttribute =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasAttribute"),
          Core.fieldTerm = (Phantoms.unTTerm asAtlasAttribute)},
        Core.Field {
          Core.fieldName = (Core.Name "relationshipTypeName"),
          Core.fieldTerm = (Phantoms.unTTerm relationshipTypeName)},
        Core.Field {
          Core.fieldName = (Core.Name "isLegacyAttribute"),
          Core.fieldTerm = (Phantoms.unTTerm isLegacyAttribute)}]}))

atlasRelationshipAttributeDefAsAtlasAttribute :: Phantoms.TTerm Atlas.AtlasRelationshipAttributeDef -> Phantoms.TTerm Atlas.AtlasAttributeDef
atlasRelationshipAttributeDefAsAtlasAttribute x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
        Core.projectionField = (Core.Name "asAtlasAttribute")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasRelationshipAttributeDefIsLegacyAttribute :: Phantoms.TTerm Atlas.AtlasRelationshipAttributeDef -> Phantoms.TTerm Bool
atlasRelationshipAttributeDefIsLegacyAttribute x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
        Core.projectionField = (Core.Name "isLegacyAttribute")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasRelationshipAttributeDefRelationshipTypeName :: Phantoms.TTerm Atlas.AtlasRelationshipAttributeDef -> Phantoms.TTerm (Maybe String)
atlasRelationshipAttributeDefRelationshipTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
        Core.projectionField = (Core.Name "relationshipTypeName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasRelationshipAttributeDefWithAsAtlasAttribute :: Phantoms.TTerm Atlas.AtlasRelationshipAttributeDef -> Phantoms.TTerm Atlas.AtlasAttributeDef -> Phantoms.TTerm Atlas.AtlasRelationshipAttributeDef
atlasRelationshipAttributeDefWithAsAtlasAttribute original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasAttribute"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "relationshipTypeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
              Core.projectionField = (Core.Name "relationshipTypeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isLegacyAttribute"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
              Core.projectionField = (Core.Name "isLegacyAttribute")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasRelationshipAttributeDefWithIsLegacyAttribute :: Phantoms.TTerm Atlas.AtlasRelationshipAttributeDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Atlas.AtlasRelationshipAttributeDef
atlasRelationshipAttributeDefWithIsLegacyAttribute original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasAttribute"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
              Core.projectionField = (Core.Name "asAtlasAttribute")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "relationshipTypeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
              Core.projectionField = (Core.Name "relationshipTypeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isLegacyAttribute"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

atlasRelationshipAttributeDefWithRelationshipTypeName :: Phantoms.TTerm Atlas.AtlasRelationshipAttributeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Atlas.AtlasRelationshipAttributeDef
atlasRelationshipAttributeDefWithRelationshipTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasAttribute"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
              Core.projectionField = (Core.Name "asAtlasAttribute")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "relationshipTypeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isLegacyAttribute"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasRelationshipAttributeDef"),
              Core.projectionField = (Core.Name "isLegacyAttribute")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasStructDef :: Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm [Atlas.AtlasAttributeDef] -> Phantoms.TTerm Atlas.AtlasStructDef
atlasStructDef asAtlasBaseType attributeDefs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasStructDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasBaseType"),
          Core.fieldTerm = (Phantoms.unTTerm asAtlasBaseType)},
        Core.Field {
          Core.fieldName = (Core.Name "attributeDefs"),
          Core.fieldTerm = (Phantoms.unTTerm attributeDefs)}]}))

atlasStructDefAsAtlasBaseType :: Phantoms.TTerm Atlas.AtlasStructDef -> Phantoms.TTerm Atlas.AtlasBaseTypeDef
atlasStructDefAsAtlasBaseType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasStructDef"),
        Core.projectionField = (Core.Name "asAtlasBaseType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasStructDefAttributeDefs :: Phantoms.TTerm Atlas.AtlasStructDef -> Phantoms.TTerm [Atlas.AtlasAttributeDef]
atlasStructDefAttributeDefs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasStructDef"),
        Core.projectionField = (Core.Name "attributeDefs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atlasStructDefWithAsAtlasBaseType :: Phantoms.TTerm Atlas.AtlasStructDef -> Phantoms.TTerm Atlas.AtlasBaseTypeDef -> Phantoms.TTerm Atlas.AtlasStructDef
atlasStructDefWithAsAtlasBaseType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasStructDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasBaseType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "attributeDefs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasStructDef"),
              Core.projectionField = (Core.Name "attributeDefs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atlasStructDefWithAttributeDefs :: Phantoms.TTerm Atlas.AtlasStructDef -> Phantoms.TTerm [Atlas.AtlasAttributeDef] -> Phantoms.TTerm Atlas.AtlasStructDef
atlasStructDefWithAttributeDefs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.atlas.AtlasStructDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "asAtlasBaseType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.atlas.AtlasStructDef"),
              Core.projectionField = (Core.Name "asAtlasBaseType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "attributeDefs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeCategoryArray :: Phantoms.TTerm Atlas.TypeCategory
typeCategoryArray =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.TypeCategory"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = Core.TermUnit}}))

typeCategoryBusinessMetadata :: Phantoms.TTerm Atlas.TypeCategory
typeCategoryBusinessMetadata =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.TypeCategory"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "businessMetadata"),
        Core.fieldTerm = Core.TermUnit}}))

typeCategoryClassification :: Phantoms.TTerm Atlas.TypeCategory
typeCategoryClassification =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.TypeCategory"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classification"),
        Core.fieldTerm = Core.TermUnit}}))

typeCategoryEntity :: Phantoms.TTerm Atlas.TypeCategory
typeCategoryEntity =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.TypeCategory"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "entity"),
        Core.fieldTerm = Core.TermUnit}}))

typeCategoryEnum :: Phantoms.TTerm Atlas.TypeCategory
typeCategoryEnum =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.TypeCategory"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = Core.TermUnit}}))

typeCategoryMap :: Phantoms.TTerm Atlas.TypeCategory
typeCategoryMap =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.TypeCategory"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = Core.TermUnit}}))

typeCategoryObjectIdType :: Phantoms.TTerm Atlas.TypeCategory
typeCategoryObjectIdType =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.TypeCategory"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectIdType"),
        Core.fieldTerm = Core.TermUnit}}))

typeCategoryPrimitive :: Phantoms.TTerm Atlas.TypeCategory
typeCategoryPrimitive =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.TypeCategory"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = Core.TermUnit}}))

typeCategoryRelationship :: Phantoms.TTerm Atlas.TypeCategory
typeCategoryRelationship =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.TypeCategory"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "relationship"),
        Core.fieldTerm = Core.TermUnit}}))

typeCategoryStruct :: Phantoms.TTerm Atlas.TypeCategory
typeCategoryStruct =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.atlas.TypeCategory"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = Core.TermUnit}}))
