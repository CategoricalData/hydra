-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.osv.schema

module Hydra.Dsl.Osv.Schema where

import qualified Hydra.Core as Core
import qualified Hydra.Osv.Schema as Schema
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

credited :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe [Schema.Url]) -> Phantoms.TTerm Schema.Credited
credited name contact =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Credited"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "contact"),
          Core.fieldTerm = (Phantoms.unTTerm contact)}]}))

creditedContact :: Phantoms.TTerm Schema.Credited -> Phantoms.TTerm (Maybe [Schema.Url])
creditedContact x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Credited"),
        Core.projectionField = (Core.Name "contact")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

creditedName :: Phantoms.TTerm Schema.Credited -> Phantoms.TTerm String
creditedName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Credited"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

creditedWithContact :: Phantoms.TTerm Schema.Credited -> Phantoms.TTerm (Maybe [Schema.Url]) -> Phantoms.TTerm Schema.Credited
creditedWithContact original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Credited"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Credited"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "contact"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

creditedWithName :: Phantoms.TTerm Schema.Credited -> Phantoms.TTerm String -> Phantoms.TTerm Schema.Credited
creditedWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Credited"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "contact"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Credited"),
              Core.projectionField = (Core.Name "contact")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ecosystem :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Ecosystem
ecosystem x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.Ecosystem"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

entry :: Phantoms.TTerm (Maybe Schema.OsvVersion) -> Phantoms.TTerm Schema.Id -> Phantoms.TTerm Schema.Timestamp -> Phantoms.TTerm (Maybe Schema.Timestamp) -> Phantoms.TTerm (Maybe Schema.Timestamp) -> Phantoms.TTerm (Maybe [Schema.Id]) -> Phantoms.TTerm (Maybe [Schema.Id]) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe Schema.Markdown) -> Phantoms.TTerm (Maybe [Schema.Severity]) -> Phantoms.TTerm (Maybe [Schema.PackageVersions]) -> Phantoms.TTerm (Maybe [Schema.Reference]) -> Phantoms.TTerm (Maybe [Schema.Credited]) -> Phantoms.TTerm Schema.Entry
entry schemaVersion id modified published withdrawn aliases related summary details severity affected references credits =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Phantoms.unTTerm schemaVersion)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Phantoms.unTTerm modified)},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Phantoms.unTTerm published)},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Phantoms.unTTerm withdrawn)},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Phantoms.unTTerm aliases)},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Phantoms.unTTerm related)},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Phantoms.unTTerm summary)},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Phantoms.unTTerm details)},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Phantoms.unTTerm severity)},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Phantoms.unTTerm affected)},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Phantoms.unTTerm references)},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Phantoms.unTTerm credits)}]}))

entryAffected :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.PackageVersions])
entryAffected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "affected")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entryAliases :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.Id])
entryAliases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "aliases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entryCredits :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.Credited])
entryCredits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "credits")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entryDetails :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe Schema.Markdown)
entryDetails x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "details")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entryId :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm Schema.Id
entryId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entryModified :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm Schema.Timestamp
entryModified x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "modified")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entryPublished :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe Schema.Timestamp)
entryPublished x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "published")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entryReferences :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.Reference])
entryReferences x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "references")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entryRelated :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.Id])
entryRelated x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "related")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entrySchemaVersion :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe Schema.OsvVersion)
entrySchemaVersion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "schemaVersion")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entrySeverity :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.Severity])
entrySeverity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "severity")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entrySummary :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe String)
entrySummary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "summary")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

entryWithAffected :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.PackageVersions]) -> Phantoms.TTerm Schema.Entry
entryWithAffected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithAliases :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.Id]) -> Phantoms.TTerm Schema.Entry
entryWithAliases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithCredits :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.Credited]) -> Phantoms.TTerm Schema.Entry
entryWithCredits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

entryWithDetails :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe Schema.Markdown) -> Phantoms.TTerm Schema.Entry
entryWithDetails original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithId :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm Schema.Id -> Phantoms.TTerm Schema.Entry
entryWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithModified :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm Schema.Timestamp -> Phantoms.TTerm Schema.Entry
entryWithModified original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithPublished :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe Schema.Timestamp) -> Phantoms.TTerm Schema.Entry
entryWithPublished original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithReferences :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.Reference]) -> Phantoms.TTerm Schema.Entry
entryWithReferences original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithRelated :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.Id]) -> Phantoms.TTerm Schema.Entry
entryWithRelated original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithSchemaVersion :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe Schema.OsvVersion) -> Phantoms.TTerm Schema.Entry
entryWithSchemaVersion original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithSeverity :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe [Schema.Severity]) -> Phantoms.TTerm Schema.Entry
entryWithSeverity original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithSummary :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Schema.Entry
entryWithSummary original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "withdrawn")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithWithdrawn :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe Schema.Timestamp) -> Phantoms.TTerm Schema.Entry
entryWithWithdrawn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Entry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "schemaVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "modified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "published"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "published")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withdrawn"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "related"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "related")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "summary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "summary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "details"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "details")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "affected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "affected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "references"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "references")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "credits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
              Core.projectionField = (Core.Name "credits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

entryWithdrawn :: Phantoms.TTerm Schema.Entry -> Phantoms.TTerm (Maybe Schema.Timestamp)
entryWithdrawn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Entry"),
        Core.projectionField = (Core.Name "withdrawn")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

eventFixed :: Phantoms.TTerm Schema.Version -> Phantoms.TTerm Schema.Event
eventFixed x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.osv.schema.Event"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fixed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

eventIntroduced :: Phantoms.TTerm Schema.VersionOrZero -> Phantoms.TTerm Schema.Event
eventIntroduced x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.osv.schema.Event"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "introduced"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

eventLastAffected :: Phantoms.TTerm Schema.Version -> Phantoms.TTerm Schema.Event
eventLastAffected x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.osv.schema.Event"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lastAffected"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

eventLimit :: Phantoms.TTerm Schema.VersionOrStar -> Phantoms.TTerm Schema.Event
eventLimit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.osv.schema.Event"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

id :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Id
id x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.Id"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

markdown :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Markdown
markdown x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.Markdown"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

osvVersion :: Phantoms.TTerm String -> Phantoms.TTerm Schema.OsvVersion
osvVersion x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.OsvVersion"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

package :: Phantoms.TTerm Schema.Ecosystem -> Phantoms.TTerm String -> Phantoms.TTerm (Maybe Schema.Url) -> Phantoms.TTerm Schema.Package
package ecosystem name purl =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ecosystem"),
          Core.fieldTerm = (Phantoms.unTTerm ecosystem)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "purl"),
          Core.fieldTerm = (Phantoms.unTTerm purl)}]}))

packageEcosystem :: Phantoms.TTerm Schema.Package -> Phantoms.TTerm Schema.Ecosystem
packageEcosystem x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Package"),
        Core.projectionField = (Core.Name "ecosystem")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

packageName :: Phantoms.TTerm Schema.Package -> Phantoms.TTerm String
packageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Package"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

packagePurl :: Phantoms.TTerm Schema.Package -> Phantoms.TTerm (Maybe Schema.Url)
packagePurl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Package"),
        Core.projectionField = (Core.Name "purl")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

packageVersions :: Phantoms.TTerm Schema.Package -> Phantoms.TTerm (Maybe [Schema.VersionRange]) -> Phantoms.TTerm (Maybe [Schema.Version]) -> Phantoms.TTerm Schema.PackageVersions
packageVersions package ranges versions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "ranges"),
          Core.fieldTerm = (Phantoms.unTTerm ranges)},
        Core.Field {
          Core.fieldName = (Core.Name "versions"),
          Core.fieldTerm = (Phantoms.unTTerm versions)}]}))

packageVersionsPackage :: Phantoms.TTerm Schema.PackageVersions -> Phantoms.TTerm Schema.Package
packageVersionsPackage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
        Core.projectionField = (Core.Name "package")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

packageVersionsRanges :: Phantoms.TTerm Schema.PackageVersions -> Phantoms.TTerm (Maybe [Schema.VersionRange])
packageVersionsRanges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
        Core.projectionField = (Core.Name "ranges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

packageVersionsVersions :: Phantoms.TTerm Schema.PackageVersions -> Phantoms.TTerm (Maybe [Schema.Version])
packageVersionsVersions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
        Core.projectionField = (Core.Name "versions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

packageVersionsWithPackage :: Phantoms.TTerm Schema.PackageVersions -> Phantoms.TTerm Schema.Package -> Phantoms.TTerm Schema.PackageVersions
packageVersionsWithPackage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ranges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
              Core.projectionField = (Core.Name "ranges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "versions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
              Core.projectionField = (Core.Name "versions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

packageVersionsWithRanges :: Phantoms.TTerm Schema.PackageVersions -> Phantoms.TTerm (Maybe [Schema.VersionRange]) -> Phantoms.TTerm Schema.PackageVersions
packageVersionsWithRanges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
              Core.projectionField = (Core.Name "package")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ranges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "versions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
              Core.projectionField = (Core.Name "versions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

packageVersionsWithVersions :: Phantoms.TTerm Schema.PackageVersions -> Phantoms.TTerm (Maybe [Schema.Version]) -> Phantoms.TTerm Schema.PackageVersions
packageVersionsWithVersions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
              Core.projectionField = (Core.Name "package")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ranges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.PackageVersions"),
              Core.projectionField = (Core.Name "ranges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "versions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

packageWithEcosystem :: Phantoms.TTerm Schema.Package -> Phantoms.TTerm Schema.Ecosystem -> Phantoms.TTerm Schema.Package
packageWithEcosystem original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ecosystem"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Package"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "purl"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Package"),
              Core.projectionField = (Core.Name "purl")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

packageWithName :: Phantoms.TTerm Schema.Package -> Phantoms.TTerm String -> Phantoms.TTerm Schema.Package
packageWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ecosystem"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Package"),
              Core.projectionField = (Core.Name "ecosystem")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "purl"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Package"),
              Core.projectionField = (Core.Name "purl")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

packageWithPurl :: Phantoms.TTerm Schema.Package -> Phantoms.TTerm (Maybe Schema.Url) -> Phantoms.TTerm Schema.Package
packageWithPurl original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ecosystem"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Package"),
              Core.projectionField = (Core.Name "ecosystem")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Package"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "purl"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

reference :: Phantoms.TTerm Schema.ReferenceType -> Phantoms.TTerm Schema.Url -> Phantoms.TTerm Schema.Reference
reference type_ url =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Reference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "url"),
          Core.fieldTerm = (Phantoms.unTTerm url)}]}))

referenceType :: Phantoms.TTerm Schema.Reference -> Phantoms.TTerm Schema.ReferenceType
referenceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Reference"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

referenceType_ :: Phantoms.TTerm String -> Phantoms.TTerm Schema.ReferenceType
referenceType_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.ReferenceType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

referenceUrl :: Phantoms.TTerm Schema.Reference -> Phantoms.TTerm Schema.Url
referenceUrl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Reference"),
        Core.projectionField = (Core.Name "url")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

referenceWithType :: Phantoms.TTerm Schema.Reference -> Phantoms.TTerm Schema.ReferenceType -> Phantoms.TTerm Schema.Reference
referenceWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Reference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "url"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Reference"),
              Core.projectionField = (Core.Name "url")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

referenceWithUrl :: Phantoms.TTerm Schema.Reference -> Phantoms.TTerm Schema.Url -> Phantoms.TTerm Schema.Reference
referenceWithUrl original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Reference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Reference"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "url"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

severity :: Phantoms.TTerm Schema.SeverityType -> Phantoms.TTerm Schema.SeverityScore -> Phantoms.TTerm Schema.Severity
severity type_ score =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Severity"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "score"),
          Core.fieldTerm = (Phantoms.unTTerm score)}]}))

severityScore :: Phantoms.TTerm Schema.Severity -> Phantoms.TTerm Schema.SeverityScore
severityScore x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Severity"),
        Core.projectionField = (Core.Name "score")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

severityScore_ :: Phantoms.TTerm String -> Phantoms.TTerm Schema.SeverityScore
severityScore_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.SeverityScore"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

severityType :: Phantoms.TTerm Schema.Severity -> Phantoms.TTerm Schema.SeverityType
severityType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.Severity"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

severityType_ :: Phantoms.TTerm String -> Phantoms.TTerm Schema.SeverityType
severityType_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.SeverityType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

severityWithScore :: Phantoms.TTerm Schema.Severity -> Phantoms.TTerm Schema.SeverityScore -> Phantoms.TTerm Schema.Severity
severityWithScore original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Severity"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Severity"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "score"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

severityWithType :: Phantoms.TTerm Schema.Severity -> Phantoms.TTerm Schema.SeverityType -> Phantoms.TTerm Schema.Severity
severityWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.Severity"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "score"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.Severity"),
              Core.projectionField = (Core.Name "score")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

timestamp :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Timestamp
timestamp x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.Timestamp"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unEcosystem :: Phantoms.TTerm Schema.Ecosystem -> Phantoms.TTerm String
unEcosystem x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.Ecosystem")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unId :: Phantoms.TTerm Schema.Id -> Phantoms.TTerm String
unId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.Id")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unMarkdown :: Phantoms.TTerm Schema.Markdown -> Phantoms.TTerm String
unMarkdown x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.Markdown")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unOsvVersion :: Phantoms.TTerm Schema.OsvVersion -> Phantoms.TTerm String
unOsvVersion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.OsvVersion")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unReferenceType :: Phantoms.TTerm Schema.ReferenceType -> Phantoms.TTerm String
unReferenceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.ReferenceType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSeverityScore :: Phantoms.TTerm Schema.SeverityScore -> Phantoms.TTerm String
unSeverityScore x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.SeverityScore")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSeverityType :: Phantoms.TTerm Schema.SeverityType -> Phantoms.TTerm String
unSeverityType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.SeverityType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unTimestamp :: Phantoms.TTerm Schema.Timestamp -> Phantoms.TTerm String
unTimestamp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.Timestamp")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unUrl :: Phantoms.TTerm Schema.Url -> Phantoms.TTerm String
unUrl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.Url")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unVersion :: Phantoms.TTerm Schema.Version -> Phantoms.TTerm String
unVersion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.Version")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unVersionOrStar :: Phantoms.TTerm Schema.VersionOrStar -> Phantoms.TTerm String
unVersionOrStar x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.VersionOrStar")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unVersionOrZero :: Phantoms.TTerm Schema.VersionOrZero -> Phantoms.TTerm String
unVersionOrZero x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.VersionOrZero")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unVersionType :: Phantoms.TTerm Schema.VersionType -> Phantoms.TTerm String
unVersionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.osv.schema.VersionType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

url :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Url
url x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.Url"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

version :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Version
version x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.Version"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

versionOrStar :: Phantoms.TTerm String -> Phantoms.TTerm Schema.VersionOrStar
versionOrStar x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.VersionOrStar"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

versionOrZero :: Phantoms.TTerm String -> Phantoms.TTerm Schema.VersionOrZero
versionOrZero x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.VersionOrZero"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

versionRange :: Phantoms.TTerm Schema.VersionType -> Phantoms.TTerm (Maybe Schema.Url) -> Phantoms.TTerm [Schema.Event] -> Phantoms.TTerm Schema.VersionRange
versionRange type_ repo events =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "repo"),
          Core.fieldTerm = (Phantoms.unTTerm repo)},
        Core.Field {
          Core.fieldName = (Core.Name "events"),
          Core.fieldTerm = (Phantoms.unTTerm events)}]}))

versionRangeEvents :: Phantoms.TTerm Schema.VersionRange -> Phantoms.TTerm [Schema.Event]
versionRangeEvents x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
        Core.projectionField = (Core.Name "events")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

versionRangeRepo :: Phantoms.TTerm Schema.VersionRange -> Phantoms.TTerm (Maybe Schema.Url)
versionRangeRepo x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
        Core.projectionField = (Core.Name "repo")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

versionRangeType :: Phantoms.TTerm Schema.VersionRange -> Phantoms.TTerm Schema.VersionType
versionRangeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

versionRangeWithEvents :: Phantoms.TTerm Schema.VersionRange -> Phantoms.TTerm [Schema.Event] -> Phantoms.TTerm Schema.VersionRange
versionRangeWithEvents original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repo"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
              Core.projectionField = (Core.Name "repo")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "events"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

versionRangeWithRepo :: Phantoms.TTerm Schema.VersionRange -> Phantoms.TTerm (Maybe Schema.Url) -> Phantoms.TTerm Schema.VersionRange
versionRangeWithRepo original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repo"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "events"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
              Core.projectionField = (Core.Name "events")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

versionRangeWithType :: Phantoms.TTerm Schema.VersionRange -> Phantoms.TTerm Schema.VersionType -> Phantoms.TTerm Schema.VersionRange
versionRangeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "repo"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
              Core.projectionField = (Core.Name "repo")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "events"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.osv.schema.VersionRange"),
              Core.projectionField = (Core.Name "events")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

versionType :: Phantoms.TTerm String -> Phantoms.TTerm Schema.VersionType
versionType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.osv.schema.VersionType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
