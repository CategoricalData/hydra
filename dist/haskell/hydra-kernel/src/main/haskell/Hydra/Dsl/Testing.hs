-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.testing

module Hydra.Dsl.Testing where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for the hydra.testing.Tag wrapper
tag :: Phantoms.TTerm String -> Phantoms.TTerm Testing.Tag
tag x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.testing.Tag"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the universal variant of hydra.testing.TestCase
testCaseUniversal :: Phantoms.TTerm Testing.UniversalTestCase -> Phantoms.TTerm Testing.TestCase
testCaseUniversal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "universal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.testing.TestCaseWithMetadata
testCaseWithMetadata :: Phantoms.TTerm String -> Phantoms.TTerm Testing.TestCase -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm [Testing.Tag] -> Phantoms.TTerm Testing.TestCaseWithMetadata
testCaseWithMetadata name case_ description tags =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Phantoms.unTTerm tags)}]}))
-- | DSL accessor for the case field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataCase :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm Testing.TestCase
testCaseWithMetadataCase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionFieldName = (Core.Name "case")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the description field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataDescription :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm (Maybe String)
testCaseWithMetadataDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataName :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm String
testCaseWithMetadataName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tags field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataTags :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm [Testing.Tag]
testCaseWithMetadataTags x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionFieldName = (Core.Name "tags")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the case field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataWithCase :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm Testing.TestCase -> Phantoms.TTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithCase original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "tags")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the description field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataWithDescription :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "tags")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataWithName :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "tags")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tags field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataWithTags :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm [Testing.Tag] -> Phantoms.TTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithTags original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.testing.TestGroup
testGroup :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm [Testing.TestGroup] -> Phantoms.TTerm [Testing.TestCaseWithMetadata] -> Phantoms.TTerm Testing.TestGroup
testGroup name description subgroups cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Phantoms.unTTerm subgroups)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.testing.TestGroup
testGroupCases :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm [Testing.TestCaseWithMetadata]
testGroupCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the description field of hydra.testing.TestGroup
testGroupDescription :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm (Maybe String)
testGroupDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.testing.TestGroup
testGroupName :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm String
testGroupName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the subgroups field of hydra.testing.TestGroup
testGroupSubgroups :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm [Testing.TestGroup]
testGroupSubgroups x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionFieldName = (Core.Name "subgroups")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cases field of hydra.testing.TestGroup
testGroupWithCases :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm [Testing.TestCaseWithMetadata] -> Phantoms.TTerm Testing.TestGroup
testGroupWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "subgroups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the description field of hydra.testing.TestGroup
testGroupWithDescription :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Testing.TestGroup
testGroupWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "subgroups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.testing.TestGroup
testGroupWithName :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TestGroup
testGroupWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "subgroups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the subgroups field of hydra.testing.TestGroup
testGroupWithSubgroups :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm [Testing.TestGroup] -> Phantoms.TTerm Testing.TestGroup
testGroupWithSubgroups original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the body of hydra.testing.Tag
unTag :: Phantoms.TTerm Testing.Tag -> Phantoms.TTerm String
unTag x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.testing.Tag")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.testing.UniversalTestCase
universalTestCase :: Phantoms.TTerm (() -> String) -> Phantoms.TTerm (() -> String) -> Phantoms.TTerm Testing.UniversalTestCase
universalTestCase actual expected =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Phantoms.unTTerm actual)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm expected)}]}))
-- | DSL accessor for the actual field of hydra.testing.UniversalTestCase
universalTestCaseActual :: Phantoms.TTerm Testing.UniversalTestCase -> Phantoms.TTerm (() -> String)
universalTestCaseActual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
        Core.projectionFieldName = (Core.Name "actual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expected field of hydra.testing.UniversalTestCase
universalTestCaseExpected :: Phantoms.TTerm Testing.UniversalTestCase -> Phantoms.TTerm (() -> String)
universalTestCaseExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
        Core.projectionFieldName = (Core.Name "expected")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the actual field of hydra.testing.UniversalTestCase
universalTestCaseWithActual :: Phantoms.TTerm Testing.UniversalTestCase -> Phantoms.TTerm (() -> String) -> Phantoms.TTerm Testing.UniversalTestCase
universalTestCaseWithActual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
              Core.projectionFieldName = (Core.Name "expected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the expected field of hydra.testing.UniversalTestCase
universalTestCaseWithExpected :: Phantoms.TTerm Testing.UniversalTestCase -> Phantoms.TTerm (() -> String) -> Phantoms.TTerm Testing.UniversalTestCase
universalTestCaseWithExpected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
              Core.projectionFieldName = (Core.Name "actual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
