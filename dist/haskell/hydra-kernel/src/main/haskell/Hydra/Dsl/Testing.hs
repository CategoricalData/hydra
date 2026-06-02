-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.testing

module Hydra.Dsl.Testing where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Testing as Testing
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for the hydra.testing.Tag wrapper
tag :: Typed.TypedTerm String -> Typed.TypedTerm Testing.Tag
tag x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.testing.Tag"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the universal variant of hydra.testing.TestCase
testCaseUniversal :: Typed.TypedTerm Testing.UniversalTestCase -> Typed.TypedTerm Testing.TestCase
testCaseUniversal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "universal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.testing.TestCaseWithMetadata
testCaseWithMetadata :: Typed.TypedTerm String -> Typed.TypedTerm Testing.TestCase -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm [Testing.Tag] -> Typed.TypedTerm Testing.TestCaseWithMetadata
testCaseWithMetadata name case_ description tags =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Typed.unTypedTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Typed.unTypedTerm tags)}]}))
-- | DSL accessor for the case field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataCase :: Typed.TypedTerm Testing.TestCaseWithMetadata -> Typed.TypedTerm Testing.TestCase
testCaseWithMetadataCase x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionFieldName = (Core.Name "case")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the description field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataDescription :: Typed.TypedTerm Testing.TestCaseWithMetadata -> Typed.TypedTerm (Maybe String)
testCaseWithMetadataDescription x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataName :: Typed.TypedTerm Testing.TestCaseWithMetadata -> Typed.TypedTerm String
testCaseWithMetadataName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tags field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataTags :: Typed.TypedTerm Testing.TestCaseWithMetadata -> Typed.TypedTerm [Testing.Tag]
testCaseWithMetadataTags x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionFieldName = (Core.Name "tags")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the case field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataWithCase :: Typed.TypedTerm Testing.TestCaseWithMetadata -> Typed.TypedTerm Testing.TestCase -> Typed.TypedTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithCase original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "tags")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the description field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataWithDescription :: Typed.TypedTerm Testing.TestCaseWithMetadata -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithDescription original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "tags")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataWithName :: Typed.TypedTerm Testing.TestCaseWithMetadata -> Typed.TypedTerm String -> Typed.TypedTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "tags")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tags field of hydra.testing.TestCaseWithMetadata
testCaseWithMetadataWithTags :: Typed.TypedTerm Testing.TestCaseWithMetadata -> Typed.TypedTerm [Testing.Tag] -> Typed.TypedTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithTags original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.testing.TestGroup
testGroup :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm [Testing.TestGroup] -> Typed.TypedTerm [Testing.TestCaseWithMetadata] -> Typed.TypedTerm Testing.TestGroup
testGroup name description subgroups cases =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Typed.unTypedTerm subgroups)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.testing.TestGroup
testGroupCases :: Typed.TypedTerm Testing.TestGroup -> Typed.TypedTerm [Testing.TestCaseWithMetadata]
testGroupCases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the description field of hydra.testing.TestGroup
testGroupDescription :: Typed.TypedTerm Testing.TestGroup -> Typed.TypedTerm (Maybe String)
testGroupDescription x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.testing.TestGroup
testGroupName :: Typed.TypedTerm Testing.TestGroup -> Typed.TypedTerm String
testGroupName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the subgroups field of hydra.testing.TestGroup
testGroupSubgroups :: Typed.TypedTerm Testing.TestGroup -> Typed.TypedTerm [Testing.TestGroup]
testGroupSubgroups x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionFieldName = (Core.Name "subgroups")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cases field of hydra.testing.TestGroup
testGroupWithCases :: Typed.TypedTerm Testing.TestGroup -> Typed.TypedTerm [Testing.TestCaseWithMetadata] -> Typed.TypedTerm Testing.TestGroup
testGroupWithCases original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "subgroups")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the description field of hydra.testing.TestGroup
testGroupWithDescription :: Typed.TypedTerm Testing.TestGroup -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Testing.TestGroup
testGroupWithDescription original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "subgroups")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.testing.TestGroup
testGroupWithName :: Typed.TypedTerm Testing.TestGroup -> Typed.TypedTerm String -> Typed.TypedTerm Testing.TestGroup
testGroupWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "subgroups")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the subgroups field of hydra.testing.TestGroup
testGroupWithSubgroups :: Typed.TypedTerm Testing.TestGroup -> Typed.TypedTerm [Testing.TestGroup] -> Typed.TypedTerm Testing.TestGroup
testGroupWithSubgroups original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the body of hydra.testing.Tag
unTag :: Typed.TypedTerm Testing.Tag -> Typed.TypedTerm String
unTag x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.testing.Tag")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.testing.UniversalTestCase
universalTestCase :: Typed.TypedTerm (() -> String) -> Typed.TypedTerm (() -> String) -> Typed.TypedTerm Testing.UniversalTestCase
universalTestCase actual expected =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Typed.unTypedTerm actual)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Typed.unTypedTerm expected)}]}))
-- | DSL accessor for the actual field of hydra.testing.UniversalTestCase
universalTestCaseActual :: Typed.TypedTerm Testing.UniversalTestCase -> Typed.TypedTerm (() -> String)
universalTestCaseActual x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
        Core.projectionFieldName = (Core.Name "actual")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expected field of hydra.testing.UniversalTestCase
universalTestCaseExpected :: Typed.TypedTerm Testing.UniversalTestCase -> Typed.TypedTerm (() -> String)
universalTestCaseExpected x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
        Core.projectionFieldName = (Core.Name "expected")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the actual field of hydra.testing.UniversalTestCase
universalTestCaseWithActual :: Typed.TypedTerm Testing.UniversalTestCase -> Typed.TypedTerm (() -> String) -> Typed.TypedTerm Testing.UniversalTestCase
universalTestCaseWithActual original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
              Core.projectionFieldName = (Core.Name "expected")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the expected field of hydra.testing.UniversalTestCase
universalTestCaseWithExpected :: Typed.TypedTerm Testing.UniversalTestCase -> Typed.TypedTerm (() -> String) -> Typed.TypedTerm Testing.UniversalTestCase
universalTestCaseWithExpected original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
              Core.projectionFieldName = (Core.Name "actual")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
