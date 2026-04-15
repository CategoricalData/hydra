-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.avro.testing

module Hydra.Dsl.Avro.Testing where

import qualified Hydra.Avro.Schema as Schema
import qualified Hydra.Avro.Testing as Testing
import qualified Hydra.Core as Core
import qualified Hydra.Json.Model as Model
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

avroTestCaseLossiness :: Phantoms.TTerm Testing.LossinessTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseLossiness x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lossiness"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

avroTestCaseNameMapping :: Phantoms.TTerm Testing.NameMappingTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseNameMapping x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nameMapping"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

avroTestCaseSchemaSerialization :: Phantoms.TTerm Testing.SchemaSerializationTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseSchemaSerialization x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "schemaSerialization"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

avroTestCaseTermLevelForward :: Phantoms.TTerm Testing.TermLevelForwardTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseTermLevelForward x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "termLevelForward"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

avroTestCaseTermLevelReverse :: Phantoms.TTerm Testing.TermLevelReverseTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseTermLevelReverse x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "termLevelReverse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

avroTestCaseTermLevelRoundTripJson :: Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseTermLevelRoundTripJson x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "termLevelRoundTripJson"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

avroTestCaseTermLevelRoundTripTerm :: Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseTermLevelRoundTripTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "termLevelRoundTripTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

avroTestCaseTypeLevelForward :: Phantoms.TTerm Testing.TypeLevelForwardTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseTypeLevelForward x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLevelForward"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

avroTestCaseTypeLevelReverse :: Phantoms.TTerm Testing.TypeLevelReverseTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseTypeLevelReverse x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLevelReverse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

avroTestCaseTypeLevelRoundTripAvro :: Phantoms.TTerm Testing.TypeLevelRoundTripAvroTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseTypeLevelRoundTripAvro x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLevelRoundTripAvro"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

avroTestCaseTypeLevelRoundTripHydra :: Phantoms.TTerm Testing.TypeLevelRoundTripHydraTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseTypeLevelRoundTripHydra x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLevelRoundTripHydra"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

avroTestCaseUnion :: Phantoms.TTerm Testing.UnionTestCase -> Phantoms.TTerm Testing.AvroTestCase
avroTestCaseUnion x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.avro.testing.AvroTestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

lossinessTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Bool -> Phantoms.TTerm Testing.LossinessTestCase
lossinessTestCase description originalSchema hydraType recoveredSchema isLossy =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "originalSchema"),
          Core.fieldTerm = (Phantoms.unTTerm originalSchema)},
        Core.Field {
          Core.fieldName = (Core.Name "hydraType"),
          Core.fieldTerm = (Phantoms.unTTerm hydraType)},
        Core.Field {
          Core.fieldName = (Core.Name "recoveredSchema"),
          Core.fieldTerm = (Phantoms.unTTerm recoveredSchema)},
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Phantoms.unTTerm isLossy)}]}))

lossinessTestCaseDescription :: Phantoms.TTerm Testing.LossinessTestCase -> Phantoms.TTerm String
lossinessTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lossinessTestCaseHydraType :: Phantoms.TTerm Testing.LossinessTestCase -> Phantoms.TTerm Core.Type
lossinessTestCaseHydraType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
        Core.projectionField = (Core.Name "hydraType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lossinessTestCaseIsLossy :: Phantoms.TTerm Testing.LossinessTestCase -> Phantoms.TTerm Bool
lossinessTestCaseIsLossy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
        Core.projectionField = (Core.Name "isLossy")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lossinessTestCaseOriginalSchema :: Phantoms.TTerm Testing.LossinessTestCase -> Phantoms.TTerm Schema.Schema
lossinessTestCaseOriginalSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
        Core.projectionField = (Core.Name "originalSchema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lossinessTestCaseRecoveredSchema :: Phantoms.TTerm Testing.LossinessTestCase -> Phantoms.TTerm Schema.Schema
lossinessTestCaseRecoveredSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
        Core.projectionField = (Core.Name "recoveredSchema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lossinessTestCaseWithDescription :: Phantoms.TTerm Testing.LossinessTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.LossinessTestCase
lossinessTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "originalSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "originalSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hydraType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "hydraType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recoveredSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "recoveredSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "isLossy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lossinessTestCaseWithHydraType :: Phantoms.TTerm Testing.LossinessTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.LossinessTestCase
lossinessTestCaseWithHydraType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "originalSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "originalSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hydraType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "recoveredSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "recoveredSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "isLossy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lossinessTestCaseWithIsLossy :: Phantoms.TTerm Testing.LossinessTestCase -> Phantoms.TTerm Bool -> Phantoms.TTerm Testing.LossinessTestCase
lossinessTestCaseWithIsLossy original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "originalSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "originalSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hydraType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "hydraType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recoveredSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "recoveredSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lossinessTestCaseWithOriginalSchema :: Phantoms.TTerm Testing.LossinessTestCase -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.LossinessTestCase
lossinessTestCaseWithOriginalSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "originalSchema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "hydraType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "hydraType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recoveredSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "recoveredSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "isLossy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lossinessTestCaseWithRecoveredSchema :: Phantoms.TTerm Testing.LossinessTestCase -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.LossinessTestCase
lossinessTestCaseWithRecoveredSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "originalSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "originalSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hydraType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "hydraType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recoveredSchema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.LossinessTestCase"),
              Core.projectionField = (Core.Name "isLossy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nameMappingTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Core.Name -> Phantoms.TTerm String -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Testing.NameMappingTestCase
nameMappingTestCase description hydraName avroName avroNamespace =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "hydraName"),
          Core.fieldTerm = (Phantoms.unTTerm hydraName)},
        Core.Field {
          Core.fieldName = (Core.Name "avroName"),
          Core.fieldTerm = (Phantoms.unTTerm avroName)},
        Core.Field {
          Core.fieldName = (Core.Name "avroNamespace"),
          Core.fieldTerm = (Phantoms.unTTerm avroNamespace)}]}))

nameMappingTestCaseAvroName :: Phantoms.TTerm Testing.NameMappingTestCase -> Phantoms.TTerm String
nameMappingTestCaseAvroName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
        Core.projectionField = (Core.Name "avroName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nameMappingTestCaseAvroNamespace :: Phantoms.TTerm Testing.NameMappingTestCase -> Phantoms.TTerm (Maybe String)
nameMappingTestCaseAvroNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
        Core.projectionField = (Core.Name "avroNamespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nameMappingTestCaseDescription :: Phantoms.TTerm Testing.NameMappingTestCase -> Phantoms.TTerm String
nameMappingTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nameMappingTestCaseHydraName :: Phantoms.TTerm Testing.NameMappingTestCase -> Phantoms.TTerm Core.Name
nameMappingTestCaseHydraName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
        Core.projectionField = (Core.Name "hydraName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nameMappingTestCaseWithAvroName :: Phantoms.TTerm Testing.NameMappingTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.NameMappingTestCase
nameMappingTestCaseWithAvroName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hydraName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "hydraName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "avroName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "avroNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "avroNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nameMappingTestCaseWithAvroNamespace :: Phantoms.TTerm Testing.NameMappingTestCase -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Testing.NameMappingTestCase
nameMappingTestCaseWithAvroNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hydraName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "hydraName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "avroName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "avroName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "avroNamespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nameMappingTestCaseWithDescription :: Phantoms.TTerm Testing.NameMappingTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.NameMappingTestCase
nameMappingTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "hydraName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "hydraName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "avroName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "avroName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "avroNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "avroNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nameMappingTestCaseWithHydraName :: Phantoms.TTerm Testing.NameMappingTestCase -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Testing.NameMappingTestCase
nameMappingTestCaseWithHydraName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hydraName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "avroName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "avroName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "avroNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.NameMappingTestCase"),
              Core.projectionField = (Core.Name "avroNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaSerializationTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Model.Value -> Phantoms.TTerm Testing.SchemaSerializationTestCase
schemaSerializationTestCase description schema json =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm schema)},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Phantoms.unTTerm json)}]}))

schemaSerializationTestCaseDescription :: Phantoms.TTerm Testing.SchemaSerializationTestCase -> Phantoms.TTerm String
schemaSerializationTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaSerializationTestCaseJson :: Phantoms.TTerm Testing.SchemaSerializationTestCase -> Phantoms.TTerm Model.Value
schemaSerializationTestCaseJson x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
        Core.projectionField = (Core.Name "json")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaSerializationTestCaseSchema :: Phantoms.TTerm Testing.SchemaSerializationTestCase -> Phantoms.TTerm Schema.Schema
schemaSerializationTestCaseSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
        Core.projectionField = (Core.Name "schema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaSerializationTestCaseWithDescription :: Phantoms.TTerm Testing.SchemaSerializationTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.SchemaSerializationTestCase
schemaSerializationTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
              Core.projectionField = (Core.Name "json")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaSerializationTestCaseWithJson :: Phantoms.TTerm Testing.SchemaSerializationTestCase -> Phantoms.TTerm Model.Value -> Phantoms.TTerm Testing.SchemaSerializationTestCase
schemaSerializationTestCaseWithJson original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

schemaSerializationTestCaseWithSchema :: Phantoms.TTerm Testing.SchemaSerializationTestCase -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.SchemaSerializationTestCase
schemaSerializationTestCaseWithSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.SchemaSerializationTestCase"),
              Core.projectionField = (Core.Name "json")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelForwardTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Model.Value -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.TermLevelForwardTestCase
termLevelForwardTestCase description schema json term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm schema)},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Phantoms.unTTerm json)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

termLevelForwardTestCaseDescription :: Phantoms.TTerm Testing.TermLevelForwardTestCase -> Phantoms.TTerm String
termLevelForwardTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelForwardTestCaseJson :: Phantoms.TTerm Testing.TermLevelForwardTestCase -> Phantoms.TTerm Model.Value
termLevelForwardTestCaseJson x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
        Core.projectionField = (Core.Name "json")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelForwardTestCaseSchema :: Phantoms.TTerm Testing.TermLevelForwardTestCase -> Phantoms.TTerm Schema.Schema
termLevelForwardTestCaseSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
        Core.projectionField = (Core.Name "schema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelForwardTestCaseTerm :: Phantoms.TTerm Testing.TermLevelForwardTestCase -> Phantoms.TTerm Core.Term
termLevelForwardTestCaseTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelForwardTestCaseWithDescription :: Phantoms.TTerm Testing.TermLevelForwardTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TermLevelForwardTestCase
termLevelForwardTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "json")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelForwardTestCaseWithJson :: Phantoms.TTerm Testing.TermLevelForwardTestCase -> Phantoms.TTerm Model.Value -> Phantoms.TTerm Testing.TermLevelForwardTestCase
termLevelForwardTestCaseWithJson original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelForwardTestCaseWithSchema :: Phantoms.TTerm Testing.TermLevelForwardTestCase -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.TermLevelForwardTestCase
termLevelForwardTestCaseWithSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "json")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelForwardTestCaseWithTerm :: Phantoms.TTerm Testing.TermLevelForwardTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.TermLevelForwardTestCase
termLevelForwardTestCaseWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelForwardTestCase"),
              Core.projectionField = (Core.Name "json")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

termLevelReverseTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Model.Value -> Phantoms.TTerm Testing.TermLevelReverseTestCase
termLevelReverseTestCase description schema term json =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm schema)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Phantoms.unTTerm json)}]}))

termLevelReverseTestCaseDescription :: Phantoms.TTerm Testing.TermLevelReverseTestCase -> Phantoms.TTerm String
termLevelReverseTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelReverseTestCaseJson :: Phantoms.TTerm Testing.TermLevelReverseTestCase -> Phantoms.TTerm Model.Value
termLevelReverseTestCaseJson x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
        Core.projectionField = (Core.Name "json")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelReverseTestCaseSchema :: Phantoms.TTerm Testing.TermLevelReverseTestCase -> Phantoms.TTerm Schema.Schema
termLevelReverseTestCaseSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
        Core.projectionField = (Core.Name "schema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelReverseTestCaseTerm :: Phantoms.TTerm Testing.TermLevelReverseTestCase -> Phantoms.TTerm Core.Term
termLevelReverseTestCaseTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelReverseTestCaseWithDescription :: Phantoms.TTerm Testing.TermLevelReverseTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TermLevelReverseTestCase
termLevelReverseTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "json")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelReverseTestCaseWithJson :: Phantoms.TTerm Testing.TermLevelReverseTestCase -> Phantoms.TTerm Model.Value -> Phantoms.TTerm Testing.TermLevelReverseTestCase
termLevelReverseTestCaseWithJson original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

termLevelReverseTestCaseWithSchema :: Phantoms.TTerm Testing.TermLevelReverseTestCase -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.TermLevelReverseTestCase
termLevelReverseTestCaseWithSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "json")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelReverseTestCaseWithTerm :: Phantoms.TTerm Testing.TermLevelReverseTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.TermLevelReverseTestCase
termLevelReverseTestCaseWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelReverseTestCase"),
              Core.projectionField = (Core.Name "json")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelRoundTripJsonTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Model.Value -> Phantoms.TTerm Model.Value -> Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase
termLevelRoundTripJsonTestCase description schema json expectedJson =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm schema)},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Phantoms.unTTerm json)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedJson"),
          Core.fieldTerm = (Phantoms.unTTerm expectedJson)}]}))

termLevelRoundTripJsonTestCaseDescription :: Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase -> Phantoms.TTerm String
termLevelRoundTripJsonTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelRoundTripJsonTestCaseExpectedJson :: Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase -> Phantoms.TTerm Model.Value
termLevelRoundTripJsonTestCaseExpectedJson x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
        Core.projectionField = (Core.Name "expectedJson")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelRoundTripJsonTestCaseJson :: Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase -> Phantoms.TTerm Model.Value
termLevelRoundTripJsonTestCaseJson x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
        Core.projectionField = (Core.Name "json")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelRoundTripJsonTestCaseSchema :: Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase -> Phantoms.TTerm Schema.Schema
termLevelRoundTripJsonTestCaseSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
        Core.projectionField = (Core.Name "schema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelRoundTripJsonTestCaseWithDescription :: Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase
termLevelRoundTripJsonTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "json")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedJson"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "expectedJson")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelRoundTripJsonTestCaseWithExpectedJson :: Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase -> Phantoms.TTerm Model.Value -> Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase
termLevelRoundTripJsonTestCaseWithExpectedJson original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "json")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedJson"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

termLevelRoundTripJsonTestCaseWithJson :: Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase -> Phantoms.TTerm Model.Value -> Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase
termLevelRoundTripJsonTestCaseWithJson original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedJson"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "expectedJson")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelRoundTripJsonTestCaseWithSchema :: Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.TermLevelRoundTripJsonTestCase
termLevelRoundTripJsonTestCaseWithSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "json")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedJson"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripJsonTestCase"),
              Core.projectionField = (Core.Name "expectedJson")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelRoundTripTermTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase
termLevelRoundTripTermTestCase description type_ term expectedTerm =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedTerm"),
          Core.fieldTerm = (Phantoms.unTTerm expectedTerm)}]}))

termLevelRoundTripTermTestCaseDescription :: Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase -> Phantoms.TTerm String
termLevelRoundTripTermTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelRoundTripTermTestCaseExpectedTerm :: Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase -> Phantoms.TTerm Core.Term
termLevelRoundTripTermTestCaseExpectedTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
        Core.projectionField = (Core.Name "expectedTerm")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelRoundTripTermTestCaseTerm :: Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase -> Phantoms.TTerm Core.Term
termLevelRoundTripTermTestCaseTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelRoundTripTermTestCaseType :: Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase -> Phantoms.TTerm Core.Type
termLevelRoundTripTermTestCaseType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termLevelRoundTripTermTestCaseWithDescription :: Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase
termLevelRoundTripTermTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "expectedTerm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelRoundTripTermTestCaseWithExpectedTerm :: Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase
termLevelRoundTripTermTestCaseWithExpectedTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedTerm"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

termLevelRoundTripTermTestCaseWithTerm :: Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase
termLevelRoundTripTermTestCaseWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "expectedTerm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termLevelRoundTripTermTestCaseWithType :: Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TermLevelRoundTripTermTestCase
termLevelRoundTripTermTestCaseWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TermLevelRoundTripTermTestCase"),
              Core.projectionField = (Core.Name "expectedTerm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeLevelForwardTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeLevelForwardTestCase
typeLevelForwardTestCase description schema type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm schema)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeLevelForwardTestCaseDescription :: Phantoms.TTerm Testing.TypeLevelForwardTestCase -> Phantoms.TTerm String
typeLevelForwardTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelForwardTestCaseSchema :: Phantoms.TTerm Testing.TypeLevelForwardTestCase -> Phantoms.TTerm Schema.Schema
typeLevelForwardTestCaseSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
        Core.projectionField = (Core.Name "schema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelForwardTestCaseType :: Phantoms.TTerm Testing.TypeLevelForwardTestCase -> Phantoms.TTerm Core.Type
typeLevelForwardTestCaseType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelForwardTestCaseWithDescription :: Phantoms.TTerm Testing.TypeLevelForwardTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TypeLevelForwardTestCase
typeLevelForwardTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeLevelForwardTestCaseWithSchema :: Phantoms.TTerm Testing.TypeLevelForwardTestCase -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.TypeLevelForwardTestCase
typeLevelForwardTestCaseWithSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeLevelForwardTestCaseWithType :: Phantoms.TTerm Testing.TypeLevelForwardTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeLevelForwardTestCase
typeLevelForwardTestCaseWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelForwardTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeLevelReverseTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.TypeLevelReverseTestCase
typeLevelReverseTestCase description type_ schema =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm schema)}]}))

typeLevelReverseTestCaseDescription :: Phantoms.TTerm Testing.TypeLevelReverseTestCase -> Phantoms.TTerm String
typeLevelReverseTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelReverseTestCaseSchema :: Phantoms.TTerm Testing.TypeLevelReverseTestCase -> Phantoms.TTerm Schema.Schema
typeLevelReverseTestCaseSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
        Core.projectionField = (Core.Name "schema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelReverseTestCaseType :: Phantoms.TTerm Testing.TypeLevelReverseTestCase -> Phantoms.TTerm Core.Type
typeLevelReverseTestCaseType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelReverseTestCaseWithDescription :: Phantoms.TTerm Testing.TypeLevelReverseTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TypeLevelReverseTestCase
typeLevelReverseTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeLevelReverseTestCaseWithSchema :: Phantoms.TTerm Testing.TypeLevelReverseTestCase -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.TypeLevelReverseTestCase
typeLevelReverseTestCaseWithSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeLevelReverseTestCaseWithType :: Phantoms.TTerm Testing.TypeLevelReverseTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeLevelReverseTestCase
typeLevelReverseTestCaseWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelReverseTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeLevelRoundTripAvroTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.TypeLevelRoundTripAvroTestCase
typeLevelRoundTripAvroTestCase description schema expectedSchema =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm schema)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedSchema"),
          Core.fieldTerm = (Phantoms.unTTerm expectedSchema)}]}))

typeLevelRoundTripAvroTestCaseDescription :: Phantoms.TTerm Testing.TypeLevelRoundTripAvroTestCase -> Phantoms.TTerm String
typeLevelRoundTripAvroTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelRoundTripAvroTestCaseExpectedSchema :: Phantoms.TTerm Testing.TypeLevelRoundTripAvroTestCase -> Phantoms.TTerm Schema.Schema
typeLevelRoundTripAvroTestCaseExpectedSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
        Core.projectionField = (Core.Name "expectedSchema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelRoundTripAvroTestCaseSchema :: Phantoms.TTerm Testing.TypeLevelRoundTripAvroTestCase -> Phantoms.TTerm Schema.Schema
typeLevelRoundTripAvroTestCaseSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
        Core.projectionField = (Core.Name "schema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelRoundTripAvroTestCaseWithDescription :: Phantoms.TTerm Testing.TypeLevelRoundTripAvroTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TypeLevelRoundTripAvroTestCase
typeLevelRoundTripAvroTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
              Core.projectionField = (Core.Name "expectedSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeLevelRoundTripAvroTestCaseWithExpectedSchema :: Phantoms.TTerm Testing.TypeLevelRoundTripAvroTestCase -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.TypeLevelRoundTripAvroTestCase
typeLevelRoundTripAvroTestCaseWithExpectedSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedSchema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeLevelRoundTripAvroTestCaseWithSchema :: Phantoms.TTerm Testing.TypeLevelRoundTripAvroTestCase -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.TypeLevelRoundTripAvroTestCase
typeLevelRoundTripAvroTestCaseWithSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripAvroTestCase"),
              Core.projectionField = (Core.Name "expectedSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeLevelRoundTripHydraTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeLevelRoundTripHydraTestCase
typeLevelRoundTripHydraTestCase description type_ expectedType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Phantoms.unTTerm expectedType)}]}))

typeLevelRoundTripHydraTestCaseDescription :: Phantoms.TTerm Testing.TypeLevelRoundTripHydraTestCase -> Phantoms.TTerm String
typeLevelRoundTripHydraTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelRoundTripHydraTestCaseExpectedType :: Phantoms.TTerm Testing.TypeLevelRoundTripHydraTestCase -> Phantoms.TTerm Core.Type
typeLevelRoundTripHydraTestCaseExpectedType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
        Core.projectionField = (Core.Name "expectedType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelRoundTripHydraTestCaseType :: Phantoms.TTerm Testing.TypeLevelRoundTripHydraTestCase -> Phantoms.TTerm Core.Type
typeLevelRoundTripHydraTestCaseType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLevelRoundTripHydraTestCaseWithDescription :: Phantoms.TTerm Testing.TypeLevelRoundTripHydraTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TypeLevelRoundTripHydraTestCase
typeLevelRoundTripHydraTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
              Core.projectionField = (Core.Name "expectedType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeLevelRoundTripHydraTestCaseWithExpectedType :: Phantoms.TTerm Testing.TypeLevelRoundTripHydraTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeLevelRoundTripHydraTestCase
typeLevelRoundTripHydraTestCaseWithExpectedType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeLevelRoundTripHydraTestCaseWithType :: Phantoms.TTerm Testing.TypeLevelRoundTripHydraTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeLevelRoundTripHydraTestCase
typeLevelRoundTripHydraTestCaseWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.TypeLevelRoundTripHydraTestCase"),
              Core.projectionField = (Core.Name "expectedType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionTestCase :: Phantoms.TTerm String -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm [(Core.Term, Model.Value)] -> Phantoms.TTerm Testing.UnionTestCase
unionTestCase description hydraType avroSchema termPairs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "hydraType"),
          Core.fieldTerm = (Phantoms.unTTerm hydraType)},
        Core.Field {
          Core.fieldName = (Core.Name "avroSchema"),
          Core.fieldTerm = (Phantoms.unTTerm avroSchema)},
        Core.Field {
          Core.fieldName = (Core.Name "termPairs"),
          Core.fieldTerm = (Phantoms.unTTerm termPairs)}]}))

unionTestCaseAvroSchema :: Phantoms.TTerm Testing.UnionTestCase -> Phantoms.TTerm Schema.Schema
unionTestCaseAvroSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
        Core.projectionField = (Core.Name "avroSchema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTestCaseDescription :: Phantoms.TTerm Testing.UnionTestCase -> Phantoms.TTerm String
unionTestCaseDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTestCaseHydraType :: Phantoms.TTerm Testing.UnionTestCase -> Phantoms.TTerm Core.Type
unionTestCaseHydraType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
        Core.projectionField = (Core.Name "hydraType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTestCaseTermPairs :: Phantoms.TTerm Testing.UnionTestCase -> Phantoms.TTerm [(Core.Term, Model.Value)]
unionTestCaseTermPairs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
        Core.projectionField = (Core.Name "termPairs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionTestCaseWithAvroSchema :: Phantoms.TTerm Testing.UnionTestCase -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Testing.UnionTestCase
unionTestCaseWithAvroSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hydraType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "hydraType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "avroSchema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "termPairs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "termPairs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionTestCaseWithDescription :: Phantoms.TTerm Testing.UnionTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.UnionTestCase
unionTestCaseWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "hydraType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "hydraType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "avroSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "avroSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termPairs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "termPairs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionTestCaseWithHydraType :: Phantoms.TTerm Testing.UnionTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.UnionTestCase
unionTestCaseWithHydraType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hydraType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "avroSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "avroSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termPairs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "termPairs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionTestCaseWithTermPairs :: Phantoms.TTerm Testing.UnionTestCase -> Phantoms.TTerm [(Core.Term, Model.Value)] -> Phantoms.TTerm Testing.UnionTestCase
unionTestCaseWithTermPairs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hydraType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "hydraType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "avroSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.avro.testing.UnionTestCase"),
              Core.projectionField = (Core.Name "avroSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termPairs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
