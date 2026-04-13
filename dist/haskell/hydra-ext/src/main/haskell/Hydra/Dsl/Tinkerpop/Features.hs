-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.tinkerpop.features

module Hydra.Dsl.Tinkerpop.Features where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Tinkerpop.Features as Features
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

dataTypeFeatures :: Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeatures supportsBooleanArrayValues supportsBooleanValues supportsByteArrayValues supportsByteValues supportsDoubleArrayValues supportsDoubleValues supportsFloatArrayValues supportsFloatValues supportsIntegerArrayValues supportsIntegerValues supportsLongArrayValues supportsLongValues supportsMapValues supportsMixedListValues supportsSerializableValues supportsStringArrayValues supportsStringValues supportsUniformListValues =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsBooleanArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsBooleanValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsByteArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsByteValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsDoubleArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsDoubleValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsFloatArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsFloatValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsIntegerArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsIntegerValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsLongArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsLongValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsMapValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsMixedListValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsSerializableValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsStringArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsStringValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Phantoms.unTTerm supportsUniformListValues)}]}))

dataTypeFeaturesSupportsBooleanArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsBooleanArrayValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsBooleanValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsBooleanValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsBooleanValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsByteArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsByteArrayValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsByteArrayValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsByteValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsByteValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsByteValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsDoubleArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsDoubleArrayValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsDoubleValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsDoubleValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsDoubleValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsFloatArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsFloatArrayValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsFloatValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsFloatValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsFloatValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsIntegerArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsIntegerArrayValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsIntegerValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsIntegerValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsIntegerValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsLongArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsLongArrayValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsLongArrayValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsLongValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsLongValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsLongValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsMapValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsMapValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsMapValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsMixedListValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsMixedListValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsMixedListValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsSerializableValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsSerializableValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsSerializableValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsStringArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsStringArrayValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsStringArrayValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsStringValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsStringValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsStringValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesSupportsUniformListValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool
dataTypeFeaturesSupportsUniformListValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionField = (Core.Name "supportsUniformListValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypeFeaturesWithSupportsBooleanArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsBooleanArrayValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsBooleanValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsBooleanValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsByteArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsByteArrayValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsByteValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsByteValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsDoubleArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsDoubleArrayValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsDoubleValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsDoubleValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsFloatArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsFloatArrayValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsFloatValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsFloatValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsIntegerArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsIntegerArrayValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsIntegerValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsIntegerValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsLongArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsLongArrayValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsLongValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsLongValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsMapValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsMapValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsMixedListValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsMixedListValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsSerializableValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsSerializableValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsStringArrayValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsStringArrayValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsStringValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsStringValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataTypeFeaturesWithSupportsUniformListValues :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsUniformListValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionField = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

edgeFeatures :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Features.EdgePropertyFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.EdgeFeatures
edgeFeatures elementFeatures properties supportsAddEdges supportsRemoveEdges supportsUpsert =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm elementFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Phantoms.unTTerm supportsAddEdges)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Phantoms.unTTerm supportsRemoveEdges)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Phantoms.unTTerm supportsUpsert)}]}))

edgeFeaturesElementFeatures :: Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Features.ElementFeatures
edgeFeaturesElementFeatures x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
        Core.projectionField = (Core.Name "elementFeatures")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeFeaturesProperties :: Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Features.EdgePropertyFeatures
edgeFeaturesProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeFeaturesSupportsAddEdges :: Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Bool
edgeFeaturesSupportsAddEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
        Core.projectionField = (Core.Name "supportsAddEdges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeFeaturesSupportsRemoveEdges :: Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Bool
edgeFeaturesSupportsRemoveEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
        Core.projectionField = (Core.Name "supportsRemoveEdges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeFeaturesSupportsUpsert :: Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Bool
edgeFeaturesSupportsUpsert x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
        Core.projectionField = (Core.Name "supportsUpsert")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeFeaturesWithElementFeatures :: Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Features.EdgeFeatures
edgeFeaturesWithElementFeatures original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsAddEdges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveEdges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeFeaturesWithProperties :: Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Features.EdgePropertyFeatures -> Phantoms.TTerm Features.EdgeFeatures
edgeFeaturesWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsAddEdges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveEdges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeFeaturesWithSupportsAddEdges :: Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.EdgeFeatures
edgeFeaturesWithSupportsAddEdges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveEdges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeFeaturesWithSupportsRemoveEdges :: Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.EdgeFeatures
edgeFeaturesWithSupportsRemoveEdges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsAddEdges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeFeaturesWithSupportsUpsert :: Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.EdgeFeatures
edgeFeaturesWithSupportsUpsert original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsAddEdges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveEdges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

edgePropertyFeatures :: Phantoms.TTerm Features.PropertyFeatures -> Phantoms.TTerm Features.EdgePropertyFeatures
edgePropertyFeatures propertyFeatures =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgePropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm propertyFeatures)}]}))

edgePropertyFeaturesPropertyFeatures :: Phantoms.TTerm Features.EdgePropertyFeatures -> Phantoms.TTerm Features.PropertyFeatures
edgePropertyFeaturesPropertyFeatures x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgePropertyFeatures"),
        Core.projectionField = (Core.Name "propertyFeatures")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgePropertyFeaturesWithPropertyFeatures :: Phantoms.TTerm Features.EdgePropertyFeatures -> Phantoms.TTerm Features.PropertyFeatures -> Phantoms.TTerm Features.EdgePropertyFeatures
edgePropertyFeaturesWithPropertyFeatures original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgePropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

elementFeatures :: Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.ElementFeatures
elementFeatures supportsAddProperty supportsAnyIds supportsCustomIds supportsNumericIds supportsRemoveProperty supportsStringIds supportsUserSuppliedIds supportsUuidIds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Phantoms.unTTerm supportsAddProperty)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Phantoms.unTTerm supportsAnyIds)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Phantoms.unTTerm supportsCustomIds)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Phantoms.unTTerm supportsNumericIds)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Phantoms.unTTerm supportsRemoveProperty)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Phantoms.unTTerm supportsStringIds)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Phantoms.unTTerm supportsUserSuppliedIds)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Phantoms.unTTerm supportsUuidIds)}]}))

elementFeaturesSupportsAddProperty :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool
elementFeaturesSupportsAddProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionField = (Core.Name "supportsAddProperty")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementFeaturesSupportsAnyIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool
elementFeaturesSupportsAnyIds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionField = (Core.Name "supportsAnyIds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementFeaturesSupportsCustomIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool
elementFeaturesSupportsCustomIds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionField = (Core.Name "supportsCustomIds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementFeaturesSupportsNumericIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool
elementFeaturesSupportsNumericIds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionField = (Core.Name "supportsNumericIds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementFeaturesSupportsRemoveProperty :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool
elementFeaturesSupportsRemoveProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionField = (Core.Name "supportsRemoveProperty")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementFeaturesSupportsStringIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool
elementFeaturesSupportsStringIds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionField = (Core.Name "supportsStringIds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementFeaturesSupportsUserSuppliedIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool
elementFeaturesSupportsUserSuppliedIds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionField = (Core.Name "supportsUserSuppliedIds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementFeaturesSupportsUuidIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool
elementFeaturesSupportsUuidIds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionField = (Core.Name "supportsUuidIds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementFeaturesWithSupportsAddProperty :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.ElementFeatures
elementFeaturesWithSupportsAddProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elementFeaturesWithSupportsAnyIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.ElementFeatures
elementFeaturesWithSupportsAnyIds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elementFeaturesWithSupportsCustomIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.ElementFeatures
elementFeaturesWithSupportsCustomIds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elementFeaturesWithSupportsNumericIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.ElementFeatures
elementFeaturesWithSupportsNumericIds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elementFeaturesWithSupportsRemoveProperty :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.ElementFeatures
elementFeaturesWithSupportsRemoveProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elementFeaturesWithSupportsStringIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.ElementFeatures
elementFeaturesWithSupportsStringIds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elementFeaturesWithSupportsUserSuppliedIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.ElementFeatures
elementFeaturesWithSupportsUserSuppliedIds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elementFeaturesWithSupportsUuidIds :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.ElementFeatures
elementFeaturesWithSupportsUuidIds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionField = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

extraFeatures :: Phantoms.TTerm (Core.Type -> Bool) -> Phantoms.TTerm (Features.ExtraFeatures a)
extraFeatures supportsMapKey =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ExtraFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapKey"),
          Core.fieldTerm = (Phantoms.unTTerm supportsMapKey)}]}))

extraFeaturesSupportsMapKey :: Phantoms.TTerm (Features.ExtraFeatures a) -> Phantoms.TTerm (Core.Type -> Bool)
extraFeaturesSupportsMapKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ExtraFeatures"),
        Core.projectionField = (Core.Name "supportsMapKey")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

extraFeaturesWithSupportsMapKey :: Phantoms.TTerm (Features.ExtraFeatures a) -> Phantoms.TTerm (Core.Type -> Bool) -> Phantoms.TTerm (Features.ExtraFeatures a)
extraFeaturesWithSupportsMapKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ExtraFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapKey"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

features :: Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Features.Features
features edge graph vertex =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Phantoms.unTTerm edge)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm graph)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTTerm vertex)}]}))

featuresEdge :: Phantoms.TTerm Features.Features -> Phantoms.TTerm Features.EdgeFeatures
featuresEdge x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
        Core.projectionField = (Core.Name "edge")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featuresGraph :: Phantoms.TTerm Features.Features -> Phantoms.TTerm Features.GraphFeatures
featuresGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
        Core.projectionField = (Core.Name "graph")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featuresVertex :: Phantoms.TTerm Features.Features -> Phantoms.TTerm Features.VertexFeatures
featuresVertex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
        Core.projectionField = (Core.Name "vertex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featuresWithEdge :: Phantoms.TTerm Features.Features -> Phantoms.TTerm Features.EdgeFeatures -> Phantoms.TTerm Features.Features
featuresWithEdge original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionField = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionField = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

featuresWithGraph :: Phantoms.TTerm Features.Features -> Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Features.Features
featuresWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionField = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionField = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

featuresWithVertex :: Phantoms.TTerm Features.Features -> Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Features.Features
featuresWithVertex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionField = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionField = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

graphFeatures :: Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VariableFeatures -> Phantoms.TTerm Features.GraphFeatures
graphFeatures supportsComputer supportsConcurrentAccess supportsIoRead supportsIoWrite supportsPersistence supportsThreadedTransactions supportsTransactions variables =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Phantoms.unTTerm supportsComputer)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Phantoms.unTTerm supportsConcurrentAccess)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Phantoms.unTTerm supportsIoRead)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Phantoms.unTTerm supportsIoWrite)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Phantoms.unTTerm supportsPersistence)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Phantoms.unTTerm supportsThreadedTransactions)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Phantoms.unTTerm supportsTransactions)},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm variables)}]}))

graphFeaturesSupportsComputer :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool
graphFeaturesSupportsComputer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionField = (Core.Name "supportsComputer")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphFeaturesSupportsConcurrentAccess :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool
graphFeaturesSupportsConcurrentAccess x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionField = (Core.Name "supportsConcurrentAccess")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphFeaturesSupportsIoRead :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool
graphFeaturesSupportsIoRead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionField = (Core.Name "supportsIoRead")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphFeaturesSupportsIoWrite :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool
graphFeaturesSupportsIoWrite x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionField = (Core.Name "supportsIoWrite")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphFeaturesSupportsPersistence :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool
graphFeaturesSupportsPersistence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionField = (Core.Name "supportsPersistence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphFeaturesSupportsThreadedTransactions :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool
graphFeaturesSupportsThreadedTransactions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionField = (Core.Name "supportsThreadedTransactions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphFeaturesSupportsTransactions :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool
graphFeaturesSupportsTransactions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionField = (Core.Name "supportsTransactions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphFeaturesVariables :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Features.VariableFeatures
graphFeaturesVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionField = (Core.Name "variables")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphFeaturesWithSupportsComputer :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.GraphFeatures
graphFeaturesWithSupportsComputer original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

graphFeaturesWithSupportsConcurrentAccess :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.GraphFeatures
graphFeaturesWithSupportsConcurrentAccess original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

graphFeaturesWithSupportsIoRead :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.GraphFeatures
graphFeaturesWithSupportsIoRead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

graphFeaturesWithSupportsIoWrite :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.GraphFeatures
graphFeaturesWithSupportsIoWrite original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

graphFeaturesWithSupportsPersistence :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.GraphFeatures
graphFeaturesWithSupportsPersistence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

graphFeaturesWithSupportsThreadedTransactions :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.GraphFeatures
graphFeaturesWithSupportsThreadedTransactions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

graphFeaturesWithSupportsTransactions :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.GraphFeatures
graphFeaturesWithSupportsTransactions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

graphFeaturesWithVariables :: Phantoms.TTerm Features.GraphFeatures -> Phantoms.TTerm Features.VariableFeatures -> Phantoms.TTerm Features.GraphFeatures
graphFeaturesWithVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionField = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyFeatures :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.PropertyFeatures
propertyFeatures dataTypeFeatures supportsProperties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm dataTypeFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsProperties"),
          Core.fieldTerm = (Phantoms.unTTerm supportsProperties)}]}))

propertyFeaturesDataTypeFeatures :: Phantoms.TTerm Features.PropertyFeatures -> Phantoms.TTerm Features.DataTypeFeatures
propertyFeaturesDataTypeFeatures x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
        Core.projectionField = (Core.Name "dataTypeFeatures")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyFeaturesSupportsProperties :: Phantoms.TTerm Features.PropertyFeatures -> Phantoms.TTerm Bool
propertyFeaturesSupportsProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
        Core.projectionField = (Core.Name "supportsProperties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyFeaturesWithDataTypeFeatures :: Phantoms.TTerm Features.PropertyFeatures -> Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Features.PropertyFeatures
propertyFeaturesWithDataTypeFeatures original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
              Core.projectionField = (Core.Name "supportsProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyFeaturesWithSupportsProperties :: Phantoms.TTerm Features.PropertyFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.PropertyFeatures
propertyFeaturesWithSupportsProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
              Core.projectionField = (Core.Name "dataTypeFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsProperties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableFeatures :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VariableFeatures
variableFeatures dataTypeFeatures supportsVariables =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm dataTypeFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsVariables"),
          Core.fieldTerm = (Phantoms.unTTerm supportsVariables)}]}))

variableFeaturesDataTypeFeatures :: Phantoms.TTerm Features.VariableFeatures -> Phantoms.TTerm Features.DataTypeFeatures
variableFeaturesDataTypeFeatures x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
        Core.projectionField = (Core.Name "dataTypeFeatures")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableFeaturesSupportsVariables :: Phantoms.TTerm Features.VariableFeatures -> Phantoms.TTerm Bool
variableFeaturesSupportsVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
        Core.projectionField = (Core.Name "supportsVariables")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableFeaturesWithDataTypeFeatures :: Phantoms.TTerm Features.VariableFeatures -> Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Features.VariableFeatures
variableFeaturesWithDataTypeFeatures original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
              Core.projectionField = (Core.Name "supportsVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableFeaturesWithSupportsVariables :: Phantoms.TTerm Features.VariableFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VariableFeatures
variableFeaturesWithSupportsVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
              Core.projectionField = (Core.Name "dataTypeFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsVariables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

vertexFeatures :: Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Features.VertexPropertyFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VertexFeatures
vertexFeatures elementFeatures properties supportsAddVertices supportsDuplicateMultiProperties supportsMetaProperties supportsMultiProperties supportsRemoveVertices supportsUpsert =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm elementFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Phantoms.unTTerm supportsAddVertices)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Phantoms.unTTerm supportsDuplicateMultiProperties)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Phantoms.unTTerm supportsMetaProperties)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Phantoms.unTTerm supportsMultiProperties)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Phantoms.unTTerm supportsRemoveVertices)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Phantoms.unTTerm supportsUpsert)}]}))

vertexFeaturesElementFeatures :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Features.ElementFeatures
vertexFeaturesElementFeatures x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionField = (Core.Name "elementFeatures")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexFeaturesProperties :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Features.VertexPropertyFeatures
vertexFeaturesProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexFeaturesSupportsAddVertices :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool
vertexFeaturesSupportsAddVertices x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionField = (Core.Name "supportsAddVertices")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexFeaturesSupportsDuplicateMultiProperties :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool
vertexFeaturesSupportsDuplicateMultiProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionField = (Core.Name "supportsDuplicateMultiProperties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexFeaturesSupportsMetaProperties :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool
vertexFeaturesSupportsMetaProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionField = (Core.Name "supportsMetaProperties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexFeaturesSupportsMultiProperties :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool
vertexFeaturesSupportsMultiProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionField = (Core.Name "supportsMultiProperties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexFeaturesSupportsRemoveVertices :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool
vertexFeaturesSupportsRemoveVertices x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionField = (Core.Name "supportsRemoveVertices")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexFeaturesSupportsUpsert :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool
vertexFeaturesSupportsUpsert x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionField = (Core.Name "supportsUpsert")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexFeaturesWithElementFeatures :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Features.VertexFeatures
vertexFeaturesWithElementFeatures original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexFeaturesWithProperties :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Features.VertexPropertyFeatures -> Phantoms.TTerm Features.VertexFeatures
vertexFeaturesWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexFeaturesWithSupportsAddVertices :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VertexFeatures
vertexFeaturesWithSupportsAddVertices original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexFeaturesWithSupportsDuplicateMultiProperties :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VertexFeatures
vertexFeaturesWithSupportsDuplicateMultiProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexFeaturesWithSupportsMetaProperties :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VertexFeatures
vertexFeaturesWithSupportsMetaProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexFeaturesWithSupportsMultiProperties :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VertexFeatures
vertexFeaturesWithSupportsMultiProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexFeaturesWithSupportsRemoveVertices :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VertexFeatures
vertexFeaturesWithSupportsRemoveVertices original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexFeaturesWithSupportsUpsert :: Phantoms.TTerm Features.VertexFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VertexFeatures
vertexFeaturesWithSupportsUpsert original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionField = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

vertexPropertyFeatures :: Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Features.PropertyFeatures -> Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VertexPropertyFeatures
vertexPropertyFeatures dataTypeFeatures propertyFeatures elementFeatures supportsRemove =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm dataTypeFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm propertyFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm elementFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemove"),
          Core.fieldTerm = (Phantoms.unTTerm supportsRemove)}]}))

vertexPropertyFeaturesDataTypeFeatures :: Phantoms.TTerm Features.VertexPropertyFeatures -> Phantoms.TTerm Features.DataTypeFeatures
vertexPropertyFeaturesDataTypeFeatures x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
        Core.projectionField = (Core.Name "dataTypeFeatures")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPropertyFeaturesElementFeatures :: Phantoms.TTerm Features.VertexPropertyFeatures -> Phantoms.TTerm Features.ElementFeatures
vertexPropertyFeaturesElementFeatures x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
        Core.projectionField = (Core.Name "elementFeatures")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPropertyFeaturesPropertyFeatures :: Phantoms.TTerm Features.VertexPropertyFeatures -> Phantoms.TTerm Features.PropertyFeatures
vertexPropertyFeaturesPropertyFeatures x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
        Core.projectionField = (Core.Name "propertyFeatures")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPropertyFeaturesSupportsRemove :: Phantoms.TTerm Features.VertexPropertyFeatures -> Phantoms.TTerm Bool
vertexPropertyFeaturesSupportsRemove x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
        Core.projectionField = (Core.Name "supportsRemove")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPropertyFeaturesWithDataTypeFeatures :: Phantoms.TTerm Features.VertexPropertyFeatures -> Phantoms.TTerm Features.DataTypeFeatures -> Phantoms.TTerm Features.VertexPropertyFeatures
vertexPropertyFeaturesWithDataTypeFeatures original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "propertyFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemove"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "supportsRemove")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexPropertyFeaturesWithElementFeatures :: Phantoms.TTerm Features.VertexPropertyFeatures -> Phantoms.TTerm Features.ElementFeatures -> Phantoms.TTerm Features.VertexPropertyFeatures
vertexPropertyFeaturesWithElementFeatures original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "dataTypeFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "propertyFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemove"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "supportsRemove")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexPropertyFeaturesWithPropertyFeatures :: Phantoms.TTerm Features.VertexPropertyFeatures -> Phantoms.TTerm Features.PropertyFeatures -> Phantoms.TTerm Features.VertexPropertyFeatures
vertexPropertyFeaturesWithPropertyFeatures original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "dataTypeFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemove"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "supportsRemove")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexPropertyFeaturesWithSupportsRemove :: Phantoms.TTerm Features.VertexPropertyFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Features.VertexPropertyFeatures
vertexPropertyFeaturesWithSupportsRemove original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "dataTypeFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "propertyFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionField = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemove"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
