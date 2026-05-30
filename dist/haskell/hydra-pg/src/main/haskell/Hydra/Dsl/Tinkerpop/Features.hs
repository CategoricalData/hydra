-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.tinkerpop.features

module Hydra.Dsl.Tinkerpop.Features where

import qualified Hydra.Core as Core
import qualified Hydra.Typed as Phantoms
import qualified Hydra.Tinkerpop.Features as Features
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

dataTypeFeatures :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeatures supportsBooleanArrayValues supportsBooleanValues supportsByteArrayValues supportsByteValues supportsDoubleArrayValues supportsDoubleValues supportsFloatArrayValues supportsFloatValues supportsIntegerArrayValues supportsIntegerValues supportsLongArrayValues supportsLongValues supportsMapValues supportsMixedListValues supportsSerializableValues supportsStringArrayValues supportsStringValues supportsUniformListValues =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsBooleanArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsBooleanValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsByteArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsByteValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsDoubleArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsDoubleValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsFloatArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsFloatValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsIntegerArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsIntegerValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsLongArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsLongValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsMapValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsMixedListValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsSerializableValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsStringArrayValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsStringValues)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsUniformListValues)}]}))

dataTypeFeaturesSupportsBooleanArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsBooleanArrayValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsBooleanValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsBooleanValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsByteArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsByteArrayValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsByteValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsByteValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsByteValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsDoubleArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsDoubleArrayValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsDoubleValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsDoubleValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsFloatArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsFloatArrayValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsFloatValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsFloatValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsIntegerArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsIntegerArrayValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsIntegerValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsIntegerValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsLongArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsLongArrayValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsLongValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsLongValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsLongValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsMapValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsMapValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsMapValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsMixedListValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsMixedListValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsSerializableValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsSerializableValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsStringArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsStringArrayValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsStringValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsStringValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsStringValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesSupportsUniformListValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool
dataTypeFeaturesSupportsUniformListValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

dataTypeFeaturesWithSupportsBooleanArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsBooleanArrayValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsBooleanValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsBooleanValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsByteArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsByteArrayValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsByteValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsByteValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsDoubleArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsDoubleArrayValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsDoubleValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsDoubleValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsFloatArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsFloatArrayValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsFloatValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsFloatValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsIntegerArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsIntegerArrayValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsIntegerValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsIntegerValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsLongArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsLongArrayValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsLongValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsLongValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsMapValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsMapValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsMixedListValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsMixedListValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsSerializableValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsSerializableValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsStringArrayValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsStringArrayValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsStringValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsStringValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUniformListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dataTypeFeaturesWithSupportsUniformListValues :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.DataTypeFeatures
dataTypeFeaturesWithSupportsUniformListValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsBooleanValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsBooleanValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsByteValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsByteValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDoubleValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDoubleValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsFloatValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsFloatValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIntegerValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIntegerValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsLongValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsLongValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMapValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMixedListValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMixedListValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsSerializableValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsSerializableValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringArrayValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringArrayValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.DataTypeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUniformListValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

edgeFeatures :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Features.EdgePropertyFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.EdgeFeatures
edgeFeatures elementFeatures properties supportsAddEdges supportsRemoveEdges supportsUpsert =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm elementFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsAddEdges)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsRemoveEdges)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsUpsert)}]}))

edgeFeaturesElementFeatures :: Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Features.ElementFeatures
edgeFeaturesElementFeatures x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
        Core.projectionFieldName = (Core.Name "elementFeatures")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeFeaturesProperties :: Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Features.EdgePropertyFeatures
edgeFeaturesProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeFeaturesSupportsAddEdges :: Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Bool
edgeFeaturesSupportsAddEdges x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsAddEdges")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeFeaturesSupportsRemoveEdges :: Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Bool
edgeFeaturesSupportsRemoveEdges x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsRemoveEdges")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeFeaturesSupportsUpsert :: Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Bool
edgeFeaturesSupportsUpsert x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
        Core.projectionFieldName = (Core.Name "supportsUpsert")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeFeaturesWithElementFeatures :: Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Features.EdgeFeatures
edgeFeaturesWithElementFeatures original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeFeaturesWithProperties :: Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Features.EdgePropertyFeatures -> Phantoms.TypedTerm Features.EdgeFeatures
edgeFeaturesWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeFeaturesWithSupportsAddEdges :: Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.EdgeFeatures
edgeFeaturesWithSupportsAddEdges original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeFeaturesWithSupportsRemoveEdges :: Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.EdgeFeatures
edgeFeaturesWithSupportsRemoveEdges original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeFeaturesWithSupportsUpsert :: Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.EdgeFeatures
edgeFeaturesWithSupportsUpsert original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgeFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

edgePropertyFeatures :: Phantoms.TypedTerm Features.PropertyFeatures -> Phantoms.TypedTerm Features.EdgePropertyFeatures
edgePropertyFeatures propertyFeatures =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgePropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm propertyFeatures)}]}))

edgePropertyFeaturesPropertyFeatures :: Phantoms.TypedTerm Features.EdgePropertyFeatures -> Phantoms.TypedTerm Features.PropertyFeatures
edgePropertyFeaturesPropertyFeatures x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.EdgePropertyFeatures"),
        Core.projectionFieldName = (Core.Name "propertyFeatures")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgePropertyFeaturesWithPropertyFeatures :: Phantoms.TypedTerm Features.EdgePropertyFeatures -> Phantoms.TypedTerm Features.PropertyFeatures -> Phantoms.TypedTerm Features.EdgePropertyFeatures
edgePropertyFeaturesWithPropertyFeatures original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.EdgePropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

elementFeatures :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.ElementFeatures
elementFeatures supportsAddProperty supportsAnyIds supportsCustomIds supportsNumericIds supportsRemoveProperty supportsStringIds supportsUserSuppliedIds supportsUuidIds =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsAddProperty)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsAnyIds)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsCustomIds)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsNumericIds)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsRemoveProperty)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsStringIds)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsUserSuppliedIds)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsUuidIds)}]}))

elementFeaturesSupportsAddProperty :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool
elementFeaturesSupportsAddProperty x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionFieldName = (Core.Name "supportsAddProperty")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

elementFeaturesSupportsAnyIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool
elementFeaturesSupportsAnyIds x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionFieldName = (Core.Name "supportsAnyIds")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

elementFeaturesSupportsCustomIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool
elementFeaturesSupportsCustomIds x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionFieldName = (Core.Name "supportsCustomIds")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

elementFeaturesSupportsNumericIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool
elementFeaturesSupportsNumericIds x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionFieldName = (Core.Name "supportsNumericIds")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

elementFeaturesSupportsRemoveProperty :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool
elementFeaturesSupportsRemoveProperty x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionFieldName = (Core.Name "supportsRemoveProperty")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

elementFeaturesSupportsStringIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool
elementFeaturesSupportsStringIds x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionFieldName = (Core.Name "supportsStringIds")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

elementFeaturesSupportsUserSuppliedIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool
elementFeaturesSupportsUserSuppliedIds x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionFieldName = (Core.Name "supportsUserSuppliedIds")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

elementFeaturesSupportsUuidIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool
elementFeaturesSupportsUuidIds x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
        Core.projectionFieldName = (Core.Name "supportsUuidIds")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

elementFeaturesWithSupportsAddProperty :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.ElementFeatures
elementFeaturesWithSupportsAddProperty original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

elementFeaturesWithSupportsAnyIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.ElementFeatures
elementFeaturesWithSupportsAnyIds original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

elementFeaturesWithSupportsCustomIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.ElementFeatures
elementFeaturesWithSupportsCustomIds original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

elementFeaturesWithSupportsNumericIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.ElementFeatures
elementFeaturesWithSupportsNumericIds original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

elementFeaturesWithSupportsRemoveProperty :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.ElementFeatures
elementFeaturesWithSupportsRemoveProperty original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

elementFeaturesWithSupportsStringIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.ElementFeatures
elementFeaturesWithSupportsStringIds original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

elementFeaturesWithSupportsUserSuppliedIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.ElementFeatures
elementFeaturesWithSupportsUserSuppliedIds original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUuidIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

elementFeaturesWithSupportsUuidIds :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.ElementFeatures
elementFeaturesWithSupportsUuidIds original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAnyIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAnyIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsCustomIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsCustomIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsNumericIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsNumericIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveProperty")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsStringIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsStringIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUserSuppliedIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ElementFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUserSuppliedIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUuidIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

extraFeatures :: Phantoms.TypedTerm (Core.Type -> Bool) -> Phantoms.TypedTerm (Features.ExtraFeatures a)
extraFeatures supportsMapKey =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ExtraFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapKey"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsMapKey)}]}))

extraFeaturesSupportsMapKey :: Phantoms.TypedTerm (Features.ExtraFeatures a) -> Phantoms.TypedTerm (Core.Type -> Bool)
extraFeaturesSupportsMapKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.ExtraFeatures"),
        Core.projectionFieldName = (Core.Name "supportsMapKey")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

extraFeaturesWithSupportsMapKey :: Phantoms.TypedTerm (Features.ExtraFeatures a) -> Phantoms.TypedTerm (Core.Type -> Bool) -> Phantoms.TypedTerm (Features.ExtraFeatures a)
extraFeaturesWithSupportsMapKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.ExtraFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsMapKey"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

features :: Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Features.Features
features edge graph vertex =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Phantoms.unTypedTerm edge)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTypedTerm graph)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertex)}]}))

featuresEdge :: Phantoms.TypedTerm Features.Features -> Phantoms.TypedTerm Features.EdgeFeatures
featuresEdge x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
        Core.projectionFieldName = (Core.Name "edge")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

featuresGraph :: Phantoms.TypedTerm Features.Features -> Phantoms.TypedTerm Features.GraphFeatures
featuresGraph x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

featuresVertex :: Phantoms.TypedTerm Features.Features -> Phantoms.TypedTerm Features.VertexFeatures
featuresVertex x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
        Core.projectionFieldName = (Core.Name "vertex")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

featuresWithEdge :: Phantoms.TypedTerm Features.Features -> Phantoms.TypedTerm Features.EdgeFeatures -> Phantoms.TypedTerm Features.Features
featuresWithEdge original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

featuresWithGraph :: Phantoms.TypedTerm Features.Features -> Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Features.Features
featuresWithGraph original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionFieldName = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

featuresWithVertex :: Phantoms.TypedTerm Features.Features -> Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Features.Features
featuresWithVertex original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionFieldName = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.Features"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

graphFeatures :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VariableFeatures -> Phantoms.TypedTerm Features.GraphFeatures
graphFeatures supportsComputer supportsConcurrentAccess supportsIoRead supportsIoWrite supportsPersistence supportsThreadedTransactions supportsTransactions variables =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsComputer)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsConcurrentAccess)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsIoRead)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsIoWrite)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsPersistence)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsThreadedTransactions)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsTransactions)},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTypedTerm variables)}]}))

graphFeaturesSupportsComputer :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool
graphFeaturesSupportsComputer x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionFieldName = (Core.Name "supportsComputer")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphFeaturesSupportsConcurrentAccess :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool
graphFeaturesSupportsConcurrentAccess x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionFieldName = (Core.Name "supportsConcurrentAccess")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphFeaturesSupportsIoRead :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool
graphFeaturesSupportsIoRead x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionFieldName = (Core.Name "supportsIoRead")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphFeaturesSupportsIoWrite :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool
graphFeaturesSupportsIoWrite x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionFieldName = (Core.Name "supportsIoWrite")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphFeaturesSupportsPersistence :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool
graphFeaturesSupportsPersistence x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionFieldName = (Core.Name "supportsPersistence")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphFeaturesSupportsThreadedTransactions :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool
graphFeaturesSupportsThreadedTransactions x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionFieldName = (Core.Name "supportsThreadedTransactions")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphFeaturesSupportsTransactions :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool
graphFeaturesSupportsTransactions x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionFieldName = (Core.Name "supportsTransactions")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphFeaturesVariables :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Features.VariableFeatures
graphFeaturesVariables x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
        Core.projectionFieldName = (Core.Name "variables")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphFeaturesWithSupportsComputer :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.GraphFeatures
graphFeaturesWithSupportsComputer original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

graphFeaturesWithSupportsConcurrentAccess :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.GraphFeatures
graphFeaturesWithSupportsConcurrentAccess original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

graphFeaturesWithSupportsIoRead :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.GraphFeatures
graphFeaturesWithSupportsIoRead original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

graphFeaturesWithSupportsIoWrite :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.GraphFeatures
graphFeaturesWithSupportsIoWrite original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

graphFeaturesWithSupportsPersistence :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.GraphFeatures
graphFeaturesWithSupportsPersistence original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

graphFeaturesWithSupportsThreadedTransactions :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.GraphFeatures
graphFeaturesWithSupportsThreadedTransactions original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

graphFeaturesWithSupportsTransactions :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.GraphFeatures
graphFeaturesWithSupportsTransactions original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

graphFeaturesWithVariables :: Phantoms.TypedTerm Features.GraphFeatures -> Phantoms.TypedTerm Features.VariableFeatures -> Phantoms.TypedTerm Features.GraphFeatures
graphFeaturesWithVariables original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsComputer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsComputer")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsConcurrentAccess"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsConcurrentAccess")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoRead"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoRead")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsIoWrite"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsIoWrite")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsPersistence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsPersistence")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsThreadedTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsThreadedTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsTransactions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.GraphFeatures"),
              Core.projectionFieldName = (Core.Name "supportsTransactions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

propertyFeatures :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.PropertyFeatures
propertyFeatures dataTypeFeatures supportsProperties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm dataTypeFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsProperties"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsProperties)}]}))

propertyFeaturesDataTypeFeatures :: Phantoms.TypedTerm Features.PropertyFeatures -> Phantoms.TypedTerm Features.DataTypeFeatures
propertyFeaturesDataTypeFeatures x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
        Core.projectionFieldName = (Core.Name "dataTypeFeatures")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyFeaturesSupportsProperties :: Phantoms.TypedTerm Features.PropertyFeatures -> Phantoms.TypedTerm Bool
propertyFeaturesSupportsProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
        Core.projectionFieldName = (Core.Name "supportsProperties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyFeaturesWithDataTypeFeatures :: Phantoms.TypedTerm Features.PropertyFeatures -> Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Features.PropertyFeatures
propertyFeaturesWithDataTypeFeatures original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
              Core.projectionFieldName = (Core.Name "supportsProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertyFeaturesWithSupportsProperties :: Phantoms.TypedTerm Features.PropertyFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.PropertyFeatures
propertyFeaturesWithSupportsProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.PropertyFeatures"),
              Core.projectionFieldName = (Core.Name "dataTypeFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsProperties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

variableFeatures :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VariableFeatures
variableFeatures dataTypeFeatures supportsVariables =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm dataTypeFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsVariables"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsVariables)}]}))

variableFeaturesDataTypeFeatures :: Phantoms.TypedTerm Features.VariableFeatures -> Phantoms.TypedTerm Features.DataTypeFeatures
variableFeaturesDataTypeFeatures x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
        Core.projectionFieldName = (Core.Name "dataTypeFeatures")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

variableFeaturesSupportsVariables :: Phantoms.TypedTerm Features.VariableFeatures -> Phantoms.TypedTerm Bool
variableFeaturesSupportsVariables x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
        Core.projectionFieldName = (Core.Name "supportsVariables")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

variableFeaturesWithDataTypeFeatures :: Phantoms.TypedTerm Features.VariableFeatures -> Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Features.VariableFeatures
variableFeaturesWithDataTypeFeatures original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
              Core.projectionFieldName = (Core.Name "supportsVariables")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

variableFeaturesWithSupportsVariables :: Phantoms.TypedTerm Features.VariableFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VariableFeatures
variableFeaturesWithSupportsVariables original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VariableFeatures"),
              Core.projectionFieldName = (Core.Name "dataTypeFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsVariables"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

vertexFeatures :: Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Features.VertexPropertyFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VertexFeatures
vertexFeatures elementFeatures properties supportsAddVertices supportsDuplicateMultiProperties supportsMetaProperties supportsMultiProperties supportsRemoveVertices supportsUpsert =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm elementFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsAddVertices)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsDuplicateMultiProperties)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsMetaProperties)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsMultiProperties)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsRemoveVertices)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsUpsert)}]}))

vertexFeaturesElementFeatures :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Features.ElementFeatures
vertexFeaturesElementFeatures x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionFieldName = (Core.Name "elementFeatures")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexFeaturesProperties :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Features.VertexPropertyFeatures
vertexFeaturesProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexFeaturesSupportsAddVertices :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool
vertexFeaturesSupportsAddVertices x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionFieldName = (Core.Name "supportsAddVertices")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexFeaturesSupportsDuplicateMultiProperties :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool
vertexFeaturesSupportsDuplicateMultiProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionFieldName = (Core.Name "supportsDuplicateMultiProperties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexFeaturesSupportsMetaProperties :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool
vertexFeaturesSupportsMetaProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionFieldName = (Core.Name "supportsMetaProperties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexFeaturesSupportsMultiProperties :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool
vertexFeaturesSupportsMultiProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionFieldName = (Core.Name "supportsMultiProperties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexFeaturesSupportsRemoveVertices :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool
vertexFeaturesSupportsRemoveVertices x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionFieldName = (Core.Name "supportsRemoveVertices")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexFeaturesSupportsUpsert :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool
vertexFeaturesSupportsUpsert x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
        Core.projectionFieldName = (Core.Name "supportsUpsert")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexFeaturesWithElementFeatures :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Features.VertexFeatures
vertexFeaturesWithElementFeatures original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexFeaturesWithProperties :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Features.VertexPropertyFeatures -> Phantoms.TypedTerm Features.VertexFeatures
vertexFeaturesWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexFeaturesWithSupportsAddVertices :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VertexFeatures
vertexFeaturesWithSupportsAddVertices original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexFeaturesWithSupportsDuplicateMultiProperties :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VertexFeatures
vertexFeaturesWithSupportsDuplicateMultiProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexFeaturesWithSupportsMetaProperties :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VertexFeatures
vertexFeaturesWithSupportsMetaProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexFeaturesWithSupportsMultiProperties :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VertexFeatures
vertexFeaturesWithSupportsMultiProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexFeaturesWithSupportsRemoveVertices :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VertexFeatures
vertexFeaturesWithSupportsRemoveVertices original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsUpsert")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexFeaturesWithSupportsUpsert :: Phantoms.TypedTerm Features.VertexFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VertexFeatures
vertexFeaturesWithSupportsUpsert original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsAddVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsAddVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsDuplicateMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsDuplicateMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMetaProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMetaProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsMultiProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsMultiProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemoveVertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemoveVertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsUpsert"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

vertexPropertyFeatures :: Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Features.PropertyFeatures -> Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VertexPropertyFeatures
vertexPropertyFeatures dataTypeFeatures propertyFeatures elementFeatures supportsRemove =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm dataTypeFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm propertyFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm elementFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemove"),
          Core.fieldTerm = (Phantoms.unTypedTerm supportsRemove)}]}))

vertexPropertyFeaturesDataTypeFeatures :: Phantoms.TypedTerm Features.VertexPropertyFeatures -> Phantoms.TypedTerm Features.DataTypeFeatures
vertexPropertyFeaturesDataTypeFeatures x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
        Core.projectionFieldName = (Core.Name "dataTypeFeatures")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexPropertyFeaturesElementFeatures :: Phantoms.TypedTerm Features.VertexPropertyFeatures -> Phantoms.TypedTerm Features.ElementFeatures
vertexPropertyFeaturesElementFeatures x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
        Core.projectionFieldName = (Core.Name "elementFeatures")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexPropertyFeaturesPropertyFeatures :: Phantoms.TypedTerm Features.VertexPropertyFeatures -> Phantoms.TypedTerm Features.PropertyFeatures
vertexPropertyFeaturesPropertyFeatures x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
        Core.projectionFieldName = (Core.Name "propertyFeatures")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexPropertyFeaturesSupportsRemove :: Phantoms.TypedTerm Features.VertexPropertyFeatures -> Phantoms.TypedTerm Bool
vertexPropertyFeaturesSupportsRemove x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
        Core.projectionFieldName = (Core.Name "supportsRemove")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexPropertyFeaturesWithDataTypeFeatures :: Phantoms.TypedTerm Features.VertexPropertyFeatures -> Phantoms.TypedTerm Features.DataTypeFeatures -> Phantoms.TypedTerm Features.VertexPropertyFeatures
vertexPropertyFeaturesWithDataTypeFeatures original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "propertyFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemove"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemove")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexPropertyFeaturesWithElementFeatures :: Phantoms.TypedTerm Features.VertexPropertyFeatures -> Phantoms.TypedTerm Features.ElementFeatures -> Phantoms.TypedTerm Features.VertexPropertyFeatures
vertexPropertyFeaturesWithElementFeatures original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "dataTypeFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "propertyFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemove"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemove")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexPropertyFeaturesWithPropertyFeatures :: Phantoms.TypedTerm Features.VertexPropertyFeatures -> Phantoms.TypedTerm Features.PropertyFeatures -> Phantoms.TypedTerm Features.VertexPropertyFeatures
vertexPropertyFeaturesWithPropertyFeatures original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "dataTypeFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemove"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "supportsRemove")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexPropertyFeaturesWithSupportsRemove :: Phantoms.TypedTerm Features.VertexPropertyFeatures -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Features.VertexPropertyFeatures
vertexPropertyFeaturesWithSupportsRemove original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dataTypeFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "dataTypeFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "propertyFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elementFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tinkerpop.features.VertexPropertyFeatures"),
              Core.projectionFieldName = (Core.Name "elementFeatures")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportsRemove"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
