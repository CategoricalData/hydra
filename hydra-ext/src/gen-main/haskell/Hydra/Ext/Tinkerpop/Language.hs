-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints based on TinkerPop Graph.Features

module Hydra.Ext.Tinkerpop.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Apache.Tinkerpop.Features as Features
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Strip as Strip
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Populate language constraints based on TinkerPop Graph.Features
tinkerpopLanguage :: Coders.LanguageName -> Features.Features -> Features.ExtraFeatures t0 -> Coders.Language
tinkerpopLanguage name features extras =

      let vpFeatures =
              Features.vertexPropertyFeaturesDataTypeFeatures (Features.vertexFeaturesProperties (Features.featuresVertex features))
          cond = \v -> \b -> Logic.ifElse b (Maybes.pure v) Nothing
          supportsLists =
                  Logic.or (Features.dataTypeFeaturesSupportsBooleanArrayValues vpFeatures) (Logic.or (Features.dataTypeFeaturesSupportsByteArrayValues vpFeatures) (Logic.or (Features.dataTypeFeaturesSupportsDoubleArrayValues vpFeatures) (Logic.or (Features.dataTypeFeaturesSupportsFloatArrayValues vpFeatures) (Logic.or (Features.dataTypeFeaturesSupportsIntegerArrayValues vpFeatures) (Logic.or (Features.dataTypeFeaturesSupportsLongArrayValues vpFeatures) (Features.dataTypeFeaturesSupportsStringArrayValues vpFeatures))))))
          supportsLiterals = True
          supportsMaps = Features.dataTypeFeaturesSupportsMapValues vpFeatures
          eliminationVariants = Sets.empty
          literalVariants =
                  Sets.fromList (Maybes.cat [
                    cond Variants.LiteralVariantBinary (Features.dataTypeFeaturesSupportsByteArrayValues vpFeatures),
                    (cond Variants.LiteralVariantBoolean (Features.dataTypeFeaturesSupportsBooleanValues vpFeatures)),
                    (cond Variants.LiteralVariantFloat (Logic.or (Features.dataTypeFeaturesSupportsFloatValues vpFeatures) (Features.dataTypeFeaturesSupportsDoubleValues vpFeatures))),
                    (cond Variants.LiteralVariantInteger (Logic.or (Features.dataTypeFeaturesSupportsIntegerValues vpFeatures) (Features.dataTypeFeaturesSupportsLongValues vpFeatures))),
                    (cond Variants.LiteralVariantString (Features.dataTypeFeaturesSupportsStringValues vpFeatures))])
          floatTypes =
                  Sets.fromList (Maybes.cat [
                    cond Core.FloatTypeFloat32 (Features.dataTypeFeaturesSupportsFloatValues vpFeatures),
                    (cond Core.FloatTypeFloat64 (Features.dataTypeFeaturesSupportsDoubleValues vpFeatures))])
          functionVariants = Sets.empty
          integerTypes =
                  Sets.fromList (Maybes.cat [
                    cond Core.IntegerTypeInt32 (Features.dataTypeFeaturesSupportsIntegerValues vpFeatures),
                    (cond Core.IntegerTypeInt64 (Features.dataTypeFeaturesSupportsLongValues vpFeatures))])
          termVariants =
                  Sets.fromList (Maybes.cat [
                    cond Variants.TermVariantList supportsLists,
                    (cond Variants.TermVariantLiteral supportsLiterals),
                    (cond Variants.TermVariantMap supportsMaps),
                    (Maybes.pure Variants.TermVariantMaybe)])
          typeVariants =
                  Sets.fromList (Maybes.cat [
                    cond Variants.TypeVariantList supportsLists,
                    (cond Variants.TypeVariantLiteral supportsLiterals),
                    (cond Variants.TypeVariantMap supportsMaps),
                    (Maybes.pure Variants.TypeVariantMaybe),
                    (Maybes.pure Variants.TypeVariantWrap)])
          typePredicate =
                  \typ ->
                    let dt = Strip.deannotateType typ
                    in case dt of
                      Core.TypeList v0 -> case (Strip.deannotateType v0) of
                        Core.TypeLiteral v1 -> case v1 of
                          Core.LiteralTypeBoolean -> Features.dataTypeFeaturesSupportsBooleanArrayValues vpFeatures
                          Core.LiteralTypeFloat v2 -> case v2 of
                            Core.FloatTypeFloat64 -> Features.dataTypeFeaturesSupportsDoubleArrayValues vpFeatures
                            Core.FloatTypeFloat32 -> Features.dataTypeFeaturesSupportsFloatArrayValues vpFeatures
                            _ -> False
                          Core.LiteralTypeInteger v2 -> case v2 of
                            Core.IntegerTypeUint8 -> Features.dataTypeFeaturesSupportsByteArrayValues vpFeatures
                            Core.IntegerTypeInt32 -> Features.dataTypeFeaturesSupportsIntegerArrayValues vpFeatures
                            Core.IntegerTypeInt64 -> Features.dataTypeFeaturesSupportsLongArrayValues vpFeatures
                            _ -> False
                          Core.LiteralTypeString -> Features.dataTypeFeaturesSupportsStringArrayValues vpFeatures
                          _ -> False
                        _ -> False
                      Core.TypeLiteral _ -> True
                      Core.TypeMap v0 -> Features.extraFeaturesSupportsMapKey extras (Core.mapTypeKeys v0)
                      Core.TypeWrap _ -> True
                      Core.TypeMaybe v0 -> case (Strip.deannotateType v0) of
                        Core.TypeLiteral _ -> True
                        _ -> False
                      _ -> True
      in Coders.Language {
        Coders.languageName = name,
        Coders.languageConstraints = Coders.LanguageConstraints {
          Coders.languageConstraintsEliminationVariants = eliminationVariants,
          Coders.languageConstraintsLiteralVariants = literalVariants,
          Coders.languageConstraintsFloatTypes = floatTypes,
          Coders.languageConstraintsFunctionVariants = functionVariants,
          Coders.languageConstraintsIntegerTypes = integerTypes,
          Coders.languageConstraintsTermVariants = termVariants,
          Coders.languageConstraintsTypeVariants = typeVariants,
          Coders.languageConstraintsTypes = typePredicate}}
