-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.variants

module Hydra.Decode.Variants where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

eliminationVariant :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Variants.EliminationVariant
eliminationVariant cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "record", (\input -> Eithers.map (\t -> Variants.EliminationVariantRecord) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "union", (\input -> Eithers.map (\t -> Variants.EliminationVariantUnion) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.EliminationVariantWrap) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

functionVariant :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Variants.FunctionVariant
functionVariant cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "elimination", (\input -> Eithers.map (\t -> Variants.FunctionVariantElimination) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "lambda", (\input -> Eithers.map (\t -> Variants.FunctionVariantLambda) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

literalVariant :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Variants.LiteralVariant
literalVariant cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "binary", (\input -> Eithers.map (\t -> Variants.LiteralVariantBinary) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "boolean", (\input -> Eithers.map (\t -> Variants.LiteralVariantBoolean) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "float", (\input -> Eithers.map (\t -> Variants.LiteralVariantFloat) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "integer", (\input -> Eithers.map (\t -> Variants.LiteralVariantInteger) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "string", (\input -> Eithers.map (\t -> Variants.LiteralVariantString) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

termVariant :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Variants.TermVariant
termVariant cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "annotated", (\input -> Eithers.map (\t -> Variants.TermVariantAnnotated) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "application", (\input -> Eithers.map (\t -> Variants.TermVariantApplication) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "either", (\input -> Eithers.map (\t -> Variants.TermVariantEither) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "function", (\input -> Eithers.map (\t -> Variants.TermVariantFunction) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "let", (\input -> Eithers.map (\t -> Variants.TermVariantLet) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "list", (\input -> Eithers.map (\t -> Variants.TermVariantList) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "literal", (\input -> Eithers.map (\t -> Variants.TermVariantLiteral) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "map", (\input -> Eithers.map (\t -> Variants.TermVariantMap) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "maybe", (\input -> Eithers.map (\t -> Variants.TermVariantMaybe) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "pair", (\input -> Eithers.map (\t -> Variants.TermVariantPair) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "record", (\input -> Eithers.map (\t -> Variants.TermVariantRecord) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "set", (\input -> Eithers.map (\t -> Variants.TermVariantSet) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "typeApplication", (\input -> Eithers.map (\t -> Variants.TermVariantTypeApplication) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "typeLambda", (\input -> Eithers.map (\t -> Variants.TermVariantTypeLambda) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "union", (\input -> Eithers.map (\t -> Variants.TermVariantUnion) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "unit", (\input -> Eithers.map (\t -> Variants.TermVariantUnit) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "variable", (\input -> Eithers.map (\t -> Variants.TermVariantVariable) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.TermVariantWrap) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

typeVariant :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Variants.TypeVariant
typeVariant cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "annotated", (\input -> Eithers.map (\t -> Variants.TypeVariantAnnotated) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "application", (\input -> Eithers.map (\t -> Variants.TypeVariantApplication) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "either", (\input -> Eithers.map (\t -> Variants.TypeVariantEither) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "forall", (\input -> Eithers.map (\t -> Variants.TypeVariantForall) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "function", (\input -> Eithers.map (\t -> Variants.TypeVariantFunction) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "list", (\input -> Eithers.map (\t -> Variants.TypeVariantList) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "literal", (\input -> Eithers.map (\t -> Variants.TypeVariantLiteral) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "map", (\input -> Eithers.map (\t -> Variants.TypeVariantMap) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "maybe", (\input -> Eithers.map (\t -> Variants.TypeVariantMaybe) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "pair", (\input -> Eithers.map (\t -> Variants.TypeVariantPair) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "record", (\input -> Eithers.map (\t -> Variants.TypeVariantRecord) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "set", (\input -> Eithers.map (\t -> Variants.TypeVariantSet) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "union", (\input -> Eithers.map (\t -> Variants.TypeVariantUnion) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "unit", (\input -> Eithers.map (\t -> Variants.TypeVariantUnit) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "variable", (\input -> Eithers.map (\t -> Variants.TypeVariantVariable) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "void", (\input -> Eithers.map (\t -> Variants.TypeVariantVoid) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.TypeVariantWrap) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
