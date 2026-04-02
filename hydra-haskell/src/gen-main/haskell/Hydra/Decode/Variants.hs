-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.variants

module Hydra.Decode.Variants where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

eliminationVariant :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Variants.EliminationVariant
eliminationVariant cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "record", (\input -> Eithers.map (\t -> Variants.EliminationVariantRecord) (Core_.decodeUnit cx input))),
                      (Core.Name "union", (\input -> Eithers.map (\t -> Variants.EliminationVariantUnion) (Core_.decodeUnit cx input))),
                      (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.EliminationVariantWrap) (Core_.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

functionVariant :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Variants.FunctionVariant
functionVariant cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "elimination", (\input -> Eithers.map (\t -> Variants.FunctionVariantElimination) (Core_.decodeUnit cx input))),
                      (Core.Name "lambda", (\input -> Eithers.map (\t -> Variants.FunctionVariantLambda) (Core_.decodeUnit cx input))),
                      (Core.Name "primitive", (\input -> Eithers.map (\t -> Variants.FunctionVariantPrimitive) (Core_.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

literalVariant :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Variants.LiteralVariant
literalVariant cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "binary", (\input -> Eithers.map (\t -> Variants.LiteralVariantBinary) (Core_.decodeUnit cx input))),
                      (Core.Name "boolean", (\input -> Eithers.map (\t -> Variants.LiteralVariantBoolean) (Core_.decodeUnit cx input))),
                      (Core.Name "float", (\input -> Eithers.map (\t -> Variants.LiteralVariantFloat) (Core_.decodeUnit cx input))),
                      (Core.Name "integer", (\input -> Eithers.map (\t -> Variants.LiteralVariantInteger) (Core_.decodeUnit cx input))),
                      (Core.Name "string", (\input -> Eithers.map (\t -> Variants.LiteralVariantString) (Core_.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

termVariant :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Variants.TermVariant
termVariant cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "annotated", (\input -> Eithers.map (\t -> Variants.TermVariantAnnotated) (Core_.decodeUnit cx input))),
                      (Core.Name "application", (\input -> Eithers.map (\t -> Variants.TermVariantApplication) (Core_.decodeUnit cx input))),
                      (Core.Name "either", (\input -> Eithers.map (\t -> Variants.TermVariantEither) (Core_.decodeUnit cx input))),
                      (Core.Name "function", (\input -> Eithers.map (\t -> Variants.TermVariantFunction) (Core_.decodeUnit cx input))),
                      (Core.Name "let", (\input -> Eithers.map (\t -> Variants.TermVariantLet) (Core_.decodeUnit cx input))),
                      (Core.Name "list", (\input -> Eithers.map (\t -> Variants.TermVariantList) (Core_.decodeUnit cx input))),
                      (Core.Name "literal", (\input -> Eithers.map (\t -> Variants.TermVariantLiteral) (Core_.decodeUnit cx input))),
                      (Core.Name "map", (\input -> Eithers.map (\t -> Variants.TermVariantMap) (Core_.decodeUnit cx input))),
                      (Core.Name "maybe", (\input -> Eithers.map (\t -> Variants.TermVariantMaybe) (Core_.decodeUnit cx input))),
                      (Core.Name "pair", (\input -> Eithers.map (\t -> Variants.TermVariantPair) (Core_.decodeUnit cx input))),
                      (Core.Name "record", (\input -> Eithers.map (\t -> Variants.TermVariantRecord) (Core_.decodeUnit cx input))),
                      (Core.Name "set", (\input -> Eithers.map (\t -> Variants.TermVariantSet) (Core_.decodeUnit cx input))),
                      (Core.Name "typeApplication", (\input -> Eithers.map (\t -> Variants.TermVariantTypeApplication) (Core_.decodeUnit cx input))),
                      (Core.Name "typeLambda", (\input -> Eithers.map (\t -> Variants.TermVariantTypeLambda) (Core_.decodeUnit cx input))),
                      (Core.Name "union", (\input -> Eithers.map (\t -> Variants.TermVariantUnion) (Core_.decodeUnit cx input))),
                      (Core.Name "unit", (\input -> Eithers.map (\t -> Variants.TermVariantUnit) (Core_.decodeUnit cx input))),
                      (Core.Name "variable", (\input -> Eithers.map (\t -> Variants.TermVariantVariable) (Core_.decodeUnit cx input))),
                      (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.TermVariantWrap) (Core_.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

typeVariant :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Variants.TypeVariant
typeVariant cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "annotated", (\input -> Eithers.map (\t -> Variants.TypeVariantAnnotated) (Core_.decodeUnit cx input))),
                      (Core.Name "application", (\input -> Eithers.map (\t -> Variants.TypeVariantApplication) (Core_.decodeUnit cx input))),
                      (Core.Name "either", (\input -> Eithers.map (\t -> Variants.TypeVariantEither) (Core_.decodeUnit cx input))),
                      (Core.Name "forall", (\input -> Eithers.map (\t -> Variants.TypeVariantForall) (Core_.decodeUnit cx input))),
                      (Core.Name "function", (\input -> Eithers.map (\t -> Variants.TypeVariantFunction) (Core_.decodeUnit cx input))),
                      (Core.Name "list", (\input -> Eithers.map (\t -> Variants.TypeVariantList) (Core_.decodeUnit cx input))),
                      (Core.Name "literal", (\input -> Eithers.map (\t -> Variants.TypeVariantLiteral) (Core_.decodeUnit cx input))),
                      (Core.Name "map", (\input -> Eithers.map (\t -> Variants.TypeVariantMap) (Core_.decodeUnit cx input))),
                      (Core.Name "maybe", (\input -> Eithers.map (\t -> Variants.TypeVariantMaybe) (Core_.decodeUnit cx input))),
                      (Core.Name "pair", (\input -> Eithers.map (\t -> Variants.TypeVariantPair) (Core_.decodeUnit cx input))),
                      (Core.Name "record", (\input -> Eithers.map (\t -> Variants.TypeVariantRecord) (Core_.decodeUnit cx input))),
                      (Core.Name "set", (\input -> Eithers.map (\t -> Variants.TypeVariantSet) (Core_.decodeUnit cx input))),
                      (Core.Name "union", (\input -> Eithers.map (\t -> Variants.TypeVariantUnion) (Core_.decodeUnit cx input))),
                      (Core.Name "unit", (\input -> Eithers.map (\t -> Variants.TypeVariantUnit) (Core_.decodeUnit cx input))),
                      (Core.Name "variable", (\input -> Eithers.map (\t -> Variants.TypeVariantVariable) (Core_.decodeUnit cx input))),
                      (Core.Name "void", (\input -> Eithers.map (\t -> Variants.TypeVariantVoid) (Core_.decodeUnit cx input))),
                      (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.TypeVariantWrap) (Core_.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)
