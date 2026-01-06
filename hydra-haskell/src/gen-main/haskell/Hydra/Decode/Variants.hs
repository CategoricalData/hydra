-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.variants

module Hydra.Decode.Variants where

import qualified Hydra.Core as Core
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

eliminationVariant :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Variants.EliminationVariant)
eliminationVariant cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "record", (\input -> Eithers.map (\t -> Variants.EliminationVariantRecord) (Helpers.decodeUnit cx input))),
                (Core.Name "union", (\input -> Eithers.map (\t -> Variants.EliminationVariantUnion) (Helpers.decodeUnit cx input))),
                (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.EliminationVariantWrap) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.variants.EliminationVariant"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

functionVariant :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Variants.FunctionVariant)
functionVariant cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "elimination", (\input -> Eithers.map (\t -> Variants.FunctionVariantElimination) (Helpers.decodeUnit cx input))),
                (Core.Name "lambda", (\input -> Eithers.map (\t -> Variants.FunctionVariantLambda) (Helpers.decodeUnit cx input))),
                (Core.Name "primitive", (\input -> Eithers.map (\t -> Variants.FunctionVariantPrimitive) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.variants.FunctionVariant"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

literalVariant :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Variants.LiteralVariant)
literalVariant cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "binary", (\input -> Eithers.map (\t -> Variants.LiteralVariantBinary) (Helpers.decodeUnit cx input))),
                (Core.Name "boolean", (\input -> Eithers.map (\t -> Variants.LiteralVariantBoolean) (Helpers.decodeUnit cx input))),
                (Core.Name "float", (\input -> Eithers.map (\t -> Variants.LiteralVariantFloat) (Helpers.decodeUnit cx input))),
                (Core.Name "integer", (\input -> Eithers.map (\t -> Variants.LiteralVariantInteger) (Helpers.decodeUnit cx input))),
                (Core.Name "string", (\input -> Eithers.map (\t -> Variants.LiteralVariantString) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.variants.LiteralVariant"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

termVariant :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Variants.TermVariant)
termVariant cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "annotated", (\input -> Eithers.map (\t -> Variants.TermVariantAnnotated) (Helpers.decodeUnit cx input))),
                (Core.Name "application", (\input -> Eithers.map (\t -> Variants.TermVariantApplication) (Helpers.decodeUnit cx input))),
                (Core.Name "either", (\input -> Eithers.map (\t -> Variants.TermVariantEither) (Helpers.decodeUnit cx input))),
                (Core.Name "function", (\input -> Eithers.map (\t -> Variants.TermVariantFunction) (Helpers.decodeUnit cx input))),
                (Core.Name "let", (\input -> Eithers.map (\t -> Variants.TermVariantLet) (Helpers.decodeUnit cx input))),
                (Core.Name "list", (\input -> Eithers.map (\t -> Variants.TermVariantList) (Helpers.decodeUnit cx input))),
                (Core.Name "literal", (\input -> Eithers.map (\t -> Variants.TermVariantLiteral) (Helpers.decodeUnit cx input))),
                (Core.Name "map", (\input -> Eithers.map (\t -> Variants.TermVariantMap) (Helpers.decodeUnit cx input))),
                (Core.Name "maybe", (\input -> Eithers.map (\t -> Variants.TermVariantMaybe) (Helpers.decodeUnit cx input))),
                (Core.Name "pair", (\input -> Eithers.map (\t -> Variants.TermVariantPair) (Helpers.decodeUnit cx input))),
                (Core.Name "record", (\input -> Eithers.map (\t -> Variants.TermVariantRecord) (Helpers.decodeUnit cx input))),
                (Core.Name "set", (\input -> Eithers.map (\t -> Variants.TermVariantSet) (Helpers.decodeUnit cx input))),
                (Core.Name "typeApplication", (\input -> Eithers.map (\t -> Variants.TermVariantTypeApplication) (Helpers.decodeUnit cx input))),
                (Core.Name "typeLambda", (\input -> Eithers.map (\t -> Variants.TermVariantTypeLambda) (Helpers.decodeUnit cx input))),
                (Core.Name "union", (\input -> Eithers.map (\t -> Variants.TermVariantUnion) (Helpers.decodeUnit cx input))),
                (Core.Name "unit", (\input -> Eithers.map (\t -> Variants.TermVariantUnit) (Helpers.decodeUnit cx input))),
                (Core.Name "variable", (\input -> Eithers.map (\t -> Variants.TermVariantVariable) (Helpers.decodeUnit cx input))),
                (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.TermVariantWrap) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.variants.TermVariant"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeVariant :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Variants.TypeVariant)
typeVariant cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "annotated", (\input -> Eithers.map (\t -> Variants.TypeVariantAnnotated) (Helpers.decodeUnit cx input))),
                (Core.Name "application", (\input -> Eithers.map (\t -> Variants.TypeVariantApplication) (Helpers.decodeUnit cx input))),
                (Core.Name "either", (\input -> Eithers.map (\t -> Variants.TypeVariantEither) (Helpers.decodeUnit cx input))),
                (Core.Name "forall", (\input -> Eithers.map (\t -> Variants.TypeVariantForall) (Helpers.decodeUnit cx input))),
                (Core.Name "function", (\input -> Eithers.map (\t -> Variants.TypeVariantFunction) (Helpers.decodeUnit cx input))),
                (Core.Name "list", (\input -> Eithers.map (\t -> Variants.TypeVariantList) (Helpers.decodeUnit cx input))),
                (Core.Name "literal", (\input -> Eithers.map (\t -> Variants.TypeVariantLiteral) (Helpers.decodeUnit cx input))),
                (Core.Name "map", (\input -> Eithers.map (\t -> Variants.TypeVariantMap) (Helpers.decodeUnit cx input))),
                (Core.Name "maybe", (\input -> Eithers.map (\t -> Variants.TypeVariantMaybe) (Helpers.decodeUnit cx input))),
                (Core.Name "pair", (\input -> Eithers.map (\t -> Variants.TypeVariantPair) (Helpers.decodeUnit cx input))),
                (Core.Name "record", (\input -> Eithers.map (\t -> Variants.TypeVariantRecord) (Helpers.decodeUnit cx input))),
                (Core.Name "set", (\input -> Eithers.map (\t -> Variants.TypeVariantSet) (Helpers.decodeUnit cx input))),
                (Core.Name "union", (\input -> Eithers.map (\t -> Variants.TypeVariantUnion) (Helpers.decodeUnit cx input))),
                (Core.Name "unit", (\input -> Eithers.map (\t -> Variants.TypeVariantUnit) (Helpers.decodeUnit cx input))),
                (Core.Name "variable", (\input -> Eithers.map (\t -> Variants.TypeVariantVariable) (Helpers.decodeUnit cx input))),
                (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.TypeVariantWrap) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.variants.TypeVariant"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
