-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.variants

module Hydra.Decode.Variants where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
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
                (Core.Name "record", (\input -> Eithers.map (\t -> Variants.EliminationVariantRecord) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "union", (\input -> Eithers.map (\t -> Variants.EliminationVariantUnion) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.EliminationVariantWrap) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
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
                (Core.Name "elimination", (\input -> Eithers.map (\t -> Variants.FunctionVariantElimination) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "lambda", (\input -> Eithers.map (\t -> Variants.FunctionVariantLambda) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "primitive", (\input -> Eithers.map (\t -> Variants.FunctionVariantPrimitive) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
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
                (Core.Name "binary", (\input -> Eithers.map (\t -> Variants.LiteralVariantBinary) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "boolean", (\input -> Eithers.map (\t -> Variants.LiteralVariantBoolean) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "float", (\input -> Eithers.map (\t -> Variants.LiteralVariantFloat) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "integer", (\input -> Eithers.map (\t -> Variants.LiteralVariantInteger) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "string", (\input -> Eithers.map (\t -> Variants.LiteralVariantString) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
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
                (Core.Name "annotated", (\input -> Eithers.map (\t -> Variants.TermVariantAnnotated) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "application", (\input -> Eithers.map (\t -> Variants.TermVariantApplication) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "either", (\input -> Eithers.map (\t -> Variants.TermVariantEither) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "function", (\input -> Eithers.map (\t -> Variants.TermVariantFunction) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "let", (\input -> Eithers.map (\t -> Variants.TermVariantLet) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "list", (\input -> Eithers.map (\t -> Variants.TermVariantList) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "literal", (\input -> Eithers.map (\t -> Variants.TermVariantLiteral) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "map", (\input -> Eithers.map (\t -> Variants.TermVariantMap) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "maybe", (\input -> Eithers.map (\t -> Variants.TermVariantMaybe) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "pair", (\input -> Eithers.map (\t -> Variants.TermVariantPair) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "record", (\input -> Eithers.map (\t -> Variants.TermVariantRecord) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "set", (\input -> Eithers.map (\t -> Variants.TermVariantSet) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "typeApplication", (\input -> Eithers.map (\t -> Variants.TermVariantTypeApplication) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "typeLambda", (\input -> Eithers.map (\t -> Variants.TermVariantTypeLambda) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "union", (\input -> Eithers.map (\t -> Variants.TermVariantUnion) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "unit", (\input -> Eithers.map (\t -> Variants.TermVariantUnit) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "variable", (\input -> Eithers.map (\t -> Variants.TermVariantVariable) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.TermVariantWrap) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
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
                (Core.Name "annotated", (\input -> Eithers.map (\t -> Variants.TypeVariantAnnotated) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "application", (\input -> Eithers.map (\t -> Variants.TypeVariantApplication) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "either", (\input -> Eithers.map (\t -> Variants.TypeVariantEither) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "forall", (\input -> Eithers.map (\t -> Variants.TypeVariantForall) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "function", (\input -> Eithers.map (\t -> Variants.TypeVariantFunction) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "list", (\input -> Eithers.map (\t -> Variants.TypeVariantList) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "literal", (\input -> Eithers.map (\t -> Variants.TypeVariantLiteral) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "map", (\input -> Eithers.map (\t -> Variants.TypeVariantMap) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "maybe", (\input -> Eithers.map (\t -> Variants.TypeVariantMaybe) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "pair", (\input -> Eithers.map (\t -> Variants.TypeVariantPair) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "record", (\input -> Eithers.map (\t -> Variants.TypeVariantRecord) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "set", (\input -> Eithers.map (\t -> Variants.TypeVariantSet) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "union", (\input -> Eithers.map (\t -> Variants.TypeVariantUnion) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "unit", (\input -> Eithers.map (\t -> Variants.TypeVariantUnit) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "variable", (\input -> Eithers.map (\t -> Variants.TypeVariantVariable) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "wrap", (\input -> Eithers.map (\t -> Variants.TypeVariantWrap) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.variants.TypeVariant"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
