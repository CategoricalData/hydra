-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.variants

module Hydra.Dsl.Variants where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

eliminationVariantRecord :: Phantoms.TTerm Variants.EliminationVariant
eliminationVariantRecord =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))

eliminationVariantUnion :: Phantoms.TTerm Variants.EliminationVariant
eliminationVariantUnion =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = Core.TermUnit}}))

eliminationVariantWrap :: Phantoms.TTerm Variants.EliminationVariant
eliminationVariantWrap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = Core.TermUnit}}))

functionVariantElimination :: Phantoms.TTerm Variants.FunctionVariant
functionVariantElimination =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.FunctionVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elimination"),
        Core.fieldTerm = Core.TermUnit}}))

functionVariantLambda :: Phantoms.TTerm Variants.FunctionVariant
functionVariantLambda =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.FunctionVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = Core.TermUnit}}))

literalVariantBinary :: Phantoms.TTerm Variants.LiteralVariant
literalVariantBinary =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = Core.TermUnit}}))

literalVariantBoolean :: Phantoms.TTerm Variants.LiteralVariant
literalVariantBoolean =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))

literalVariantDecimal :: Phantoms.TTerm Variants.LiteralVariant
literalVariantDecimal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = Core.TermUnit}}))

literalVariantFloat :: Phantoms.TTerm Variants.LiteralVariant
literalVariantFloat =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))

literalVariantInteger :: Phantoms.TTerm Variants.LiteralVariant
literalVariantInteger =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = Core.TermUnit}}))

literalVariantString :: Phantoms.TTerm Variants.LiteralVariant
literalVariantString =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantAnnotated :: Phantoms.TTerm Variants.TermVariant
termVariantAnnotated =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantApplication :: Phantoms.TTerm Variants.TermVariant
termVariantApplication =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantCases :: Phantoms.TTerm Variants.TermVariant
termVariantCases =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cases"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantEither :: Phantoms.TTerm Variants.TermVariant
termVariantEither =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantInject :: Phantoms.TTerm Variants.TermVariant
termVariantInject =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inject"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantLambda :: Phantoms.TTerm Variants.TermVariant
termVariantLambda =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantLet :: Phantoms.TTerm Variants.TermVariant
termVariantLet =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantList :: Phantoms.TTerm Variants.TermVariant
termVariantList =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantLiteral :: Phantoms.TTerm Variants.TermVariant
termVariantLiteral =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantMap :: Phantoms.TTerm Variants.TermVariant
termVariantMap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantMaybe :: Phantoms.TTerm Variants.TermVariant
termVariantMaybe =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybe"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantPair :: Phantoms.TTerm Variants.TermVariant
termVariantPair =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantProject :: Phantoms.TTerm Variants.TermVariant
termVariantProject =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantRecord :: Phantoms.TTerm Variants.TermVariant
termVariantRecord =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantSet :: Phantoms.TTerm Variants.TermVariant
termVariantSet =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantTypeApplication :: Phantoms.TTerm Variants.TermVariant
termVariantTypeApplication =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeApplication"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantTypeLambda :: Phantoms.TTerm Variants.TermVariant
termVariantTypeLambda =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLambda"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantUnit :: Phantoms.TTerm Variants.TermVariant
termVariantUnit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantUnwrap :: Phantoms.TTerm Variants.TermVariant
termVariantUnwrap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unwrap"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantVariable :: Phantoms.TTerm Variants.TermVariant
termVariantVariable =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = Core.TermUnit}}))

termVariantWrap :: Phantoms.TTerm Variants.TermVariant
termVariantWrap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantAnnotated :: Phantoms.TTerm Variants.TypeVariant
typeVariantAnnotated =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantApplication :: Phantoms.TTerm Variants.TypeVariant
typeVariantApplication =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantEither :: Phantoms.TTerm Variants.TypeVariant
typeVariantEither =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantForall :: Phantoms.TTerm Variants.TypeVariant
typeVariantForall =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forall"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantFunction :: Phantoms.TTerm Variants.TypeVariant
typeVariantFunction =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantList :: Phantoms.TTerm Variants.TypeVariant
typeVariantList =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantLiteral :: Phantoms.TTerm Variants.TypeVariant
typeVariantLiteral =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantMap :: Phantoms.TTerm Variants.TypeVariant
typeVariantMap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantMaybe :: Phantoms.TTerm Variants.TypeVariant
typeVariantMaybe =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybe"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantPair :: Phantoms.TTerm Variants.TypeVariant
typeVariantPair =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantRecord :: Phantoms.TTerm Variants.TypeVariant
typeVariantRecord =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantSet :: Phantoms.TTerm Variants.TypeVariant
typeVariantSet =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantUnion :: Phantoms.TTerm Variants.TypeVariant
typeVariantUnion =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantUnit :: Phantoms.TTerm Variants.TypeVariant
typeVariantUnit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantVariable :: Phantoms.TTerm Variants.TypeVariant
typeVariantVariable =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantVoid :: Phantoms.TTerm Variants.TypeVariant
typeVariantVoid =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariantWrap :: Phantoms.TTerm Variants.TypeVariant
typeVariantWrap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = Core.TermUnit}}))
