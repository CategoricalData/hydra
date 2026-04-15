-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.variants

module Hydra.Encode.Variants where

import qualified Hydra.Core as Core
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

eliminationVariant :: Variants.EliminationVariant -> Core.Term
eliminationVariant x =
    case x of
      Variants.EliminationVariantRecord -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "record"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.EliminationVariantUnion -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "union"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.EliminationVariantWrap -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "wrap"),
          Core.fieldTerm = Core.TermUnit}})

functionVariant :: Variants.FunctionVariant -> Core.Term
functionVariant x =
    case x of
      Variants.FunctionVariantElimination -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.FunctionVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "elimination"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.FunctionVariantLambda -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.FunctionVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "lambda"),
          Core.fieldTerm = Core.TermUnit}})

literalVariant :: Variants.LiteralVariant -> Core.Term
literalVariant x =
    case x of
      Variants.LiteralVariantBinary -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "binary"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.LiteralVariantBoolean -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "boolean"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.LiteralVariantDecimal -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "decimal"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.LiteralVariantFloat -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "float"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.LiteralVariantInteger -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "integer"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.LiteralVariantString -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = Core.TermUnit}})

termVariant :: Variants.TermVariant -> Core.Term
termVariant x =
    case x of
      Variants.TermVariantAnnotated -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "annotated"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantApplication -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "application"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantCases -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantEither -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "either"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantInject -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "inject"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantLambda -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "lambda"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantLet -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "let"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantList -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "list"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantLiteral -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantMap -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantMaybe -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "maybe"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantPair -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "pair"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantProject -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "project"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantRecord -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "record"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantSet -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "set"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantTypeApplication -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typeApplication"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantTypeLambda -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typeLambda"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantUnit -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantUnwrap -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unwrap"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantVariable -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TermVariantWrap -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "wrap"),
          Core.fieldTerm = Core.TermUnit}})

typeVariant :: Variants.TypeVariant -> Core.Term
typeVariant x =
    case x of
      Variants.TypeVariantAnnotated -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "annotated"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantApplication -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "application"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantEither -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "either"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantForall -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "forall"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantFunction -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantList -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "list"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantLiteral -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantMap -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantMaybe -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "maybe"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantPair -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "pair"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantRecord -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "record"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantSet -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "set"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantUnion -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "union"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantUnit -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantVariable -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantVoid -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "void"),
          Core.fieldTerm = Core.TermUnit}})
      Variants.TypeVariantWrap -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "wrap"),
          Core.fieldTerm = Core.TermUnit}})
