-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.variants

module Hydra.Encode.Variants where

import qualified Hydra.Core as Core
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

eliminationVariant :: (Variants.EliminationVariant -> Core.Term)
eliminationVariant x = case x of
  Variants.EliminationVariantRecord -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "record"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.EliminationVariantUnion -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.EliminationVariantWrap -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "wrap"),
      Core.fieldTerm = Core.TermUnit}}))

functionVariant :: (Variants.FunctionVariant -> Core.Term)
functionVariant x = case x of
  Variants.FunctionVariantElimination -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.FunctionVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "elimination"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.FunctionVariantLambda -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.FunctionVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "lambda"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.FunctionVariantPrimitive -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.FunctionVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "primitive"),
      Core.fieldTerm = Core.TermUnit}}))

literalVariant :: (Variants.LiteralVariant -> Core.Term)
literalVariant x = case x of
  Variants.LiteralVariantBinary -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "binary"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.LiteralVariantBoolean -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "boolean"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.LiteralVariantFloat -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "float"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.LiteralVariantInteger -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "integer"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.LiteralVariantString -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "string"),
      Core.fieldTerm = Core.TermUnit}}))

termVariant :: (Variants.TermVariant -> Core.Term)
termVariant x = case x of
  Variants.TermVariantAnnotated -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "annotated"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantApplication -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "application"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantEither -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "either"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantFunction -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantLet -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "let"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantList -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "list"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantLiteral -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "literal"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantMap -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "map"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantMaybe -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "maybe"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantPair -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "pair"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantRecord -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "record"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantSet -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "set"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantTypeApplication -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeApplication"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantTypeLambda -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeLambda"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantUnion -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantUnit -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "unit"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantVariable -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "variable"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TermVariantWrap -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "wrap"),
      Core.fieldTerm = Core.TermUnit}}))

typeVariant :: (Variants.TypeVariant -> Core.Term)
typeVariant x = case x of
  Variants.TypeVariantAnnotated -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "annotated"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantApplication -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "application"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantEither -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "either"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantForall -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "forall"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantFunction -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantList -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "list"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantLiteral -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "literal"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantMap -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "map"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantMaybe -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "maybe"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantPair -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "pair"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantRecord -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "record"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantSet -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "set"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantUnion -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantUnit -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "unit"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantVariable -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "variable"),
      Core.fieldTerm = Core.TermUnit}}))
  Variants.TypeVariantWrap -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "wrap"),
      Core.fieldTerm = Core.TermUnit}}))
