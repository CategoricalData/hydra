-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.variants

module Hydra.Dsl.Variants where
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the record variant of hydra.variants.EliminationVariant
eliminationVariantRecord :: Phantoms.TTerm Variants.EliminationVariant
eliminationVariantRecord =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the union variant of hydra.variants.EliminationVariant
eliminationVariantUnion :: Phantoms.TTerm Variants.EliminationVariant
eliminationVariantUnion =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the wrap variant of hydra.variants.EliminationVariant
eliminationVariantWrap :: Phantoms.TTerm Variants.EliminationVariant
eliminationVariantWrap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.EliminationVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the elimination variant of hydra.variants.FunctionVariant
functionVariantElimination :: Phantoms.TTerm Variants.FunctionVariant
functionVariantElimination =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.FunctionVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elimination"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lambda variant of hydra.variants.FunctionVariant
functionVariantLambda :: Phantoms.TTerm Variants.FunctionVariant
functionVariantLambda =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.FunctionVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the binary variant of hydra.variants.LiteralVariant
literalVariantBinary :: Phantoms.TTerm Variants.LiteralVariant
literalVariantBinary =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the boolean variant of hydra.variants.LiteralVariant
literalVariantBoolean :: Phantoms.TTerm Variants.LiteralVariant
literalVariantBoolean =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the decimal variant of hydra.variants.LiteralVariant
literalVariantDecimal :: Phantoms.TTerm Variants.LiteralVariant
literalVariantDecimal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the float variant of hydra.variants.LiteralVariant
literalVariantFloat :: Phantoms.TTerm Variants.LiteralVariant
literalVariantFloat =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the integer variant of hydra.variants.LiteralVariant
literalVariantInteger :: Phantoms.TTerm Variants.LiteralVariant
literalVariantInteger =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the string variant of hydra.variants.LiteralVariant
literalVariantString :: Phantoms.TTerm Variants.LiteralVariant
literalVariantString =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotated variant of hydra.variants.TermVariant
termVariantAnnotated :: Phantoms.TTerm Variants.TermVariant
termVariantAnnotated =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the application variant of hydra.variants.TermVariant
termVariantApplication :: Phantoms.TTerm Variants.TermVariant
termVariantApplication =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the cases variant of hydra.variants.TermVariant
termVariantCases :: Phantoms.TTerm Variants.TermVariant
termVariantCases =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cases"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the either variant of hydra.variants.TermVariant
termVariantEither :: Phantoms.TTerm Variants.TermVariant
termVariantEither =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the inject variant of hydra.variants.TermVariant
termVariantInject :: Phantoms.TTerm Variants.TermVariant
termVariantInject =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inject"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lambda variant of hydra.variants.TermVariant
termVariantLambda :: Phantoms.TTerm Variants.TermVariant
termVariantLambda =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the let variant of hydra.variants.TermVariant
termVariantLet :: Phantoms.TTerm Variants.TermVariant
termVariantLet =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the list variant of hydra.variants.TermVariant
termVariantList :: Phantoms.TTerm Variants.TermVariant
termVariantList =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the literal variant of hydra.variants.TermVariant
termVariantLiteral :: Phantoms.TTerm Variants.TermVariant
termVariantLiteral =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the map variant of hydra.variants.TermVariant
termVariantMap :: Phantoms.TTerm Variants.TermVariant
termVariantMap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the maybe variant of hydra.variants.TermVariant
termVariantMaybe :: Phantoms.TTerm Variants.TermVariant
termVariantMaybe =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybe"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pair variant of hydra.variants.TermVariant
termVariantPair :: Phantoms.TTerm Variants.TermVariant
termVariantPair =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the project variant of hydra.variants.TermVariant
termVariantProject :: Phantoms.TTerm Variants.TermVariant
termVariantProject =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the record variant of hydra.variants.TermVariant
termVariantRecord :: Phantoms.TTerm Variants.TermVariant
termVariantRecord =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the set variant of hydra.variants.TermVariant
termVariantSet :: Phantoms.TTerm Variants.TermVariant
termVariantSet =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeApplication variant of hydra.variants.TermVariant
termVariantTypeApplication :: Phantoms.TTerm Variants.TermVariant
termVariantTypeApplication =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeApplication"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeLambda variant of hydra.variants.TermVariant
termVariantTypeLambda :: Phantoms.TTerm Variants.TermVariant
termVariantTypeLambda =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLambda"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unit variant of hydra.variants.TermVariant
termVariantUnit :: Phantoms.TTerm Variants.TermVariant
termVariantUnit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unwrap variant of hydra.variants.TermVariant
termVariantUnwrap :: Phantoms.TTerm Variants.TermVariant
termVariantUnwrap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unwrap"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the variable variant of hydra.variants.TermVariant
termVariantVariable :: Phantoms.TTerm Variants.TermVariant
termVariantVariable =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the wrap variant of hydra.variants.TermVariant
termVariantWrap :: Phantoms.TTerm Variants.TermVariant
termVariantWrap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotated variant of hydra.variants.TypeVariant
typeVariantAnnotated :: Phantoms.TTerm Variants.TypeVariant
typeVariantAnnotated =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the application variant of hydra.variants.TypeVariant
typeVariantApplication :: Phantoms.TTerm Variants.TypeVariant
typeVariantApplication =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the either variant of hydra.variants.TypeVariant
typeVariantEither :: Phantoms.TTerm Variants.TypeVariant
typeVariantEither =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the forall variant of hydra.variants.TypeVariant
typeVariantForall :: Phantoms.TTerm Variants.TypeVariant
typeVariantForall =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forall"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the function variant of hydra.variants.TypeVariant
typeVariantFunction :: Phantoms.TTerm Variants.TypeVariant
typeVariantFunction =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the list variant of hydra.variants.TypeVariant
typeVariantList :: Phantoms.TTerm Variants.TypeVariant
typeVariantList =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the literal variant of hydra.variants.TypeVariant
typeVariantLiteral :: Phantoms.TTerm Variants.TypeVariant
typeVariantLiteral =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the map variant of hydra.variants.TypeVariant
typeVariantMap :: Phantoms.TTerm Variants.TypeVariant
typeVariantMap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the maybe variant of hydra.variants.TypeVariant
typeVariantMaybe :: Phantoms.TTerm Variants.TypeVariant
typeVariantMaybe =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybe"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pair variant of hydra.variants.TypeVariant
typeVariantPair :: Phantoms.TTerm Variants.TypeVariant
typeVariantPair =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the record variant of hydra.variants.TypeVariant
typeVariantRecord :: Phantoms.TTerm Variants.TypeVariant
typeVariantRecord =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the set variant of hydra.variants.TypeVariant
typeVariantSet :: Phantoms.TTerm Variants.TypeVariant
typeVariantSet =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the union variant of hydra.variants.TypeVariant
typeVariantUnion :: Phantoms.TTerm Variants.TypeVariant
typeVariantUnion =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unit variant of hydra.variants.TypeVariant
typeVariantUnit :: Phantoms.TTerm Variants.TypeVariant
typeVariantUnit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the variable variant of hydra.variants.TypeVariant
typeVariantVariable :: Phantoms.TTerm Variants.TypeVariant
typeVariantVariable =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the void variant of hydra.variants.TypeVariant
typeVariantVoid :: Phantoms.TTerm Variants.TypeVariant
typeVariantVoid =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the wrap variant of hydra.variants.TypeVariant
typeVariantWrap :: Phantoms.TTerm Variants.TypeVariant
typeVariantWrap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = Core.TermUnit}}))
