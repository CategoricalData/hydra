-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.variants

module Hydra.Dsl.Variants where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Typed as Typed
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the binary variant of hydra.variants.LiteralVariant
literalVariantBinary :: Typed.TypedTerm Variants.LiteralVariant
literalVariantBinary =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the boolean variant of hydra.variants.LiteralVariant
literalVariantBoolean :: Typed.TypedTerm Variants.LiteralVariant
literalVariantBoolean =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the decimal variant of hydra.variants.LiteralVariant
literalVariantDecimal :: Typed.TypedTerm Variants.LiteralVariant
literalVariantDecimal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the float variant of hydra.variants.LiteralVariant
literalVariantFloat :: Typed.TypedTerm Variants.LiteralVariant
literalVariantFloat =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the integer variant of hydra.variants.LiteralVariant
literalVariantInteger :: Typed.TypedTerm Variants.LiteralVariant
literalVariantInteger =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the string variant of hydra.variants.LiteralVariant
literalVariantString :: Typed.TypedTerm Variants.LiteralVariant
literalVariantString =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.LiteralVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotated variant of hydra.variants.TermVariant
termVariantAnnotated :: Typed.TypedTerm Variants.TermVariant
termVariantAnnotated =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the application variant of hydra.variants.TermVariant
termVariantApplication :: Typed.TypedTerm Variants.TermVariant
termVariantApplication =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the cases variant of hydra.variants.TermVariant
termVariantCases :: Typed.TypedTerm Variants.TermVariant
termVariantCases =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cases"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the either variant of hydra.variants.TermVariant
termVariantEither :: Typed.TypedTerm Variants.TermVariant
termVariantEither =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the inject variant of hydra.variants.TermVariant
termVariantInject :: Typed.TypedTerm Variants.TermVariant
termVariantInject =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inject"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lambda variant of hydra.variants.TermVariant
termVariantLambda :: Typed.TypedTerm Variants.TermVariant
termVariantLambda =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the let variant of hydra.variants.TermVariant
termVariantLet :: Typed.TypedTerm Variants.TermVariant
termVariantLet =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the list variant of hydra.variants.TermVariant
termVariantList :: Typed.TypedTerm Variants.TermVariant
termVariantList =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the literal variant of hydra.variants.TermVariant
termVariantLiteral :: Typed.TypedTerm Variants.TermVariant
termVariantLiteral =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the map variant of hydra.variants.TermVariant
termVariantMap :: Typed.TypedTerm Variants.TermVariant
termVariantMap =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the optional variant of hydra.variants.TermVariant
termVariantOptional :: Typed.TypedTerm Variants.TermVariant
termVariantOptional =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pair variant of hydra.variants.TermVariant
termVariantPair :: Typed.TypedTerm Variants.TermVariant
termVariantPair =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the project variant of hydra.variants.TermVariant
termVariantProject :: Typed.TypedTerm Variants.TermVariant
termVariantProject =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the record variant of hydra.variants.TermVariant
termVariantRecord :: Typed.TypedTerm Variants.TermVariant
termVariantRecord =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the set variant of hydra.variants.TermVariant
termVariantSet :: Typed.TypedTerm Variants.TermVariant
termVariantSet =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeApplication variant of hydra.variants.TermVariant
termVariantTypeApplication :: Typed.TypedTerm Variants.TermVariant
termVariantTypeApplication =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeApplication"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeLambda variant of hydra.variants.TermVariant
termVariantTypeLambda :: Typed.TypedTerm Variants.TermVariant
termVariantTypeLambda =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLambda"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unit variant of hydra.variants.TermVariant
termVariantUnit :: Typed.TypedTerm Variants.TermVariant
termVariantUnit =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unwrap variant of hydra.variants.TermVariant
termVariantUnwrap :: Typed.TypedTerm Variants.TermVariant
termVariantUnwrap =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unwrap"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the variable variant of hydra.variants.TermVariant
termVariantVariable :: Typed.TypedTerm Variants.TermVariant
termVariantVariable =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the wrap variant of hydra.variants.TermVariant
termVariantWrap :: Typed.TypedTerm Variants.TermVariant
termVariantWrap =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TermVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotated variant of hydra.variants.TypeVariant
typeVariantAnnotated :: Typed.TypedTerm Variants.TypeVariant
typeVariantAnnotated =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the application variant of hydra.variants.TypeVariant
typeVariantApplication :: Typed.TypedTerm Variants.TypeVariant
typeVariantApplication =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the either variant of hydra.variants.TypeVariant
typeVariantEither :: Typed.TypedTerm Variants.TypeVariant
typeVariantEither =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the forall variant of hydra.variants.TypeVariant
typeVariantForall :: Typed.TypedTerm Variants.TypeVariant
typeVariantForall =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forall"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the function variant of hydra.variants.TypeVariant
typeVariantFunction :: Typed.TypedTerm Variants.TypeVariant
typeVariantFunction =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the list variant of hydra.variants.TypeVariant
typeVariantList :: Typed.TypedTerm Variants.TypeVariant
typeVariantList =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the literal variant of hydra.variants.TypeVariant
typeVariantLiteral :: Typed.TypedTerm Variants.TypeVariant
typeVariantLiteral =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the map variant of hydra.variants.TypeVariant
typeVariantMap :: Typed.TypedTerm Variants.TypeVariant
typeVariantMap =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the optional variant of hydra.variants.TypeVariant
typeVariantOptional :: Typed.TypedTerm Variants.TypeVariant
typeVariantOptional =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pair variant of hydra.variants.TypeVariant
typeVariantPair :: Typed.TypedTerm Variants.TypeVariant
typeVariantPair =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the record variant of hydra.variants.TypeVariant
typeVariantRecord :: Typed.TypedTerm Variants.TypeVariant
typeVariantRecord =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the set variant of hydra.variants.TypeVariant
typeVariantSet :: Typed.TypedTerm Variants.TypeVariant
typeVariantSet =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the union variant of hydra.variants.TypeVariant
typeVariantUnion :: Typed.TypedTerm Variants.TypeVariant
typeVariantUnion =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unit variant of hydra.variants.TypeVariant
typeVariantUnit :: Typed.TypedTerm Variants.TypeVariant
typeVariantUnit =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the variable variant of hydra.variants.TypeVariant
typeVariantVariable :: Typed.TypedTerm Variants.TypeVariant
typeVariantVariable =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the void variant of hydra.variants.TypeVariant
typeVariantVoid :: Typed.TypedTerm Variants.TypeVariant
typeVariantVoid =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the wrap variant of hydra.variants.TypeVariant
typeVariantWrap :: Typed.TypedTerm Variants.TypeVariant
typeVariantWrap =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.variants.TypeVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = Core.TermUnit}}))
