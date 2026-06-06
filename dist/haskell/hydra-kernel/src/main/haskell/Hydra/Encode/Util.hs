-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.util

module Hydra.Encode.Util where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Packaging as Packaging
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.util.CaseConvention
caseConvention :: Util.CaseConvention -> Core.Term
caseConvention x =
    case x of
      Util.CaseConventionCamel -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "camel"),
          Core.fieldTerm = Core.TermUnit}})
      Util.CaseConventionPascal -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "pascal"),
          Core.fieldTerm = Core.TermUnit}})
      Util.CaseConventionLowerSnake -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "lowerSnake"),
          Core.fieldTerm = Core.TermUnit}})
      Util.CaseConventionUpperSnake -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "upperSnake"),
          Core.fieldTerm = Core.TermUnit}})
-- | Encoder for hydra.util.Comparison
comparison :: Util.Comparison -> Core.Term
comparison x =
    case x of
      Util.ComparisonLessThan -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "lessThan"),
          Core.fieldTerm = Core.TermUnit}})
      Util.ComparisonEqualTo -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "equalTo"),
          Core.fieldTerm = Core.TermUnit}})
      Util.ComparisonGreaterThan -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "greaterThan"),
          Core.fieldTerm = Core.TermUnit}})
-- | Encoder for hydra.util.FileExtension
fileExtension :: Util.FileExtension -> Core.Term
fileExtension x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.util.FileExtension"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Util.unFileExtension x))})
-- | Encoder for hydra.util.ModuleNames
moduleNames :: (t0 -> Core.Term) -> Util.ModuleNames t0 -> Core.Term
moduleNames n x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.ModuleNames"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "focus"),
          Core.fieldTerm = ((\p -> Core.TermPair (Pairs.bimap Packaging.moduleName n p)) (Util.moduleNamesFocus x))},
        Core.Field {
          Core.fieldName = (Core.Name "mapping"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap Packaging.moduleName n m)) (Util.moduleNamesMapping x))}]})
-- | Encoder for hydra.util.Precision
precision :: Util.Precision -> Core.Term
precision x =
    case x of
      Util.PrecisionArbitrary -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "arbitrary"),
          Core.fieldTerm = Core.TermUnit}})
      Util.PrecisionBits v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "bits"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v0)))}})
-- | Encoder for hydra.util.QualifiedName
qualifiedName :: Util.QualifiedName -> Core.Term
qualifiedName x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.util.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map Packaging.moduleName opt)) (Util.qualifiedNameModuleName x))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Util.qualifiedNameLocal x))}]})
