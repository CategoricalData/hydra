-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.coders

module Hydra.Encode.Coders where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Errors as Errors
import qualified Hydra.Encode.Typing as Typing
import qualified Hydra.Encode.Util as Util
import qualified Hydra.Encode.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.coders.CaseConventions
caseConventions :: Coders.CaseConventions -> Core.Term
caseConventions x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Util.caseConvention (Coders.caseConventionsConstant x))},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Util.caseConvention (Coders.caseConventionsDirectory x))},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Util.caseConvention (Coders.caseConventionsEnumValue x))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Util.caseConvention (Coders.caseConventionsField x))},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Util.caseConvention (Coders.caseConventionsFile x))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Util.caseConvention (Coders.caseConventionsModule x))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Util.caseConvention (Coders.caseConventionsTerm x))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Util.caseConvention (Coders.caseConventionsTermVariable x))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Util.caseConvention (Coders.caseConventionsType x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Util.caseConvention (Coders.caseConventionsTypeVariable x))}]})
-- | Encoder for hydra.coders.CoderDirection
coderDirection :: Coders.CoderDirection -> Core.Term
coderDirection x =
    case x of
      Coders.CoderDirectionEncode -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = Core.TermUnit}})
      Coders.CoderDirectionDecode -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = Core.TermUnit}})
-- | Encoder for hydra.coders.LanguageFeature
languageFeature :: Coders.LanguageFeature -> Core.Term
languageFeature x =
    case x of
      Coders.LanguageFeaturePartialApplication -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.coders.LanguageFeature"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "partialApplication"),
          Core.fieldTerm = Core.TermUnit}})
      Coders.LanguageFeatureNestedCaseStatements -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.coders.LanguageFeature"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "nestedCaseStatements"),
          Core.fieldTerm = Core.TermUnit}})
      Coders.LanguageFeatureNestedPolymorphicLetBindings -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.coders.LanguageFeature"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "nestedPolymorphicLetBindings"),
          Core.fieldTerm = Core.TermUnit}})
-- | Encoder for hydra.coders.LanguageName
languageName :: Coders.LanguageName -> Core.Term
languageName x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coders.LanguageName"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Coders.unLanguageName x))})
-- | Encoder for hydra.coders.TraversalOrder
traversalOrder :: Coders.TraversalOrder -> Core.Term
traversalOrder x =
    case x of
      Coders.TraversalOrderPre -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "pre"),
          Core.fieldTerm = Core.TermUnit}})
      Coders.TraversalOrderPost -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "post"),
          Core.fieldTerm = Core.TermUnit}})
