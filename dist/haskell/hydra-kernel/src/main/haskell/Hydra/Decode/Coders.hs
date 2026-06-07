-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.coders

module Hydra.Decode.Coders where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.Errors as DecodeErrors
import qualified Hydra.Decode.Typing as Typing
import qualified Hydra.Decode.Util as DecodeUtil
import qualified Hydra.Decode.Variants as Variants
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.coders.CaseConventions
caseConventions :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Coders.CaseConventions
caseConventions cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "constant" DecodeUtil.caseConvention fieldMap cx) (\field_constant -> Eithers.bind (ExtractCore.requireField "directory" DecodeUtil.caseConvention fieldMap cx) (\field_directory -> Eithers.bind (ExtractCore.requireField "enumValue" DecodeUtil.caseConvention fieldMap cx) (\field_enumValue -> Eithers.bind (ExtractCore.requireField "field" DecodeUtil.caseConvention fieldMap cx) (\field_field -> Eithers.bind (ExtractCore.requireField "file" DecodeUtil.caseConvention fieldMap cx) (\field_file -> Eithers.bind (ExtractCore.requireField "module" DecodeUtil.caseConvention fieldMap cx) (\field_module -> Eithers.bind (ExtractCore.requireField "term" DecodeUtil.caseConvention fieldMap cx) (\field_term -> Eithers.bind (ExtractCore.requireField "termVariable" DecodeUtil.caseConvention fieldMap cx) (\field_termVariable -> Eithers.bind (ExtractCore.requireField "type" DecodeUtil.caseConvention fieldMap cx) (\field_type -> Eithers.bind (ExtractCore.requireField "typeVariable" DecodeUtil.caseConvention fieldMap cx) (\field_typeVariable -> Right (Coders.CaseConventions {
          Coders.caseConventionsConstant = field_constant,
          Coders.caseConventionsDirectory = field_directory,
          Coders.caseConventionsEnumValue = field_enumValue,
          Coders.caseConventionsField = field_field,
          Coders.caseConventionsFile = field_file,
          Coders.caseConventionsModule = field_module,
          Coders.caseConventionsTerm = field_term,
          Coders.caseConventionsTermVariable = field_termVariable,
          Coders.caseConventionsType = field_type,
          Coders.caseConventionsTypeVariable = field_typeVariable}))))))))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.coders.CaseConventions")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.coders.CoderDirection
coderDirection :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Coders.CoderDirection
coderDirection cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "encode", (\input -> Eithers.map (\t -> Coders.CoderDirectionEncode) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "decode", (\input -> Eithers.map (\t -> Coders.CoderDirectionDecode) (ExtractCore.decodeUnit cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.coders.LanguageFeature
languageFeature :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Coders.LanguageFeature
languageFeature cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (
                        Core.Name "partialApplication",
                        (\input -> Eithers.map (\t -> Coders.LanguageFeaturePartialApplication) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "nestedCaseStatements",
                        (\input -> Eithers.map (\t -> Coders.LanguageFeatureNestedCaseStatements) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "nestedPolymorphicLetBindings",
                        (\input -> Eithers.map (\t -> Coders.LanguageFeatureNestedPolymorphicLetBindings) (ExtractCore.decodeUnit cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.coders.LanguageName
languageName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Coders.LanguageName
languageName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Coders.LanguageName b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.coders.TraversalOrder
traversalOrder :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Coders.TraversalOrder
traversalOrder cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "pre", (\input -> Eithers.map (\t -> Coders.TraversalOrderPre) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "post", (\input -> Eithers.map (\t -> Coders.TraversalOrderPost) (ExtractCore.decodeUnit cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
