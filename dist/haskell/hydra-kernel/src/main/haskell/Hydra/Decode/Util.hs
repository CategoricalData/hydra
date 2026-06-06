-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.util

module Hydra.Decode.Util where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.Packaging as Packaging
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
-- | Decoder for hydra.util.CaseConvention
caseConvention :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Util.CaseConvention
caseConvention cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "camel", (\input -> Eithers.map (\t -> Util.CaseConventionCamel) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "pascal", (\input -> Eithers.map (\t -> Util.CaseConventionPascal) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "lowerSnake", (\input -> Eithers.map (\t -> Util.CaseConventionLowerSnake) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "upperSnake", (\input -> Eithers.map (\t -> Util.CaseConventionUpperSnake) (ExtractCore.decodeUnit cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.util.Comparison
comparison :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Util.Comparison
comparison cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "lessThan", (\input -> Eithers.map (\t -> Util.ComparisonLessThan) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "equalTo", (\input -> Eithers.map (\t -> Util.ComparisonEqualTo) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "greaterThan", (\input -> Eithers.map (\t -> Util.ComparisonGreaterThan) (ExtractCore.decodeUnit cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.util.FileExtension
fileExtension :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Util.FileExtension
fileExtension cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Util.FileExtension b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.util.ModuleNames
moduleNames :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Util.ModuleNames t0)
moduleNames n cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "focus" (ExtractCore.decodePair Packaging.moduleName n) fieldMap cx) (\field_focus -> Eithers.bind (ExtractCore.requireField "mapping" (ExtractCore.decodeMap Packaging.moduleName n) fieldMap cx) (\field_mapping -> Right (Util.ModuleNames {
          Util.moduleNamesFocus = field_focus,
          Util.moduleNamesMapping = field_mapping}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.util.ModuleNames")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.util.Precision
precision :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Util.Precision
precision cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "arbitrary", (\input -> Eithers.map (\t -> Util.PrecisionArbitrary) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "bits",
                        (\input -> Eithers.map (\t -> Util.PrecisionBits t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueInt32 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected int32 value")
                            _ -> Left (Errors.DecodingError "expected int32 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input))))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.util.QualifiedName
qualifiedName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Util.QualifiedName
qualifiedName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "moduleName" (ExtractCore.decodeMaybe Packaging.moduleName) fieldMap cx) (\field_moduleName -> Eithers.bind (ExtractCore.requireField "local" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_local -> Right (Util.QualifiedName {
          Util.qualifiedNameModuleName = field_moduleName,
          Util.qualifiedNameLocal = field_local}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.util.QualifiedName")) (ExtractCore.stripWithDecodingError cx raw)
