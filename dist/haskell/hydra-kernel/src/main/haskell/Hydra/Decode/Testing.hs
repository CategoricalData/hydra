-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.testing

module Hydra.Decode.Testing where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

tag :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Testing.Tag
tag cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Testing.Tag b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)

testCase :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Testing.TestCase
testCase cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "universal", (\input -> Eithers.map (\t -> Testing.TestCaseUniversal t) (universalTestCase cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

testCaseWithMetadata :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Testing.TestCaseWithMetadata
testCaseWithMetadata cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "case" testCase fieldMap cx) (\field_case -> Eithers.bind (ExtractCore.requireField "description" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_description -> Eithers.bind (ExtractCore.requireField "tags" (ExtractCore.decodeList tag) fieldMap cx) (\field_tags -> Right (Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = field_name,
          Testing.testCaseWithMetadataCase = field_case,
          Testing.testCaseWithMetadataDescription = field_description,
          Testing.testCaseWithMetadataTags = field_tags}))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

testGroup :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Testing.TestGroup
testGroup cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "description" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_description -> Eithers.bind (ExtractCore.requireField "subgroups" (ExtractCore.decodeList testGroup) fieldMap cx) (\field_subgroups -> Eithers.bind (ExtractCore.requireField "cases" (ExtractCore.decodeList testCaseWithMetadata) fieldMap cx) (\field_cases -> Right (Testing.TestGroup {
          Testing.testGroupName = field_name,
          Testing.testGroupDescription = field_description,
          Testing.testGroupSubgroups = field_subgroups,
          Testing.testGroupCases = field_cases}))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

universalTestCase :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Testing.UniversalTestCase
universalTestCase cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "actual" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_actual -> Eithers.bind (ExtractCore.requireField "expected" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_expected -> Right (Testing.UniversalTestCase {
          Testing.universalTestCaseActual = field_actual,
          Testing.universalTestCaseExpected = field_expected}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
