-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.gradle

module Hydra.Decode.Gradle where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.File as File
import qualified Hydra.Decode.Packaging as Packaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Gradle as Gradle
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.gradle.AntlrConfig
antlrConfig :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Gradle.AntlrConfig
antlrConfig cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "arguments" (ExtractCore.decodeList (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_arguments -> Eithers.bind (ExtractCore.requireField "outputDirectory" File.filePath fieldMap cx) (\field_outputDirectory -> Right (Gradle.AntlrConfig {
          Gradle.antlrConfigArguments = field_arguments,
          Gradle.antlrConfigOutputDirectory = field_outputDirectory}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.gradle.AntlrConfig")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.gradle.GradleBuildConfiguration
gradleBuildConfiguration :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Gradle.GradleBuildConfiguration
gradleBuildConfiguration cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "dependencies" (ExtractCore.decodeList Packaging.packageDependency) fieldMap cx) (\field_dependencies -> Eithers.bind (ExtractCore.requireField "excludes" (ExtractCore.decodeList (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_excludes -> Eithers.bind (ExtractCore.requireField "extraSourceDirs" (ExtractCore.decodeList File.filePath) fieldMap cx) (\field_extraSourceDirs -> Eithers.bind (ExtractCore.requireField "plugins" (ExtractCore.decodeList (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_plugins -> Eithers.bind (ExtractCore.requireField "antlr" (ExtractCore.decodeMaybe antlrConfig) fieldMap cx) (\field_antlr -> Right (Gradle.GradleBuildConfiguration {
          Gradle.gradleBuildConfigurationDependencies = field_dependencies,
          Gradle.gradleBuildConfigurationExcludes = field_excludes,
          Gradle.gradleBuildConfigurationExtraSourceDirs = field_extraSourceDirs,
          Gradle.gradleBuildConfigurationPlugins = field_plugins,
          Gradle.gradleBuildConfigurationAntlr = field_antlr})))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.gradle.GradleBuildConfiguration")) (ExtractCore.stripWithDecodingError cx raw)
