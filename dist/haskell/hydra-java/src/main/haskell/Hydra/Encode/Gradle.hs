-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.gradle

module Hydra.Encode.Gradle where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.File as File
import qualified Hydra.Encode.Packaging as Packaging
import qualified Hydra.Gradle as Gradle
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.gradle.GradleBuildConfiguration
gradleBuildConfiguration :: Gradle.GradleBuildConfiguration -> Core.Term
gradleBuildConfiguration x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.gradle.GradleBuildConfiguration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map Packaging.packageDependency xs)) (Gradle.gradleBuildConfigurationDependencies x))},
        Core.Field {
          Core.fieldName = (Core.Name "excludes"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) xs)) (Gradle.gradleBuildConfigurationExcludes x))},
        Core.Field {
          Core.fieldName = (Core.Name "extraSourceDirs"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map File.filePath xs)) (Gradle.gradleBuildConfigurationExtraSourceDirs x))},
        Core.Field {
          Core.fieldName = (Core.Name "plugins"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) xs)) (Gradle.gradleBuildConfigurationPlugins x))},
        Core.Field {
          Core.fieldName = (Core.Name "raw"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Gradle.gradleBuildConfigurationRaw x))}]})
