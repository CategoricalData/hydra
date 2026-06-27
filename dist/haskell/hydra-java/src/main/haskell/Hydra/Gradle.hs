-- Note: this is an automatically generated file. Do not edit.
-- | Build configuration for Gradle-built distribution packages.

module Hydra.Gradle where
import qualified Hydra.Core as Core
import qualified Hydra.File as File
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | The build configuration for a single Gradle-built distribution package, beyond its generated sources.
data GradleBuildConfiguration =
  GradleBuildConfiguration {
    -- | Third-party (non-project) dependencies required by the package. Inter-package dependencies within the project are derived separately and are not listed here. Each dependency's scope (api/runtime/test/tool) is significant; its name carries the Maven group and artifact separated by a colon.
    gradleBuildConfigurationDependencies :: [Packaging.PackageDependency],
    -- | Source paths or patterns to exclude from compilation, relative to the package root.
    gradleBuildConfigurationExcludes :: [String],
    -- | Additional source directories to fold into the package's main source set, beyond the generated and overlaid sources (e.g. a directory of build-tool-generated sources).
    gradleBuildConfigurationExtraSourceDirs :: [File.FilePath],
    -- | Gradle plugin identifiers to apply to the build, e.g. "antlr". Applied in the given order.
    gradleBuildConfigurationPlugins :: [String],
    -- | An optional fragment of host-native build configuration (Groovy) spliced verbatim into the generated build script, for imperative needs that the structured fields above cannot express (e.g. configuring a code-generation task). Opaque to Hydra; preferred only when no structured field applies.
    gradleBuildConfigurationRaw :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)
_GradleBuildConfiguration = Core.Name "hydra.gradle.GradleBuildConfiguration"
_GradleBuildConfiguration_dependencies = Core.Name "dependencies"
_GradleBuildConfiguration_excludes = Core.Name "excludes"
_GradleBuildConfiguration_extraSourceDirs = Core.Name "extraSourceDirs"
_GradleBuildConfiguration_plugins = Core.Name "plugins"
_GradleBuildConfiguration_raw = Core.Name "raw"
