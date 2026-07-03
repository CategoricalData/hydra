-- Note: this is an automatically generated file. Do not edit.

-- | Build configuration for Gradle-built distribution packages.

module Hydra.Gradle where

import qualified Hydra.Core as Core
import qualified Hydra.File as File
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Configuration for the Gradle antlr plugin's grammar generation (generateGrammarSource).
data AntlrConfig =
  AntlrConfig {
    -- | Arguments passed to the ANTLR tool, e.g. ["-visitor"] or ["-visitor", "-listener"]. Faithful to ANTLR's command-line interface; covers all ANTLR flags without enumerating them.
    antlrConfigArguments :: [String],
    -- | Directory into which ANTLR emits generated lexer/parser sources, relative to the package root (e.g. "build/generated-src/antlr/main"). The host also folds this into the main source set.
    antlrConfigOutputDirectory :: File.FilePath}
  deriving (Eq, Ord, Read, Show)

_AntlrConfig = Core.Name "hydra.gradle.AntlrConfig"

_AntlrConfig_arguments = Core.Name "arguments"

_AntlrConfig_outputDirectory = Core.Name "outputDirectory"

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
    -- | ANTLR grammar-generation configuration, present when the package uses the antlr plugin. When given, the host emits the generateGrammarSource configuration and the compileJava-depends-on-generateGrammarSource ordering. Modeled as structured vocabulary rather than a raw Groovy fragment.
    gradleBuildConfigurationAntlr :: (Maybe AntlrConfig)}
  deriving (Eq, Ord, Read, Show)

_GradleBuildConfiguration = Core.Name "hydra.gradle.GradleBuildConfiguration"

_GradleBuildConfiguration_dependencies = Core.Name "dependencies"

_GradleBuildConfiguration_excludes = Core.Name "excludes"

_GradleBuildConfiguration_extraSourceDirs = Core.Name "extraSourceDirs"

_GradleBuildConfiguration_plugins = Core.Name "plugins"

_GradleBuildConfiguration_antlr = Core.Name "antlr"
