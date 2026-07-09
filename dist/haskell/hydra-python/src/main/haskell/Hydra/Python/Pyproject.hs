-- Note: this is an automatically generated file. Do not edit.

-- | Build configuration for PEP 621 (pyproject.toml) Python distribution packages.

module Hydra.Python.Pyproject where

import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

data PyProjectBuildConfiguration =
  PyProjectBuildConfiguration {
    pyProjectBuildConfigurationDependencies :: [Packaging.PackageDependency]}
  deriving (Eq, Ord, Read, Show)

_PyProjectBuildConfiguration = Core.Name "hydra.python.pyproject.PyProjectBuildConfiguration"

_PyProjectBuildConfiguration_dependencies = Core.Name "dependencies"
