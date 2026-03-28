-- Note: this is an automatically generated file. Do not edit.

-- | A model for Hydra packages

module Hydra.Packaging where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A package, which is a named collection of modules with metadata and dependencies
data Package =
  Package {
    -- | The name of the package
    packageName :: PackageName,
    -- | The modules in this package
    packageModules :: [Module.Module],
    -- | The packages which this package depends on
    packageDependencies :: [PackageName],
    -- | An optional human-readable description of the package
    packageDescription :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Package = Core.Name "hydra.packaging.Package"

_Package_name = Core.Name "name"

_Package_modules = Core.Name "modules"

_Package_dependencies = Core.Name "dependencies"

_Package_description = Core.Name "description"

-- | The unique name of a package, e.g. "hydra-kernel" or "hydra-python"
newtype PackageName =
  PackageName {
    unPackageName :: String}
  deriving (Eq, Ord, Read, Show)

_PackageName = Core.Name "hydra.packaging.PackageName"
