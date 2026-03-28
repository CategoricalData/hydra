module Hydra.Sources.Kernel.Types.Packaging where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Module as Module


ns :: Namespace
ns = Namespace "hydra.packaging"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns (map toTypeDef elements) [Module.ns] [] $
    Just "A model for Hydra packages"
  where
    elements = [
      package,
      packageName]

package :: Binding
package = define "Package" $
  doc "A package, which is a named collection of modules with metadata and dependencies" $
  T.record [
    "name">:
      doc "The name of the package"
      packageName,
    "modules">:
      doc "The modules in this package" $
      T.list Module.module',
    "dependencies">:
      doc "The packages which this package depends on" $
      T.list packageName,
    "description">:
      doc "An optional human-readable description of the package" $
      T.maybe T.string]

packageName :: Binding
packageName = define "PackageName" $
  doc "The unique name of a package, e.g. \"hydra-kernel\" or \"hydra-python\"" $
  T.wrap T.string
