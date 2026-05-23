module Hydra.Sources.Kernel.Types.Classes where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.classes"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleDescription = Just "Type classes"}
  where
    definitions = [
      typeClass]

typeClass :: Binding
typeClass = define "TypeClass" $
  doc "Any of a small number of built-in type classes" $
  T.enum [
    "equality",
    "ordering"]
