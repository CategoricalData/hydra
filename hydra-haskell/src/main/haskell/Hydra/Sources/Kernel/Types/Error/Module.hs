module Hydra.Sources.Kernel.Types.Error.Module where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap


ns :: Namespace
ns = Namespace "hydra.error.module"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [] [] $
    Just "Error types for module validation"
  where
    elements = []
