module Hydra.Sources.Kernel.Types.Error where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Context as Context


ns :: Namespace
ns = Namespace "hydra.error"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Context.ns] [Context.ns] $
    Just "Error types specific to the Hydra kernel"
  where
    elements = [
      decodingError]

decodingError :: Binding
decodingError = define "DecodingError" $
  doc "An error that occurred during decoding of a term" $
  T.wrap T.string
