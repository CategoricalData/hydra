module Hydra.Sources.Kernel.Types.Error where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Context as Context
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.error"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Context.ns, Core.ns] [Context.ns, Core.ns] $
    Just "Error types specific to the Hydra kernel"
  where
    elements = [
      decodingError,
      error_,
      otherError,
      unificationError]

decodingError :: Binding
decodingError = define "DecodingError" $
  doc "An error that occurred during decoding of a term" $
  T.wrap T.string

error_ :: Binding
error_ = define "Error" $
  doc "An error of any kind, with kernel errors particularly differentiated" $
  T.union [
    "decoding">:
      doc "An error that occurred during decoding of a term" $
      decodingError,
    "other">:
      doc "Any other error" $
      otherError,
    "unification">:
      doc "A type unification error" $
      unificationError]

otherError :: Binding
otherError = define "OtherError" $
  doc "Any other error" $
  T.wrap T.string

unificationError :: Binding
unificationError = define "UnificationError" $
  doc "An error that occurred during type unification" $
  T.record [
    "leftType">:
      doc "The left-hand type in the unification" $
      Core.type_,
    "rightType">:
      doc "The right-hand type in the unification" $
      Core.type_,
    "message">:
      doc "A human-readable error message" $
      T.string]
