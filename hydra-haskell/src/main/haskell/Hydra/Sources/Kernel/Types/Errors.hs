module Hydra.Sources.Kernel.Types.Errors where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Context as Context
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Error.Checking as ErrorsChecking
import qualified Hydra.Sources.Kernel.Types.Error.Core as ErrorsCore
import qualified Hydra.Sources.Kernel.Types.Typing as Typing
import qualified Hydra.Sources.Kernel.Types.Variants as Variants


ns :: Namespace
ns = Namespace "hydra.errors"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions)
    [Context.ns, Core.ns, ErrorsChecking.ns, ErrorsCore.ns, Typing.ns, Variants.ns]
    [Context.ns, Core.ns, ErrorsChecking.ns, ErrorsCore.ns, Typing.ns, Variants.ns] $
    Just "Top-level error types for the Hydra kernel"
  where
    definitions = [
      decodingError,
      error_,
      otherError,
      unificationError]

error_ :: Binding
error_ = define "Error" $
  doc "An error of any kind, with kernel errors particularly differentiated" $
  T.union [
    "checking">:
      doc "A type checking error" $
      ErrorsChecking.checkingError,
    "decoding">:
      doc "An error that occurred during decoding of a term" $
      decodingError,
    "duplicateBinding">:
      doc "A duplicate binding name error" $
      ErrorsCore.duplicateBindingError,
    "duplicateField">:
      doc "A duplicate field name error" $
      ErrorsCore.duplicateFieldError,
    "other">:
      doc "Any other error" $
      otherError,
    "undefinedField">:
      doc "A reference to an undefined field" $
      ErrorsCore.undefinedFieldError,
    "undefinedTermVariable">:
      doc "A reference to an undefined term variable" $
      ErrorsCore.undefinedTermVariableError,
    "untypedTermVariable">:
      doc "A term variable whose type is not known" $
      ErrorsCore.untypedTermVariableError,
    "unexpectedTermVariant">:
      doc "An unexpected term variant" $
      ErrorsCore.unexpectedTermVariantError,
    "unexpectedTypeVariant">:
      doc "An unexpected type variant" $
      ErrorsCore.unexpectedTypeVariantError,
    "unification">:
      doc "A type unification error" $
      unificationError]

decodingError :: Binding
decodingError = define "DecodingError" $
  doc "An error that occurred during decoding of a term" $
  T.wrap T.string

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
