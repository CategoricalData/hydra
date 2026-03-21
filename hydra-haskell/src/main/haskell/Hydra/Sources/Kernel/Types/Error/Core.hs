module Hydra.Sources.Kernel.Types.Error.Core where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Accessors as Accessors
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Variants as Variants


ns :: Namespace
ns = Namespace "hydra.error.core"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Accessors.ns, Core.ns, Variants.ns] [Accessors.ns, Core.ns, Variants.ns] $
    Just "Error types for core type and term validation"
  where
    elements = [
      duplicateBindingError,
      duplicateFieldError,
      invalidTermError,
      undefinedFieldError,
      undefinedTermError,
      undefinedTypeError,
      unexpectedTermVariantError,
      unexpectedTypeVariantError]

duplicateBindingError :: Binding
duplicateBindingError = define "DuplicateBindingError" $
  doc "A duplicate binding name in a let expression" $
  T.record [
    "location">:
      doc "The path to the duplicate binding within the term" $
      Accessors.accessorPath,
    "name">:
      doc "The duplicated binding name" $
      Core.name]

duplicateFieldError :: Binding
duplicateFieldError = define "DuplicateFieldError" $
  doc "A duplicate field name in a record or union type" $
  T.record [
    "location">:
      doc "The path to the duplicate field within the term" $
      Accessors.accessorPath,
    "name">:
      doc "The duplicated field name" $
      Core.name]

invalidTermError :: Binding
invalidTermError = define "InvalidTermError" $
  doc "An error indicating that a term is invalid" $
  T.union [
    "duplicateBinding">:
      doc "A duplicate binding name in a let expression" $
      duplicateBindingError,
    "duplicateField">:
      doc "A duplicate field name in a record or union type" $
      duplicateFieldError]

undefinedFieldError :: Binding
undefinedFieldError = define "UndefinedFieldError" $
  doc "A reference to a field that does not exist in the given type" $
  T.record [
    "fieldName">:
      doc "The name of the undefined field" $
      Core.name,
    "typeName">:
      doc "The name of the type in which the field was expected" $
      Core.name]

undefinedTermError :: Binding
undefinedTermError = define "UndefinedTermError" $
  doc "A reference to a term (element, binding, or primitive) that is not defined" $
  T.record [
    "name">:
      doc "The name of the undefined term" $
      Core.name]

undefinedTypeError :: Binding
undefinedTypeError = define "UndefinedTypeError" $
  doc "A reference to a type or type variable that is not defined" $
  T.record [
    "name">:
      doc "The name of the undefined type" $
      Core.name]

unexpectedTermVariantError :: Binding
unexpectedTermVariantError = define "UnexpectedTermVariantError" $
  doc "An unexpected term variant was encountered" $
  T.record [
    "expectedVariant">:
      doc "The expected term variant" $
      Variants.termVariant,
    "actualTerm">:
      doc "The actual term that was encountered" $
      Core.term]

unexpectedTypeVariantError :: Binding
unexpectedTypeVariantError = define "UnexpectedTypeVariantError" $
  doc "An unexpected type variant was encountered" $
  T.record [
    "expectedVariant">:
      doc "The expected type variant" $
      Variants.typeVariant,
    "actualType">:
      doc "The actual type that was encountered" $
      Core.type_]
