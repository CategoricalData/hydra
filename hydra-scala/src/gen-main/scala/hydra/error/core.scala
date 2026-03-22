package hydra.error.core

import hydra.accessors.*

import hydra.core.*

import hydra.variants.*

import hydra.accessors

import hydra.core

import hydra.variants

case class DuplicateBindingError(location: hydra.accessors.AccessorPath, name: hydra.core.Name)

case class DuplicateFieldError(location: hydra.accessors.AccessorPath, name: hydra.core.Name)

enum InvalidTermError :
   case duplicateBinding(value: hydra.error.core.DuplicateBindingError) extends InvalidTermError
   case duplicateField(value: hydra.error.core.DuplicateFieldError) extends InvalidTermError

case class UndefinedFieldError(fieldName: hydra.core.Name, typeName: hydra.core.Name)

case class UndefinedTermError(name: hydra.core.Name)

case class UndefinedTypeError(name: hydra.core.Name)

case class UnexpectedTermVariantError(expectedVariant: hydra.variants.TermVariant, actualTerm: hydra.core.Term)

case class UnexpectedTypeVariantError(expectedVariant: hydra.variants.TypeVariant, actualType: hydra.core.Type)
