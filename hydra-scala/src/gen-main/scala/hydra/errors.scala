package hydra.errors

import hydra.core.*

import hydra.error.checking.*

import hydra.error.core.*

import hydra.core

import hydra.error.checking

import hydra.error.core

type DecodingError = scala.Predef.String

enum Error :
   case checking(value: hydra.error.checking.CheckingError) extends Error
   case decoding(value: hydra.errors.DecodingError) extends Error
   case duplicateBinding(value: hydra.error.core.DuplicateBindingError) extends Error
   case duplicateField(value: hydra.error.core.DuplicateFieldError) extends Error
   case other(value: hydra.errors.OtherError) extends Error
   case undefinedField(value: hydra.error.core.UndefinedFieldError) extends Error
   case undefinedTerm(value: hydra.error.core.UndefinedTermError) extends Error
   case undefinedType(value: hydra.error.core.UndefinedTypeError) extends Error
   case unexpectedTermVariant(value: hydra.error.core.UnexpectedTermVariantError) extends Error
   case unexpectedTypeVariant(value: hydra.error.core.UnexpectedTypeVariantError) extends Error
   case unification(value: hydra.errors.UnificationError) extends Error

type OtherError = scala.Predef.String

case class UnificationError(leftType: hydra.core.Type, rightType: hydra.core.Type, message: scala.Predef.String)
