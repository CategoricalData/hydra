package hydra.errors

import hydra.core.*

import hydra.error.checking.*

import hydra.error.core.*

import hydra.paths.*

import hydra.core

import hydra.error.checking

import hydra.error.core

import hydra.paths

type DecodingError = scala.Predef.String

type EmptyListError = Unit

enum Error :
   case checking(value: hydra.error.checking.CheckingError) extends Error
   case decoding(value: hydra.errors.DecodingError) extends Error
   case duplicateBinding(value: hydra.error.core.DuplicateBindingError) extends Error
   case duplicateField(value: hydra.error.core.DuplicateFieldError) extends Error
   case extraction(value: hydra.errors.ExtractionError) extends Error
   case inference(value: hydra.errors.InferenceError) extends Error
   case other(value: hydra.errors.OtherError) extends Error
   case resolution(value: hydra.errors.ResolutionError) extends Error
   case undefinedField(value: hydra.error.core.UndefinedFieldError) extends Error
   case undefinedTermVariable(value: hydra.error.core.UndefinedTermVariableError) extends Error
   case untypedTermVariable(value: hydra.error.core.UntypedTermVariableError) extends Error
   case unexpectedTermVariant(value: hydra.error.core.UnexpectedTermVariantError) extends Error
   case unexpectedTypeVariant(value: hydra.error.core.UnexpectedTypeVariantError) extends Error
   case unification(value: hydra.errors.UnificationError) extends Error

enum ExtractionError :
   case emptyList(value: hydra.errors.EmptyListError) extends ExtractionError
   case multipleBindings(value: hydra.errors.MultipleBindingsError) extends ExtractionError
   case multipleFields(value: hydra.errors.MultipleFieldsError) extends ExtractionError
   case noMatchingField(value: hydra.errors.NoMatchingFieldError) extends ExtractionError
   case noSuchBinding(value: hydra.errors.NoSuchBindingError) extends ExtractionError
   case notEnoughCases(value: hydra.errors.NotEnoughCasesError) extends ExtractionError
   case unexpectedShape(value: hydra.errors.UnexpectedShapeError) extends ExtractionError

enum InferenceError :
   case checking(value: hydra.error.checking.CheckingError) extends InferenceError
   case other(value: hydra.errors.OtherInferenceError) extends InferenceError
   case unification(value: hydra.errors.UnificationInferenceError) extends InferenceError

case class MultipleBindingsError(name: hydra.core.Name)

case class MultipleFieldsError(fieldName: hydra.core.Name)

case class NoMatchingFieldError(fieldName: hydra.core.Name)

case class NoSuchBindingError(name: hydra.core.Name)

case class NoSuchPrimitiveError(name: hydra.core.Name)

type NotEnoughCasesError = Unit

type OtherError = scala.Predef.String

case class OtherInferenceError(path: hydra.paths.SubtermPath, message: scala.Predef.String)

type OtherResolutionError = scala.Predef.String

enum ResolutionError :
   case noSuchBinding(value: hydra.errors.NoSuchBindingError) extends ResolutionError
   case noSuchPrimitive(value: hydra.errors.NoSuchPrimitiveError) extends ResolutionError
   case noMatchingField(value: hydra.errors.NoMatchingFieldError) extends ResolutionError
   case other(value: hydra.errors.OtherResolutionError) extends ResolutionError
   case unexpectedShape(value: hydra.errors.UnexpectedShapeError) extends ResolutionError

case class UnexpectedShapeError(expected: scala.Predef.String, actual: scala.Predef.String)

case class UnificationError(leftType: hydra.core.Type, rightType: hydra.core.Type, message: scala.Predef.String)

case class UnificationInferenceError(path: hydra.paths.SubtermPath, cause: hydra.errors.UnificationError)
