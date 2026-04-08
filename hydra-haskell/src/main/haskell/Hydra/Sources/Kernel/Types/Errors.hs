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
import qualified Hydra.Sources.Kernel.Types.Paths as Paths
import qualified Hydra.Sources.Kernel.Types.Typing as Typing
import qualified Hydra.Sources.Kernel.Types.Variants as Variants


ns :: Namespace
ns = Namespace "hydra.errors"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions)
    [Context.ns, Core.ns, ErrorsChecking.ns, ErrorsCore.ns, Paths.ns, Typing.ns, Variants.ns]
    [Context.ns, Core.ns, ErrorsChecking.ns, ErrorsCore.ns, Paths.ns, Typing.ns, Variants.ns] $
    Just "Top-level error types for the Hydra kernel"
  where
    definitions = [
      decodingError,
      emptyListError,
      error_,
      extractionError,
      inferenceError,
      multipleBindingsError,
      multipleFieldsError,
      noMatchingFieldError,
      noSuchBindingError,
      noSuchPrimitiveError,
      notEnoughCasesError,
      otherError,
      otherInferenceError,
      otherResolutionError,
      resolutionError,
      unexpectedShapeError,
      unificationError,
      unificationInferenceError]

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
    "extraction">:
      doc "An error that occurred while extracting a value from a term" $
      extractionError,
    "inference">:
      doc "A type inference error" $
      inferenceError,
    "other">:
      doc "Any other error" $
      otherError,
    "resolution">:
      doc "A name-resolution error" $
      resolutionError,
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

resolutionError :: Binding
resolutionError = define "ResolutionError" $
  doc "An error that occurred while resolving a name, primitive, or record/union shape in a graph" $
  T.union [
    "noSuchBinding">:
      doc "No binding with the expected name was found in the graph" $
      noSuchBindingError,
    "noSuchPrimitive">:
      doc "No primitive function with the expected name was found in the graph" $
      noSuchPrimitiveError,
    "noMatchingField">:
      doc "No field with the expected name was present in a record or case statement" $
      noMatchingFieldError,
    "other">:
      doc "A generic resolution error carrying a message" $
      otherResolutionError,
    "unexpectedShape">:
      doc "A term had a shape other than the one expected (e.g. a record, an injection)" $
      unexpectedShapeError]

otherResolutionError :: Binding
otherResolutionError = define "OtherResolutionError" $
  doc "A generic resolution error: message" $
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

unificationInferenceError :: Binding
unificationInferenceError = define "UnificationInferenceError" $
  doc "A unification failure at a specific subterm locus during inference" $
  T.record [
    "path">:
      doc "The subterm path at which the unification failure was observed" $
      Paths.subtermPath,
    "cause">:
      doc "The underlying unification error" $
      unificationError]

extractionError :: Binding
extractionError = define "ExtractionError" $
  doc "An error that occurred while extracting a typed value from a term" $
  T.union [
    "emptyList">:
      doc "An empty list was encountered where a non-empty list was required" $
      emptyListError,
    "multipleBindings">:
      doc "Multiple let bindings were found with the same name" $
      multipleBindingsError,
    "multipleFields">:
      doc "Multiple record fields were found with the same field name" $
      multipleFieldsError,
    "noMatchingField">:
      doc "No field with the expected name was found in a record" $
      noMatchingFieldError,
    "noSuchBinding">:
      doc "No let binding with the expected name was found" $
      noSuchBindingError,
    "notEnoughCases">:
      doc "A case statement did not contain enough cases to match the target" $
      notEnoughCasesError,
    "unexpectedShape">:
      doc "A term, type, literal, or other value had an unexpected shape" $
      unexpectedShapeError]

inferenceError :: Binding
inferenceError = define "InferenceError" $
  doc "An error that occurred during type inference" $
  T.union [
    "checking">:
      doc "A type checking error encountered during inference" $
      ErrorsChecking.checkingError,
    "other">:
      doc ("A generic inference error carrying a message and a subterm path."
        <> " Placeholder arm; sites should migrate to typed variants.") $
      otherInferenceError,
    "unification">:
      doc "A unification failure encountered while inferring types" $
      unificationInferenceError]

otherInferenceError :: Binding
otherInferenceError = define "OtherInferenceError" $
  doc "A generic inference error: message + subterm path" $
  T.record [
    "path">:
      doc "The subterm path at which the error was observed" $
      Paths.subtermPath,
    "message">:
      doc "A human-readable error message" $
      T.string]

emptyListError :: Binding
emptyListError = define "EmptyListError" $
  doc "An empty list was encountered where a non-empty list was required" $
  T.unit

multipleBindingsError :: Binding
multipleBindingsError = define "MultipleBindingsError" $
  doc "Multiple let bindings with the same name were found" $
  T.record [
    "name">:
      doc "The binding name which was duplicated" $
      Core.name]

multipleFieldsError :: Binding
multipleFieldsError = define "MultipleFieldsError" $
  doc "Multiple fields with the same name were found in a record" $
  T.record [
    "fieldName">:
      doc "The field name which appeared more than once" $
      Core.name]

noMatchingFieldError :: Binding
noMatchingFieldError = define "NoMatchingFieldError" $
  doc "No field with the expected name was present" $
  T.record [
    "fieldName">:
      doc "The field name which was not found" $
      Core.name]

noSuchBindingError :: Binding
noSuchBindingError = define "NoSuchBindingError" $
  doc "No let binding with the expected name was present" $
  T.record [
    "name">:
      doc "The binding name which was not found" $
      Core.name]

noSuchPrimitiveError :: Binding
noSuchPrimitiveError = define "NoSuchPrimitiveError" $
  doc "No primitive function with the expected name was registered in the graph" $
  T.record [
    "name">:
      doc "The primitive name which was not found" $
      Core.name]

notEnoughCasesError :: Binding
notEnoughCasesError = define "NotEnoughCasesError" $
  doc "A case statement was missing a case for the requested variant" $
  T.unit

unexpectedShapeError :: Binding
unexpectedShapeError = define "UnexpectedShapeError" $
  doc "A term, type, literal, or related value had a shape other than the one expected" $
  T.record [
    "expected">:
      doc "A description of the expected shape" $
      T.string,
    "actual">:
      doc "A description of the shape actually encountered" $
      T.string]
