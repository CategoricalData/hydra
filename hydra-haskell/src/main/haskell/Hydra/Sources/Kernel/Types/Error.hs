module Hydra.Sources.Kernel.Types.Error where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Context as Context
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Typing as Typing
import qualified Hydra.Sources.Kernel.Types.Variants as Variants


ns :: Namespace
ns = Namespace "hydra.error"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Context.ns, Core.ns, Typing.ns, Variants.ns] [Context.ns, Core.ns, Typing.ns, Variants.ns] $
    Just "Error types specific to the Hydra kernel"
  where
    elements = [
      checkingError,
      decodingError,
      duplicateBindingError,
      duplicateFieldError,
      error_,
      incorrectUnificationError,
      notAForallTypeError,
      notAFunctionTypeError,
      otherError,
      typeArityMismatchError,
      typeMismatchError,
      unboundTypeVariablesError,
      undefinedFieldError,
      undefinedTermError,
      undefinedTypeError,
      unequalTypesError,
      unexpectedTermVariantError,
      unexpectedTypeVariantError,
      unificationError,
      unsupportedTermVariantError,
      untypedLambdaError,
      untypedLetBindingError]

checkingError :: Binding
checkingError = define "CheckingError" $
  doc "An error that occurred during type checking" $
  T.union [
    "incorrectUnification">:
      doc "A post-unification consistency check failure" $
      incorrectUnificationError,
    "notAForallType">:
      doc "A type that is not a forall type when one was expected" $
      notAForallTypeError,
    "notAFunctionType">:
      doc "A type that is not a function type when one was expected" $
      notAFunctionTypeError,
    "typeArityMismatch">:
      doc "A type constructor applied to the wrong number of arguments" $
      typeArityMismatchError,
    "typeMismatch">:
      doc "A type mismatch between expected and actual types" $
      typeMismatchError,
    "unboundTypeVariables">:
      doc "Type variables that are not bound in scope" $
      unboundTypeVariablesError,
    "unequalTypes">:
      doc "Multiple types that should be equal but are not" $
      unequalTypesError,
    "unsupportedTermVariant">:
      doc "A term variant that the type checker does not support" $
      unsupportedTermVariantError,
    "untypedLambda">:
      doc "A lambda expression without a type annotation on its parameter" $
      untypedLambdaError,
    "untypedLetBinding">:
      doc "A let binding without a type annotation" $
      untypedLetBindingError]

decodingError :: Binding
decodingError = define "DecodingError" $
  doc "An error that occurred during decoding of a term" $
  T.wrap T.string

duplicateBindingError :: Binding
duplicateBindingError = define "DuplicateBindingError" $
  doc "A duplicate binding name in a let expression" $
  T.record [
    "name">:
      doc "The duplicated binding name" $
      Core.name]

duplicateFieldError :: Binding
duplicateFieldError = define "DuplicateFieldError" $
  doc "A duplicate field name in a record or union type" $
  T.record [
    "name">:
      doc "The duplicated field name" $
      Core.name]

error_ :: Binding
error_ = define "Error" $
  doc "An error of any kind, with kernel errors particularly differentiated" $
  T.union [
    "checking">:
      doc "A type checking error" $
      checkingError,
    "decoding">:
      doc "An error that occurred during decoding of a term" $
      decodingError,
    "duplicateBinding">:
      doc "A duplicate binding name error" $
      duplicateBindingError,
    "duplicateField">:
      doc "A duplicate field name error" $
      duplicateFieldError,
    "other">:
      doc "Any other error" $
      otherError,
    "undefinedField">:
      doc "A reference to an undefined field" $
      undefinedFieldError,
    "undefinedTerm">:
      doc "A reference to an undefined term" $
      undefinedTermError,
    "undefinedType">:
      doc "A reference to an undefined type" $
      undefinedTypeError,
    "unexpectedTermVariant">:
      doc "An unexpected term variant" $
      unexpectedTermVariantError,
    "unexpectedTypeVariant">:
      doc "An unexpected type variant" $
      unexpectedTypeVariantError,
    "unification">:
      doc "A type unification error" $
      unificationError]

incorrectUnificationError :: Binding
incorrectUnificationError = define "IncorrectUnificationError" $
  doc "A post-unification consistency check failure" $
  T.record [
    "substitution">:
      doc "The substitution that failed the consistency check" $
      Typing.typeSubst]

notAForallTypeError :: Binding
notAForallTypeError = define "NotAForallTypeError" $
  doc "A type that is not a forall type when type arguments are being applied" $
  T.record [
    "type">:
      doc "The actual type encountered" $
      Core.type_,
    "typeArguments">:
      doc "The type arguments that were being applied" $
      T.list Core.type_]

notAFunctionTypeError :: Binding
notAFunctionTypeError = define "NotAFunctionTypeError" $
  doc "A type that is not a function type when one was expected in an application" $
  T.record [
    "type">:
      doc "The actual type encountered" $
      Core.type_]

otherError :: Binding
otherError = define "OtherError" $
  doc "Any other error" $
  T.wrap T.string

typeArityMismatchError :: Binding
typeArityMismatchError = define "TypeArityMismatchError" $
  doc "A type constructor applied to the wrong number of type arguments" $
  T.record [
    "type">:
      doc "The type being checked" $
      Core.type_,
    "expectedArity">:
      doc "The expected number of type arguments" $
      T.int32,
    "actualArity">:
      doc "The actual number of type arguments provided" $
      T.int32,
    "typeArguments">:
      doc "The type arguments that were provided" $
      T.list Core.type_]

typeMismatchError :: Binding
typeMismatchError = define "TypeMismatchError" $
  doc "A type mismatch between expected and actual types" $
  T.record [
    "expectedType">:
      doc "The expected type" $
      Core.type_,
    "actualType">:
      doc "The actual type encountered" $
      Core.type_]

unboundTypeVariablesError :: Binding
unboundTypeVariablesError = define "UnboundTypeVariablesError" $
  doc "Type variables that appear free in a type but are not bound in scope" $
  T.record [
    "variables">:
      doc "The set of unbound type variable names" $
      T.set Core.name,
    "type">:
      doc "The type containing the unbound variables" $
      Core.type_]

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

unequalTypesError :: Binding
unequalTypesError = define "UnequalTypesError" $
  doc "Multiple types that should all be equal but are not" $
  T.record [
    "types">:
      doc "The list of types that are not all equal" $
      T.list Core.type_,
    "description">:
      doc "A description of the context in which the types were expected to be equal" $
      T.string]

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

unsupportedTermVariantError :: Binding
unsupportedTermVariantError = define "UnsupportedTermVariantError" $
  doc "A term variant that the type checker does not support" $
  T.record [
    "termVariant">:
      doc "The unsupported term variant" $
      Variants.termVariant]

untypedLambdaError :: Binding
untypedLambdaError = define "UntypedLambdaError" $
  doc "A lambda expression without a type annotation on its parameter" $
  T.record []

untypedLetBindingError :: Binding
untypedLetBindingError = define "UntypedLetBindingError" $
  doc "A let binding without a type annotation" $
  T.record [
    "binding">:
      doc "The untyped binding" $
      Core.binding]
