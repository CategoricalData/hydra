module Hydra.Sources.Kernel.Types.Error.Checking where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Typing as Typing
import qualified Hydra.Sources.Kernel.Types.Variants as Variants


ns :: Namespace
ns = Namespace "hydra.error.checking"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [Core.ns, Typing.ns, Variants.ns] [Core.ns, Typing.ns, Variants.ns] $
    Just "Error types for type checking"
  where
    definitions = [
      checkingError,
      incorrectUnificationError,
      notAForallTypeError,
      notAFunctionTypeError,
      typeArityMismatchError,
      typeMismatchError,
      unboundTypeVariablesError,
      unequalTypesError,
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
