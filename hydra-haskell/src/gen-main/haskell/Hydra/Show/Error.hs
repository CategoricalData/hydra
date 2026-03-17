-- Note: this is an automatically generated file. Do not edit.

-- | String representations of hydra.error types

module Hydra.Show.Error where

import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Show.Meta as Meta
import qualified Hydra.Show.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Show a checking error as a string
checkingError :: Error.CheckingError -> String
checkingError ce =
    case ce of
      Error.CheckingErrorIncorrectUnification v0 -> incorrectUnificationError v0
      Error.CheckingErrorNotAForallType v0 -> notAForallTypeError v0
      Error.CheckingErrorNotAFunctionType v0 -> notAFunctionTypeError v0
      Error.CheckingErrorTypeArityMismatch v0 -> typeArityMismatchError v0
      Error.CheckingErrorTypeMismatch v0 -> typeMismatchError v0
      Error.CheckingErrorUnboundTypeVariables v0 -> unboundTypeVariablesError v0
      Error.CheckingErrorUnequalTypes v0 -> unequalTypesError v0
      Error.CheckingErrorUnsupportedTermVariant v0 -> unsupportedTermVariantError v0
      Error.CheckingErrorUntypedLambda v0 -> untypedLambdaError v0
      Error.CheckingErrorUntypedLetBinding v0 -> untypedLetBindingError v0

-- | Show a decoding error as a string
decodingError :: Error.DecodingError -> String
decodingError de = Strings.cat2 "decoding error: " (Error.unDecodingError de)

-- | Show a duplicate binding error as a string
duplicateBindingError :: Error.DuplicateBindingError -> String
duplicateBindingError e = Strings.cat2 "duplicate binding: " (Core.unName (Error.duplicateBindingErrorName e))

-- | Show a duplicate field error as a string
duplicateFieldError :: Error.DuplicateFieldError -> String
duplicateFieldError e = Strings.cat2 "duplicate field: " (Core.unName (Error.duplicateFieldErrorName e))

-- | Show an error as a string
error :: Error.Error -> String
error e =
    case e of
      Error.ErrorChecking v0 -> checkingError v0
      Error.ErrorDecoding v0 -> decodingError v0
      Error.ErrorDuplicateBinding v0 -> duplicateBindingError v0
      Error.ErrorDuplicateField v0 -> duplicateFieldError v0
      Error.ErrorOther v0 -> otherError v0
      Error.ErrorUndefinedField v0 -> undefinedFieldError v0
      Error.ErrorUndefinedTerm v0 -> undefinedTermError v0
      Error.ErrorUndefinedType v0 -> undefinedTypeError v0
      Error.ErrorUnexpectedTermVariant v0 -> unexpectedTermVariantError v0
      Error.ErrorUnexpectedTypeVariant v0 -> unexpectedTypeVariantError v0
      Error.ErrorUnification v0 -> unificationError v0

-- | Show an incorrect unification error as a string
incorrectUnificationError :: Error.IncorrectUnificationError -> String
incorrectUnificationError e =

      let subst = Error.incorrectUnificationErrorSubstitution e
      in (Strings.cat2 "incorrect unification: " (Typing.typeSubst subst))

-- | Show a not-a-forall-type error as a string
notAForallTypeError :: Error.NotAForallTypeError -> String
notAForallTypeError e =

      let typ = Error.notAForallTypeErrorType e
          args = Error.notAForallTypeErrorTypeArguments e
      in (Strings.cat [
        "not a forall type: ",
        (Core_.type_ typ),
        ". Trying to apply ",
        (Literals.showInt32 (Lists.length args)),
        " type argument(s): ",
        (Formatting.showList Core_.type_ args)])

-- | Show a not-a-function-type error as a string
notAFunctionTypeError :: Error.NotAFunctionTypeError -> String
notAFunctionTypeError e =

      let typ = Error.notAFunctionTypeErrorType e
      in (Strings.cat2 "not a function type: " (Core_.type_ typ))

-- | Show an other error as a string
otherError :: Error.OtherError -> String
otherError oe = Error.unOtherError oe

-- | Show a type arity mismatch error as a string
typeArityMismatchError :: Error.TypeArityMismatchError -> String
typeArityMismatchError e =

      let typ = Error.typeArityMismatchErrorType e
          expected = Error.typeArityMismatchErrorExpectedArity e
          actual = Error.typeArityMismatchErrorActualArity e
          args = Error.typeArityMismatchErrorTypeArguments e
      in (Strings.cat [
        "type ",
        (Core_.type_ typ),
        " applied to the wrong number of type arguments (expected ",
        (Literals.showInt32 expected),
        ", got ",
        (Literals.showInt32 actual),
        "): ",
        (Formatting.showList Core_.type_ args)])

-- | Show a type mismatch error as a string
typeMismatchError :: Error.TypeMismatchError -> String
typeMismatchError e =

      let expected = Error.typeMismatchErrorExpectedType e
          actual = Error.typeMismatchErrorActualType e
      in (Strings.cat [
        "type mismatch: expected ",
        (Core_.type_ expected),
        " but found ",
        (Core_.type_ actual)])

-- | Show an unbound type variables error as a string
unboundTypeVariablesError :: Error.UnboundTypeVariablesError -> String
unboundTypeVariablesError e =

      let vars = Error.unboundTypeVariablesErrorVariables e
          typ = Error.unboundTypeVariablesErrorType e
      in (Strings.cat [
        "unbound type variables: {",
        (Strings.intercalate ", " (Lists.map Core.unName (Sets.toList vars))),
        "} in type ",
        (Core_.type_ typ)])

-- | Show an undefined field error as a string
undefinedFieldError :: Error.UndefinedFieldError -> String
undefinedFieldError e =

      let fname = Error.undefinedFieldErrorFieldName e
          tname = Error.undefinedFieldErrorTypeName e
      in (Strings.cat [
        "no such field \"",
        (Core.unName fname),
        "\" in type \"",
        (Core.unName tname),
        "\""])

-- | Show an undefined term error as a string
undefinedTermError :: Error.UndefinedTermError -> String
undefinedTermError e = Strings.cat2 "undefined term: " (Core.unName (Error.undefinedTermErrorName e))

-- | Show an undefined type error as a string
undefinedTypeError :: Error.UndefinedTypeError -> String
undefinedTypeError e = Strings.cat2 "undefined type: " (Core.unName (Error.undefinedTypeErrorName e))

-- | Show an unequal types error as a string
unequalTypesError :: Error.UnequalTypesError -> String
unequalTypesError e =

      let types = Error.unequalTypesErrorTypes e
          desc = Error.unequalTypesErrorDescription e
      in (Strings.cat [
        "unequal types ",
        (Formatting.showList Core_.type_ types),
        " in ",
        desc])

-- | Show an unexpected term variant error as a string
unexpectedTermVariantError :: Error.UnexpectedTermVariantError -> String
unexpectedTermVariantError e =

      let expected = Error.unexpectedTermVariantErrorExpectedVariant e
          actual = Error.unexpectedTermVariantErrorActualTerm e
      in (Strings.cat [
        "expected ",
        (Meta.termVariant expected),
        " term but found ",
        (Core_.term actual)])

-- | Show an unexpected type variant error as a string
unexpectedTypeVariantError :: Error.UnexpectedTypeVariantError -> String
unexpectedTypeVariantError e =

      let expected = Error.unexpectedTypeVariantErrorExpectedVariant e
          actual = Error.unexpectedTypeVariantErrorActualType e
      in (Strings.cat [
        "expected ",
        (Meta.typeVariant expected),
        " type but found ",
        (Core_.type_ actual)])

-- | Show a unification error as a string
unificationError :: Error.UnificationError -> String
unificationError e =

      let lt = Error.unificationErrorLeftType e
          rt = Error.unificationErrorRightType e
          msg = Error.unificationErrorMessage e
      in (Strings.cat [
        "unification error: cannot unify ",
        (Core_.type_ lt),
        " with ",
        (Core_.type_ rt),
        ": ",
        msg])

-- | Show an unsupported term variant error as a string
unsupportedTermVariantError :: Error.UnsupportedTermVariantError -> String
unsupportedTermVariantError e =
    Strings.cat2 "unsupported term variant: " (Meta.termVariant (Error.unsupportedTermVariantErrorTermVariant e))

-- | Show an untyped lambda error as a string
untypedLambdaError :: t0 -> String
untypedLambdaError _ = "untyped lambda"

-- | Show an untyped let binding error as a string
untypedLetBindingError :: Error.UntypedLetBindingError -> String
untypedLetBindingError e =

      let b = Error.untypedLetBindingErrorBinding e
      in (Strings.cat2 "untyped let binding: " (Core_.binding b))
