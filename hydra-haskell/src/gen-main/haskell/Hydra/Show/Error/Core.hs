-- Note: this is an automatically generated file. Do not edit.

-- | String representations of hydra.errors.core types

module Hydra.Show.Error.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as Core_
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Show.Meta as Meta
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Show a duplicate binding error as a string
duplicateBindingError :: Core_.DuplicateBindingError -> String
duplicateBindingError e =
    Strings.cat [
      "duplicate binding: ",
      (Core.unName (Core_.duplicateBindingErrorName e))]

-- | Show a duplicate field error as a string
duplicateFieldError :: Core_.DuplicateFieldError -> String
duplicateFieldError e =
    Strings.cat [
      "duplicate field: ",
      (Core.unName (Core_.duplicateFieldErrorName e))]

-- | Show an invalid term error as a string
invalidTermError :: Core_.InvalidTermError -> String
invalidTermError e =
    Strings.cat2 "invalid term: " (case e of
      Core_.InvalidTermErrorDuplicateBinding v0 -> duplicateBindingError v0
      Core_.InvalidTermErrorDuplicateField v0 -> duplicateFieldError v0)

-- | Show an undefined field error as a string
undefinedFieldError :: Core_.UndefinedFieldError -> String
undefinedFieldError e =

      let fname = Core_.undefinedFieldErrorFieldName e
          tname = Core_.undefinedFieldErrorTypeName e
      in (Strings.cat [
        "no such field \"",
        (Core.unName fname),
        "\" in type \"",
        (Core.unName tname),
        "\""])

-- | Show an undefined term error as a string
undefinedTermError :: Core_.UndefinedTermError -> String
undefinedTermError e = Strings.cat2 "undefined term: " (Core.unName (Core_.undefinedTermErrorName e))

-- | Show an undefined type error as a string
undefinedTypeError :: Core_.UndefinedTypeError -> String
undefinedTypeError e = Strings.cat2 "undefined type: " (Core.unName (Core_.undefinedTypeErrorName e))

-- | Show an unexpected term variant error as a string
unexpectedTermVariantError :: Core_.UnexpectedTermVariantError -> String
unexpectedTermVariantError e =

      let expected = Core_.unexpectedTermVariantErrorExpectedVariant e
          actual = Core_.unexpectedTermVariantErrorActualTerm e
      in (Strings.cat [
        "expected ",
        (Meta.termVariant expected),
        " term but found ",
        (Core__.term actual)])

-- | Show an unexpected type variant error as a string
unexpectedTypeVariantError :: Core_.UnexpectedTypeVariantError -> String
unexpectedTypeVariantError e =

      let expected = Core_.unexpectedTypeVariantErrorExpectedVariant e
          actual = Core_.unexpectedTypeVariantErrorActualType e
      in (Strings.cat [
        "expected ",
        (Meta.typeVariant expected),
        " type but found ",
        (Core__.type_ actual)])
