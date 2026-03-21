-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.errors

module Hydra.Encode.Errors where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Encode.Error.Checking as Checking
import qualified Hydra.Encode.Error.Core as Core__
import qualified Hydra.Errors as Errors
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

decodingError :: Errors.DecodingError -> Core.Term
decodingError x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
      Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Errors.unDecodingError x))})

error :: Errors.Error -> Core.Term
error x =
    case x of
      Errors.ErrorChecking v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "checking"),
          Core.fieldTerm = (Checking.checkingError v0)}})
      Errors.ErrorDecoding v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "decoding"),
          Core.fieldTerm = (decodingError v0)}})
      Errors.ErrorDuplicateBinding v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateBinding"),
          Core.fieldTerm = (Core__.duplicateBindingError v0)}})
      Errors.ErrorDuplicateField v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateField"),
          Core.fieldTerm = (Core__.duplicateFieldError v0)}})
      Errors.ErrorOther v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (otherError v0)}})
      Errors.ErrorUndefinedField v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedField"),
          Core.fieldTerm = (Core__.undefinedFieldError v0)}})
      Errors.ErrorUndefinedTerm v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTerm"),
          Core.fieldTerm = (Core__.undefinedTermError v0)}})
      Errors.ErrorUndefinedType v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedType"),
          Core.fieldTerm = (Core__.undefinedTypeError v0)}})
      Errors.ErrorUnexpectedTermVariant v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unexpectedTermVariant"),
          Core.fieldTerm = (Core__.unexpectedTermVariantError v0)}})
      Errors.ErrorUnexpectedTypeVariant v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unexpectedTypeVariant"),
          Core.fieldTerm = (Core__.unexpectedTypeVariantError v0)}})
      Errors.ErrorUnification v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unification"),
          Core.fieldTerm = (unificationError v0)}})

otherError :: Errors.OtherError -> Core.Term
otherError x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherError"),
      Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Errors.unOtherError x))})

unificationError :: Errors.UnificationError -> Core.Term
unificationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Core_.type_ (Errors.unificationErrorLeftType x))},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Core_.type_ (Errors.unificationErrorRightType x))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Errors.unificationErrorMessage x))}]})
