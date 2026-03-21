-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.error.core

module Hydra.Encode.Error.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Accessors as Accessors
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Encode.Variants as Variants
import qualified Hydra.Error.Core as Core__
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

duplicateBindingError :: Core__.DuplicateBindingError -> Core.Term
duplicateBindingError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.duplicateBindingErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.duplicateBindingErrorName x))}]})

duplicateFieldError :: Core__.DuplicateFieldError -> Core.Term
duplicateFieldError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Accessors.accessorPath (Core__.duplicateFieldErrorLocation x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.duplicateFieldErrorName x))}]})

invalidTermError :: Core__.InvalidTermError -> Core.Term
invalidTermError x =
    case x of
      Core__.InvalidTermErrorDuplicateBinding v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateBinding"),
          Core.fieldTerm = (duplicateBindingError v0)}})
      Core__.InvalidTermErrorDuplicateField v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateField"),
          Core.fieldTerm = (duplicateFieldError v0)}})

undefinedFieldError :: Core__.UndefinedFieldError -> Core.Term
undefinedFieldError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Core_.name (Core__.undefinedFieldErrorFieldName x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core_.name (Core__.undefinedFieldErrorTypeName x))}]})

undefinedTermError :: Core__.UndefinedTermError -> Core.Term
undefinedTermError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.undefinedTermErrorName x))}]})

undefinedTypeError :: Core__.UndefinedTypeError -> Core.Term
undefinedTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Core__.undefinedTypeErrorName x))}]})

unexpectedTermVariantError :: Core__.UnexpectedTermVariantError -> Core.Term
unexpectedTermVariantError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Variants.termVariant (Core__.unexpectedTermVariantErrorExpectedVariant x))},
        Core.Field {
          Core.fieldName = (Core.Name "actualTerm"),
          Core.fieldTerm = (Core_.term (Core__.unexpectedTermVariantErrorActualTerm x))}]})

unexpectedTypeVariantError :: Core__.UnexpectedTypeVariantError -> Core.Term
unexpectedTypeVariantError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Variants.typeVariant (Core__.unexpectedTypeVariantErrorExpectedVariant x))},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Core_.type_ (Core__.unexpectedTypeVariantErrorActualType x))}]})
