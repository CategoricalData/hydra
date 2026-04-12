(* Top-level error types for the Hydra kernel *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.paths hydra.error.checking hydra.error.core.
Record UnificationError : Type := Build_UnificationError {
  unificationError_leftType : Type_ ;
  unificationError_rightType : Type_ ;
  unificationError_message : string
}.

Record UnificationInferenceError : Type := Build_UnificationInferenceError {
  unificationInferenceError_path : SubtermPath ;
  unificationInferenceError_cause : UnificationError
}.

Record UnexpectedShapeError : Type := Build_UnexpectedShapeError {
  unexpectedShapeError_expected : string ;
  unexpectedShapeError_actual : string
}.

Definition OtherResolutionError : Type :=
  string.

Record OtherInferenceError : Type := Build_OtherInferenceError {
  otherInferenceError_path : SubtermPath ;
  otherInferenceError_message : string
}.

Definition OtherError : Type :=
  string.

Definition NotEnoughCasesError : Type :=
  unit.

Record NoSuchPrimitiveError : Type := Build_NoSuchPrimitiveError {
  noSuchPrimitiveError_name : Name
}.

Record NoSuchBindingError : Type := Build_NoSuchBindingError {
  noSuchBindingError_name : Name
}.

Record NoMatchingFieldError : Type := Build_NoMatchingFieldError {
  noMatchingFieldError_fieldName : Name
}.

Inductive ResolutionError : Type :=
| ResolutionError_NoSuchBinding : NoSuchBindingError -> ResolutionError
| ResolutionError_NoSuchPrimitive : NoSuchPrimitiveError -> ResolutionError
| ResolutionError_NoMatchingField : NoMatchingFieldError -> ResolutionError
| ResolutionError_Other : OtherResolutionError -> ResolutionError
| ResolutionError_UnexpectedShape : UnexpectedShapeError -> ResolutionError.

Record MultipleFieldsError : Type := Build_MultipleFieldsError {
  multipleFieldsError_fieldName : Name
}.

Record MultipleBindingsError : Type := Build_MultipleBindingsError {
  multipleBindingsError_name : Name
}.

Inductive InferenceError : Type :=
| InferenceError_Checking : CheckingError -> InferenceError
| InferenceError_Other : OtherInferenceError -> InferenceError
| InferenceError_Unification : UnificationInferenceError -> InferenceError.

Definition EmptyListError : Type :=
  unit.

Inductive ExtractionError : Type :=
| ExtractionError_EmptyList : EmptyListError -> ExtractionError
| ExtractionError_MultipleBindings : MultipleBindingsError -> ExtractionError
| ExtractionError_MultipleFields : MultipleFieldsError -> ExtractionError
| ExtractionError_NoMatchingField : NoMatchingFieldError -> ExtractionError
| ExtractionError_NoSuchBinding : NoSuchBindingError -> ExtractionError
| ExtractionError_NotEnoughCases : NotEnoughCasesError -> ExtractionError
| ExtractionError_UnexpectedShape : UnexpectedShapeError -> ExtractionError.

Definition DecodingError : Type :=
  string.

Inductive Error : Type :=
| Error_Checking : CheckingError -> Error
| Error_Decoding : DecodingError -> Error
| Error_DuplicateBinding : DuplicateBindingError -> Error
| Error_DuplicateField : DuplicateFieldError -> Error
| Error_Extraction : ExtractionError -> Error
| Error_Inference : InferenceError -> Error
| Error_Other : OtherError -> Error
| Error_Resolution : ResolutionError -> Error
| Error_UndefinedField : UndefinedFieldError -> Error
| Error_UndefinedTermVariable : UndefinedTermVariableError -> Error
| Error_UntypedTermVariable : UntypedTermVariableError -> Error
| Error_UnexpectedTermVariant : UnexpectedTermVariantError -> Error
| Error_UnexpectedTypeVariant : UnexpectedTypeVariantError -> Error
| Error_Unification : UnificationError -> Error.

