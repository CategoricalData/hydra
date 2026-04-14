(* Top-level error types for the Hydra kernel *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.paths hydra.error.checking hydra.error.core.
Record UnificationError : Type := Build_UnificationError {
unificationError_leftType : Type_ ;
unificationError_rightType : Type_ ;
unificationError_message : string ;
}.

Record UnificationInferenceError : Type := Build_UnificationInferenceError {
unificationInferenceError_path : SubtermPath ;
unificationInferenceError_cause : UnificationError ;
}.

Record UnexpectedShapeError : Type := Build_UnexpectedShapeError {
unexpectedShapeError_expected : string ;
unexpectedShapeError_actual : string ;
}.

Definition OtherResolutionError : Type := string.

Record OtherInferenceError : Type := Build_OtherInferenceError {
otherInferenceError_path : SubtermPath ;
otherInferenceError_message : string ;
}.

Definition OtherError : Type := string.

Definition NotEnoughCasesError : Type := unit.

Record NoSuchPrimitiveError : Type := Build_NoSuchPrimitiveError {
noSuchPrimitiveError_name : Name ;
}.

Record NoSuchBindingError : Type := Build_NoSuchBindingError {
noSuchBindingError_name : Name ;
}.

Record NoMatchingFieldError : Type := Build_NoMatchingFieldError {
noMatchingFieldError_fieldName : Name ;
}.

Inductive ResolutionError : Type :=
| ResolutionError_NoSuchBinding : forall (_ : NoSuchBindingError) , ResolutionError
| ResolutionError_NoSuchPrimitive : forall (_ : NoSuchPrimitiveError) , ResolutionError
| ResolutionError_NoMatchingField : forall (_ : NoMatchingFieldError) , ResolutionError
| ResolutionError_Other : forall (_ : OtherResolutionError) , ResolutionError
| ResolutionError_UnexpectedShape : forall (_ : UnexpectedShapeError) , ResolutionError.

Record MultipleFieldsError : Type := Build_MultipleFieldsError {
multipleFieldsError_fieldName : Name ;
}.

Record MultipleBindingsError : Type := Build_MultipleBindingsError {
multipleBindingsError_name : Name ;
}.

Inductive InferenceError : Type :=
| InferenceError_Checking : forall (_ : CheckingError) , InferenceError
| InferenceError_Other : forall (_ : OtherInferenceError) , InferenceError
| InferenceError_Unification : forall (_ : UnificationInferenceError) , InferenceError.

Definition EmptyListError : Type := unit.

Inductive ExtractionError : Type :=
| ExtractionError_EmptyList : forall (_ : EmptyListError) , ExtractionError
| ExtractionError_MultipleBindings : forall (_ : MultipleBindingsError) , ExtractionError
| ExtractionError_MultipleFields : forall (_ : MultipleFieldsError) , ExtractionError
| ExtractionError_NoMatchingField : forall (_ : NoMatchingFieldError) , ExtractionError
| ExtractionError_NoSuchBinding : forall (_ : NoSuchBindingError) , ExtractionError
| ExtractionError_NotEnoughCases : forall (_ : NotEnoughCasesError) , ExtractionError
| ExtractionError_UnexpectedShape : forall (_ : UnexpectedShapeError) , ExtractionError.

Definition DecodingError : Type := string.

Inductive Error : Type :=
| Error_Checking : forall (_ : CheckingError) , Error
| Error_Decoding : forall (_ : DecodingError) , Error
| Error_DuplicateBinding : forall (_ : DuplicateBindingError) , Error
| Error_DuplicateField : forall (_ : DuplicateFieldError) , Error
| Error_Extraction : forall (_ : ExtractionError) , Error
| Error_Inference : forall (_ : InferenceError) , Error
| Error_Other : forall (_ : OtherError) , Error
| Error_Resolution : forall (_ : ResolutionError) , Error
| Error_UndefinedField : forall (_ : UndefinedFieldError) , Error
| Error_UndefinedTermVariable : forall (_ : UndefinedTermVariableError) , Error
| Error_UntypedTermVariable : forall (_ : UntypedTermVariableError) , Error
| Error_UnexpectedTermVariant : forall (_ : UnexpectedTermVariantError) , Error
| Error_UnexpectedTypeVariant : forall (_ : UnexpectedTypeVariantError) , Error
| Error_Unification : forall (_ : UnificationError) , Error.

