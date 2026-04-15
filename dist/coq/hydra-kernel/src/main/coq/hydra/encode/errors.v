(* Term encoders for hydra.errors *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.encode.core hydra.encode.error.checking hydra.encode.error.core hydra.encode.paths hydra.error.checking hydra.error.core hydra.errors.

Definition decodingError : forall (_ : DecodingError) , Term := fun (x : DecodingError) => (Term_Wrap) ((Build_WrappedTerm) ("hydra.errors.DecodingError"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun w_ => w_) (x)))).
Definition emptyListError (t0 : Type) : forall (_ : t0) , Term := fun (_ : t0) => (Term_Unit) (tt).
Arguments emptyListError {t0}.
Definition multipleBindingsError : forall (_ : MultipleBindingsError) , Term := fun (x : MultipleBindingsError) => (Term_Record) ((Build_Record_) ("hydra.errors.MultipleBindingsError"%string) ((cons) ((Build_Field) ("name"%string) ((hydra.encode.core.name) ((fun r_ => (multipleBindingsError_name) (r_)) (x)))) (nil))).
Definition multipleFieldsError : forall (_ : MultipleFieldsError) , Term := fun (x : MultipleFieldsError) => (Term_Record) ((Build_Record_) ("hydra.errors.MultipleFieldsError"%string) ((cons) ((Build_Field) ("fieldName"%string) ((hydra.encode.core.name) ((fun r_ => (multipleFieldsError_fieldName) (r_)) (x)))) (nil))).
Definition noMatchingFieldError : forall (_ : NoMatchingFieldError) , Term := fun (x : NoMatchingFieldError) => (Term_Record) ((Build_Record_) ("hydra.errors.NoMatchingFieldError"%string) ((cons) ((Build_Field) ("fieldName"%string) ((hydra.encode.core.name) ((fun r_ => (noMatchingFieldError_fieldName) (r_)) (x)))) (nil))).
Definition noSuchBindingError : forall (_ : NoSuchBindingError) , Term := fun (x : NoSuchBindingError) => (Term_Record) ((Build_Record_) ("hydra.errors.NoSuchBindingError"%string) ((cons) ((Build_Field) ("name"%string) ((hydra.encode.core.name) ((fun r_ => (noSuchBindingError_name) (r_)) (x)))) (nil))).
Definition notEnoughCasesError (t0 : Type) : forall (_ : t0) , Term := fun (_ : t0) => (Term_Unit) (tt).
Arguments notEnoughCasesError {t0}.
Definition unexpectedShapeError : forall (_ : UnexpectedShapeError) , Term := fun (x : UnexpectedShapeError) => (Term_Record) ((Build_Record_) ("hydra.errors.UnexpectedShapeError"%string) ((cons) ((Build_Field) ("expected"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (unexpectedShapeError_expected) (r_)) (x)))) ((cons) ((Build_Field) ("actual"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (unexpectedShapeError_actual) (r_)) (x)))) (nil)))).
Definition extractionError : forall (_ : ExtractionError) , Term := fun x_ => match x_ with
| ExtractionError_EmptyList v_ => (fun (y : EmptyListError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ExtractionError"%string) ((Build_Field) ("emptyList"%string) ((emptyListError) (y))))) (v_)
| ExtractionError_MultipleBindings v_ => (fun (y : MultipleBindingsError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ExtractionError"%string) ((Build_Field) ("multipleBindings"%string) ((multipleBindingsError) (y))))) (v_)
| ExtractionError_MultipleFields v_ => (fun (y : MultipleFieldsError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ExtractionError"%string) ((Build_Field) ("multipleFields"%string) ((multipleFieldsError) (y))))) (v_)
| ExtractionError_NoMatchingField v_ => (fun (y : NoMatchingFieldError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ExtractionError"%string) ((Build_Field) ("noMatchingField"%string) ((noMatchingFieldError) (y))))) (v_)
| ExtractionError_NoSuchBinding v_ => (fun (y : NoSuchBindingError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ExtractionError"%string) ((Build_Field) ("noSuchBinding"%string) ((noSuchBindingError) (y))))) (v_)
| ExtractionError_NotEnoughCases v_ => (fun (y : NotEnoughCasesError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ExtractionError"%string) ((Build_Field) ("notEnoughCases"%string) ((notEnoughCasesError) (y))))) (v_)
| ExtractionError_UnexpectedShape v_ => (fun (y : UnexpectedShapeError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ExtractionError"%string) ((Build_Field) ("unexpectedShape"%string) ((unexpectedShapeError) (y))))) (v_)
end.
Definition otherInferenceError : forall (_ : OtherInferenceError) , Term := fun (x : OtherInferenceError) => (Term_Record) ((Build_Record_) ("hydra.errors.OtherInferenceError"%string) ((cons) ((Build_Field) ("path"%string) ((hydra.encode.paths.subtermPath) ((fun r_ => (otherInferenceError_path) (r_)) (x)))) ((cons) ((Build_Field) ("message"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (otherInferenceError_message) (r_)) (x)))) (nil)))).
Definition unificationError : forall (_ : UnificationError) , Term := fun (x : UnificationError) => (Term_Record) ((Build_Record_) ("hydra.errors.UnificationError"%string) ((cons) ((Build_Field) ("leftType"%string) ((hydra.encode.core.type) ((fun r_ => (unificationError_leftType) (r_)) (x)))) ((cons) ((Build_Field) ("rightType"%string) ((hydra.encode.core.type) ((fun r_ => (unificationError_rightType) (r_)) (x)))) ((cons) ((Build_Field) ("message"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (unificationError_message) (r_)) (x)))) (nil))))).
Definition unificationInferenceError : forall (_ : UnificationInferenceError) , Term := fun (x : UnificationInferenceError) => (Term_Record) ((Build_Record_) ("hydra.errors.UnificationInferenceError"%string) ((cons) ((Build_Field) ("path"%string) ((hydra.encode.paths.subtermPath) ((fun r_ => (unificationInferenceError_path) (r_)) (x)))) ((cons) ((Build_Field) ("cause"%string) ((unificationError) ((fun r_ => (unificationInferenceError_cause) (r_)) (x)))) (nil)))).
Definition inferenceError : forall (_ : InferenceError) , Term := fun x_ => match x_ with
| InferenceError_Checking v_ => (fun (y : CheckingError) => (Term_Inject) ((Build_Injection) ("hydra.errors.InferenceError"%string) ((Build_Field) ("checking"%string) ((hydra.encode.error.checking.checkingError) (y))))) (v_)
| InferenceError_Other v_ => (fun (y : OtherInferenceError) => (Term_Inject) ((Build_Injection) ("hydra.errors.InferenceError"%string) ((Build_Field) ("other"%string) ((otherInferenceError) (y))))) (v_)
| InferenceError_Unification v_ => (fun (y : UnificationInferenceError) => (Term_Inject) ((Build_Injection) ("hydra.errors.InferenceError"%string) ((Build_Field) ("unification"%string) ((unificationInferenceError) (y))))) (v_)
end.
Definition otherError : forall (_ : OtherError) , Term := fun (x : OtherError) => (Term_Wrap) ((Build_WrappedTerm) ("hydra.errors.OtherError"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun w_ => w_) (x)))).
Definition noSuchPrimitiveError : forall (_ : NoSuchPrimitiveError) , Term := fun (x : NoSuchPrimitiveError) => (Term_Record) ((Build_Record_) ("hydra.errors.NoSuchPrimitiveError"%string) ((cons) ((Build_Field) ("name"%string) ((hydra.encode.core.name) ((fun r_ => (noSuchPrimitiveError_name) (r_)) (x)))) (nil))).
Definition otherResolutionError : forall (_ : OtherResolutionError) , Term := fun (x : OtherResolutionError) => (Term_Wrap) ((Build_WrappedTerm) ("hydra.errors.OtherResolutionError"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun w_ => w_) (x)))).
Definition resolutionError : forall (_ : ResolutionError) , Term := fun x_ => match x_ with
| ResolutionError_NoSuchBinding v_ => (fun (y : NoSuchBindingError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ResolutionError"%string) ((Build_Field) ("noSuchBinding"%string) ((noSuchBindingError) (y))))) (v_)
| ResolutionError_NoSuchPrimitive v_ => (fun (y : NoSuchPrimitiveError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ResolutionError"%string) ((Build_Field) ("noSuchPrimitive"%string) ((noSuchPrimitiveError) (y))))) (v_)
| ResolutionError_NoMatchingField v_ => (fun (y : NoMatchingFieldError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ResolutionError"%string) ((Build_Field) ("noMatchingField"%string) ((noMatchingFieldError) (y))))) (v_)
| ResolutionError_Other v_ => (fun (y : OtherResolutionError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ResolutionError"%string) ((Build_Field) ("other"%string) ((otherResolutionError) (y))))) (v_)
| ResolutionError_UnexpectedShape v_ => (fun (y : UnexpectedShapeError) => (Term_Inject) ((Build_Injection) ("hydra.errors.ResolutionError"%string) ((Build_Field) ("unexpectedShape"%string) ((unexpectedShapeError) (y))))) (v_)
end.
Definition error : forall (_ : Error) , Term := fun x_ => match x_ with
| Error_Checking v_ => (fun (y : CheckingError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("checking"%string) ((hydra.encode.error.checking.checkingError) (y))))) (v_)
| Error_Decoding v_ => (fun (y : DecodingError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("decoding"%string) ((decodingError) (y))))) (v_)
| Error_DuplicateBinding v_ => (fun (y : DuplicateBindingError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("duplicateBinding"%string) ((hydra.encode.error.core.duplicateBindingError) (y))))) (v_)
| Error_DuplicateField v_ => (fun (y : DuplicateFieldError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("duplicateField"%string) ((hydra.encode.error.core.duplicateFieldError) (y))))) (v_)
| Error_Extraction v_ => (fun (y : ExtractionError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("extraction"%string) ((extractionError) (y))))) (v_)
| Error_Inference v_ => (fun (y : InferenceError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("inference"%string) ((inferenceError) (y))))) (v_)
| Error_Other v_ => (fun (y : OtherError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("other"%string) ((otherError) (y))))) (v_)
| Error_Resolution v_ => (fun (y : ResolutionError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("resolution"%string) ((resolutionError) (y))))) (v_)
| Error_UndefinedField v_ => (fun (y : UndefinedFieldError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("undefinedField"%string) ((hydra.encode.error.core.undefinedFieldError) (y))))) (v_)
| Error_UndefinedTermVariable v_ => (fun (y : UndefinedTermVariableError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("undefinedTermVariable"%string) ((hydra.encode.error.core.undefinedTermVariableError) (y))))) (v_)
| Error_UntypedTermVariable v_ => (fun (y : UntypedTermVariableError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("untypedTermVariable"%string) ((hydra.encode.error.core.untypedTermVariableError) (y))))) (v_)
| Error_UnexpectedTermVariant v_ => (fun (y : UnexpectedTermVariantError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("unexpectedTermVariant"%string) ((hydra.encode.error.core.unexpectedTermVariantError) (y))))) (v_)
| Error_UnexpectedTypeVariant v_ => (fun (y : UnexpectedTypeVariantError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("unexpectedTypeVariant"%string) ((hydra.encode.error.core.unexpectedTypeVariantError) (y))))) (v_)
| Error_Unification v_ => (fun (y : UnificationError) => (Term_Inject) ((Build_Injection) ("hydra.errors.Error"%string) ((Build_Field) ("unification"%string) ((unificationError) (y))))) (v_)
end.

