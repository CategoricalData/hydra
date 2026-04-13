(* Term encoders for hydra.error.checking *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.error.checking hydra.core hydra.encode.paths hydra.encode.core hydra.encode.variants hydra.lib.lists hydra.lib.sets hydra.encode.typing.

Definition untypedTermVariableCheckingError : UntypedTermVariableCheckingError -> Term :=
  fun (x : UntypedTermVariableCheckingError) => (Term_Record) ((Build_Record_) ("UntypedTermVariableCheckingError"%string) ((cons) ((Build_Field) ("path"%string) ((hydra.encode.paths.subtermPath) ((fun r_ => (untypedTermVariableCheckingError_path) (r_)) (x)))) ((cons) ((Build_Field) ("name"%string) ((hydra.encode.core.name) ((fun r_ => (untypedTermVariableCheckingError_name) (r_)) (x)))) (nil)))).
Definition untypedLetBindingError : UntypedLetBindingError -> Term :=
  fun (x : UntypedLetBindingError) => (Term_Record) ((Build_Record_) ("UntypedLetBindingError"%string) ((cons) ((Build_Field) ("binding"%string) ((hydra.encode.core.binding) ((fun r_ => (untypedLetBindingError_binding) (r_)) (x)))) (nil))).
Definition untypedLambdaError (t0 : Type) : t0 -> Term :=
  fun (x : t0) => (Term_Record) ((Build_Record_) ("UntypedLambdaError"%string) (nil)).
Arguments untypedLambdaError {t0}.
Definition unsupportedTermVariantError : UnsupportedTermVariantError -> Term :=
  fun (x : UnsupportedTermVariantError) => (Term_Record) ((Build_Record_) ("UnsupportedTermVariantError"%string) ((cons) ((Build_Field) ("termVariant"%string) ((hydra.encode.variants.termVariant) ((fun r_ => (unsupportedTermVariantError_termVariant) (r_)) (x)))) (nil))).
Definition unequalTypesError : UnequalTypesError -> Term :=
  fun (x : UnequalTypesError) => (Term_Record) ((Build_Record_) ("UnequalTypesError"%string) ((cons) ((Build_Field) ("types"%string) ((fun (xs : (list) (Type_)) => (Term_List) (((lists.map) (hydra.encode.core.type)) (xs))) ((fun r_ => (unequalTypesError_types) (r_)) (x)))) ((cons) ((Build_Field) ("description"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (unequalTypesError_description) (r_)) (x)))) (nil)))).
Definition undefinedTermVariableCheckingError : UndefinedTermVariableCheckingError -> Term :=
  fun (x : UndefinedTermVariableCheckingError) => (Term_Record) ((Build_Record_) ("UndefinedTermVariableCheckingError"%string) ((cons) ((Build_Field) ("path"%string) ((hydra.encode.paths.subtermPath) ((fun r_ => (undefinedTermVariableCheckingError_path) (r_)) (x)))) ((cons) ((Build_Field) ("name"%string) ((hydra.encode.core.name) ((fun r_ => (undefinedTermVariableCheckingError_name) (r_)) (x)))) (nil)))).
Definition unboundTypeVariablesError : UnboundTypeVariablesError -> Term :=
  fun (x : UnboundTypeVariablesError) => (Term_Record) ((Build_Record_) ("UnboundTypeVariablesError"%string) ((cons) ((Build_Field) ("variables"%string) ((fun (s : (list) (Name)) => (Term_Set) (((sets.map) (hydra.encode.core.name)) (s))) ((fun r_ => (unboundTypeVariablesError_variables) (r_)) (x)))) ((cons) ((Build_Field) ("type"%string) ((hydra.encode.core.type) ((fun r_ => (unboundTypeVariablesError_type) (r_)) (x)))) (nil)))).
Definition typeMismatchError : TypeMismatchError -> Term :=
  fun (x : TypeMismatchError) => (Term_Record) ((Build_Record_) ("TypeMismatchError"%string) ((cons) ((Build_Field) ("expectedType"%string) ((hydra.encode.core.type) ((fun r_ => (typeMismatchError_expectedType) (r_)) (x)))) ((cons) ((Build_Field) ("actualType"%string) ((hydra.encode.core.type) ((fun r_ => (typeMismatchError_actualType) (r_)) (x)))) (nil)))).
Definition typeArityMismatchError : TypeArityMismatchError -> Term :=
  fun (x : TypeArityMismatchError) => (Term_Record) ((Build_Record_) ("TypeArityMismatchError"%string) ((cons) ((Build_Field) ("type"%string) ((hydra.encode.core.type) ((fun r_ => (typeArityMismatchError_type) (r_)) (x)))) ((cons) ((Build_Field) ("expectedArity"%string) ((fun (x2 : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x2)))) ((fun r_ => (typeArityMismatchError_expectedArity) (r_)) (x)))) ((cons) ((Build_Field) ("actualArity"%string) ((fun (x2 : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x2)))) ((fun r_ => (typeArityMismatchError_actualArity) (r_)) (x)))) ((cons) ((Build_Field) ("typeArguments"%string) ((fun (xs : (list) (Type_)) => (Term_List) (((lists.map) (hydra.encode.core.type)) (xs))) ((fun r_ => (typeArityMismatchError_typeArguments) (r_)) (x)))) (nil)))))).
Definition otherCheckingError : OtherCheckingError -> Term :=
  fun (x : OtherCheckingError) => (Term_Record) ((Build_Record_) ("OtherCheckingError"%string) ((cons) ((Build_Field) ("path"%string) ((hydra.encode.paths.subtermPath) ((fun r_ => (otherCheckingError_path) (r_)) (x)))) ((cons) ((Build_Field) ("message"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (otherCheckingError_message) (r_)) (x)))) (nil)))).
Definition notAFunctionTypeError : NotAFunctionTypeError -> Term :=
  fun (x : NotAFunctionTypeError) => (Term_Record) ((Build_Record_) ("NotAFunctionTypeError"%string) ((cons) ((Build_Field) ("type"%string) ((hydra.encode.core.type) ((fun r_ => (notAFunctionTypeError_type) (r_)) (x)))) (nil))).
Definition notAForallTypeError : NotAForallTypeError -> Term :=
  fun (x : NotAForallTypeError) => (Term_Record) ((Build_Record_) ("NotAForallTypeError"%string) ((cons) ((Build_Field) ("type"%string) ((hydra.encode.core.type) ((fun r_ => (notAForallTypeError_type) (r_)) (x)))) ((cons) ((Build_Field) ("typeArguments"%string) ((fun (xs : (list) (Type_)) => (Term_List) (((lists.map) (hydra.encode.core.type)) (xs))) ((fun r_ => (notAForallTypeError_typeArguments) (r_)) (x)))) (nil)))).
Definition incorrectUnificationError : IncorrectUnificationError -> Term :=
  fun (x : IncorrectUnificationError) => (Term_Record) ((Build_Record_) ("IncorrectUnificationError"%string) ((cons) ((Build_Field) ("substitution"%string) ((hydra.encode.typing.typeSubst) ((fun r_ => (incorrectUnificationError_substitution) (r_)) (x)))) (nil))).
Definition checkingError : CheckingError -> Term :=
  fun x_ => match x_ with
| CheckingError_IncorrectUnification v_ => (fun (y : IncorrectUnificationError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("incorrectUnification"%string) ((incorrectUnificationError) (y))))) (v_)
| CheckingError_NotAForallType v_ => (fun (y : NotAForallTypeError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("notAForallType"%string) ((notAForallTypeError) (y))))) (v_)
| CheckingError_NotAFunctionType v_ => (fun (y : NotAFunctionTypeError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("notAFunctionType"%string) ((notAFunctionTypeError) (y))))) (v_)
| CheckingError_Other v_ => (fun (y : OtherCheckingError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("other"%string) ((otherCheckingError) (y))))) (v_)
| CheckingError_TypeArityMismatch v_ => (fun (y : TypeArityMismatchError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("typeArityMismatch"%string) ((typeArityMismatchError) (y))))) (v_)
| CheckingError_TypeMismatch v_ => (fun (y : TypeMismatchError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("typeMismatch"%string) ((typeMismatchError) (y))))) (v_)
| CheckingError_UnboundTypeVariables v_ => (fun (y : UnboundTypeVariablesError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("unboundTypeVariables"%string) ((unboundTypeVariablesError) (y))))) (v_)
| CheckingError_UndefinedTermVariable v_ => (fun (y : UndefinedTermVariableCheckingError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("undefinedTermVariable"%string) ((undefinedTermVariableCheckingError) (y))))) (v_)
| CheckingError_UnequalTypes v_ => (fun (y : UnequalTypesError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("unequalTypes"%string) ((unequalTypesError) (y))))) (v_)
| CheckingError_UnsupportedTermVariant v_ => (fun (y : UnsupportedTermVariantError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("unsupportedTermVariant"%string) ((unsupportedTermVariantError) (y))))) (v_)
| CheckingError_UntypedLambda v_ => (fun (y : UntypedLambdaError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("untypedLambda"%string) ((untypedLambdaError) (y))))) (v_)
| CheckingError_UntypedLetBinding v_ => (fun (y : UntypedLetBindingError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("untypedLetBinding"%string) ((untypedLetBindingError) (y))))) (v_)
| CheckingError_UntypedTermVariable v_ => (fun (y : UntypedTermVariableCheckingError) => (Term_Union) ((Build_Injection) ("CheckingError"%string) ((Build_Field) ("untypedTermVariable"%string) ((untypedTermVariableCheckingError) (y))))) (v_)
end.

