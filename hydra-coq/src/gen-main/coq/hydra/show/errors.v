(* String representations of hydra.error types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.error.checking hydra.lib.strings hydra.show.core hydra.show.variants hydra.errors hydra.formatting hydra.lib.lists hydra.lib.sets hydra.lib.literals hydra.show.typing hydra.show.error.core.

Definition untypedLetBindingError : UntypedLetBindingError -> string :=
  fun (e : UntypedLetBindingError) => let b := (fun r_ => (untypedLetBindingError_binding) (r_)) (e) in ((strings.cat2) ("untyped let binding: "%string)) ((hydra.show.core.binding) (b)).
Definition untypedLambdaError (t0 : Type) : t0 -> string :=
  fun (_ : t0) => "untyped lambda"%string.
Arguments untypedLambdaError {t0}.
Definition unsupportedTermVariantError : UnsupportedTermVariantError -> string :=
  fun (e : UnsupportedTermVariantError) => ((strings.cat2) ("unsupported term variant: "%string)) ((hydra.show.variants.termVariant) ((fun r_ => (unsupportedTermVariantError_termVariant) (r_)) (e))).
Definition unificationError : UnificationError -> string :=
  fun (e : UnificationError) => let rt := (fun r_ => (unificationError_rightType) (r_)) (e) in let msg := (fun r_ => (unificationError_message) (r_)) (e) in let lt := (fun r_ => (unificationError_leftType) (r_)) (e) in (strings.cat) ((cons) ("unification error: cannot unify "%string) ((cons) ((hydra.show.core.type) (lt)) ((cons) (" with "%string) ((cons) ((hydra.show.core.type) (rt)) ((cons) (": "%string) ((cons) (msg) (nil))))))).
Definition unequalTypesError : UnequalTypesError -> string :=
  fun (e : UnequalTypesError) => let types := (fun r_ => (unequalTypesError_types) (r_)) (e) in let desc := (fun r_ => (unequalTypesError_description) (r_)) (e) in (strings.cat) ((cons) ("unequal types "%string) ((cons) (((showList) (hydra.show.core.type)) (types)) ((cons) (" in "%string) ((cons) (desc) (nil))))).
Definition unboundTypeVariablesError : UnboundTypeVariablesError -> string :=
  fun (e : UnboundTypeVariablesError) => let vars := (fun r_ => (unboundTypeVariablesError_variables) (r_)) (e) in let typ := (fun r_ => (unboundTypeVariablesError_type) (r_)) (e) in (strings.cat) ((cons) ("unbound type variables: {"%string) ((cons) (((strings.intercalate) (", "%string)) (((lists.map) (fun w_ => w_)) ((sets.toList) (vars)))) ((cons) ("} in type "%string) ((cons) ((hydra.show.core.type) (typ)) (nil))))).
Definition typeMismatchError : TypeMismatchError -> string :=
  fun (e : TypeMismatchError) => let expected := (fun r_ => (typeMismatchError_expectedType) (r_)) (e) in let actual := (fun r_ => (typeMismatchError_actualType) (r_)) (e) in (strings.cat) ((cons) ("type mismatch: expected "%string) ((cons) ((hydra.show.core.type) (expected)) ((cons) (" but found "%string) ((cons) ((hydra.show.core.type) (actual)) (nil))))).
Definition typeArityMismatchError : TypeArityMismatchError -> string :=
  fun (e : TypeArityMismatchError) => let typ := (fun r_ => (typeArityMismatchError_type) (r_)) (e) in let expected := (fun r_ => (typeArityMismatchError_expectedArity) (r_)) (e) in let args := (fun r_ => (typeArityMismatchError_typeArguments) (r_)) (e) in let actual := (fun r_ => (typeArityMismatchError_actualArity) (r_)) (e) in (strings.cat) ((cons) ("type "%string) ((cons) ((hydra.show.core.type) (typ)) ((cons) (" applied to the wrong number of type arguments (expected "%string) ((cons) ((literals.showInt32) (expected)) ((cons) (", got "%string) ((cons) ((literals.showInt32) (actual)) ((cons) ("): "%string) ((cons) (((showList) (hydra.show.core.type)) (args)) (nil))))))))).
Definition otherError : OtherError -> string :=
  fun (oe : OtherError) => (fun w_ => w_) (oe).
Definition notAFunctionTypeError : NotAFunctionTypeError -> string :=
  fun (e : NotAFunctionTypeError) => let typ := (fun r_ => (notAFunctionTypeError_type) (r_)) (e) in ((strings.cat2) ("not a function type: "%string)) ((hydra.show.core.type) (typ)).
Definition notAForallTypeError : NotAForallTypeError -> string :=
  fun (e : NotAForallTypeError) => let typ := (fun r_ => (notAForallTypeError_type) (r_)) (e) in let args := (fun r_ => (notAForallTypeError_typeArguments) (r_)) (e) in (strings.cat) ((cons) ("not a forall type: "%string) ((cons) ((hydra.show.core.type) (typ)) ((cons) (". Trying to apply "%string) ((cons) ((literals.showInt32) ((lists.length) (args))) ((cons) (" type argument(s): "%string) ((cons) (((showList) (hydra.show.core.type)) (args)) (nil))))))).
Definition incorrectUnificationError : IncorrectUnificationError -> string :=
  fun (e : IncorrectUnificationError) => let subst := (fun r_ => (incorrectUnificationError_substitution) (r_)) (e) in ((strings.cat2) ("incorrect unification: "%string)) ((hydra.show.typing.typeSubst) (subst)).
Definition decodingError : DecodingError -> string :=
  fun (de : DecodingError) => ((strings.cat2) ("decoding error: "%string)) ((fun w_ => w_) (de)).
Definition checkingError : CheckingError -> string :=
  fun (ce : CheckingError) => (fun x_ => match x_ with
| CheckingError_IncorrectUnification v_ => (incorrectUnificationError) (v_)
| CheckingError_NotAForallType v_ => (notAForallTypeError) (v_)
| CheckingError_NotAFunctionType v_ => (notAFunctionTypeError) (v_)
| CheckingError_TypeArityMismatch v_ => (typeArityMismatchError) (v_)
| CheckingError_TypeMismatch v_ => (typeMismatchError) (v_)
| CheckingError_UnboundTypeVariables v_ => (unboundTypeVariablesError) (v_)
| CheckingError_UnequalTypes v_ => (unequalTypesError) (v_)
| CheckingError_UnsupportedTermVariant v_ => (unsupportedTermVariantError) (v_)
| CheckingError_UntypedLambda v_ => (untypedLambdaError) (v_)
| CheckingError_UntypedLetBinding v_ => (untypedLetBindingError) (v_)
| _ => hydra_unreachable
end) (ce).
Definition error : Error -> string :=
  fun (e : Error) => (fun x_ => match x_ with
| Error_Checking v_ => (checkingError) (v_)
| Error_Decoding v_ => (decodingError) (v_)
| Error_DuplicateBinding v_ => (hydra.show.error.core.duplicateBindingError) (v_)
| Error_DuplicateField v_ => (hydra.show.error.core.duplicateFieldError) (v_)
| Error_Other v_ => (otherError) (v_)
| Error_UndefinedField v_ => (hydra.show.error.core.undefinedFieldError) (v_)
| Error_UndefinedTermVariable v_ => (hydra.show.error.core.undefinedTermVariableError) (v_)
| Error_UntypedTermVariable v_ => (hydra.show.error.core.untypedTermVariableError) (v_)
| Error_UnexpectedTermVariant v_ => (hydra.show.error.core.unexpectedTermVariantError) (v_)
| Error_UnexpectedTypeVariant v_ => (hydra.show.error.core.unexpectedTypeVariantError) (v_)
| Error_Unification v_ => (unificationError) (v_)
| _ => hydra_unreachable
end) (e).

