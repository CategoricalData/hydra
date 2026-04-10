(* Error types for type checking *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.paths hydra.core hydra.variants hydra.typing.
Record UntypedTermVariableCheckingError : Type := Build_UntypedTermVariableCheckingError {
  untypedTermVariableCheckingError_path : SubtermPath ;
  untypedTermVariableCheckingError_name : Name
}.

Record UntypedLetBindingError : Type := Build_UntypedLetBindingError {
  untypedLetBindingError_binding : Binding
}.

Definition UntypedLambdaError : Type :=
  unit.

Record UnsupportedTermVariantError : Type := Build_UnsupportedTermVariantError {
  unsupportedTermVariantError_termVariant : TermVariant
}.

Record UnequalTypesError : Type := Build_UnequalTypesError {
  unequalTypesError_types : (list) (Type_) ;
  unequalTypesError_description : string
}.

Record UndefinedTermVariableCheckingError : Type := Build_UndefinedTermVariableCheckingError {
  undefinedTermVariableCheckingError_path : SubtermPath ;
  undefinedTermVariableCheckingError_name : Name
}.

Record UnboundTypeVariablesError : Type := Build_UnboundTypeVariablesError {
  unboundTypeVariablesError_variables : (list) (Name) ;
  unboundTypeVariablesError_type : Type_
}.

Record TypeMismatchError : Type := Build_TypeMismatchError {
  typeMismatchError_expectedType : Type_ ;
  typeMismatchError_actualType : Type_
}.

Record TypeArityMismatchError : Type := Build_TypeArityMismatchError {
  typeArityMismatchError_type : Type_ ;
  typeArityMismatchError_expectedArity : Z ;
  typeArityMismatchError_actualArity : Z ;
  typeArityMismatchError_typeArguments : (list) (Type_)
}.

Record OtherCheckingError : Type := Build_OtherCheckingError {
  otherCheckingError_path : SubtermPath ;
  otherCheckingError_message : string
}.

Record NotAFunctionTypeError : Type := Build_NotAFunctionTypeError {
  notAFunctionTypeError_type : Type_
}.

Record NotAForallTypeError : Type := Build_NotAForallTypeError {
  notAForallTypeError_type : Type_ ;
  notAForallTypeError_typeArguments : (list) (Type_)
}.

Record IncorrectUnificationError : Type := Build_IncorrectUnificationError {
  incorrectUnificationError_substitution : TypeSubst
}.

Inductive CheckingError : Type :=
| CheckingError_IncorrectUnification : IncorrectUnificationError -> CheckingError
| CheckingError_NotAForallType : NotAForallTypeError -> CheckingError
| CheckingError_NotAFunctionType : NotAFunctionTypeError -> CheckingError
| CheckingError_Other : OtherCheckingError -> CheckingError
| CheckingError_TypeArityMismatch : TypeArityMismatchError -> CheckingError
| CheckingError_TypeMismatch : TypeMismatchError -> CheckingError
| CheckingError_UnboundTypeVariables : UnboundTypeVariablesError -> CheckingError
| CheckingError_UndefinedTermVariable : UndefinedTermVariableCheckingError -> CheckingError
| CheckingError_UnequalTypes : UnequalTypesError -> CheckingError
| CheckingError_UnsupportedTermVariant : UnsupportedTermVariantError -> CheckingError
| CheckingError_UntypedLambda : UntypedLambdaError -> CheckingError
| CheckingError_UntypedLetBinding : UntypedLetBindingError -> CheckingError
| CheckingError_UntypedTermVariable : UntypedTermVariableCheckingError -> CheckingError.

