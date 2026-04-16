(* Error types for type checking *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.paths hydra.typing hydra.variants.
Record IncorrectUnificationError : Type := Build_IncorrectUnificationError {
incorrectUnificationError_substitution : TypeSubst ;
}.

Record NotAForallTypeError : Type := Build_NotAForallTypeError {
notAForallTypeError_type : Type_ ;
notAForallTypeError_typeArguments : (list) (Type_) ;
}.

Record NotAFunctionTypeError : Type := Build_NotAFunctionTypeError {
notAFunctionTypeError_type : Type_ ;
}.

Record OtherCheckingError : Type := Build_OtherCheckingError {
otherCheckingError_path : SubtermPath ;
otherCheckingError_message : string ;
}.

Record TypeArityMismatchError : Type := Build_TypeArityMismatchError {
typeArityMismatchError_type : Type_ ;
typeArityMismatchError_expectedArity : Z ;
typeArityMismatchError_actualArity : Z ;
typeArityMismatchError_typeArguments : (list) (Type_) ;
}.

Record TypeMismatchError : Type := Build_TypeMismatchError {
typeMismatchError_expectedType : Type_ ;
typeMismatchError_actualType : Type_ ;
}.

Record UnboundTypeVariablesError : Type := Build_UnboundTypeVariablesError {
unboundTypeVariablesError_variables : (list) (Name) ;
unboundTypeVariablesError_type : Type_ ;
}.

Record UndefinedTermVariableCheckingError : Type := Build_UndefinedTermVariableCheckingError {
undefinedTermVariableCheckingError_path : SubtermPath ;
undefinedTermVariableCheckingError_name : Name ;
}.

Record UnequalTypesError : Type := Build_UnequalTypesError {
unequalTypesError_types : (list) (Type_) ;
unequalTypesError_description : string ;
}.

Record UnsupportedTermVariantError : Type := Build_UnsupportedTermVariantError {
unsupportedTermVariantError_termVariant : TermVariant ;
}.

Definition UntypedLambdaError : Type := unit.

Record UntypedLetBindingError : Type := Build_UntypedLetBindingError {
untypedLetBindingError_binding : Binding ;
}.

Record UntypedTermVariableCheckingError : Type := Build_UntypedTermVariableCheckingError {
untypedTermVariableCheckingError_path : SubtermPath ;
untypedTermVariableCheckingError_name : Name ;
}.

Inductive CheckingError : Type :=
| CheckingError_IncorrectUnification : forall (_ : IncorrectUnificationError) , CheckingError
| CheckingError_NotAForallType : forall (_ : NotAForallTypeError) , CheckingError
| CheckingError_NotAFunctionType : forall (_ : NotAFunctionTypeError) , CheckingError
| CheckingError_Other : forall (_ : OtherCheckingError) , CheckingError
| CheckingError_TypeArityMismatch : forall (_ : TypeArityMismatchError) , CheckingError
| CheckingError_TypeMismatch : forall (_ : TypeMismatchError) , CheckingError
| CheckingError_UnboundTypeVariables : forall (_ : UnboundTypeVariablesError) , CheckingError
| CheckingError_UndefinedTermVariable : forall (_ : UndefinedTermVariableCheckingError) , CheckingError
| CheckingError_UnequalTypes : forall (_ : UnequalTypesError) , CheckingError
| CheckingError_UnsupportedTermVariant : forall (_ : UnsupportedTermVariantError) , CheckingError
| CheckingError_UntypedLambda : forall (_ : UntypedLambdaError) , CheckingError
| CheckingError_UntypedLetBinding : forall (_ : UntypedLetBindingError) , CheckingError
| CheckingError_UntypedTermVariable : forall (_ : UntypedTermVariableCheckingError) , CheckingError.

