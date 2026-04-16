(* Types supporting type inference and type reconstruction. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.context hydra.core.
Record FunctionStructure (env : Type) : Type := Build_FunctionStructure {
functionStructure_typeParams : (list) (Name) ;
functionStructure_params : (list) (Name) ;
functionStructure_bindings : (list) (Binding) ;
functionStructure_body : Term ;
functionStructure_domains : (list) (Type_) ;
functionStructure_codomain : (option) (Type_) ;
functionStructure_environment : env ;
}.

Definition TypeSubst : Type := (list) ((prod) (Name) (Type_)).

Record InferenceResult : Type := Build_InferenceResult {
inferenceResult_term : Term ;
inferenceResult_type : Type_ ;
inferenceResult_subst : TypeSubst ;
inferenceResult_classConstraints : (list) ((prod) (Name) (TypeVariableMetadata)) ;
inferenceResult_context : Context_ ;
}.

Definition TermSubst : Type := (list) ((prod) (Name) (Term)).

Record TypeConstraint : Type := Build_TypeConstraint {
typeConstraint_left : Type_ ;
typeConstraint_right : Type_ ;
typeConstraint_comment : string ;
}.

Arguments Build_FunctionStructure {env}.
Arguments functionStructure_typeParams {env}.
Arguments functionStructure_params {env}.
Arguments functionStructure_bindings {env}.
Arguments functionStructure_body {env}.
Arguments functionStructure_domains {env}.
Arguments functionStructure_codomain {env}.
Arguments functionStructure_environment {env}.

