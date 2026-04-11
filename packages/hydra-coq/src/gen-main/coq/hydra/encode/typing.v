(* Term encoders for hydra.typing *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.typing hydra.core hydra.lib.maps hydra.encode.core hydra.encode.context hydra.lib.lists hydra.lib.maybes.

Definition typeSubst : TypeSubst -> Term :=
  fun (x : TypeSubst) => (Term_Wrap) ((Build_WrappedTerm) ("TypeSubst"%string) ((fun (m : (list) ((prod) (Name) (Type_))) => (Term_Map) ((((maps.bimap) (hydra.encode.core.name)) (hydra.encode.core.type)) (m))) ((fun w_ => w_) (x)))).
Definition typeConstraint : TypeConstraint -> Term :=
  fun (x : TypeConstraint) => (Term_Record) ((Build_Record_) ("TypeConstraint"%string) ((cons) ((Build_Field) ("left"%string) ((hydra.encode.core.type) ((fun r_ => (typeConstraint_left) (r_)) (x)))) ((cons) ((Build_Field) ("right"%string) ((hydra.encode.core.type) ((fun r_ => (typeConstraint_right) (r_)) (x)))) ((cons) ((Build_Field) ("comment"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (typeConstraint_comment) (r_)) (x)))) (nil))))).
Definition termSubst : TermSubst -> Term :=
  fun (x : TermSubst) => (Term_Wrap) ((Build_WrappedTerm) ("TermSubst"%string) ((fun (m : (list) ((prod) (Name) (Term))) => (Term_Map) ((((maps.bimap) (hydra.encode.core.name)) (hydra.encode.core.term)) (m))) ((fun w_ => w_) (x)))).
Definition inferenceResult : InferenceResult -> Term :=
  fun (x : InferenceResult) => (Term_Record) ((Build_Record_) ("InferenceResult"%string) ((cons) ((Build_Field) ("term"%string) ((hydra.encode.core.term) ((fun r_ => (inferenceResult_term) (r_)) (x)))) ((cons) ((Build_Field) ("type"%string) ((hydra.encode.core.type) ((fun r_ => (inferenceResult_type) (r_)) (x)))) ((cons) ((Build_Field) ("subst"%string) ((typeSubst) ((fun r_ => (inferenceResult_subst) (r_)) (x)))) ((cons) ((Build_Field) ("classConstraints"%string) ((fun (m : (list) ((prod) (Name) (TypeVariableMetadata))) => (Term_Map) ((((maps.bimap) (hydra.encode.core.name)) (hydra.encode.core.typeVariableMetadata)) (m))) ((fun r_ => (inferenceResult_classConstraints) (r_)) (x)))) ((cons) ((Build_Field) ("context"%string) ((hydra.encode.context.context) ((fun r_ => (inferenceResult_context) (r_)) (x)))) (nil))))))).
Definition functionStructure (t0 : Type) : (t0 -> Term) -> (FunctionStructure) (t0) -> Term :=
  fun (env : t0 -> Term) => fun (x : (FunctionStructure) (t0)) => (Term_Record) ((Build_Record_) ("FunctionStructure"%string) ((cons) ((Build_Field) ("typeParams"%string) ((fun (xs : (list) (Name)) => (Term_List) (((lists.map) (hydra.encode.core.name)) (xs))) ((fun r_ => (functionStructure_typeParams) (r_)) (x)))) ((cons) ((Build_Field) ("params"%string) ((fun (xs : (list) (Name)) => (Term_List) (((lists.map) (hydra.encode.core.name)) (xs))) ((fun r_ => (functionStructure_params) (r_)) (x)))) ((cons) ((Build_Field) ("bindings"%string) ((fun (xs : (list) (Binding)) => (Term_List) (((lists.map) (hydra.encode.core.binding)) (xs))) ((fun r_ => (functionStructure_bindings) (r_)) (x)))) ((cons) ((Build_Field) ("body"%string) ((hydra.encode.core.term) ((fun r_ => (functionStructure_body) (r_)) (x)))) ((cons) ((Build_Field) ("domains"%string) ((fun (xs : (list) (Type_)) => (Term_List) (((lists.map) (hydra.encode.core.type)) (xs))) ((fun r_ => (functionStructure_domains) (r_)) (x)))) ((cons) ((Build_Field) ("codomain"%string) ((fun (opt : (option) (Type_)) => (Term_Maybe) (((maybes.map) (hydra.encode.core.type)) (opt))) ((fun r_ => (functionStructure_codomain) (r_)) (x)))) ((cons) ((Build_Field) ("environment"%string) ((env) ((fun r_ => (functionStructure_environment) (r_)) (x)))) (nil))))))))).
Arguments functionStructure {t0}.

